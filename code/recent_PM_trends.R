rm(list = ls())

# Load Packages ------------
packages <- c("tidyverse", "aws.s3", "arrow", "lubridate", "writexl", "ggrepel", "readr", "showtext", "ggtext", "scales")


# Check if packages needed are installed, if they are then load, if not install
for (package in packages) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package)
        library(package, character.only = TRUE)
    }
}

source("code/parameters.R") # Loads latest_year, data_capture_threshold and folder locations
source("code/functions.R") # Loads compute_pei() and build_cohort() functions for PERT
source("code/themes_colours.R")

dir.create(tables_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(data_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_folder, recursive = TRUE, showWarnings = FALSE)

base_size <- 20
colour_dictionary <- c(
    "Sulphur Dioxide" = gss_colour_palette[1],
    "Nitrogen Dioxide" = gss_colour_palette[2],
    "Ozone" = gss_colour_palette[3],
    "PM2.5" = gss_colour_palette[4],
    "PM10" = gss_colour_palette[6],
    "Roadside" = gss_colour_palette[4],
    "Urban Background" = gss_colour_palette[1],
    "Rural Background" = gss_colour_palette[3],
    "Rural" = gss_colour_palette[6],
    "Industrial" = gss_colour_palette[5],
    "Projection" = gss_colour_palette[4],
    "All sites" = gss_colour_palette[4],
    "Historic data" = gss_colour_palette[4],
    "PERT sites" = gss_colour_palette[5]
)

# Load current year data from openair ------------

current_year <- latest_year + 1

# Load current year data if it isn't already loaded
if (!file.exists(file.path(data_folder, paste0("pm25_data_", current_year, ".rds")))) {
    # Load metadata and current year pm2.5 data
    aurn_meta <- openair::importMeta(source = "aurn", all = TRUE)
    metadata_to_bind <- aurn_meta |>
        select(site, site_type, latitude, longitude, zone, agglomeration, local_authority) |>
        distinct(site, .keep_all = TRUE) |>
        mutate(
            local_authority = ifelse(is.na(local_authority), "Unknown Local Authority", local_authority),
            zone = ifelse(is.na(zone), "Unknown Zone", zone),
            agglomeration = ifelse(is.na(agglomeration), "Non-Agglomeration", agglomeration),
        )
    sitecodes <- tolower(as.character(unique(aurn_meta$code))) |> setdiff("mh")

    latest_pm25_data_hourly <- openair::importAURN(
        site = sitecodes,
        year = current_year,
        data_type = "hourly",
        pollutant = "pm2.5",
        meta = FALSE,
        meteo = FALSE,
        verbose = FALSE,
        to_narrow = TRUE,
        ratified = TRUE
    )

    latest_pm25_data_daily <- openair::importAURN(
        site = sitecodes,
        year = current_year,
        data_type = "daily",
        pollutant = "gr_pm2.5",
        meta = FALSE,
        meteo = FALSE,
        verbose = FALSE,
        to_narrow = TRUE,
        ratified = TRUE
    )

    # Format 2025 PM2.5 data
    latest_pm25_data_formatted <- bind_rows(latest_pm25_data_hourly, latest_pm25_data_daily) |>
        # Join metadata
        left_join(metadata_to_bind, by = "site") |>
        mutate(
            # Add averaging variable
            averaging = "None",
            # Add measurement variable
            measurement = ifelse(str_detect(pollutant, "^gr_"), "Daily", "Hourly"), # Addition since we are combining daily and hourly data
            # Make other changes and add other variables
            year = as.numeric(lubridate::year(date)),
            site = ifelse(site == "Dewsbury Ashworth Grove", "Dewsbury Ashworth Grange", site),
            pollutant = toupper(sub("^gr_", "", pollutant)),
            source = "openair",
            network = "aurn"
        ) |>
        # Renames columns if needed (e.g qc -> ratified) and adds NA columns if needed e.g. data capture for hourly data
        fix_columns()

    write_rds(latest_pm25_data_formatted, paste0("data/pm25_data_", current_year, ".rds"))
}


# Load older data from bucket and calculate annual means ------------

# Read in data (filter before loading in for efficiency!)
hourly_pm25 <- open_dataset(aurn_database_folder) |>
    filter(
        measurement == "Hourly",
        averaging == "None",
        pollutant == "PM2.5",
        year >= 2009
    ) |>
    collect() |>
    select(site, year, date, value)

hourly_annual_pm25 <- hourly_pm25 %>%
    group_by(site, year) %>%
    summarise(
        mean = mean(value, na.rm = TRUE), # Annual mean PM2.5
        n_ok = sum(!is.na(value)), # Count of valid (non-NA) hourly values
        sd = sd(value, na.rm = TRUE), # Standard deviation
        se_sampling_within_year = sd / sqrt(n_ok), # Standard error from sampling
        .groups = "drop"
    ) %>%
    mutate(
        capture = ifelse(
            year %% 4 == 0, # Leap year check
            (n_ok / (366 * 24)) * 100, # Capture rate for leap years
            (n_ok / (365 * 24)) * 100 # Capture rate for normal years
        ),
        se_measurement = 0 * mean, # Currently we ignore measurement uncertainty but we could add this in future.
        se_sampling_within_site = sqrt(se_sampling_within_year^2 + se_measurement^2),
        measurement = "Hourly"
    ) %>%
    select(site, year, mean, se_sampling_within_site, capture, measurement) # Final tidy output

daily_pm25 <- open_dataset(aurn_database_folder) |>
    filter(
        measurement == "Daily",
        pollutant == "PM2.5",
        year >= 2009
    ) |>
    collect() |>
    select(site, year, date, value)

# Get annual means and data capture rates from daily sites
daily_annual_pm25 <- daily_pm25 %>%
    group_by(site, year) %>%
    summarise(
        mean = mean(value, na.rm = TRUE), # Annual mean PM2.5
        n_ok = sum(!is.na(value)), # Count of valid (non-NA) daily values
        sd = sd(value, na.rm = TRUE), # Standard deviation
        se_sampling_within_year = sd / sqrt(n_ok), # Standard error from averaging all the measurements for a year
        .groups = "drop"
    ) %>%
    mutate(
        capture = ifelse(
            year %% 4 == 0, # Leap year check
            (n_ok / 366) * 100, # Capture rate for leap years
            (n_ok / 365) * 100 # Capture rate for normal years
        ),
        se_measurement = 0 * mean, # Currently we ignore measurement uncertainty but we could add this in future.
        se_sampling_within_site = sqrt(se_sampling_within_year^2 + se_measurement^2),
        measurement = "Daily"
    ) %>%
    select(site, year, mean, se_sampling_within_site, capture, measurement) # Final tidy output

# Bind Hourly to Daily
annual_site_pm25 <- rbind(daily_annual_pm25, hourly_annual_pm25)

# Combine latest data and calculate monthly means ------------

latest_pm25 <- read_rds(file.path(data_folder, paste0("pm25_data_", current_year, ".rds"))) |>
    select(site, year, date, value, measurement)

latest_pm25_hourly <- latest_pm25 |>
    filter(measurement == "Hourly") |>
    select(site, year, date, value)

latest_pm25_daily <- latest_pm25 |>
    filter(measurement == "Daily") |>
    select(site, year, date, value)

hourly_monthly_pm25 <- bind_rows(hourly_pm25, latest_pm25_hourly) %>%
    mutate(date = floor_date(date, unit = "month")) |>
    group_by(site, date) %>%
    summarise(
        n_ok = sum(!is.na(value)), # Count of valid (non-NA) hourly values
        mean = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE), # Annual mean PM2.5
        sd = sd(value, na.rm = TRUE), # Standard deviation
        se_sampling_within_month = sd / sqrt(n_ok), # Standard error from sampling
        .groups = "drop"
    ) %>%
    mutate(
        capture = (n_ok / (lubridate::days_in_month(first(date)) * 24)) * 100,
        se_measurement = 0 * mean, # Currently we ignore measurement uncertainty but we could add this in future.
        se_sampling_within_site = sqrt(se_sampling_within_month^2 + se_measurement^2),
        measurement = "Hourly",
        year = as.numeric(year(date))
    ) %>%
    select(site, year, date, mean, se_sampling_within_site, capture, measurement) # Final tidy output

# Get annual means and data capture rates from daily sites
daily_monthly_pm25 <- bind_rows(daily_pm25, latest_pm25_daily) %>%
    mutate(date = floor_date(date, unit = "month")) |>
    group_by(site, date) %>%
    summarise(
        n_ok = sum(!is.na(value)), # Count of valid (non-NA) hourly values
        mean = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE), # Annual mean PM2.5
        sd = sd(value, na.rm = TRUE), # Standard deviation
        se_sampling_within_month = sd / sqrt(n_ok), # Standard error from sampling
        .groups = "drop"
    ) %>%
    mutate(
        capture = (n_ok / (lubridate::days_in_month(first(date)) * 24)) * 100,
        se_measurement = 0 * mean, # Currently we ignore measurement uncertainty but we could add this in future.
        se_sampling_within_site = sqrt(se_sampling_within_month^2 + se_measurement^2),
        measurement = "Daily",
        year = as.numeric(year(date))
    ) %>%
    select(site, year, date, mean, se_sampling_within_site, capture, measurement) # Final tidy output

# Bind Hourly to Daily
monthly_site_pm25 <- rbind(daily_monthly_pm25, hourly_monthly_pm25)

# Add metadata and filter sites ------------

# Now add additional metadata
site_info <- openair::importMeta(source = "aurn", all = TRUE) %>%
    # Select only PM2.5 monitoring sites
    filter(variable %in% c("PM2.5", "GR2.5")) |>
    mutate(measurement = ifelse(variable == "PM2.5", "Hourly", "Daily")) %>%
    select(site, code, latitude, longitude, site_type, zone, measurement, start_date, end_date) %>% # we need zone for the PERT
    unique() |>
    # Change the name of sites that changed their name to their current name
    # As openair metadata only gives current name
    mutate(site = case_when(
        site == "Dewsbury Ashworth Grove" ~ "Dewsbury Ashworth Grange",
        TRUE ~ site
    )) |>
    # Change start date and end date to years
    mutate(
        end_date = if_else(end_date == "ongoing", NA_character_, end_date),
        start_date = lubridate::year(as.Date(start_date, format = "%Y-%m-%d")),
        end_date = lubridate::year(as.Date(end_date, format = "%Y-%m-%d"))
    )

# Add metadata and add site operation variable
annual_site_pm25 <- annual_site_pm25 %>%
    left_join(
        site_info[, c("site", "measurement", "site_type", "zone", "start_date", "end_date")],
        by = c("site", "measurement")
    ) |>
    mutate(operational_all_year = start_date < year & coalesce(end_date, Inf) > year) %>%
    # Round data capture to nearest integer
    mutate(capture = janitor::round_half_up(capture)) |>
    # Remove zero capture sites
    filter(capture > 0) |>
    # Add PERT flag and rename site types
    mutate(
        pert = !str_detect(zone, "Scottish|Scotland|Highland|Wales|Ireland") &
            site_type %in% c("Suburban Background", "Urban Background") &
            !(site == "Warrington" & year < 2022) &
            !(site == "Middlesbrough" & year < 2022),
        site_type_grouped = case_when(
            site_type %in% c("Urban Background", "Suburban Background") ~ "Urban Background",
            site_type == "Urban Traffic" ~ "Roadside",
            site_type %in% c("Urban Industrial", "Suburban Industrial") ~ "Industrial",
            site_type == "Rural Background" ~ "Rural",
            TRUE ~ site_type # fallback: keep original for anything unmatched
        )
    )

# Add metadata and add site operation variable
monthly_site_pm25 <- monthly_site_pm25 %>%
    left_join(
        site_info[, c("site", "measurement", "site_type", "zone", "start_date", "end_date")],
        by = c("site", "measurement")
    ) |>
    mutate(operational_all_year = start_date < year & coalesce(end_date, Inf) > year) %>%
    # Round data capture to nearest integer
    mutate(capture = janitor::round_half_up(capture)) |>
    # Remove zero capture sites
    filter(capture > 0) |>
    # Add PERT flag
    mutate(
        pert = !str_detect(zone, "Scottish|Scotland|Highland|Wales|Ireland") &
            site_type %in% c("Suburban Background", "Urban Background") &
            !(site == "Warrington" & year < 2022) &
            !(site == "Middlesbrough" & year < 2022),
        site_type_grouped = case_when(
            site_type %in% c("Urban Background", "Suburban Background") ~ "Urban Background",
            site_type == "Urban Traffic" ~ "Roadside",
            site_type %in% c("Urban Industrial", "Suburban Industrial") ~ "Industrial",
            site_type == "Rural Background" ~ "Rural",
            TRUE ~ site_type # fallback: keep original for anything unmatched
        )
    )

# Annual mean summaries ------------

# Annual summary all sites
annual_pm25 <- annual_site_pm25 |>
    # Apply data capture and site operation rules to each year independently
    filter(
        # Note we technically don't need to apply the operational requirement for increments calculations
        # This is because sites in the cohort are already operational for the whole 4 years
        operational_all_year == TRUE,
        capture >= data_capture_threshold
    ) |>
    group_by(year, site) |>
    # Filter out the daily measurement if there are hourly measurements for that site/year
    filter(!(measurement == "Daily" & any(measurement == "Hourly"))) %>%
    ungroup() %>%
    # Calculate annual mean across sites
    group_by(year) %>%
    summarise(
        n_sites = n(),
        se_sampling_between_site = sd(mean, na.rm = TRUE) / sqrt(n_sites),
        # combine standard errors of annual mean from each site assuming independence:
        se_sampling_within_site_combined = sqrt(sum(se_sampling_within_site^2, na.rm = TRUE)) / n_sites,
        se_total = sqrt(se_sampling_between_site^2 + se_sampling_within_site_combined^2),
        mean = mean(mean, na.rm = TRUE),
        sites = paste(sort(unique(site)), collapse = "; "),
        .groups = "drop"
    ) |>
    select(year, mean, se_total, n_sites, sites)


# Annual summary pert sites
annual_pm25_pert <- annual_site_pm25 |>
    filter(pert == TRUE) |>
    # Apply data capture and site operation rules to each year independently
    filter(
        # Note we technically don't need to apply the operational requirement for increments calculations
        # This is because sites in the cohort are already operational for the whole 4 years
        operational_all_year == TRUE,
        capture >= data_capture_threshold
    ) |>
    group_by(year, site) |>
    # Filter out the daily measurement if there are hourly measurements for that site/year
    filter(!(measurement == "Daily" & any(measurement == "Hourly"))) %>%
    ungroup() %>%
    # Calculate annual mean across sites
    group_by(year) %>%
    summarise(
        n_sites = n(),
        se_sampling_between_site = sd(mean, na.rm = TRUE) / sqrt(n_sites),
        # combine standard errors of annual mean from each site assuming independence:
        se_sampling_within_site_combined = sqrt(sum(se_sampling_within_site^2, na.rm = TRUE)) / n_sites,
        se_total = sqrt(se_sampling_between_site^2 + se_sampling_within_site_combined^2),
        mean = mean(mean, na.rm = TRUE),
        sites = paste(sort(unique(site)), collapse = "; "),
        .groups = "drop"
    ) |>
    select(year, mean, se_total, n_sites, sites)

# Annual summary by site type
annual_pm25_site_type <- annual_site_pm25 |>
    # Apply data capture and site operation rules to each year independently
    filter(
        # Note we technically don't need to apply the operational requirement for increments calculations
        # This is because sites in the cohort are already operational for the whole 4 years
        operational_all_year == TRUE,
        capture >= data_capture_threshold
    ) |>
    group_by(year, site) |>
    # Filter out the daily measurement if there are hourly measurements for that site/year
    filter(!(measurement == "Daily" & any(measurement == "Hourly"))) %>%
    ungroup() %>%
    # Calculate annual mean across sites
    group_by(year, site_type_grouped) %>%
    summarise(
        n_sites = n(),
        se_sampling_between_site = sd(mean, na.rm = TRUE) / sqrt(n_sites),
        # combine standard errors of annual mean from each site assuming independence:
        se_sampling_within_site_combined = sqrt(sum(se_sampling_within_site^2, na.rm = TRUE)) / n_sites,
        se_total = sqrt(se_sampling_between_site^2 + se_sampling_within_site_combined^2),
        mean = mean(mean, na.rm = TRUE),
        sites = paste(sort(unique(site)), collapse = "; "),
        .groups = "drop"
    ) |>
    select(year, site_type_grouped, mean, se_total, n_sites, sites)

# Figure 1 - recent trends ------------

annual_pm25$group_var <- "All sites"

df <- bind_rows(
    annual_pm25[, c("year", "mean", "se_total", "group_var")]
) |>
    filter(year >= 2015) |>
    mutate(
        mean = janitor::round_half_up(mean, digits = 2),
        CI = janitor::round_half_up(1.96 * se_total, digits = 2),
        lower = mean - CI,
        upper = mean + CI
    ) |>
    select(group_var, year, mean, lower, upper)

fig1_plot <- line_plot(df, base_size = 25, label_spacing_factor = 0.12)

# Save function which turns off clipping automatically
save_govuk(
    filename = "fig1a_annual_pm_trends.svg",
    plot = fig1_plot,
    device = "svg",
    path = figures_folder
)

# Monthly mean summary ------------

monthly_pm25 <- monthly_site_pm25 |>
    # Apply data capture and site operation rules to each year independently
    filter(
        # Note we technically don't need to apply the operational requirement for increments calculations
        # This is because sites in the cohort are already operational for the whole 4 years
        operational_all_year == TRUE,
        capture >= data_capture_threshold
    ) |>
    group_by(year, date, site) |>
    # Filter out the daily measurement if there are hourly measurements for that site/month
    filter(!(measurement == "Daily" & any(measurement == "Hourly"))) %>%
    ungroup() %>%
    # Calculate annual mean across sites
    group_by(year, date) %>%
    summarise(
        n_sites = n(),
        se_sampling_between_site = sd(mean, na.rm = TRUE) / sqrt(n_sites),
        # combine standard errors of annual mean from each site assuming independence:
        se_sampling_within_site_combined = sqrt(sum(se_sampling_within_site^2, na.rm = TRUE)) / n_sites,
        se_total = sqrt(se_sampling_between_site^2 + se_sampling_within_site_combined^2),
        mean = mean(mean, na.rm = TRUE),
        sites = paste(sort(unique(site)), collapse = "; "),
        .groups = "drop"
    ) |>
    select(year, date, mean, se_total, n_sites, sites)

# Figure 2 - monthly means ------------

monthly_pm25$group_var <- "All sites"

df <- bind_rows(
    monthly_pm25[, c("year", "date", "mean", "se_total", "group_var")]
) |>
    filter(year >= 2018) |>
    mutate(
        mean = janitor::round_half_up(mean, digits = 2),
        CI = janitor::round_half_up(1.96 * se_total, digits = 2),
        lower = mean - CI,
        upper = mean + CI
    ) |>
    select(group_var, date, mean, lower, upper) |>
    mutate(date = as.Date(date))

fig2_plot <- monthly_line_plot(df, base_size = 25, label_spacing_factor = 0.12)

# Save function which turns off clipping automatically
save_govuk(
    filename = "fig2a_monthly_pm_trends.svg",
    plot = fig2_plot,
    device = "svg",
    path = figures_folder
)

# Plot by month
df <- bind_rows(
    monthly_pm25[, c("year", "date", "mean", "se_total", "group_var")]
) |>
    filter(year >= 2020) |>
    mutate(
        mean = janitor::round_half_up(mean, digits = 2),
        CI = janitor::round_half_up(1.96 * se_total, digits = 2),
        lower = mean - CI,
        upper = mean + CI,
        month = factor(month(date), levels = 1:12, labels = month.abb, ordered = TRUE),
        year = as.character(year), 
        highlight = if_else(year == as.character(2025), "highlight", "other")
    ) |>
    select(year, month, mean, lower, upper, highlight)  |> 
    filter(month <= "Jun")

fig2_plot <- monthly_line_plot_by_month(df, base_size = 25, label_spacing_factor = 0.12)

# Save function which turns off clipping automatically
save_govuk(
    filename = "fig2b_monthly_pm_trends.svg",
    plot = fig2_plot,
    device = "svg",
    path = figures_folder
)

# 2025 annual mean modelling ------------

# Jun-Dec data from 2024
# What would 2025 annual mean be
monthly_site_pm25 |>  
    group_by(date) |> 
    summarise(mean = mean(mean, na.rm = TRUE), .groups = "drop") |> 
    filter((year(date) == 2024 & month(date) %in% 7:12) | year(date) == 2025 & month(date) %in% 1:6) |> 
    summarise(mean = mean(mean))

# Calculate annual and jan-may mean for each year
yearly_summary <- monthly_site_pm25 %>%
    filter(date <= as.Date("2025-06-01")) |> 
    mutate(month = month(date)) |> 
    # For each year–month, average across all sites to get a “national” monthly mean:
    group_by(year, month) %>%
    summarize(
        all_sites_monthly_mean = mean(mean, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    # Now for each year, compute:
    #   jan_may_mean = mean of months 1:5
    #   annual_mean  = mean of months 1:12
    group_by(year) %>%
    summarize(
        jan_apr_mean = mean(
            all_sites_monthly_mean[month %in% 1:4],
            na.rm = TRUE
        ),
        jan_may_mean = mean(
            all_sites_monthly_mean[month %in% 1:5],
            na.rm = TRUE
        ),
        jan_jun_mean = mean(
            all_sites_monthly_mean[month %in% 1:6],
            na.rm = TRUE
        ),
        annual_mean = mean(
            all_sites_monthly_mean[month %in% 1:12],
            na.rm = TRUE
        ),
        .groups = "drop"
    ) %>%
    arrange(year)

# Get years where we have all 12 months
train_df <- yearly_summary %>%
    filter(year < 2025)

# Fit linear model and check summary
fit_jan_apr <- lm(annual_mean ~ jan_apr_mean, data = train_df)
summary(fit_jan_apr)$adj.r.squared

fit_jan_may <- lm(annual_mean ~ jan_may_mean, data = train_df)
summary(fit_jan_may)$adj.r.squared

fit_jan_jun <- lm(annual_mean ~ jan_jun_mean, data = train_df)
summary(fit_jan_jun)$adj.r.squared
# june model is the best

coef(fit_jan_may)
# Annual mean = 1.80 + 0.71 * (Jan-May mean)

# Predict 2025 annual mean
test_df <- yearly_summary %>%
    filter(year == 2025) %>%
    select(jan_jun_mean)

pred_2025 <- predict(
    fit_jan_jun,
    newdata = test_df,
    interval = "prediction",
    level = 0.95
)

pred_2025
# Predicting an annual mean of 10.09 ug/m^3 with 95% CI: (7.76, 12.41)
cat("2025 annual mean prediction : ", round(pred_2025[1, "fit"], 1), " ug/m^3\n")
cat(
    "2025 annual mean prediction interval: (", round(pred_2025[1, "lwr"], 1),
    ", ", round(pred_2025[1, "upr"], 1), ")\n"
)

# Chec prediction width:.
pred_vals <- data.frame(
    year = current_year,
    mean = pred_2025[1, "fit"],
    lower = pred_2025[1, "lwr"],
    upper = pred_2025[1, "upr"]
) |> mutate(group_var = "Projection")

# Now compute width percentage: (upr − lwr) / fit × 100
pred_width_pct <- (pred_vals["upper"] - pred_vals["lower"]) / pred_vals["mean"] * 100
cat("2025 prediction interval width: +-", round(pull(pred_width_pct) / 2, 1), "%\n")

# Figure 5 Recent trends with projection ------------

df <- bind_rows(
    annual_pm25[, c("year", "mean", "se_total", "group_var")]
) |>
    filter(year >= 2015) |>
    mutate(
        mean = janitor::round_half_up(mean, digits = 2),
        CI = janitor::round_half_up(1.96 * se_total, digits = 2),
        lower = mean - CI,
        upper = mean + CI,
        group_var = "Historic data"
    ) |>
    select(group_var, year, mean, lower, upper) 

projections <- df  |> slice(nrow(df))  |> 
    bind_rows(pred_vals) |> 
    mutate(group_var = "Projection")

df <- df |> bind_rows(projections)

line_type_dictionary <- c(
    "Historic data" = "solid",
    "Projection" = "dashed"
)

fig5_plot <- line_plot(df, base_size = 25, label_spacing_factor = 0.12, 
space_on_right = 9.5, line_type_dictionary = line_type_dictionary) +
      annotate(
          "rect",
          xmin = 2024.5, xmax = 2025.5,
          ymin = -Inf, ymax = Inf,
          fill = "gray", alpha = 0.2
      ) +
      geom_point(data = df |> slice(nrow(df)), aes(x = year, y = mean), size = 3.5,  colour = gss_colour_palette[4])

# Save function which turns off clipping automatically
save_govuk(
    filename = "fig5a_annual_pm_projected.svg",
    plot = fig5_plot,
    device = "svg",
    path = figures_folder
)

# Map of sites ------------

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

df <- annual_site_pm25 |>
    # Apply data capture and site operation rules to each year independently
    filter(
        # Note we technically don't need to apply the operational requirement for increments calculations
        # This is because sites in the cohort are already operational for the whole 4 years
        operational_all_year == TRUE,
        capture >= data_capture_threshold
    ) |>
    group_by(year, site, pert) |>
    # Filter out the daily measurement if there are hourly measurements for that site/year
    filter(!(measurement == "Daily" & any(measurement == "Hourly"))) %>%
    ungroup() |> 
    filter(year == 2024) |> 
    select(site, measurement, mean, pert) |> 
        left_join(site_info, by = c("site", "measurement")) |>
            mutate(
                pert = factor(pert, levels = c(TRUE, FALSE), labels = c("PERT", "Other"))
            ) 

site_sf <- df %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) # WGS84

# Get UK country outlines
uk_map <- ne_countries(scale = "medium", country = c("United Kingdom", "Ireland"), returnclass = "sf")

site_map <- ggplot() +
    geom_sf(data = uk_map, fill = "grey95", color = "grey50") +
    geom_sf(data = site_sf, aes(color = pert), size = 2) +
    coord_sf(xlim = c(-8.5, 2), ylim = c(49.8, 60), expand = FALSE) +
    theme_minimal(base_size = 14) +
    labs(color = "Site") +
    theme(legend.position = "bottom")

ggsave("outputs/figures/site_map.jpg", site_map, width = 4.5, height = 7, dpi = 300)
