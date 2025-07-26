rm(list = ls())

# Load Packages ------------
packages <- c("tidyverse", "aws.s3", "arrow", "lubridate", "readr", "writexl")


# Check if packages needed are installed, if they are then load, if not install
for (package in packages) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package)
        library(package, character.only = TRUE)
    }
}

source("code/parameters.R") # Loads latest_year, data_capture_threshold and folder locations 
source("code/functions.R") # Loads compute_pei() and build_cohort() functions for PERT

dir.create(tables_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(data_folder, recursive = TRUE, showWarnings = FALSE)

# Load Data ------------

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
    select(site, year, value)

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
    mutate(operational_all_year = start_date < year & coalesce(end_date, Inf) > year)

# Remove sites that aren't needed for PERT
annual_site_pm25_pert <- annual_site_pm25 %>%
    # Round data capture to nearest integer
    mutate(capture = janitor::round_half_up(capture)) |>
    filter(
        # Remove zero capture sites
        capture > 0,
        # Ensure it is England only data
        str_detect(zone, "Scottish|Scotland|Highland|Wales|Ireland", negate = TRUE),
        # Keep only UB or sub-UB site types for PERT
        site_type %in% c("Suburban Background", "Urban Background"),
        # Remove Warrington and Middlesborough prior to 2022
        # As these sites changed site type from Industrial to UB in 2022
        !(site == "Warrington" & year < 2022),
        !(site == "Middlesbrough" & year < 2022)
    )

saveRDS(annual_site_pm25_pert, file = file.path(data_folder, "pm25_annual_site_data_for_PERT.RDS"))

# Calculate PEI base ------------

# Calculate PEI base dataframe
pei_base <- annual_site_pm25_pert |>
    filter(year %in% 2016:2018) |>
    compute_pei()

# Average the three sites and round for PEI base
pei_base_summary <- pei_base |>
    summarise(
        mean = janitor::round_half_up(mean(mean, na.rm = TRUE), digits = 2),
        se = sqrt(sum(se_total^2, na.rm = TRUE)) / 3,
        .groups = "drop"
    ) |>
    mutate(year = 2018) |>
    select(year, PEI = mean, se_PEI = se)

# Calculate increments ------------

# delta_pei_n represents the increment from year n-1 to n

# Obtain list of increment years
years <- sort(unique(annual_site_pm25_pert$year))
increment_years <- years[years > 2018]

# For each increment year, n, compute RI and PEI using functions defined above
all_increments <- map_dfr(increment_years, function(n) {
    # Get the cohort of sites for the increment
    cohort_df <- build_cohort(annual_site_pm25_pert, n)

    # Reference Indicator (n-3 to n-1)
    RI <- compute_pei(cohort_df, PEI_year = n - 1) %>%
        mutate(
            increment_year = n,
            indicator = "RI"
        )

    # Population exposure indicator for year n (n-2 to n)
    PEI <- compute_pei(cohort_df, PEI_year = n) %>%
        mutate(
            increment_year = n,
            indicator = "PEI"
        )

    bind_rows(RI, PEI)
})

# Now compute ΔPEI for each increment
delta_summary <- all_increments %>%
    # First average the annual means to calculate the mean and standard error of each RI and PEI
    group_by(increment_year, indicator) |>
    summarise(
        mean = mean(mean, na.rm = TRUE),
        se = sqrt(sum(se_total^2, na.rm = TRUE)) / 3,
        .groups = "drop"
    ) |>
    # Now calculate all the increments and their standard error
    pivot_wider(
        names_from  = indicator,
        values_from = c(mean, se),
        names_sep   = "_"
    ) |>
    mutate(
        # First calculate each increment and its standard error
        delta_PEI = janitor::round_half_up(mean_RI - mean_PEI, digits = 2), # Round the increments to two decimal places
        se_delta_PEI = sqrt(se_PEI^2 + se_RI^2), # assumes independent
        # Next calculate the cumulative increment and standard error
        delta_PEI_cumulative = cumsum(delta_PEI),
        se_delta_PEI_cumulative = sqrt(cumsum(se_delta_PEI^2)), # again assumes independent
    )

# Calculate PERT summary ------------

# Finally calculate the PERT summary table
# Need to add PEI for each year and percent change
PERT_summary <- delta_summary |>
    select(year = increment_year, delta_PEI, se_delta_PEI, delta_PEI_cumulative, se_delta_PEI_cumulative) |>
    # Add PEI base to the dataframe
    bind_rows(pei_base_summary) |>
    arrange(year) |>
    # Calculate statistics
    mutate(

        # Add base PEI and SE once, to use for all subsequent years
        base_PEI = PEI[year == 2018],
        base_se_PEI = se_PEI[year == 2018],

        # Add PEI and standard error
        PEI = if_else(
            year > 2018,
            base_PEI - delta_PEI_cumulative,
            PEI
        ),
        se_PEI = if_else(
            year > 2018,
            sqrt(base_se_PEI^2 + se_delta_PEI_cumulative^2), # Note that we have already summed the standard errors for the increments by this point
            se_PEI
        ),

        # Add percent change and standard error
        perc_change = janitor::round_half_up(100 * (delta_PEI_cumulative / base_PEI), digits = 2), # we have to round this to an integer for comparing with PERT but after computing confidence intervals
        # Note we have to calculate the standard error using the delta method here because we are dividing random variables instead of adding
        # For full workings of this see our GitHub Wiki or the document in the uncertainties folder on sharepoint
        se_perc_change = sqrt((100 / base_PEI)^2 * se_delta_PEI_cumulative^2 + (100 * delta_PEI_cumulative / base_PEI^2)^2 * base_se_PEI^2),
    ) |>
    # Tidy up columns
    select(year, PEI, se_PEI, everything(), -base_PEI, -base_se_PEI)

# Replace standard errors with normal confidence intervals
# To date we have had at least 26 sites used in the calculation of any annual mean, on average about 30
# As a result it is okay to use a normal confidence interval.
# The impact of using the t-distribution would also be small, assuming df = 26-1 would give critical value = 2.06
# This is a 5% increase in the margin of error compared to using a normal confidence interval (critical value 1.96)
PERT_summary_CIs <- PERT_summary %>%
    # Add CIs
    mutate(
        ci_PEI = janitor::round_half_up(1.96 * se_PEI, digits = 2),
        ci_delta_PEI = janitor::round_half_up(1.96 * se_delta_PEI, digits = 2),
        ci_delta_PEI_cumulative = janitor::round_half_up(1.96 * se_delta_PEI_cumulative, digits = 2),
        ci_perc_change = janitor::round_half_up(1.96 * se_perc_change, digits = 2),
    ) 

# For accessibility upper and lower bounds should be given
PERT_summary_CIs_bounds <- PERT_summary_CIs |> 
    # Convert to lower and upper bounds
    mutate(
        ci_PEI_lower = PEI - ci_PEI,
        ci_PEI_upper = PEI + ci_PEI,
        ci_delta_PEI_lower = delta_PEI - ci_delta_PEI,
        ci_delta_PEI_upper = delta_PEI + ci_delta_PEI,
        ci_delta_PEI_cumulative_lower = delta_PEI_cumulative - ci_delta_PEI_cumulative,
        ci_delta_PEI_cumulative_upper = delta_PEI_cumulative + ci_delta_PEI_cumulative,
        ci_perc_change_lower = janitor::round_half_up(perc_change - ci_perc_change, digits = 0),
        ci_perc_change_upper = janitor::round_half_up(perc_change + ci_perc_change, digits = 0)
    ) |>
    # Round percent change to integer for comparison with PERT
    mutate(perc_change = janitor::round_half_up(perc_change, digits = 0)) |>
    select(
        year, PEI, ci_PEI_lower, ci_PEI_upper,
        delta_PEI, ci_delta_PEI_lower, ci_delta_PEI_upper,
        delta_PEI_cumulative, ci_delta_PEI_cumulative_lower, ci_delta_PEI_cumulative_upper,
        perc_change, ci_perc_change_lower, ci_perc_change_upper
    )

# Produce another version with the +- absolute uncertainty (not using % uncertainty as it might be confusing for the % change columns)
PERT_summary_CIs_absolute <- PERT_summary_CIs |>
    # Here we have not rounded percent change 
    select(
        year, PEI, ci_PEI,
        delta_PEI, ci_delta_PEI,
        delta_PEI_cumulative, ci_delta_PEI_cumulative,
        perc_change, ci_perc_change
    )

# Save PERT summary ------------

# Save RDS for additional analysis and plots
saveRDS(PERT_summary_CIs_bounds, file = file.path(data_folder, "PERT_summary_accessible.RDS"))
saveRDS(PERT_summary_CIs_absolute, file = file.path(data_folder, "PERT_summary_data_unrounded.RDS"))

# Save PERT summary table accessible
PERT_summary_CIs_bounds %>%
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>% # Needed to avoid unwanted floating point errors
    select(
        "Year" = year,
        "PEI (µg/m³)" = PEI,
        "Lower bound - 95% confidence interval for PEI (µg/m³)" = ci_PEI_lower,
        "Upper bound - 95% confidence interval for PEI (µg/m³)" = ci_PEI_upper,
        "Year-on-year change in PEI (µg/m³)" = delta_PEI,
        "Lower bound - 95% confidence interval for year-on-year change (µg/m³)" = ci_delta_PEI_lower,
        "Upper bound - 95% confidence interval for year-on-year change (µg/m³)" = ci_delta_PEI_upper,
        "Total change in PEI since 2018 (µg/m³)" = delta_PEI_cumulative,
        "Lower bound - 95% confidence interval for total change (µg/m³)" = ci_delta_PEI_cumulative_lower,
        "Upper bound - 95% confidence interval for total change (µg/m³)" = ci_delta_PEI_cumulative_upper,
        "Total change in PEI since 2018 (%)" = perc_change,
        "Lower bound - 95% confidence interval for percent change (%)" = ci_perc_change_lower,
        "Upper bound - 95% confidence interval for percent change (%)" = ci_perc_change_upper
    ) |>
    write_excel_csv(file = file.path(tables_folder, "PERT_summary_accessible.csv"))

# Save PERT summary table
PERT_summary_CIs_absolute |> 
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>% # Needed to avoid unwanted floating point errors
    select(
        "Year" = year,
        "PEI (µg/m³)" = PEI,
        "+/- 95% confidence interval for PEI (µg/m³)" = ci_PEI,
        "Year-on-year change in PEI (µg/m³)" = delta_PEI,
        "+/- 95% confidence interval for year-on-year change (µg/m³)" = ci_delta_PEI,
        "Total change in PEI since 2018 (µg/m³)" = delta_PEI_cumulative,
        "+/- 95% confidence interval for total change (µg/m³)" = ci_delta_PEI_cumulative,
        "Total change in PEI since 2018 (% unrounded)" = perc_change,
        "+/- 95% confidence interval for percent change (% unrounded)" = ci_perc_change,
    ) |>
    write_excel_csv(file = file.path(tables_folder, "PERT_summary_unrounded.csv"))

# Save list of sites   ------------

# Get detailed info on which sites are included in which annual mean
# Prepare increment and PEI base data
increment_data_to_save <- all_increments |>
    mutate(indicator = paste0(indicator, " ", increment_year)) |>
    select(-increment_year)
pei_base_data_to_save <- pei_base |>
    mutate(indicator = "PEI base")

# Combine the two and order columns
site_summary_data <- bind_rows(
    pei_base_data_to_save,
    increment_data_to_save
) |>
    select(indicator, year, mean, se_total, n_sites, sites)

# Save the PERT data showing annual mean and sites included for each indicator
write_excel_csv(site_summary_data, file = file.path(tables_folder, "PERT_indicator_data.csv"))

# Save RDS for additional analysis and plots
saveRDS(site_summary_data, file = file.path(data_folder, "PERT_indicator_data.RDS"))

# Save site data for Imperial ------------

# Imperial are going to try to use modelled grid square data to forecast the PERT
# We will send them a list of sites to use each year, and the annual mean for those sites

# Pull out row with maximum number of sites for each year
sites_to_include_per_year_summary <- site_summary_data %>%
    group_by(year) %>%
    slice_max(n_sites, with_ties = FALSE) %>%
    ungroup() |>
    select(year, n_sites, sites)

# Get full site data
# Get site_year combos to extract
site_year_combos <- sites_to_include_per_year_summary %>%
    mutate(site = str_split(sites, ";\\s*")) %>% # Split by "; "
    select(year, site) %>%
    unnest(site)

site_data_filtered <- site_year_combos %>%
    left_join(annual_site_pm25_pert, by = c("site", "year")) |> # May have joined extra times for hourly and daily measurements
    left_join(site_info[, c("site", "measurement", "code", "latitude", "longitude")], by = c("site", "measurement")) |>
    # Filter out the daily measurement if there are hourly measurements for that site/year
    group_by(year, site) |>
    filter(!(measurement == "Daily" & any(measurement == "Hourly"))) %>%
    ungroup() %>%
    select(site, code, latitude, longitude, year, mean, se = se_sampling_within_site, capture, measurement, site_type, zone)

# Last thing for Imperial is to check % of sites excluded due to data capture for forecasting
data_capture_statistics <- annual_site_pm25_pert |>
    filter(operational_all_year, year >= 2016) |>
    group_by(year) |>
    summarise(
        "total sites" = n(),
        "sites meeting data capture threshold" = sum(capture >= data_capture_threshold),
        "percent meeting data capture threshold" = round(100 * `sites meeting data capture threshold` / `total sites`, 1)
    )

# Save data for imperial
write_xlsx(
    list(
        sites_to_include = sites_to_include_per_year_summary,
        site_data = site_data_filtered,
        data_capture_statistics = data_capture_statistics
    ),
    path = file.path(tables_folder, "PERT_data_for_Imperial.xlsx")
)
