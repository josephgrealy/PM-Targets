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

