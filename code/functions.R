# Define functions for calculating PERT ------------

# Function to compute the population exposure indicator for a specific year
compute_pei <- function(df, PEI_year = NULL) {
    # Check required columns are present
    required_cols <- c(
        "year", "site", "measurement", "mean",
        "se_sampling_within_site", "capture", "operational_all_year"
    )
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        stop(
            "Input data frame is missing required columns: ",
            paste(missing_cols, collapse = ", ")
        )
    }

    # Define default the PEI_year to the max year present, if not provided
    if (is.null(PEI_year)) {
        PEI_year <- max(df$year, na.rm = TRUE)
    }

    # Run pipe to calculate PEI
    df |>
        # Select the data for three years needed to calculate the PEI
        filter(year %in% (PEI_year - 2):PEI_year) |>
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
            se_sampling_within_site_combined = 0, #sqrt(sum(se_sampling_within_site^2, na.rm = TRUE)) / n_sites,
            se_total = sqrt(se_sampling_between_site^2 + se_sampling_within_site_combined^2),
            mean = mean(mean, na.rm = TRUE),
            sites = paste(sort(unique(site)), collapse = "; "),
            .groups = "drop"
        ) |>
        select(year, mean, se_total, n_sites, sites)
}

# Define function to build the cohort of sites for a given increment year n
# This checks
#   1. sites are operational for the 4 years used in the increment calculation
#   2. sites meet data capture threshold in at least 3 of the 4 years
build_cohort <- function(df, n) {
    df %>%
        filter(between(year, n - 3, n)) %>% # years n-3, n-2, n-1, n
        group_by(site, measurement) %>%
        filter(
            all(operational_all_year), # operational all four years
            sum(capture >= data_capture_threshold) >= 3 # above data capture threshold in at least 3 of the 4 years
        ) %>%
        ungroup()
}

# Function to rename/add columns that are missing
fix_columns <- function(df) {
    rename_map <- c(qc = "ratified", poll_index = "value")
    standard_cols <- c(
        "network", "source", "year", "date", "site", "code", "latitude", "longitude",
        "site_type", "local_authority", "zone", "agglomeration", "pollutant", "measurement", "averaging",
        "data_capture", "value", "ratified"
    )

    # Rename columns that match rename_map
    cols <- names(df)
    # Find columns to rename
    rename_from <- intersect(names(rename_map), cols)
    if (length(rename_from) > 0) {
        names(df)[match(rename_from, names(df))] <- rename_map[rename_from]
    }

    # Keep only standard columns (remove extras)
    df <- df[, intersect(standard_cols, names(df)), drop = FALSE]

    # Add any missing columns with NA
    missing_cols <- setdiff(standard_cols, names(df))
    if (length(missing_cols) > 0) {
        df[missing_cols] <- NA
    }

    # Reorder columns to standard
    df <- df[, standard_cols]

    df
}
