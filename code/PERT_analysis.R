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

source("code/parameters.R") # Loads latest year and folder locations
source("code/functions.R") # Loads compute_pei() and build_cohort() functions for PERT

dir.create(tables_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(data_folder, recursive = TRUE, showWarnings = FALSE)

# Check impact of site operation requirements  ------------

site_summary_data <- readRDS(file.path(data_folder, "PERT_indicator_data.RDS"))

# Now lets see the impact of considering site operation (see the PERT_calculation document on Sharepoint)
# We will ignore uncertainties here for simplicity
site_summary_data_filtered <- site_summary_data |>
    select(-se_total, -sites) |>
    # Exclude all but the first and last annual mean in each increment indicator since they cancel out
    group_by(indicator) |>
    filter(
        indicator == "PEI base" | # Keep all PEI base rows
            (startsWith(indicator, "RI") & row_number() == 1) | # Keep first row in RI group
            (startsWith(indicator, "PEI") & row_number() == n()) # Keep last row in PEI group
    ) |>
    ungroup() |>
    # Sort so years are chronological after base year
    mutate(
        sort_group = ifelse(indicator == "PEI base", 0, 1)
    ) |>
    arrange(sort_group, year) |>
    select(-sort_group) # Remove the helper column

# Now lets split up into the change in annual mean and the percentage reduction components
# Identify rows where the year is the same as the previous or next row - these are the ones where we calculate the change in mean
adjacent_years <- site_summary_data_filtered |>
    mutate(
        year_prev = lag(year),
        year_next = lead(year),
        is_duplicate_adjacent = year == year_prev | year == year_next
    )
# Split into two dataframes
change_due_to_operation <- adjacent_years |>
    filter(is_duplicate_adjacent) |>
    select(indicator, year, mean) |>
    mutate(
        type = if_else(grepl("^PEI", indicator), "PEI", "RI")
    ) |>
    select(-indicator) |>
    pivot_wider(names_from = type, values_from = mean) |>
    mutate(change_in_annual_mean = -(PEI - RI))

core_reduction <- adjacent_years |>
    filter(!is_duplicate_adjacent | is.na(is_duplicate_adjacent)) |>
    select(-year_prev, -year_next, -is_duplicate_adjacent) |>
    mutate(group = case_when(
        grepl("PEI base", indicator) ~ "PEI_base",
        grepl("^RI", indicator) ~ "PEI_base_plus_3",
        grepl("^PEI 20", indicator) ~ "PEI_target_minus_3"
    )) |>
    group_by(group) |>
    summarise(total = sum(mean), .groups = "drop") |>
    pivot_wider(names_from = group, values_from = total)

perc_reduction_component <- 100 * (core_reduction$PEI_target_minus_3 - core_reduction$PEI_base_plus_3) / core_reduction$PEI_base
change_due_to_operation_component <- 100 * sum(change_due_to_operation$change_in_annual_mean) / core_reduction$PEI_base

PERT <- perc_reduction_component - change_due_to_operation_component

# PERT =
#      % reduction from 2018 to target (3yr average and semi-accounting for changes in site operation)
#            -
#      change in annual means due to sites going out of operation and new sites becoming operational

# The base-to-target-year concentration reduction would have resulted in a 24.73% reduction in population exposure.
# Changes in site operation slightly offset this, contributing a further 0.19% reduction in population exposure.
# Therefore, the final observed reduction is 24.92%.
# This is slightly different to the true PERT value (24.98% - see below) because we no longer round increments as we don't calculate them.
# Since the percentage is rounded to the nearest integer that won't matter.

true_PERT <- readRDS(file.path(data_folder, "PERT_summary_data_unrounded.RDS")) |>
    filter(year == max(year)) %>%
    pull(perc_change)


# Checking how simple percent reduction compares to PERT ------------

# Here we will try simplifying the PERT calculation further,
# We will use a % reduction from 2018 to the target year using a 3-year average
# We apply data capture and site operation requirements to each year independently (as in PEI base)
# Note that this is (almost) equivalent to the above when you ignore changes in site operation

annual_site_pm25_pert <- readRDS(file.path(data_folder, "pm25_annual_site_data_for_PERT.RDS"))

# Calculate PEI base dataframe
pei_base_data <- annual_site_pm25_pert |>
    filter(year %in% 2016:2018) |>
    compute_pei()
pei_base <- pei_base_data |>
    summarise(
        mean = janitor::round_half_up(mean(mean, na.rm = TRUE), digits = 2),
        se = sqrt(sum(se_total^2, na.rm = TRUE)) / 3,
        .groups = "drop"
    ) |>
    mutate(year = 2018) |>
    select(year, PEI = mean, se_PEI = se)

latest_year <- max(annual_site_pm25_pert$year, na.rm = TRUE)
pei_target_data <- annual_site_pm25_pert |>
    filter(year %in% (latest_year - 2):latest_year) |>
    compute_pei()
pei_target <- pei_target_data |>
    summarise(
        mean = janitor::round_half_up(mean(mean, na.rm = TRUE), digits = 2),
        se = sqrt(sum(se_total^2, na.rm = TRUE)) / 3,
        .groups = "drop"
    ) |>
    mutate(year = latest_year) |>
    select(year, PEI = mean, se_PEI = se)

simplified_PERT <- 100 * (pei_target$PEI - pei_base$PEI) / pei_base$PEI

# The simplified PERT results in a reduction of 23.98% (lower than the correct value 24.98%)
# This is equivalent to a % error of around -4% (see below) which is relatively small.
# The reason for the difference is new sites which have come online in the last few years which are increasing the annual mean.
# For example in 2024 - 31 sites are used in the PERT calculation compared to 51 in the simplified PERT.

perc_error <- 100 * -(true_PERT - simplified_PERT) / true_PERT

simplified_PERT_sites <- pei_target_data |> select(year, simplified_PERT_sites = n_sites)
true_PERT_sites <- adjacent_years |>
    filter(year %in% (latest_year - 2):latest_year) |>
    select(year, true_PERT_sites = n_sites)

sites_included_comparison <- left_join(simplified_PERT_sites, true_PERT_sites, by = c("year"))
