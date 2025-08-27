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

# Functions from read_data_functions() in aos code ------------

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

# Updated plotting functions for V2 ------------

# Saving function amended from afcharts R package
# In developing I also considered using svglite package
save_govuk <- function(filename,
                       plot = ggplot2::last_plot(),
                       device = c("svg", "png", "jpg"),
                       path = NULL,
                       ...) {
    device <- match.arg(device)

    # Divide pixels by 72 if using svg to convert to inches.
    # This is because the dpi is 72.
    if (device == "svg") {
        width <- 960 / 72
        height <- 640 / 72
        units <- "in"
    } else {
        width <- 960
        height <- 640
        units <- "px"
    }

    # Addition to turn off clipping
    plot <- ggplotGrob(plot)
    plot$layout$clip[plot$layout$name == "panel"] <- "off"

    ggplot2::ggsave(
        filename = filename,
        plot = plot,
        device = device,
        path = path,
        width = width,
        height = height,
        units = units,
        dpi = 72,
        ...
    )
}

compute_y_breaks <- function(max_val, max_breaks = 6) {
    n <- max_breaks
    repeat {
        brks <- pretty_breaks(n)(c(0, max_val))
        # ensure 0 is included
        brks <- unique(c(0, brks))
        # if top break covers the data, we're done
        if (max(brks) >= max_val) break
        n <- n + 1
    }
    brks
}

compute_x_breaks <- function(min_year, max_year, max_breaks = 8) {
    # generate a pretty sequence without forcing the endpoint
    base_breaks <- pretty_breaks(max_breaks)(c(min_year, max_year))
    # ensure final year is present
    sort(unique(c(base_breaks[base_breaks < max_year], max_year)))
}

# Adjust y_label to prevent overlaps
adjust_label_positions <- function(label_df, min_gap = 0.5) {
    # Assume it's already sorted by mean
    y_labels <- label_df$y_label

    for (i in 2:length(y_labels)) {
        if ((y_labels[i] - y_labels[i - 1]) < min_gap) {
            y_labels[i] <- y_labels[i - 1] + min_gap
        }
    }

    label_df$y_label <- y_labels
    return(label_df)
}

# Line plot function
line_plot <- function(df, base_size, max_breaks_x = 8, 
max_breaks_y = 6, label_spacing_factor = 0.05, 
space_on_right = 8.8, label_width = 15, 
line_type_dictionary = NULL) {
    # Get the maximum data point y value for labels and axis
    max_data <- max(df$upper, na.rm = TRUE)
    x_range <- max(df$year, na.rm = TRUE) - min(df$year, na.rm = TRUE)

    # Compute the label positions
    label_df <- df %>%
        group_by(group_var) %>%
        filter(year == max(year)) %>%
        ungroup() %>%
        arrange(mean) %>%
        mutate(y_label = mean)

    # Adjust labels in case of any overlap
    if (nrow(label_df) > 1) {
        label_df <- adjust_label_positions(label_df, min_gap = label_spacing_factor * max_data)
    }

    # Compute your axis breaks
    x_breaks <- compute_x_breaks(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), max_breaks = max_breaks_x)
    y_breaks <- compute_y_breaks(max_data, max_breaks = max_breaks_y)
    max_y <- max(y_breaks)

    # Build the plot
    p <- ggplot(df, aes(x = year, y = mean, group = group_var, color = group_var, fill = group_var)) +
        geom_line(aes(linetype = group_var), linewidth = base_size / 12) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
        theme_af(legend = "none", axis = "none", grid = "y", base_size = base_size) +
        scale_color_manual(values = colour_dictionary) +
        scale_fill_manual(values = colour_dictionary) +
        coord_cartesian(
            xlim = c(min(df$year, na.rm = TRUE) - 0.04 * x_range, max(df$year, na.rm = TRUE)),
            ylim = c(0, max_y),
            clip = "off"
        ) +

        # X axis: pretty, final year forced
        scale_x_continuous(breaks = x_breaks, expand = c(0, 0)) +

        # Y axis: start at 0, top > data, nice breaks
        scale_y_continuous(breaks = y_breaks, expand = c(0, 0)) +

        # Labels and segments
        geom_segment(
            data = label_df,
            # aes(x = year + 0.1, xend = year + 0.75, y = mean, yend = y_label),
            aes(x = year + 0.005 * x_range, xend = year + 0.025 * x_range, y = mean, yend = y_label),
            color = "#a3a3a3",
            linewidth = base_size / 24
        ) +
        geom_textbox(
            data = label_df,
            aes(label = stringr::str_wrap(group_var, label_width), y = y_label), # default 15
            color = "black", size = base_size / 3,
            nudge_x = 0.022 * x_range, hjust = 0, # was 0.075
            box.colour = NA, fill = NA,
            width = unit(10, "lines"), lineheight = 0.9
        ) +

        # Axis titles
        labs(x = NULL, y = "µg/m³") +
        theme(
            # plot.margin = unit(c(3.2, 8.8, 0.25, 0.3), "lines"),
            plot.margin = unit(c(3.5, space_on_right, 0.25, 0.3), "lines"),
            axis.title.y = element_text(
                angle = 0, hjust = 0.5, vjust = 1.08, # 1.07 default
                margin = ggplot2::margin(r = -70) # default 55
            )
        )

    # Add linetype mapping if dictionary is provided
    if (!is.null(line_type_dictionary)) {
        p <- p + scale_linetype_manual(values = line_type_dictionary)
    }

    return(p)
}

# Line plot function
monthly_line_plot <- function(df, base_size, max_breaks_x = 8, max_breaks_y = 6, label_spacing_factor = 0.05, space_on_right = 8.8, label_width = 15) {
    # Get the maximum data point y value for labels and axis
    max_data <- max(df$upper, na.rm = TRUE)
    x_range <- as.numeric(difftime(max(df$date, na.rm = TRUE), min(df$date, na.rm = TRUE), units = "days")) / 365

    # Compute the label positions
    label_df <- df %>%
        group_by(group_var) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        arrange(mean) %>%
        mutate(y_label = mean)

    # Adjust labels in case of any overlap
    if (nrow(label_df) > 1) {
        label_df <- adjust_label_positions(label_df, min_gap = label_spacing_factor * max_data)
    }

    # Compute your axis breaks
    # x_breaks <- compute_x_breaks(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), max_breaks = max_breaks_x)
    y_breaks <- compute_y_breaks(max_data, max_breaks = max_breaks_y)
    max_y <- max(y_breaks)

    # Build the plot
    ggplot(df, aes(x = date, y = mean, group = group_var, color = group_var, fill = group_var)) +
        geom_line(linewidth = base_size / 12) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
        theme_af(legend = "none", axis = "none", grid = "y", base_size = base_size) +
        scale_color_manual(values = colour_dictionary) +
        scale_fill_manual(values = colour_dictionary) +
        coord_cartesian(
            # xlim = c(min(df$year, na.rm = TRUE) - 0.04 * x_range, max(df$year, na.rm = TRUE)),
            xlim = c(min(df$date), as.Date("2026-01-01")),
            ylim = c(0, max_y),
            clip = "off"
        ) +

        # X axis: pretty, final year forced
        # scale_x_continuous(breaks = x_breaks, expand = c(0, 0)) +
        scale_x_date(
            breaks = seq(as.Date(min(df$date)), as.Date("2026-01-01"), by = "1 year"),
            labels = scales::date_format("%Y"),
            expand = c(0, 0)
        ) +

        # Y axis: start at 0, top > data, nice breaks
        scale_y_continuous(breaks = y_breaks, expand = c(0, 0)) +

        # Labels and segments
        geom_segment(
            data = label_df,
            # aes(x = year + 0.1, xend = year + 0.75, y = mean, yend = y_label),
            aes(x = as.Date(max(df$date)) + days(30), xend = as.Date("2026-01-01"), y = mean, yend = y_label),
            color = "#a3a3a3",
            linewidth = base_size / 24
        ) +
        geom_textbox(
            data = label_df |> mutate(date = as.Date("2026-01-01")),
            aes(label = stringr::str_wrap(group_var, label_width), y = y_label), # default 15
            color = "black", size = base_size / 3,
            nudge_x = 10, hjust = 0, # was 0.075 trhen 0.022 * x_range
            box.colour = NA, fill = NA,
            width = unit(10, "lines"), lineheight = 0.9
        ) +

        # Axis titles
        labs(x = NULL, y = "µg/m³") +
        theme(
            # plot.margin = unit(c(3.2, 8.8, 0.25, 0.3), "lines"),
            plot.margin = unit(c(3.5, space_on_right, 0.25, 0.3), "lines"),
            axis.title.y = element_text(
                angle = 0, hjust = 0.5, vjust = 1.08, # 1.07 default
                margin = ggplot2::margin(r = -70) # default 55
            )
        )
}

monthly_line_plot_by_month <- function(df, base_size = 14,
                                       max_breaks_y = 6,
                                       label_spacing_factor = 0.05,
                                       label_width = 15,
                                       highlight_year = 2025) {
    # Custom color scale: highlight 2025, dim others
    years <- unique(df$year)
    colour_dictionary <- setNames(rep("#3D3D3D", length(years)), years)
    if (highlight_year %in% years) {
        colour_dictionary[as.character(highlight_year)] <- "#801650"
    }

    # Max y-axis for breaks and ribbons
    max_data <- max(df$upper, na.rm = TRUE)
    y_breaks <- compute_y_breaks(max_data, max_breaks = max_breaks_y)
    max_y <- max(y_breaks)

    # Label positions (latest month in year)
    label_df <- df %>%
        group_by(year) %>%
        filter(month == last(month)) %>%
        ungroup() %>%
        arrange(mean) %>%
        mutate(y_label = mean) |>
        filter(year == highlight_year)

    if (nrow(label_df) > 1) {
        label_df <- adjust_label_positions(label_df, min_gap = label_spacing_factor * max_data)
    }

    ggplot(df, aes(x = month, y = mean, group = year, color = year, fill = year, alpha = highlight)) +
        geom_line(linewidth = base_size / 12) +
        # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
        scale_color_manual(values = colour_dictionary) +
        scale_fill_manual(values = colour_dictionary) +
        scale_alpha_manual(values = c("highlight" = 1, "other" = 0.3), guide = "none") +
        scale_y_continuous(breaks = y_breaks, expand = c(0, 0)) +
        theme_af(legend = "none", axis = "x", grid = "y", base_size = base_size) +
        labs(x = NULL, y = "µg/m³") +
        geom_textbox(
            data = label_df,
            aes(label = year, y = y_label),
            color = "black", size = base_size / 3,
            box.colour = NA, fill = NA,
            width = unit(10, "lines"), lineheight = 0.9,
            nudge_x = 0.5, hjust = 0
        ) +
        geom_segment(
            data = label_df,
            # aes(x = year + 0.1, xend = year + 0.75, y = mean, yend = y_label),
            aes(x = 6.1, xend = 6.5, y = mean, yend = y_label),
            color = "#a3a3a3",
            linewidth = base_size / 24
        ) +
        coord_cartesian(ylim = c(0, max_y)) +
        theme(
            plot.margin = unit(c(3.5, 7.5, 0.25, 0.3), "lines"),
            axis.title.y = element_text(
                angle = 0, hjust = 0.5, vjust = 1.08,
                margin = ggplot2::margin(r = -60)
            )
        )
}
