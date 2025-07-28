### Categorical colour palette
gss_colour_palette <- c(
    "#12436D", # Dark blue
    "#28A197", # Turquoise
    "#801650", # Dark pink
    "#F46A25", # Orange
    "#3D3D3D", # Dark grey
    "#A285D1" # Light purple
)

### Sequential colour palette (blues)
# When you are presenting sequential data, it is best to use a single hue,
# or small set of closely related hues.
gss_colour_palette_blues <- c(
    "#12436D", # Dark blue
    "#2073BC", # Mid blue
    "#6BACE6" # Light blue
)

# Set the colours for the line charts
road_colour <- gss_colour_palette[4]
urban_back_colour <- gss_colour_palette[1]
rural_colour <- gss_colour_palette[3]
urban_colour <- gss_colour_palette[5]

no2_colour <- gss_colour_palette[2]
so2_colour <- gss_colour_palette[1]
o3_colour <- gss_colour_palette[3]
pm25_colour <- gss_colour_palette[4]
pm10_colour <- gss_colour_palette[6]

# Set the colours for HAM figues
cbPalette <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")

# Set the colours for the bar charts
bar_colours <- c("#12436D", "#2073BC", "#6BACE6", "#F46A25")
facet_colours <- c(no2_colour, o3_colour, pm10_colour, pm25_colour, so2_colour)

# Define the line chart format
aqie_style_line <- function() {
    font <- "Arial"
    theme_bw() +
        ggplot2::theme(
            plot.title = element_text(size = 9, hjust = 0), ## -2.8
            panel.grid.minor = element_blank(), # turn off minor grid
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", linewidth = 0.2), # set the horizontal lines
            legend.position = "none", # turn off legend
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_text(colour = "black", size = 6),
            axis.text.x = element_text(vjust = -1),
            # axis.text.y = element_text(margin = margin(r = 7)),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_line(colour = "grey", linewidth = 0.75),
            axis.ticks.y = element_blank(),
            axis.ticks.length = unit(0.08, "cm"),
            plot.caption = element_blank(), # changes size of caption
            panel.border = element_blank(), # makes the panel border blank
            axis.line = element_line(color = "black"),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
        )
}

# Define the hours above moderate (area chart) format
aqie_area_graph <- function() {
    font <- "Arial"
    theme_bw() +
        ggplot2::theme(
            plot.title = element_text(size = 9, hjust = 0.15),
            panel.grid.minor = element_blank(), # turn off minor grid
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", linewidth = 0.2), # set the horizontal lines
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_line(colour = "grey", linewidth = 0.75),
            axis.ticks.y = element_blank(),
            axis.text = element_text(colour = "black", size = 5),
            axis.text.x = element_text(vjust = -2),
            # axis.ticks.length =  unit(0.036, "cm"),
            plot.caption = element_blank(), # changes size of caption
            panel.border = element_blank(), # makes the panel border blank
            axis.line = element_line(color = "black"),
            plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "lines"),
        )
}

# Define the legend format
aqie_legend <- function() {
    ggplot2::theme(
        legend.position = "right",
        legend.box = "horizontal",
        legend.text = element_text(size = 5),
        legend.key.size = unit(.6, "cm"),
        legend.title = element_blank()
    )
}

# Doesn't look like this is used?
aqie_style_3 <- function() {
    font <- "Arial"
    theme_bw() +
        ggplot2::theme(
            plot.title = element_text(size = 6, hjust = -0.25, vjust = 2.5),
            panel.grid.minor = element_blank(), # turn off minor grid
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", linewidth = 0.2), # set the horizontal lines
            legend.position = "none", # turn off legend
            axis.title.x = element_blank(),
            # axis.title.y = element_blank(), # THIS IS WHERE IT DIFFERS
            axis.text = element_text(colour = "black", size = 5), # Amend size of axis labels
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            # axis.ticks.length =  unit(0.036, "cm"),
            plot.caption = element_blank(), # changes size of caption
            panel.border = element_blank(), # makes the panel border blank
            axis.line = element_line(color = "black")
        )
}



















# style for line charts
aqie_style_2 <- function() {
    font <- "Arial"
    theme_bw() +
        ggplot2::theme(
            plot.title = element_text(size = 6, hjust = -0.25, vjust = 2.5),
            panel.grid.minor = element_blank(), # turn off minor grid
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", linewidth = 0.2), # set the horizontal lines
            legend.position = "none", # turn off legend
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_text(colour = "black", size = 5), # Amend size of axis labels
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            # axis.ticks.length =  unit(0.036, "cm"),
            plot.caption = element_blank(), # changes size of caption
            panel.border = element_blank(), # makes the panel border blank
            axis.line = element_line(color = "black")
        )
}

# Analysis function theme taken from
# https://github.com/best-practice-and-impact/afcharts/blob/main/R/theme_af.R

theme_af <- function(base_size = 14,
                     base_line_size = base_size / 24,
                     base_rect_size = base_size / 24,
                     grid = c("y", "x", "xy", "none"),
                     axis = c("x", "y", "xy", "none"),
                     ticks = c("xy", "x", "y", "none"),
                     legend = c("right", "left", "top", "bottom", "none")) {
    grid <- match.arg(grid)
    axis <- match.arg(axis)
    ticks <- match.arg(ticks)
    legend <- match.arg(legend)

    # Set colours
    light_grey <- "#d9d9d9"

    # Use built in sans font
    afcharts_font <- "sans"

    # The half-line (base_size / 2) sets up the basic vertical
    # rhythm of the theme. Most margins will be set to this value.
    # However, when we work with relative sizes, we may want to multiply
    # `half_line` with the appropriate relative size. This applies in
    # particular for axis tick sizes. And also, for axis ticks and
    # axis titles, `half_size` is too large a distance, and we use `half_size/2`
    # instead.
    half_line <- base_size / 2

    # Set grid lines dependent on grid arg
    grid_line <- ggplot2::element_line(colour = light_grey)
    grid_blank <- ggplot2::element_blank()
    grid_x <- if (grid %in% c("x", "xy")) grid_line else grid_blank
    grid_y <- if (grid %in% c("y", "xy")) grid_line else grid_blank

    # Set axis lines dependent on axis arg
    axis_line <- ggplot2::element_line()
    axis_blank <- ggplot2::element_blank()
    axis_x <- if (axis %in% c("x", "xy")) axis_line else axis_blank
    axis_y <- if (axis %in% c("y", "xy")) axis_line else axis_blank

    # Set axis ticks dependent on ticks arg
    axis_ticks <- ggplot2::element_line()
    no_ticks <- ggplot2::element_blank()
    ticks_x <- if (ticks %in% c("x", "xy")) axis_ticks else no_ticks
    ticks_y <- if (ticks %in% c("y", "xy")) axis_ticks else no_ticks

    ggplot2::theme(

        # Set parent characteristics
        line = ggplot2::element_line(
            colour = light_grey,
            linewidth = base_line_size,
            linetype = 1,
            lineend = "butt"
        ),
        rect = ggplot2::element_rect(
            fill = "white",
            colour = "black",
            linewidth = base_rect_size,
            linetype = 1
        ),
        text = ggplot2::element_text(
            family = afcharts_font,
            face = "plain",
            colour = "black",
            size = base_size,
            lineheight = 0.9,
            hjust = 0.5,
            vjust = 0.5,
            angle = 0,
            margin = ggplot2::margin(),
            debug = FALSE
        ),

        # In following sections, arguments set to NULL will inherit from
        # parent; e.g. axis.line = NULL will inherit characteristics from line arg
        # set in previous section

        # Axis:
        axis.line = NULL,
        axis.line.x = axis_x,
        axis.line.y = axis_y,
        axis.text = NULL,
        axis.text.x = ggplot2::element_text(
            margin = ggplot2::margin(t = 0.8 * half_line / 2),
            vjust = 1
        ),
        axis.text.x.top = ggplot2::element_text(
            margin = ggplot2::margin(b = 0.8 * half_line / 2),
            vjust = 0
        ),
        axis.text.y = ggplot2::element_text(
            margin = ggplot2::margin(r = 0.8 * half_line / 2),
            hjust = 1
        ),
        axis.text.y.right = ggplot2::element_text(
            margin = ggplot2::margin(l = 0.8 * half_line / 2),
            hjust = 0
        ),
        axis.ticks = NULL,
        axis.ticks.x = ticks_x,
        axis.ticks.y = ticks_y,
        axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
        axis.title.x = ggplot2::element_text(
            margin = ggplot2::margin(t = half_line / 2),
            vjust = 1
        ),
        axis.title.x.top = ggplot2::element_text(
            margin = ggplot2::margin(b = half_line / 2),
            vjust = 0
        ),
        axis.title.y = ggplot2::element_text(
            angle = 0,
            margin = ggplot2::margin(r = half_line / 2),
            vjust = 1,
            hjust = 0.5
        ),
        axis.title.y.right = ggplot2::element_text(
            angle = 0,
            margin = ggplot2::margin(l = half_line / 2),
            vjust = 1,
            hjust = 0.5
        ),

        # Legend
        legend.background = ggplot2::element_rect(colour = NA),
        legend.spacing = ggplot2::unit(2 * half_line, "pt"),
        legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
        legend.key = ggplot2::element_rect(fill = NA, colour = NA),
        legend.key.size = ggplot2::unit(1.2, "lines"), # CHECK
        legend.text = ggplot2::element_text(size = ggplot2::rel(1)),
        legend.text.align = NULL,
        legend.title = ggplot2::element_text(hjust = 0),
        legend.title.align = NULL,
        legend.position = legend,
        legend.direction = NULL,
        legend.justification = "centre",
        legend.box = NULL,
        legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
        legend.box.background = ggplot2::element_blank(),
        legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),

        # Panel:
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid.major.x = grid_x,
        panel.grid.major.y = grid_y,
        panel.grid.minor = ggplot2::element_blank(),
        panel.spacing = ggplot2::unit(half_line, "pt"),
        panel.ontop = FALSE,

        # Strip:
        strip.background = ggplot2::element_blank(),
        strip.clip = "inherit",
        strip.text = ggplot2::element_text(
            margin = ggplot2::margin(
                0.8 * half_line, 0.8 * half_line,
                0.8 * half_line, 0.8 * half_line
            )
        ),
        strip.text.y = ggplot2::element_text(angle = -90),
        strip.text.y.left = ggplot2::element_text(angle = 90),
        strip.placement = "inside",
        strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
        strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),

        # Plot:
        plot.background = ggplot2::element_rect(colour = "white"),
        plot.title = ggplot2::element_text(
            size = ggplot2::rel(1.6),
            hjust = 0, vjust = 1,
            margin = ggplot2::margin(b = half_line * 2)
        ),
        plot.title.position = "panel",
        plot.subtitle = ggplot2::element_text(
            hjust = 0, vjust = 1,
            margin = ggplot2::margin(b = half_line * 2)
        ),
        plot.caption = ggplot2::element_text(
            size = ggplot2::rel(1),
            hjust = 0, vjust = 1,
            margin = ggplot2::margin(t = half_line)
        ),
        plot.caption.position = "panel",
        plot.tag = ggplot2::element_text(
            size = ggplot2::rel(1.2),
            hjust = 0.5, vjust = 0.5
        ),
        plot.tag.position = "topleft",
        plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
        complete = TRUE
    )
}
