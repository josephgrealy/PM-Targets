# Update this to the latest reporting year
latest_year <- 2024

# Define data capture threshold for PERT
data_capture_threshold <- 85

# Define folder locations
tables_folder <- "outputs/tables/"
data_folder <- "data/"
figures_folder <- "outputs/figures/"

# Helper variables
previous_year <- latest_year - 1
decade_year <- latest_year - 10
weekday_levels <- c(
  "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday", "Sunday"
)
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May",
  "Jun", "Jul", "Aug", "Sep", "Oct",
  "Nov", "Dec"
)

# Define folder paths to database here: