# Update this to the latest reporting year
latest_year <- 2024

# Define data capture threshold for PERT
data_capture_threshold <- 85

# Define folder locations
tables_folder <- "outputs/tables/"
data_folder <- "data/"

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

# It is important NOT to change the folder paths as they are used for reading/writing the data
bucket_name <- "s3-ranch-033"
aurn_database_folder <- file.path(paste0("s3://", bucket_name, "/aos_air_quality/", latest_year, "/aurn_database"))
rds_folder <- file.path(paste0("aos_air_quality/", latest_year, "/rds_files"))