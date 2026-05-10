# =============================================================================
# DATA INGESTION
# =============================================================================
# Reads raw data from the CSV file defined in config.R.
# =============================================================================

# If raw_data does not exist
if (!exists("raw_data")) {
  # Create raw_data from the CSV file
  raw_data <- read_csv(csv_path, show_col_types = FALSE)
  
  # Derive a sheet name from the CSV filename (without path or extension)
  sheet_name <- tools::file_path_sans_ext(basename(csv_path))
}