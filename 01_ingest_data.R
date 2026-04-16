# =============================================================================
# DATA INGESTION
# =============================================================================
# Pulls raw data and metadata from the Google Sheet defined in config.R.
# Requires the googlesheets4 package and valid OAuth credentials.
# =============================================================================

# If raw_data does not exist
if (!exists("raw_data")) {
  # Create raw_data from associated google sheet
  raw_data      <- read_sheet(google_sheet_url)
  
  # Populate sheet meta data
  sheet_meta_data <- gs4_get(google_sheet_url)
  
  # Populate sheet name
  sheet_name    <- sheet_meta_data$name
}