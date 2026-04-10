# Pulls from Google Sheets; guarded by exists() to prevent redundant API calls on re-runs within the same session
if (!exists("raw_data")) {
  raw_data <- read_sheet(google_sheet_url)
}

sheet_meta_data <- gs4_get(google_sheet_url)
sheet_name <- sheet_meta_data$name
