# Stops upon any pipeline failure
safe_source <- function(file) {
  tryCatch(
    {
      source(file)
      message(paste("Successfully ran:", file))
      TRUE
    },
    error = function(error) {
      message(paste("Error in:", file))
      message(error$message)
      FALSE
    }
  )
}

# Run pipeline
if (!safe_source("config.R")) stop("Pipeline stopped")
if (!safe_source("00_load_packages.R")) stop("Pipeline stopped")
if (!safe_source("01_ingest_data.R")) stop("Pipeline stopped")
if (!safe_source("02_clean_data.R")) stop("Pipeline stopped")
if (!safe_source("03_anonymization.R")) stop("Pipeline stopped")
if (!safe_source("04_characterize.R")) stop("Pipeline stopped")
