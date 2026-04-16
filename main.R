# =============================================================================
# PIPELINE ORCHESTRATOR
# =============================================================================
# This script serves as the single entry point for the data processing
# pipeline. It defines a safe_source() utility that wraps R's built-in
# source() with error handling, then uses it to execute each pipeline stage
# in strict sequential order:
#
#   config.R              – Environment and parameter configuration
#   00_load_packages.R    – Package dependencies
#   01_ingest_data.R      – Raw data ingestion
#   02_clean_data.R       – Cleaning and preprocessing
#   03_anonymization.R    – Data anonymization
#   04_characterize.R     – Descriptive statistics
#
# If any stage fails, the pipeline halts immediately and reports which
# script caused the failure.
# =============================================================================

#' Safely source an R script with error handling
#'
#' Attempts to run an R script using source(). If an error occurs,
#' it catches the error, prints a message, and returns FALSE instead
#' of stopping execution.
#'
#' @param file Character string. Path to the R script to be sourced.
#' @return Logical. TRUE if the script runs successfully, FALSE otherwise.
safe_source <- function(file) {
  # Use tryCatch to handle errors without stopping execution
  tryCatch(
    
    # ---- TRY BLOCK ----
    {
      # Execute the R script located at the designated file path
      source(file)
      
      # Print a success message indicating which file successfully ran
      message(paste("Successfully ran:", file))
      
      # Return TRUE to indicate success
      TRUE
    },
    
    # ---- ERROR HANDLER ----
    error = function(error) {
      # Print a message indicating which file caused the error
      message(paste("Error in:", file))
     
       # Print the specific error message for debugging
      message(error$message)
     
       # Return FALSE to indicate failure
      FALSE
    }
  )
}

# ---- PIPELINE EXECUTION ----
# Sequentially run each script in the pipeline using safe_source().
# If any step fails (returns FALSE), stop execution immediately.

# Run configuration setup script
# Stop the pipeline if the script fails
if (!safe_source("config.R")) stop("Pipeline stopped")

# Load required packages and dependencies
# Stop the pipeline if package loading fails
if (!safe_source("00_load_packages.R")) stop("Pipeline stopped")

# Ingest raw data from external sources
# Stop the pipeline if data ingestion fails
if (!safe_source("01_ingest_data.R")) stop("Pipeline stopped")

# Clean and preprocess the raw data
# Stop the pipeline if data cleaning fails
if (!safe_source("02_clean_data.R")) stop("Pipeline stopped")

# Perform data anonymization
# Stop the pipeline if anonymization fails
if (!safe_source("03_anonymization.R")) stop("Pipeline stopped")

# Generate descriptive statistics about the ingested data
# Stop the pipeline if characterization fails
if (!safe_source("04_characterize.R")) stop("Pipeline stopped")
