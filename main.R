# =============================================================================
# PIPELINE ORCHESTRATOR
# =============================================================================
# This script serves as the single entry point for the data processing
# pipeline. It defines a safe_source() utility that wraps R's built-in
# source() with error handling, then uses it to execute each pipeline stage
# in strict sequential order:
#
#   config.R                   – Environment and parameter configuration
#   load_packages.R            – Package dependencies
#   ingest_data.R              – Raw data ingestion
#   participation_report.R     – Participation report (before anonymization)
#   anonymization.R            – Data anonymization
#   wrangle_data.R             – Cleaning and preprocessing
#   eda.R                      – Descriptive statistics
#
# If any stage fails, the pipeline halts immediately and reports which
# script caused the failure.
# =============================================================================
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

if (!safe_source("config.R"))               stop("Pipeline stopped")
if (!safe_source("load_packages.R"))        stop("Pipeline stopped")
if (!safe_source("ingest_data.R"))          stop("Pipeline stopped")
if (!safe_source("participation_report.R")) stop("Pipeline stopped")
if (!safe_source("anonymization.R"))        stop("Pipeline stopped")
if (!safe_source("wrangle_data.R"))         stop("Pipeline stopped")
if (!safe_source("eda.R"))                  stop("Pipeline stopped")
