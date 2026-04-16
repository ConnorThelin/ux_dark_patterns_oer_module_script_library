# =============================================================================
# PACKAGE LOADER
# =============================================================================
# Installs and loads all packages listed in `packages` (defined in config.R),
# then authenticates with Google Drive and Google Sheets using a shared OAuth
# token so that downstream scripts can read the survey data without prompting
# for credentials again.
# =============================================================================

# Helper function to load a collection of packages, installing first if necessary
load_or_install <- function(pkg) {
  # Check if package is available without explicitly loading it, install it if not
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  
  # Load the package, suppressing startup messages
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Apply load_or_install to each package in 'packages', suppressing associated console messages
invisible(lapply(packages, load_or_install))

# Authenticate access to Google Drive (opens browser or uses cached credentials)
drive_auth()

# Authenticate access to Google Sheets using the Drive authentication token
gs4_auth(token = drive_token())