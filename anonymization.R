# =============================================================================
# ANONYMIZE RESPONDENT NAMES
# =============================================================================
# Converts raw name strings to MD5 hashes in anon_data before wrangling.
# This allows raw_data to be preserved with real names intact, while
# anon_data is the anonymized version consumed by all downstream scripts.
#
# anonymize_names() normalizes input (lowercase, trimmed) before hashing so
# that formatting variations of the same name produce a consistent hash.
# Empty strings and NAs both resolve to NA rather than hashing to a value.
#
# Must run after participation_report.R, as real names are permanently
# discarded here before wrangling begins.
#
# TODO: Refactor hash format to:
#         <school>_<class_code>_<quarter_code>_<unique_id>
#       and response ID format to:
#         Response_<quarter_code>_<unique_row_id>
# =============================================================================

# Helper function to anonymize names using a hashing algorithm (default: MD5)
anonymize_names <- function(name_column, algo = "md5") {
  sapply(name_column, function(single_name) {
    if (is.na(single_name) || single_name == "") {
      return(NA)
    } else {
      single_name <- tolower(trimws(single_name))
      return(digest(single_name, algo = algo))
    }
  })
}

# Capture unique cleaned names before hashing for use in sanity check
pre_hash_unique_names <- unique(tolower(trimws(raw_data[[2]])))

# Create an anonymized copy of raw_data, preserving the original
anon_data <- raw_data
anon_data[[2]] <- anonymize_names(anon_data[[2]])

# Sanity check:
# Ensure that the number of unique cleaned names before hashing
# matches the number of unique hashes after anonymization
if (length(pre_hash_unique_names) != length(unique(anon_data[[2]]))) {
  stop("Sanity check failed: mismatch between cleaned names and hash IDs")
}

# Export anonymized raw data to CSV
write.csv(anon_data, "student_ready_anonymized_raw_data.csv", row.names = FALSE)