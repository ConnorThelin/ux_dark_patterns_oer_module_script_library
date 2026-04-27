# =============================================================================
# ANONYMIZE RESPONDENT NAMES
# =============================================================================
# Converts raw name strings to MD5 hashes, then renames the column to
# hash_id to reflect its new role as a pseudonymous identifier. A sanity
# check confirms that the number of unique hashes matches the number of
# unique input names.
#
# anonymize_names() normalizes input (lowercase, trimmed) before hashing so
# that formatting variations of the same name produce a consistent hash.
# Empty strings and NAs both resolve to NA rather than hashing to a value.
#
# TODO: Refactor hash format to:
#         <school>_<class_code>_<quarter_code>_<unique_id>
#       and response ID format to:
#         Response_<quarter_code>_<unique_row_id>
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# PARTICIPATION REPORT
# -----------------------------------------------------------------------------
# Generated before anonymization while real names are still available.
# Contains only name and response count, no survey content. Intended for
# the instructor to verify participation for grading purposes.
# -----------------------------------------------------------------------------
participation_report <- modified_data %>%
  mutate(name = tolower(trimws(name))) %>%
  filter(!is.na(name) & name != "") %>%
  group_by(name) %>%
  summarise(response_count = n(), .groups = "drop") %>%
  arrange(name)

# -----------------------------------------------------------------------------
# ANONYMIZATION
# -----------------------------------------------------------------------------
# Replaces the name column with an MD5 hash (hash_id). Must run after the
# participation report is written, as real names are permanently discarded.
# -----------------------------------------------------------------------------

# Helper function to anonymize names using a hashing algorithm (default: MD5)
anonymize_names <- function(name_column, algo = "md5") {
  sapply(name_column, function(single_name) {
    # If the value is missing or empty, return NA
    if (is.na(single_name) || single_name == "") {
      return(NA)
    } else {
      
      # Standardize the name (lowercase + trim white space)
      single_name <- tolower(trimws(single_name))
      
      # Return hashed version of the cleaned name
      return(digest(single_name, algo = algo))
    }
  })
}

# Apply anonymization to the 'name' column
modified_data$name <- anonymize_names(modified_data$name)

# Rename the anonymized column to 'hash_id' for clarity
modified_data <- rename(modified_data, hash_id = name)

# Sanity check:
# Ensure that the number of unique cleaned names in the raw data
# matches the number of unique hash IDs after anonymization
if (length(unique(tolower(trimws(raw_data$`Your name as it appears in Canvas  (Will be anonymized before incidents are shared with the class)`)))) !=
    length(unique(modified_data$hash_id))) {
  
  # Stop execution if there is a mismatch
  stop("Sanity check failed: mismatch between cleaned names and hash IDs")
}