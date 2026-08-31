# =============================================================================
# FILTER AND ANONYMIZE CONSENTING STUDENTS
# =============================================================================
# Filters name_corrected_data down to only rows belonging to students who
# provided consent, then anonymizes the result using the same MD5 hashing
# applied to anon_data above.
#
# consenting_student_names holds the ground-truth list of consenting students.
# Names are normalized (lowercase, trimmed) before matching so that formatting
# variations do not cause a student to be incorrectly excluded.
#
# filtered_by_consent_data is preserved with real names intact, while
# anon_filtered_data is the anonymized version consumed by all downstream
# scripts. All rows for a consenting student are retained, as multiple
# responses per student are expected.
# =============================================================================
# TODO Formalize this to work in conjunction with an additional column in the 
# diary study that asks for data consent
consenting_student_names <- c("consenting student names")

filtered_by_consent_data <- name_corrected_data[
  tolower(trimws(name_corrected_data[[2]])) %in% consenting_student_names, 
]

# Capture unique cleaned names before hashing for use in sanity check
pre_hash_unique_names_consent <- unique(filtered_by_consent_data[[2]])

# Create an anonymized copy of filtered_by_consent_data, preserving the original
anon_filtered_data <- filtered_by_consent_data
anon_filtered_data[[2]] <- anonymize_names(anon_filtered_data[[2]])

# Sanity check
if (length(pre_hash_unique_names_consent) != length(unique(anon_filtered_data[[2]]))) {
  stop("Sanity check failed: mismatch between cleaned names and hash IDs in consent-filtered data")
}

# Export anonymized filtered data to CSV
write.csv(anon_filtered_data, "anonymized_consent_filtered_data.csv", row.names = FALSE)
