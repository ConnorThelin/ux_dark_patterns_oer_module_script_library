# =============================================================================
# PARTICIPATION REPORT
# =============================================================================
# Generated before anonymization while real names are still available.
# Contains only name and response count, no survey content. Intended for
# the instructor to verify participation for grading purposes.
#
# Must run after ingest and before anonymization, as real
# names are permanently discarded during anonymization.
#
# Sources directly from raw_data using column position [2] to avoid
# special character issues in the original header string.
# =============================================================================

participation_report <- raw_data %>%
  mutate(name = tolower(trimws(.[[2]]))) %>%
  filter(!is.na(name) & name != "") %>%
  group_by(name) %>%
  summarise(response_count = n(), .groups = "drop") %>%
  arrange(name)

# Write participation report to CSV
write.csv(participation_report, "participation_report.csv", row.names = FALSE)

# Sanity check:
# Verify that the number of unique respondents matches the expected count
if (nrow(participation_report) != expected_respondent_count) {
  warning(paste0(
    "Respondent count mismatch: expected ", expected_respondent_count,
    " but found ", nrow(participation_report), "."
  ))
}