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

# Step 1: Build initial report for inspection
participation_report <- raw_data %>%
  mutate(name = tolower(trimws(.[[2]]))) %>%
  filter(!is.na(name) & name != "") %>%
  group_by(name) %>%
  summarise(response_count = n(), .groups = "drop") %>%
  arrange(name)

# Step 2: Enter corrections identified via manual inspection
name_corrections <- c(
  # "old cleaned name" = "corrected name"
  "fiona tampi (day 1 & 2)" = "fiona tampi",
  "dhruva p" = "dhruva pyapali",
  "nehemiah seobroto" = "nehemiah soebroto",
  "taylor kuo" = "taylor ruby kuo",
  "zachary" = "zachary savage"
)

# Step 3: Rebuild report with corrections applied
participation_report <- raw_data %>%
  mutate(name = tolower(trimws(.[[2]]))) %>%
  mutate(name = recode(name, !!!name_corrections)) %>%
  filter(!is.na(name) & name != "") %>%
  group_by(name) %>%
  summarise(response_count = n(), .groups = "drop") %>%
  arrange(name)

# Step 4: Sanity check
if (nrow(participation_report) != expected_respondent_count) {
  warning(paste0(
    "Respondent count mismatch: expected ", expected_respondent_count,
    " but found ", nrow(participation_report), "."
  ))
}

# Step 5: Write the corrections into the raw data, while preserving the original data set.
name_corrected_data <- raw_data
name_corrected_data[[2]] <- recode(tolower(trimws(name_corrected_data[[2]])), !!!name_corrections)

# Write participation report to CSV
write.csv(participation_report, "participation_report.csv", row.names = FALSE)

# Remove intermediate data objects
rm(
  name_corrections,
  expected_respondent_count
)
