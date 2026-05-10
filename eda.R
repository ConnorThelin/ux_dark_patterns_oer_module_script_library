# =============================================================================
# EXPLORATORY DATA ANALYSIS
# =============================================================================
# Computes descriptive summaries across seven dimensions;
# Optionally prints and exports each as a CSV.
#
# Outputs:
#   descriptive_stats            : top-level counts, percents, and date range
#   respondent_summary       : per-respondent response counts
#   per_question_summary     : per-question answered counts and response rates
#   concern_level_summary    : frequency and % breakdown of concern levels
#   pattern_category_summary : frequency and % breakdown of dark pattern types
#   incomplete_rows_summary  : rows where all question fields are blank/NA
#   open_response_summary    : filtered view of complete open-response rows
#   other_pattern_summary    : grouping of dark patterns characterized as `Other`
#   cooccurrence_summary     : Co-occurrence rate of dark pattern pairs
# =============================================================================

# -----------------------------------------------------------------------------
# RESPONSE COUNTS
# -----------------------------------------------------------------------------
# Summarize total respondents (unique hash_ids) and total responses, then
# breaks down responses by answer to dark_pattern_interaction (Yes/Unsure/No)
# with counts and percentages.
#
# Sanity check: Yes + Unsure + No must equal total response count.
# -----------------------------------------------------------------------------
descriptive_stats <- modified_data %>%
  summarise(
    # Keep track of the sheet/source name
    sheet_name             = sheet_name,
    
    # Count of unique respondents (based on anonymized IDs)
    respondant_total_count = n_distinct(hash_id),
    
    # Total number of responses (rows)
    response_total_count   = n(),
    
    # Count and percentage of "Yes" responses
    response_yes_count     = sum(dark_pattern_interaction == "Yes",    na.rm = TRUE),
    response_yes_pct       = round(response_yes_count    / response_total_count * 100, 1),
    
    # Count and percentage of "Unsure" responses
    response_unsure_count  = sum(dark_pattern_interaction == "Unsure", na.rm = TRUE),
    response_unsure_pct    = round(response_unsure_count / response_total_count * 100, 1),
    
    # Count and percentage of "No" responses
    response_no_count      = sum(dark_pattern_interaction == "No",     na.rm = TRUE),
    response_no_pct        = round(response_no_count     / response_total_count * 100, 1)
  )

# Sanity check:
# Verify that total responses equal the sum of Yes + Unsure + No responses
if (descriptive_stats$response_total_count !=
    descriptive_stats$response_yes_count +
    descriptive_stats$response_unsure_count +
    descriptive_stats$response_no_count) {
  
  # Stop execution if counts do not match
  stop(paste0(
    "Sanity check failed: total (", descriptive_stats$response_total_count,
    ") != sum of categories (",
    descriptive_stats$response_yes_count +
      descriptive_stats$response_unsure_count +
      descriptive_stats$response_no_count, ")"
  ))
}

# -----------------------------------------------------------------------------
# DATE RANGE
# -----------------------------------------------------------------------------
# Derives the collection window from the earliest and latest timestamps,
# expressed as a date range and as integer spans in days and weeks (floored).
# Appended to descriptive_stats as additional columns.
#
# NOTE: Update the `timestamp_format` string below if your CSV uses a
#       different date/time format. Common examples:
#         "%m/%d/%Y %H:%M:%S"  →  01/15/2025 14:30:00
#         "%Y-%m-%d %H:%M:%S"  →  2025-01-15 14:30:00
#         "%m/%d/%Y %H:%M"     →  01/15/2025 14:30
#
# TODO: Add error check for missing or malformed timestamps.
# TODO: Add tracking for most active day, time of day, etc.
# -----------------------------------------------------------------------------
if ("timestamp" %in% names(modified_data)) {
  
  # Update this format string to match your CSV's timestamp format
  timestamp_format <- "%m/%d/%Y %H:%M:%S"
  
  # Parse timestamp column as POSIXct using the specified format
  parsed_timestamps <- as.POSIXct(modified_data$timestamp, format = timestamp_format)
  
  # Warn and skip if parsing failed entirely
  if (all(is.na(parsed_timestamps))) {
    warning("Timestamp column found but could not be parsed, check `timestamp_format` in the date range section. Date range summary skipped.")
  } else {
    descriptive_stats <- descriptive_stats %>%
      mutate(
        start_date      = as.Date(min(parsed_timestamps, na.rm = TRUE)),
        end_date        = as.Date(max(parsed_timestamps, na.rm = TRUE)),
        time_span_days  = floor(as.numeric(difftime(
          max(parsed_timestamps, na.rm = TRUE),
          min(parsed_timestamps, na.rm = TRUE),
          units = "days"
        ))),
        time_span_weeks = floor(as.numeric(difftime(
          max(parsed_timestamps, na.rm = TRUE),
          min(parsed_timestamps, na.rm = TRUE),
          units = "weeks"
        )))
      )
  }
} else {
  warning("No 'timestamp' column found in data — date range summary skipped.")
}

# -----------------------------------------------------------------------------
# FLAG INCOMPLETE ROWS
# -----------------------------------------------------------------------------
# Identify rows where all survey question fields are missing or empty.
#
# Rows are considered complete if there is at least one question answered.
# -----------------------------------------------------------------------------
incomplete_rows_summary <- modified_data %>%
  
  # Keep only rows where every column listed in question_lookup
  # is either NA or an empty string
  filter(
    if_all(
      all_of(names(question_lookup)),
      ~ is.na(.) | . == ""
    )
  )

# -----------------------------------------------------------------------------
# PER-QUESTION RESPONSE RATE
# -----------------------------------------------------------------------------
# For each question column defined in question_lookup, counts non-blank,
# non-NA answers and expresses them as a percentage of total responses.
# Pivots wide-to-long so each row represents one question.
# -----------------------------------------------------------------------------
per_question_summary <- modified_data %>%
  
  # Count non-missing, non-empty responses for each question column
  summarise(across(
    all_of(names(question_lookup)),
    ~ sum(!is.na(.) & . != "")
  )) %>%
  
  # Convert from wide format (one column per question)
  # to long format (one row per question)
  pivot_longer(
    everything(),
    names_to  = "question_key",
    values_to = "answered_count"
  ) %>%
  
  mutate(
    # Assign a sequential question number
    question_number = row_number(),
    
    # Calculate response rate (%) based on total responses
    response_rate_pct = round(
      answered_count / descriptive_stats$response_total_count * 100, 1
    ),
    
    # Map question keys to their full text using the lookup table
    question_text = unname(question_lookup[question_key])
  ) %>%
  
  # Keep and order relevant columns
  select(
    question_number,
    question_key,
    answered_count,
    response_rate_pct,
    question_text
  )

# -----------------------------------------------------------------------------
# CONCERN LEVEL SUMMARY
# -----------------------------------------------------------------------------
# Appends mean, median, and SD of concern_level (as numeric) to descriptive_stats,
# along with a count and percentage of NA responses.
#
# concern_level_summary provides a full frequency breakdown: one row per
# concern level (1–5) with count and percentage of total non-NA responses.
# Rows are ordered by concern_level for readability.
#
# NOTE: concern_level must be cast to numeric for arithmetic; it is stored as
#       an ordered factor after data wrangling.
# -----------------------------------------------------------------------------
descriptive_stats <- descriptive_stats %>%
  mutate(
    # Mean of concern level (converted to numeric), excluding missing values
    mean_concern = mean(as.numeric(modified_data$concern_level), na.rm = TRUE),
    
    # Median of concern level (numeric conversion), excluding missing values
    median_concern = median(as.numeric(modified_data$concern_level), na.rm = TRUE),
    
    # Standard deviation of concern level (numeric conversion)
    sd_concern = sd(as.numeric(modified_data$concern_level), na.rm = TRUE),
    
    # Count of missing concern level values
    na_concern_count = sum(is.na(modified_data$concern_level)),
    
    # Proportion of missing concern level values relative to total responses
    na_concern_pct = na_concern_count / response_total_count
  )

# Create a frequency table for concern levels with labels
concern_level_summary <- modified_data %>%
  
  # Count occurrences of each concern level and its label
  count(concern_level, concern_level_label, name = "count") %>%
  
  # Compute percentage of total responses per category
  mutate(pct = round(count / sum(count) * 100, 1)) %>%
  
  # Order results by underlying numeric concern level
  arrange(concern_level)

# -----------------------------------------------------------------------------
# DARK PATTERN CATEGORY SUMMARY
# -----------------------------------------------------------------------------
# Explodes the multi-select involved_pattern column
# into one row per pattern selection, then maps each to one of the eight
# FTC-defined dark pattern categories. Rows with no pattern selected by a
# Yes/Unsure respondent are labelled "No Dark Pattern Selected", all other
# unmatched values fall to "Other".
#
# TODO: Replace the inline case_when category map with a look-up table in
#       config.R to make category additions/edits easier to manage.
# -----------------------------------------------------------------------------
# Clean and standardize missing values in involved_pattern
modified_data <- modified_data %>%
  mutate(
    # Convert empty strings to NA for consistency
    involved_pattern = na_if(involved_pattern, ""),
    
    # Convert literal "NA" strings to actual NA values
    involved_pattern = na_if(involved_pattern, "NA")
  )

# Expand and categorize involved patterns into structured categories
pattern_category_df <- modified_data %>%
  
  # Split patterns into one row per pattern.
  # The REGEX (?<=\)) uses a lookbehind to split only on ", " that is
  # immediately preceded by ")", ensuring commas inside parentheses
  # (e.g. sub-examples within a category) are not treated as delimiters.
  separate_rows(involved_pattern, sep = "(?<=\\)), ") %>%
  
  # Assign each pattern to a meaningful category
  mutate(
    category = case_when(
      
      # Match known dark pattern categories
      str_detect(involved_pattern, "Social Proof")           ~ "Social Proof",
      str_detect(involved_pattern, "Scarcity")               ~ "Scarcity",
      str_detect(involved_pattern, "Urgency")                ~ "Urgency",
      str_detect(involved_pattern, "Obstruction")            ~ "Obstruction",
      str_detect(involved_pattern, "Sneaking")               ~ "Sneaking",
      str_detect(involved_pattern, "Interface Interference") ~ "Interface Interference",
      str_detect(involved_pattern, "Coerced Action")         ~ "Coerced Action",
      str_detect(involved_pattern, "Asymmetric Choice")      ~ "Asymmetric Choice",
      
      # Cases where interaction was reported but no pattern was selected
      (dark_pattern_interaction %in% c("Yes", "Unsure") & is.na(involved_pattern)) |
        (dark_pattern_interaction %in% c("Yes", "Unsure") & involved_pattern == "") ~ "No Dark Pattern Selected",
      
      # Explicit missing values
      is.na(involved_pattern) ~ "NA",
      
      # Catch-all for anything not matched above
      TRUE ~ "Other"
    ),
    
    # Define ordering of categories for consistent reporting
    category = factor(category, levels = c(
      "Social Proof", "Scarcity", "Urgency", "Obstruction", "Sneaking",
      "Interface Interference", "Coerced Action", "Asymmetric Choice",
      "No Dark Pattern Selected", "Other"
    ))
  )

# Summarize category counts and percentages
pattern_category_summary <- pattern_category_df %>%
  
  # Count occurrences of each category (keep empty categories if present)
  count(category, name = "category_count", .drop = FALSE) %>%
  
  # Compute percentage share of each category
  mutate(pct_of_total = round(category_count / sum(category_count) * 100, 1))

# Summarize how often each selection count occurs
pattern_count_summary <- pattern_category_df %>%
  
  # Count distinct patterns selected per response
  filter(
    !category %in% c("No Dark Pattern Selected", "Other"),
    !is.na(category)
  ) %>%
  group_by(hash_id, response_id) %>%
  summarise(pattern_count = n(), .groups = "drop") %>%
  
  # Summarize how often each count occurs
  count(pattern_count, name = "response_count") %>%
  mutate(pct_of_total = round(response_count / sum(response_count) * 100, 1)) %>%
  arrange(pattern_count)

# -----------------------------------------------------------------------------
# 'OTHER' PATTERN SUMMARY
# -----------------------------------------------------------------------------
# Extract all "Other" categorized patterns for further inspection
# -----------------------------------------------------------------------------
other_pattern_summary <- pattern_category_df %>%
  
  # Keep only rows classified as "Other"
  filter(category == "Other") %>%
  
  # Select key identifiers and the raw pattern text for review
  select(hash_id, response_id, involved_pattern) %>%
  
  # Remove duplicate rows to avoid repeated entries
  distinct()

# -----------------------------------------------------------------------------
# OPEN RESPONSE SUMMARY
# -----------------------------------------------------------------------------
# Filters to rows where all four core open-response fields are present
# (non-NA), then selects and orders them for manual review or export.
# -----------------------------------------------------------------------------
open_response_summary <- modified_data %>%
  
  # Keep only rows where all key open-response fields are present
  filter(
    !is.na(response_id) &
      !is.na(link_to_system) &
      !is.na(task) &
      !is.na(suspicious_element) &
      !is.na(personal_impact)
  ) %>%
  
  # Select only the relevant open-ended response columns for analysis
  select(
    response_id,
    link_to_system,
    task,
    suspicious_element,
    personal_impact
  ) %>%
  
  # Sort rows by response ID for easier review and tracing
  arrange(response_id)

# -----------------------------------------------------------------------------
# COOCCURRENCE SUMMARY
# -----------------------------------------------------------------------------
# Counts how often each pair of dark pattern categories was tagged together
# within the same response (same hash_id + response_id combination).
# Filters out non-selection categories ("Other", "No Dark Pattern Selected",
# and NA) before counting, then deduplicates symmetric pairs so (A, B) and
# (B, A) are not counted separately.
# -----------------------------------------------------------------------------
cooccurrence_summary <- pattern_category_df %>%
  
  # Exclude non-explicit category selection and missing values
  filter(
    !category %in% c("Other", "No Dark Pattern Selected"),
    !is.na(category)
  ) %>%
  
  # Create a unique identifier for each response-level grouping
  unite(category_group, hash_id, response_id, remove = FALSE) %>%
  
  # Count how often pairs co-occur within the same response
  pairwise_count(category, category_group, sort = TRUE) %>%
  
  # Standardize ordering of category pairs (A–B == B–A)
  mutate(
    item1 = as.character(item1),
    item2 = as.character(item2),
    
    pair1 = pmin(item1, item2),
    pair2 = pmax(item1, item2)
  ) %>%
  
  # Remove duplicate unordered pairs
  distinct(pair1, pair2, .keep_all = TRUE) %>%
  
  # Drop helper columns
  select(-pair1, -pair2) %>%
  
  # Rename final output columns for clarity
  rename(
    pattern_A          = item1,
    pattern_B          = item2,
    cooccurrence_count = n
  )

# -----------------------------------------------------------------------------
# READOUT
# -----------------------------------------------------------------------------
# When print_and_write is TRUE, prints all summaries to the console and writes
# each to a CSV in the working directory. Set to FALSE to suppress output
# during non-interactive or pipeline runs.
#
# TODO: Replace the print_and_write flag with a formal config.R parameter or
#       command-line argument for cleaner pipeline control.
# -----------------------------------------------------------------------------
print_and_write <- TRUE

if (print_and_write) {
  # Console Output Block
  print(descriptive_stats)
  print(per_question_summary)
  print(concern_level_summary)
  print(pattern_category_summary)
  print(incomplete_rows_summary)
  print(open_response_summary)
  print(other_pattern_summary)
  print(cooccurrence_summary)
  print(pattern_count_summary)
  
  # CSV Writing Block
  write.csv(descriptive_stats,            "descriptive_stats.csv",            row.names = FALSE)
  write.csv(per_question_summary,     "per_question_summary.csv",     row.names = FALSE)
  write.csv(concern_level_summary,    "concern_level_summary.csv",    row.names = FALSE)
  write.csv(pattern_category_summary, "pattern_category_summary.csv", row.names = FALSE)
  write.csv(incomplete_rows_summary,  "incomplete_rows.csv",          row.names = FALSE)
  write.csv(open_response_summary,    "open_response_summary.csv",    row.names = FALSE)
  write.csv(other_pattern_summary,    "other_pattern_summary.csv",    row.names = FALSE)
  write.csv(cooccurrence_summary,     "cooccurrence_summary.csv",     row.names = FALSE)
  write.csv(pattern_count_summary,    "pattern_count_summary.csv",    row.names = FALSE)
}