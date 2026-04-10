
# FIRST EDA PASS ----------------------------------------------------------

# RESPONSE COUNTS ------------------------------------------

# Counts Yes / Unsure / No responses to the dark pattern encounter question
# TODO More advanced API calls are required to pull the name of the form (rather than the sheet)
summary_stats <- modified_data %>%
  summarise(
    sheet_name             = sheet_name,
    respondant_total_count = n_distinct(hash_id),
    response_total_count   = n(),
    response_yes_count     = sum(dark_pattern_interaction == "Yes",    na.rm = TRUE),
    response_yes_pct       = round(response_yes_count    / response_total_count * 100, 1),
    response_unsure_count  = sum(dark_pattern_interaction == "Unsure", na.rm = TRUE),
    response_unsure_pct    = round(response_unsure_count / response_total_count * 100, 1),
    response_no_count      = sum(dark_pattern_interaction == "No",     na.rm = TRUE),
    response_no_pct        = round(response_no_count     / response_total_count * 100, 1)
  )

# Sanity check: yes + unsure + no should equal total
if (summary_stats$response_total_count != summary_stats$response_yes_count + summary_stats$response_unsure_count + summary_stats$response_no_count) {
  stop(
    paste0(
      "Sanity check failed: total (",
      summary_stats$response_total_count,
      ") != sum of categories (",
      summary_stats$response_yes_count + summary_stats$response_unsure_count + summary_stats$response_no_count,
      ")"
    )
  )
}


# RESPONDANT RESPONSE COUNT -----------------------------------------------

respondent_summary <- modified_data %>%
  group_by(hash_id) %>%
  summarise(response_count = n(), .groups = "drop")


# DATE RANGE -----------------------------------------------

# Captures collection window from first to last timestamp
# TODO Add error check
# TODO Add tracking for most relevant day, time, etc
summary_stats <- summary_stats %>%
  mutate(
    start_date = as.Date(min(modified_data$timestamp)),
    end_date = as.Date(max(modified_data$timestamp)),
    time_span_days = floor(as.numeric(difftime(max(modified_data$timestamp), min(modified_data$timestamp), units = "days"))),
    time_span_weeks = floor(as.numeric(difftime(max(modified_data$timestamp), min(modified_data$timestamp), units = "weeks")))
  )


# FLAG INCOMPLETE ---------------------------------------------------------

incomplete_rows_summary <- modified_data %>%
  filter(
    if_all(all_of(names(question_lookup)), ~ is.na(.) | . == "")
  )

# PER QUESTION RESPONSE RATE -------------------------------

per_question_summary <- modified_data %>%
  summarise(across(all_of(names(question_lookup)), ~ sum(!is.na(.) & . != ""))) %>%
  pivot_longer(everything(), names_to = "question_key", values_to = "answered_count") %>%
  mutate(
    question_number = row_number(),
    response_rate   = round(answered_count / summary_stats$response_total_count * 100, 1),
    question_text   = unname(question_lookup[question_key])
  ) %>%
  select(question_number, question_key, answered_count, response_rate, question_text)

# CONCERN LEVEL SUMMARY --------------------------------------------------

summary_stats <- summary_stats %>%
  mutate(
    mean_concern     = mean(as.numeric(modified_data$concern_level), na.rm = TRUE),
    median_concern   = median(as.numeric(modified_data$concern_level), na.rm = TRUE),
    sd_concern       = sd(as.numeric(modified_data$concern_level), na.rm = TRUE),
    na_concern_count = sum(is.na(modified_data$concern_level)),
    na_concern_pct   = na_concern_count / response_total_count
  )

concern_level_summary <- modified_data %>%
  count(concern_level, concern_level_label) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  arrange(concern_level)


# PATTERN CATEGORY SUMMARY ------------------------------------------------

modified_data <- modified_data %>%
  mutate(
    involved_pattern = na_if(involved_pattern, ""),
    involved_pattern = na_if(involved_pattern, "NA")
  )

pattern_category_df <- modified_data %>%
  separate_rows(involved_pattern, sep = "(?<=\\)), ") %>%
  mutate(category = case_when(
    str_detect(involved_pattern, "Social Proof")           ~ "Social Proof",
    str_detect(involved_pattern, "Scarcity")               ~ "Scarcity",
    str_detect(involved_pattern, "Urgency")                ~ "Urgency",
    str_detect(involved_pattern, "Obstruction")            ~ "Obstruction",
    str_detect(involved_pattern, "Sneaking")               ~ "Sneaking",
    str_detect(involved_pattern, "Interface Interference") ~ "Interface Interference",
    str_detect(involved_pattern, "Coerced Action")         ~ "Coerced Action",
    str_detect(involved_pattern, "Asymmetric Choice")      ~ "Asymmetric Choice",
    dark_pattern_interaction %in% c("Yes", "Unsure") & is.na(involved_pattern) | involved_pattern == "" ~ "No Dark Pattern Selected",
    is.na(involved_pattern)                                ~ "NA",
    TRUE                                                   ~ "Other"
  ),
  category = factor(category, levels = c(
    "Social Proof", "Scarcity", "Urgency", "Obstruction", "Sneaking",
    "Interface Interference", "Coerced Action", "Asymmetric Choice", "No Dark Pattern Selected", "Other"
  )))

pattern_category_df %>%
  filter(category == "Other") %>%
  select(involved_pattern) %>%
  distinct()

pattern_category_summary <- pattern_category_df %>%
  count(category, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))


# OPEN RESPONSE SUMMARY ---------------------------------------------------

open_response_summary <- modified_data %>%
  filter(
    !is.na(response_id) &
      !is.na(link_to_system) &
      !is.na(task) &
      !is.na(suspisious_element) &
      !is.na(personal_impact)
  ) %>%
  select(response_id, link_to_system, task, suspisious_element, personal_impact) %>%
  arrange(response_id)

# READOUT -----------------------------------------------------------------
print_and_write = FALSE

if (print_and_write == TRUE) {
  print(summary_stats)
  print(respondent_summary)
  print(per_question_summary)
  print(concern_level_summary)
  print(pattern_category_summary)
  print(incomplete_rows_summary)
  print(open_response_summary)

  write.csv(summary_stats,            "summary_stats.csv",            row.names = FALSE)
  write.csv(respondent_summary,       "respondent_summary.csv",       row.names = FALSE)
  write.csv(per_question_summary,     "per_question_summary.csv",     row.names = FALSE)
  write.csv(concern_level_summary,    "concern_level_summary.csv",    row.names = FALSE)
  write.csv(pattern_category_summary, "pattern_category_summary.csv", row.names = FALSE)
  write.csv(incomplete_rows_summary,  "incomplete_rows.csv",          row.names = FALSE)
  write.csv(open_response_summary,    "open_response_summary.csv",    row.names = FALSE)
  }