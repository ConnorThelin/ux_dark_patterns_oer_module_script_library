# SETUP -------------------------------------------------------------------

target_dir <- "C:/Users/conno/OneDrive/School/UW_2023-20XX/CSS Independent Study/R Version/Dark Patterns R"
if (getwd() != target_dir) {
  setwd(target_dir)
}
rm(target_dir)


# PACKAGES ----------------------------------------------------------------

load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

packages <- c(
  "tidyverse",
  "googlesheets4",
  "googledrive",
  "digest",
  "janitor",
  "styler"
)
invisible(lapply(packages, load_or_install))

# Usages:
# tidyverse: Base
# ggplot2: Visualizations
# googlesheets4: Google Sheet integration
# googledrive: Google Drive Integration
# digest: anonymization
# janitor: clean_names()
# styler: formatting


# RAW DATA ----------------------------------------------------------------
# Pulls from Google Sheets; guarded by exists() to prevent redundant API calls on re-runs within the same session
if (!exists("raw_data")) {
  sheet_url <- "https://docs.google.com/spreadsheets/d/1vMhttsyyit3jwvAmZ0DEi_jWj7Hp-RS5SQR6I_E1ZFQ/edit?usp=sharing"
  raw_data <- read_sheet(sheet_url)
}

# RENAME ROWS -------------------------------------------------------------
# clean_names() snake_cases all column names; rename() replaces Google Form question strings with readable column names
modified_data <- raw_data %>%
  clean_names() %>%
  rename(
    name = "your_name_as_it_appears_in_canvas_will_be_anonymized_before_incidents_are_shared_with_the_class",
    dark_pattern_interaction = "did_you_have_any_interactions_today_that_you_suspect_might_involve_a_ux_dark_pattern",
    link_to_system = "if_you_reported_yes_or_unsure_please_answer_the_following_questions_name_and_link_to_the_system_or_service",
    task = "what_were_you_trying_to_do",
    suspisious_element = "what_was_suss",
    personal_impact = "did_encountering_this_pattern_impact_you_in_any_way_please_explain_examples_of_possible_impacts_include_but_are_not_limited_to_not_achieving_what_you_want_to_accomplish_hindering_what_you_wanted_to_accomplish_costing_you_more_time_money_or_effort_than_anticipated_desired_less_confidence_or_less_certainty_that_you_have_achieved_your_aim_less_confidence_or_less_certainty_that_you_have_mitigated_undesirable_consequences_undue_emotional_labor_possible_consequences_for_others_and_so_on",
    concern_level = "based_on_your_personal_feelings_at_this_time_please_mark_the_degree_of_concern_you_have_about_this_incident_no_concern_1_mild_concern_2_moderate_concern_3_high_concern_4_extreme_concern_5",
    image_captured = "images_or_video_of_a_problematic_interaction_are_very_helpful_for_analysis_did_you_capture_any_images_or_video_of_the_interaction_you_are_reporting_on_we_will_reach_out_when_the_collection_period_is_concluded_to_collect",
    involved_pattern = "which_ux_dark_patterns_do_you_think_might_be_involved_if_any_check_all_that_apply_patterns_are_from_appendix_a_of_2022_us_ftc_staff_report_bringing_dark_patterns_to_light",
    comments = "anything_else",
    image = "if_you_have_screenshots_or_other_documentation_you_can_upload_to_10_files_of_100_mb_in_size_via_one_submission"
  )

# ANONYMIZE ---------------------------------------------------------------
# Anonymize names via MD5 hashing, empty names results in NA
# TODO Generate more human friendly IDs or use different anonymization method
# TODO Investigate building an anonymization screen that contains identifiers for school, course name, etc

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

modified_data$name <- anonymize_names(modified_data$name)
modified_data <- rename(modified_data, hash_id = name)

# ADD REPONSE ID ----------------------------------------------------------
# Assigns stable integer ID
modified_data <- modified_data %>%
  mutate(response_id = row_number()) %>%
  relocate(response_id, .before = hash_id)


# SUMMARY STATS: RESPONSE COUNTS ------------------------------------------
# Counts Yes / Unsure / No responses to the dark pattern encounter question
# TODO Add capability to catch NAs into Unsure
summary_stats <- data.frame(
  total_count  = nrow(modified_data),
  yes_count    = nrow(filter(modified_data, dark_pattern_interaction == "Yes")),
  unsure_count = nrow(filter(modified_data, dark_pattern_interaction == "Unsure")),
  no_count     = nrow(filter(modified_data, dark_pattern_interaction == "No"))
)

# Sanity check: yes + unsure + no should equal total
if (summary_stats$total_count != summary_stats$yes_count + summary_stats$unsure_count + summary_stats$no_count) {
  print("ERROR: total != combined count")
}

# SUMMARY STATS: RESPONSE PCT ---------------------------------------------
# TODO Add error check
summary_stats <- summary_stats %>%
  select(total_count, yes_count, unsure_count, no_count) %>%
  mutate(
    yes_pct = (yes_count / total_count),
    unsure_pct = (unsure_count / total_count),
    no_pct = (no_count / total_count)
  ) %>%
  relocate(yes_pct, .after = yes_count) %>%
  relocate(unsure_pct, .after = unsure_count) %>%
  relocate(no_pct, .after = no_count)

# SUMMARY STATS: DATE RANGE -----------------------------------------------
# Captures collection window from first to last timestamp
# TODO Add error check
# TODO Add tracking for most relevant day, time, etc
# TODO Force data format (should be received as proper format though)
summary_stats <- summary_stats %>%
  mutate(
    start_date = min(modified_data$timestamp),
    end_date = max(modified_data$timestamp),
    time_span_days = floor(as.numeric(difftime(max(modified_data$timestamp), min(modified_data$timestamp), units = "days"))),
    time_span_months = floor(as.numeric(difftime(max(modified_data$timestamp), min(modified_data$timestamp), units = "weeks")))
  )


# SUMMARY STATS: CONCERN LEVEL --------------------------------------------
# Concern is a 1-5 Likert scale
# TODO: Investigate NA replacement strategies

summary_stats <- summary_stats %>%
  mutate(
    mean_concern     = mean(modified_data$concern_level, na.rm = TRUE),
    median_concern   = median(modified_data$concern_level, na.rm = TRUE),
    sd_concern       = sd(modified_data$concern_level, na.rm = TRUE),
    na_concern_count = sum(is.na(modified_data$concern_level)),
    na_concern_pct   = na_concern_count / total_count
  )

# TEST BLOCK: NA REPLACEMENT ----------------------------------------------
# Compares the effect of replacing concern_level NAs with mean vs median

summary_stats_na_replacement_test_mean <- summary_stats %>%
  mutate(
    mean_concern_filled     = mean(ifelse(is.na(modified_data$concern_level), mean_concern, modified_data$concern_level)),
    median_concern_filled   = median(ifelse(is.na(modified_data$concern_level), mean_concern, modified_data$concern_level)),
    sd_concern_filled       = sd(ifelse(is.na(modified_data$concern_level), mean_concern, modified_data$concern_level)),
    na_concern_count_filled = sum(is.na(ifelse(is.na(modified_data$concern_level), mean_concern, modified_data$concern_level)))
  )

summary_stats_na_replacement_test_median <- summary_stats %>%
  mutate(
    mean_concern_filled     = mean(ifelse(is.na(modified_data$concern_level), median_concern, modified_data$concern_level)),
    median_concern_filled   = median(ifelse(is.na(modified_data$concern_level), median_concern, modified_data$concern_level)),
    sd_concern_filled       = sd(ifelse(is.na(modified_data$concern_level), median_concern, modified_data$concern_level)),
    na_concern_count_filled = sum(is.na(ifelse(is.na(modified_data$concern_level), median_concern, modified_data$concern_level)))
  )

cat("=== Concern Level: Original (with NAs) ===\n")
cat(sprintf("Mean:   %.4f\n", summary_stats$mean_concern))
cat(sprintf("Median: %.4f\n", summary_stats$median_concern))
cat(sprintf("SD:     %.4f\n", summary_stats$sd_concern))
cat(sprintf("NAs:    %d (%.1f%%)\n\n", summary_stats$na_concern_count, summary_stats$na_concern_pct * 100))

cat("=== Concern Level: NAs Replaced with Mean ===\n")
cat(sprintf("Mean:   %.4f\n", summary_stats_na_replacement_test_mean$mean_concern_filled))
cat(sprintf("Median: %.4f\n", summary_stats_na_replacement_test_mean$median_concern_filled))
cat(sprintf("SD:     %.4f\n", summary_stats_na_replacement_test_mean$sd_concern_filled))
cat(sprintf("NAs:    %d\n\n", summary_stats_na_replacement_test_mean$na_concern_count_filled))

cat("=== Concern Level: NAs Replaced with Median ===\n")
cat(sprintf("Mean:   %.4f\n", summary_stats_na_replacement_test_median$mean_concern_filled))
cat(sprintf("Median: %.4f\n", summary_stats_na_replacement_test_median$median_concern_filled))
cat(sprintf("SD:     %.4f\n", summary_stats_na_replacement_test_median$sd_concern_filled))
cat(sprintf("NAs:    %d\n\n", summary_stats_na_replacement_test_median$na_concern_count_filled))


# BUILD INTERACTION ANALYSIS --------------------------------------------------------
# This separates each row into multiple rows per selection, delineated on "),"; splits unknown categories into "Other". Translates data from n-respondents to n-selections
# TODO Investigate benefits between elongation (1 row/selection) vs widening (1 col/selection)
# TODO Add a way to collect NAs only associated with "No" in the encounter question

pre_elongation_data <- modified_data

modified_data <- modified_data %>%
  filter(!is.na(involved_pattern)) %>%
  separate_rows(involved_pattern, sep = "(?<=\\)), ") %>%
  mutate(category = case_when(
    str_detect(involved_pattern, "Social Proof")            ~ "Social Proof",
    str_detect(involved_pattern, "Scarcity")                ~ "Scarcity",
    str_detect(involved_pattern, "Urgency")                 ~ "Urgency",
    str_detect(involved_pattern, "Obstruction")             ~ "Obstruction",
    str_detect(involved_pattern, "Sneaking")                ~ "Sneaking",
    str_detect(involved_pattern, "Interface Interference")  ~ "Interface Interference",
    str_detect(involved_pattern, "Coerced Action")          ~ "Coerced Action",
    str_detect(involved_pattern, "Asymmetric Choice")       ~ "Asymmetric Choice",
    TRUE                                                    ~ "Other"
  ),
  category = factor(category, levels = c(
    "Social Proof",
    "Scarcity",
    "Urgency",
    "Obstruction",
    "Sneaking",
    "Interface Interference",
    "Coerced Action",
    "Asymmetric Choice",
    "Other"
  )))

# SUMMARY STATS: CATEGORY COUNTS ------------------------------------------
# Counts selections per dark pattern category from the elongated data
# na_interaction_count counts respondents with no pattern selected (from pre-elongation data), expected for "No" responses to dark_pattern_interaction
# unexpected_na_count flags respondents who answered Yes/Unsure but left the pattern question blank, these are potential data errors

summary_stats <- summary_stats %>%
  mutate(
    social_proof_count           = sum(modified_data$category == "Social Proof", na.rm = TRUE),
    scarcity_count               = sum(modified_data$category == "Scarcity", na.rm = TRUE),
    urgency_count                = sum(modified_data$category == "Urgency", na.rm = TRUE),
    obstruction_count            = sum(modified_data$category == "Obstruction", na.rm = TRUE),
    sneaking_count               = sum(modified_data$category == "Sneaking", na.rm = TRUE),
    interface_interference_count = sum(modified_data$category == "Interface Interference", na.rm = TRUE),
    coerced_action_count         = sum(modified_data$category == "Coerced Action", na.rm = TRUE),
    asymmetric_choice_count      = sum(modified_data$category == "Asymmetric Choice", na.rm = TRUE),
    other_interaction_count      = sum(modified_data$category == "Other", na.rm = TRUE),
    na_interaction_count         = sum(is.na(pre_elongation_data$involved_pattern)),
    na_interaction_pct           = na_interaction_count / respondant_total_count,
    unexpected_na_count         = pre_elongation_data %>%
      filter(dark_pattern_interaction %in% c("Yes", "Unsure") & is.na(involved_pattern)) %>%
      nrow()
  )

# Category selection frequency
modified_data %>%
  count(category, sort = TRUE)

# SUMMARY STATS: READ-OUT -------------------------------------------------

summary_stats %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(output = paste0(metric, " : ", value)) %>%
  pull(output) %>%
  cat(sep = "\n")

# Checks the most common pairs of dark patterns selected together
# Self-joins elongated data on hash_id to create all category pairs per respondent
modified_data %>%
  filter(!is.na(category)) %>%
  select(hash_id, category) %>%
  inner_join(., ., by = "hash_id", relationship = "many-to-many") %>%
  filter(as.integer(category.x) < as.integer(category.y)) %>%
  count(category.x, category.y, sort = TRUE)

