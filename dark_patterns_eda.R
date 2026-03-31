
# SETUP -------------------------------------------------------------------

target_dir <- "C:/Users/conno/OneDrive/School/UW_2023-20XX/CSS Independent Study/R Version/Dark Patterns R"
if (getwd() != target_dir)
  setwd(target_dir)
rm(target_dir)


# PACKAGES ----------------------------------------------------------------

load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

packages <- c("tidyverse",
              "ggplot2",
              "googlesheets4",
              "googledrive",
              "digest",
              "janitor"
              )
invisible(lapply(packages, load_or_install))

# Usages:
# tidyverse: Base
# ggplot2: Visualizations
# googlesheets4: Google Sheet integration
# googledrive: Google Drive Integration
# digest: anonymization
# janitor: clean_names()


# RAW DATA ----------------------------------------------------------------

if (!exists("raw_data")) {
  sheet_url <- "https://docs.google.com/spreadsheets/d/1vMhttsyyit3jwvAmZ0DEi_jWj7Hp-RS5SQR6I_E1ZFQ/edit?usp=sharing"
  raw_data <- read_sheet(sheet_url)
}


# RENAME ROWS -------------------------------------------------------------

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
# Anonymize names via MD5 hashing

anonymize_names <- function(name_column, algo = "md5") {
  sapply(name_column, function(single_name) {
    if (is.na(single_name) || single_name == "") {
      return(NA)
    }
    else {
      tolower(trimws(single_name))
      return(digest(single_name, algo = algo))
    }
  })
}

modified_data$name <- anonymize_names(modified_data$name)

