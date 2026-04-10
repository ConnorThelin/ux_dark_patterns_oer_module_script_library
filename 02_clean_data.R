# RENAME ROWS -------------------------------------------------------------
# clean_names() snake_cases all column names; rename() replaces Google Form question strings with readable column names
# TODO Add rename block to config.R
modified_data <- raw_data %>%
  clean_names() %>%
  mutate(original_row_id = row_number()) %>%
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



# ADD REPONSE ID ----------------------------------------------------------
# Assigns stable integer ID
# TODO Mostly for debugging, can relegate to RENAME ROWS block at a later stage
modified_data <- modified_data %>%
  mutate(response_id = original_row_id) %>%
  relocate(response_id, .before = timestamp)


# FACTORIZE CONCERN LEVEL + ADD SEMANTIC MEANING --------------------------

modified_data <- modified_data %>%
  mutate(
    concern_level = factor(
      concern_level,
      levels = as.integer(names(concern_level_lookup))
    ),
    concern_level_label = concern_level_lookup[as.character(concern_level)],
    concern_level_label = factor(
      concern_level_label,
      levels = concern_level_lookup
    )
  ) %>%
  relocate(concern_level_label, .after = concern_level)


# FORCE PROPER NA ON WRITTEN NA -------------------------------------------
# TODO Add an open response look up table to reference
clean_missing <- function(x) {
  x <- trimws(tolower(x))
  
  x[x %in% c(
    "", "na", "n/a", "none", "null", "nil", "n.a", "n a"
  )] <- NA
  
  return(x)
}

modified_data <- modified_data %>%
  mutate(across(
    c(link_to_system, task, suspisious_element, personal_impact, comments),
    clean_missing
  ))

