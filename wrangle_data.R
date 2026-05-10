# =============================================================================
# DATA WRANGLING
# =============================================================================
# Transforms raw_data into modified_data through five sequential steps:
#   1. Clean names    — snake_case standardization
#   2. Add columns    — creates friendly-named columns alongside originals
#   3. Parse timestamp — converts raw string to POSIXct for date arithmetic
#   4. Add response ID — stable integer key for row tracking/debugging
#   5. Factorize       — converts concern_level to ordered factor with labels
#   6. Normalize NAs   — coerces written NA variants to proper R NA
# =============================================================================

# -----------------------------------------------------------------------------
# CLEAN NAMES
# -----------------------------------------------------------------------------
# Standardizes all column names to snake_case.
# -----------------------------------------------------------------------------
modified_data <- anon_data %>%
  clean_names() %>%
  mutate(original_row_id = row_number())

# -----------------------------------------------------------------------------
# ADD COLUMNS
# -----------------------------------------------------------------------------
# Creates friendly-named columns alongside the original snake_case columns
#
# TODO: Move mappings to config.R for easier maintenance.
# -----------------------------------------------------------------------------
modified_data <- modified_data %>%
  mutate(
    timestamp                = timestamp,
    hash_id                  = your_name_as_it_appears_in_canvas_will_be_anonymized_before_incidents_are_shared_with_the_class,
    dark_pattern_interaction = did_you_have_any_interactions_today_that_you_suspect_might_involve_a_ux_dark_pattern,
    link_to_system           = if_you_reported_yes_or_unsure_please_answer_the_following_questions_name_and_link_to_the_system_or_service,
    task                     = what_were_you_trying_to_do,
    suspicious_element       = what_was_suss,
    personal_impact          = did_encountering_this_pattern_impact_you_in_any_way_please_explain_examples_of_possible_impacts_include_but_are_not_limited_to_not_achieving_what_you_want_to_accomplish_hindering_what_you_wanted_to_accomplish_costing_you_more_time_money_or_effort_than_anticipated_desired_less_confidence_or_less_certainty_that_you_have_achieved_your_aim_less_confidence_or_less_certainty_that_you_have_mitigated_undesirable_consequences_undue_emotional_labor_possible_consequences_for_others_and_so_on,
    concern_level            = based_on_your_personal_feelings_at_this_time_please_mark_the_degree_of_concern_you_have_about_this_incident_no_concern_1_mild_concern_2_moderate_concern_3_high_concern_4_extreme_concern_5,
    image_captured           = images_or_video_of_a_problematic_interaction_are_very_helpful_for_analysis_did_you_capture_any_images_or_video_of_the_interaction_you_are_reporting_on_we_will_reach_out_when_the_collection_period_is_concluded_to_collect,
    involved_pattern         = which_ux_dark_patterns_do_you_think_might_be_involved_if_any_check_all_that_apply_patterns_are_from_appendix_a_of_2022_us_ftc_staff_report_bringing_dark_patterns_to_light,
    comments                 = anything_else,
    image                    = if_you_have_screenshots_or_other_documentation_you_can_upload_to_10_files_of_100_mb_in_size_via_one_submission
  )

# -----------------------------------------------------------------------------
# PARSE TIMESTAMP
# -----------------------------------------------------------------------------
# Converts the raw timestamp string to POSIXct for date arithmetic.
# Format matches Google Form CSV export: M/D/YYYY H:MM:SS
# -----------------------------------------------------------------------------
modified_data <- modified_data %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M:%S"))

# -----------------------------------------------------------------------------
# ADD RESPONSE ID
# -----------------------------------------------------------------------------
# Assigns a stable integer ID (response_id) derived from original_row_id and
# positions it as the first column for easy row identification.
#
# TODO: Primarily used for debugging, consider folding into step 1 once stable.
# -----------------------------------------------------------------------------
modified_data <- modified_data %>%
  mutate(response_id = original_row_id) %>%
  relocate(response_id, .before = 1)

# -----------------------------------------------------------------------------
# FACTORIZE CONCERN LEVEL
# -----------------------------------------------------------------------------
# Converts the raw integer concern_level (1–5) to an ordered factor, then
# derives concern_level_label from concern_level_lookup (defined in config.R),
# also as an ordered factor. The label column is placed immediately after
# concern_level for readability.
# -----------------------------------------------------------------------------
modified_data <- modified_data %>%
  mutate(
    concern_level = factor(
      concern_level,
      levels = as.integer(names(concern_level_lookup))
    ),
    
    # Create a new column with descriptive labels by indexing the concern_level_lookup in config.R
    concern_level_label = concern_level_lookup[as.character(concern_level)],
    
    # Convert the label column into a factor with a defined ordering
    concern_level_label = factor(
      concern_level_label,
      levels = concern_level_lookup
    )
  ) %>%
  relocate(concern_level_label, .after = concern_level)

# -----------------------------------------------------------------------------
# NORMALIZE WRITTEN NAs
# -----------------------------------------------------------------------------
# Respondents sometimes enter NA variants as free text (e.g. "n/a", "none").
# clean_missing() trims white space, forces lowercase, and coerces known NA strings
# to proper R NA across all open-response columns.
#
# TODO: Move NA variant list to config.R as a look-up table.
# -----------------------------------------------------------------------------
clean_missing <- function(x) {
  x <- trimws(tolower(x))
  x[x %in% c("", "na", "n/a", "none", "null", "nil", "n.a", "n a")] <- NA
  return(x)
}

modified_data <- modified_data %>%
  mutate(across(
    c(link_to_system, task, suspicious_element, personal_impact, comments),
    clean_missing
  ))