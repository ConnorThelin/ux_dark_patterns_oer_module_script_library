# =============================================================================
# CONFIGURATION
# =============================================================================
# Central configuration file for the dark pattern survey data pipeline.
# Defines all shared constants consumed by downstream scripts. Nothing here
# should need to change between pipeline runs except when onboarding a new
# course, quarter, or survey instrument.
#
# Contents:
#   Packages         – Libraries required across the pipeline
#   CSV Path         – Source of raw survey response data
#   Course metadata  – School, course, and quarter abbreviation mappings
#   Question lookup  – Maps short variable names to full survey question text
#   Concern level    – Maps numeric Likert scores (1–5) to labeled categories
# =============================================================================

# -----------------------------------------------------------------------------
# PACKAGE REQUIREMENTS
# -----------------------------------------------------------------------------
# Usages:
# tidyverse: Base
# digest:    anonymization
# janitor:   clean_names()
# widyr:     pairwise_count()
# styler:    formatting
# -----------------------------------------------------------------------------
packages <- c(
  "tidyverse",
  "digest",
  "janitor",
  "widyr",
  "styler"
)

# -----------------------------------------------------------------------------
# CSV PATH
# -----------------------------------------------------------------------------
# A file path, in quotations, to the CSV housing the survey data.
# Can be a relative path (e.g., "data/survey_responses.csv") or absolute.
csv_path <- "CSS 478 Diary Study.csv"

# -----------------------------------------------------------------------------
# EXPECTED RESPONDENT COUNT
# -----------------------------------------------------------------------------
# The number of respondents expected to have participated in the survey.
# Used as a sanity check in participation_report.R to verify that the
# number of unique participants matches expectations.
# -----------------------------------------------------------------------------
expected_respondent_count <- 84


# -----------------------------------------------------------------------------
# COURSE META DATA TABLES (WIP)
# -----------------------------------------------------------------------------
school_lookup <- c("University of Washington Bothell" = "UWB")

course_lookup <- c("Usability and User-Centered Design" = "CSS478")

quarter_lookup <- c(
  "Autumn" = "Au",
  "Winter" = "Wi",
  "Spring" = "Sp",
  "Summer" = "Su"
)


# -----------------------------------------------------------------------------
# QUESTION LOOK-UP TABLE
# -----------------------------------------------------------------------------
# Maps R variable names to full survey question text shown to participants
question_lookup <- c(
  dark_pattern_interaction = "Did you have any interactions today that you suspect might involve a UX Dark Pattern ?",
  link_to_system = "If you reported 'yes' or 'unsure' please answer the following questions. Name and link to the system or service",
  task = "What were you trying to do?",
  suspicious_element = "What was suss?",
  personal_impact =
    "Did encountering this pattern impact you in any way?  Please explain.
  (Examples of possible impacts include but are not limited to:
-not achieving what you want to accomplish;
-hindering what you wanted to accomplish;
-costing you more time, money, or effort than anticipated/desired;
-less confidence or less certainty that you have achieved your aim
-less confidence or less certainty that you have mitigated undesirable consequences;
-undue emotional labor;
-possible consequences for others;
and so on.) ",
  concern_level = "Based on your personal feelings at this time, please mark the degree of concern you have about this incident.
No concern (1)
Mild concern (2)
Moderate concern (3)
High concern (4)
Extreme concern(5)",
  image_captured = "Images or video of a problematic interaction are very helpful for analysis. Did you capture any images or video of the interaction you are reporting on?  (We will reach out when the collection period is concluded to collect.)",
  involved_pattern = "Which UX Dark Patterns do you think might be involved (if any)? (Check all that apply)
Patterns are from Appendix A of  2022 US FTC Staff Report Bringing Dark Patterns to Light'",
  comments = "Anything else?",
  image = "If you have screenshots or other documentation, you can upload to 10 files of 100 MB in size via one submission."
)

# -----------------------------------------------------------------------------
# CONCERN LEVEL LOOK-UP TABLE
# -----------------------------------------------------------------------------
# Maps the associated Likert scale with its respective integer
concern_level_lookup <- c(
  "1" = "No concern",
  "2" = "Mild concern",
  "3" = "Moderate concern",
  "4" = "High concern",
  "5" = "Extreme concern"
)