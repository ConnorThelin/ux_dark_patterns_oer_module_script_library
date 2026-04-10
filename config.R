# Usages:
# tidyverse: Base
# googlesheets4: Google Sheet integration
# googledrive: Google Drive Integration
# digest: anonymization
# janitor: clean_names()
# styler: formatting
packages <- c(
  "tidyverse",
  "ggplot2",
  "googlesheets4",
  "googledrive",
  "digest",
  "janitor",
  "styler"
)

# A URL, in quotations, for the google sheet housing the survey data
google_sheet_url <- "https://docs.google.com/spreadsheets/d/1vMhttsyyit3jwvAmZ0DEi_jWj7Hp-RS5SQR6I_E1ZFQ/edit?usp=sharing"

# Course metadata, used for anonymization
school_lookup <- c("University of Washington Bothell" = "UWB")

course_lookup <- c("Usability and User-Centered Design" = "CSS478")

quarter_lookup <- c(
  "Autumn" = "Au",
  "Winter" = "Wi",
  "Spring" = "Sp",
  "Summer" = "Su"
)

# Question look-up table
question_lookup <- c(
  dark_pattern_interaction = "Did you have any interactions today that you suspect might involve a UX Dark Pattern ?",
  link_to_system = "If you reported 'yes' or 'unsure' please answer the following questions. Name and link to the system or service",
  task = "What were you trying to do?",
  suspisious_element = "What was suss?",
  personal_impact = "Did encountering this pattern impact you in any way?  Please explain.
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

# Concern Level look-up table
concern_level_lookup <- c(
  "1" = "No concern",
  "2" = "Mild concern",
  "3" = "Moderate concern",
  "4" = "High concern",
  "5" = "Extreme concern"
)