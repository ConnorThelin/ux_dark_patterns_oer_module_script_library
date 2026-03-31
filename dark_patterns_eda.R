
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
              "googledrive"
              )
invisible(lapply(packages, load_or_install))


# RAW DATA ----------------------------------------------------------------

if (!exists("raw_data")) {
  sheet_url <- "https://docs.google.com/spreadsheets/d/1vMhttsyyit3jwvAmZ0DEi_jWj7Hp-RS5SQR6I_E1ZFQ/edit?usp=sharing"
  raw_data <- read_sheet(sheet_url)
}
