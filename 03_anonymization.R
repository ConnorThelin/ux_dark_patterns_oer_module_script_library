# ANONYMIZE ---------------------------------------------------------------
# Anonymize names via MD5 hashing, empty names results in NA
# TODO Refactor to use form: <school>_<class_code>_<quarter_code>_<unique_id> and Response_<quarter_code>_<unique_row_id>

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

if (length(unique(tolower(trimws(raw_data$`Your name as it appears in Canvas  (Will be anonymized before incidents are shared with the class)`)))) !=
    length(unique(modified_data$hash_id))) {
  stop("Sanity check failed: mismatch between cleaned names and hash IDs")
}

