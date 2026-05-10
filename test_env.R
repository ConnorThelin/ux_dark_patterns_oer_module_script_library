
# TEXT MINING -------------------------------------------------------------

install.packages("tidytext")
library(tidytext)
data("stop_words")

test_data <- modified_data %>%
  select(response_id, link_to_system) %>%
  filter(!is.na(link_to_system)) %>%
  unnest_tokens(word, link_to_system) %>%
  anti_join(stop_words)

test_data <- test_data %>%
  filter(!str_detect(word, "^(http|www)$")) %>%
  filter(!str_detect(word, "^(com|org|net|edu|gov)$"))




library(stringr)

clean_data <- modified_data %>%
  mutate(
    url = str_extract(link_to_system, "https?://\\S+"),
    domain = str_extract(link_to_system, "(?<=https?://)[^/]+")
  )

clean_data <- clean_data %>%
  mutate(
    text_clean = str_remove(link_to_system, "https?://\\S+")
  )

clean_data <- clean_data %>%
  mutate(
    text_clean = str_remove(text_clean, ":\\s*$")
  )

library(tidytext)

tokens <- clean_data %>%
  select(response_id, text_clean) %>%
  filter(!is.na(text_clean)) %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words)

library(dplyr)
library(ggplot2)

library(scales)

tokens %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(
    title = "Most Frequent Words in Reported System Links",
    x = "Word",
    y = "Count"
  )


library(dplyr)

sentiment_data <- tokens %>%
  inner_join(get_sentiments("bing"), by = "word")


# COLUMN BY COLUMN --------------------------------------------------------

open_response_summary <- modified_data %>%
  filter(
    !is.na(response_id) &
      !is.na(link_to_system) &
      !is.na(task) &
      !is.na(suspicious_element) &
      !is.na(personal_impact)
  ) %>%
  select(response_id, link_to_system, task, suspisious_element, personal_impact) %>%
  arrange(response_id)


# COMBINATION FREQUENCY ---------------------------------------------------

library(tidyr)
library(widyr)

# V1 A - B, B - A problem (fixed)
cooccurrence2 <- pattern_category_df %>%
  filter(!category %in% c("Other", "No Dark Pattern Selected"),
         !is.na(category)) %>%
  unite(category_group, hash_id, response_id, remove = FALSE) %>%
  pairwise_count(category, category_group, sort = TRUE) %>%
  mutate(
    item1 = as.character(item1),
    item2 = as.character(item2),
    pair1 = pmin(item1, item2),
    pair2 = pmax(item1, item2)
  ) %>%
  distinct(pair1, pair2, .keep_all = TRUE) %>%
  select(-pair1, -pair2)

# V2 DEAD
pattern_category_df %>%
  filter(!is.na(category)) %>%
  distinct(hash_id, category) %>%
  inner_join(., ., by = "hash_id", relationship = "many-to-many") %>%
  filter(as.character(category.x) < as.character(category.y)) %>%
  count(category.x, category.y, sort = TRUE)

# V3 DEAD
cooccurrence <- pattern_category_df %>%
  filter(!category %in% c("Other", "No Dark Pattern Selected"),
         !is.na(category)) %>%
  mutate(doc_id = paste(hash_id, response_id, sep = "_")) %>%
  pairwise_count(category, doc_id, sort = TRUE) %>%
  transmute(
    pair1 = pmin(as.character(item1), as.character(item2)),
    pair2 = pmax(as.character(item1), as.character(item2)),
    n
  ) %>%
  group_by(pair1, pair2) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n))

# Validaion Testing

test_count <- pattern_category_df %>%
  filter(
    category %in% c("Interface Interference", "Sneaking"),
    !is.na(category)
  ) %>%
  group_by(original_row_id) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  select(original_row_id, category)

test_count2 <- pattern_category_df %>% filter( category %in% c("Interface Interference", "Sneaking"), !is.na(category) ) %>% select(original_row_id, category)

write.csv(test_count, "test_count.csv", row.names = FALSE)






# Name fixing

library(stringdist)

names <- unique(tolower(trimws(modified_data$name)))
dist_matrix <- stringdistmatrix(names, names, method = "jw")
# Flag pairs with distance below a threshold
suspects <- which(dist_matrix < 0.15 & dist_matrix > 0, arr.ind = TRUE)

suspects_df <- data.frame(
  name_1 = names[suspects[, 1]],
  name_2 = names[suspects[, 2]],
  distance = dist_matrix[suspects]
) %>%
  filter(name_1 < name_2) %>%   # remove mirror duplicates (A,B) and (B,A)
  arrange(distance)

print(suspects_df)

# Flag pairs where one name is a substring of the other
subset_suspects <- outer(names, names, function(a, b) {
  mapply(function(x, y) x != y & (grepl(x, y, fixed = TRUE) | grepl(y, x, fixed = TRUE)), a, b)
})

subset_pairs <- which(subset_suspects & upper.tri(subset_suspects), arr.ind = TRUE)

subset_df <- data.frame(
  name_1 = names[subset_pairs[, 1]],
  name_2 = names[subset_pairs[, 2]]
) %>%
  filter(name_1 < name_2)  # remove mirror duplicates

print(subset_df)

library(stringdist)
library(dplyr)
library(digest)

participation_report <- modified_data %>%
  # Basic normalization
  mutate(
    name = tolower(trimws(name)),
    name = gsub("\\s+", " ", name),
  ) %>%
  filter(!is.na(name) & name != "") %>%

  # Fuzzy deduplication via Jaro-Winkler + token subset matching
  mutate(name = {
    unique_names <- unique(name)
    dist_matrix  <- stringdistmatrix(unique_names, unique_names, method = "jw")
    canonical    <- unique_names

    # --- Pass 1: Jaro-Winkler (catches typos) ---
    for (i in seq_along(unique_names)) {
      close_matches <- which(dist_matrix[i, ] < 0.15 & dist_matrix[i, ] > 0)
      if (length(close_matches) > 0) {
        cluster          <- c(i, close_matches)
        winner           <- unique_names[min(cluster)]
        canonical[cluster] <- winner
      }
    }

    # --- Pass 2: Token subset matching (catches partial names) ---
    tokens <- strsplit(unique_names, " ")
    for (i in seq_along(unique_names)) {
      for (j in seq_along(unique_names)) {
        if (i == j) next
        ti <- tokens[[i]]
        tj <- tokens[[j]]
        # If all tokens in the shorter name exist in the longer name, merge them
        if (length(ti) < length(tj) && all(ti %in% tj)) {
          # Prefer the longer (more complete) name as canonical
          canonical[i] <- canonical[j]
        }
      }
    }

    # Print merge candidates before applying corrections
    merge_log <- data.frame(
      raw       = unique_names,
      canonical = canonical
    ) %>% filter(raw != canonical)

    if (nrow(merge_log) > 0) {
      message("\n--- Fuzzy Match Corrections ---")
      print(merge_log, row.names = FALSE)
      message("-------------------------------\n")
    } else {
      message("\n--- No fuzzy matches found ---\n")
    }

    canonical[match(name, unique_names)]
  }) %>%

  # Hash canonical name for stable respondent ID
  mutate(respondent_id = sapply(name, digest)) %>%

  group_by(respondent_id, name) %>%
  summarise(response_count = n(), .groups = "drop") %>%
  arrange



library(stringdist)
library(dplyr)
library(digest)

annotation_keywords <- c("day", "and", "or", "session", "part", "week",
                         "only", "both", "morning", "afternoon", "evening",
                         as.character(1:31))

# Strip punctuation and annotation keywords, return clean name tokens only
extract_name_tokens <- function(name) {
  tokens <- strsplit(gsub("[^a-z0-9 ]", "", name), " ")[[1]]
  tokens[!tokens %in% annotation_keywords & !grepl("^[0-9]", tokens)]
}

participation_report <- modified_data %>%
  mutate(
    name = tolower(trimws(name)),
    name = gsub("\\s+", " ", name)
  ) %>%
  filter(!is.na(name) & name != "") %>%
  mutate(name = {
    unique_names  <- unique(name)
    name_tokens   <- lapply(unique_names, extract_name_tokens)
    canonical     <- unique_names
    
    for (i in seq_along(unique_names)) {
      for (j in seq_along(unique_names)) {
        if (i == j) next
        ti <- name_tokens[[i]]
        tj <- name_tokens[[j]]
        
        # i is a subset of j — point i to j's canonical (longer real name wins)
        # i has noise beyond j — point i to j's canonical (clean name wins)
        if (length(ti) <= length(tj) && all(ti %in% tj)) {
          canonical[i] <- canonical[j]
        }
      }
    }
    
    # Also catch typos via Jaro-Winkler on cleaned tokens
    clean_names  <- sapply(name_tokens, paste, collapse = " ")
    dist_matrix  <- stringdistmatrix(clean_names, clean_names, method = "jw")
    for (i in seq_along(unique_names)) {
      close <- which(dist_matrix[i, ] < 0.15 & dist_matrix[i, ] > 0)
      if (length(close) > 0) canonical[c(i, close)] <- canonical[min(c(i, close))]
    }
    
    merge_log <- data.frame(raw = unique_names, canonical) %>%
      filter(raw != canonical)
    
    if (nrow(merge_log) > 0) {
      message("\n--- Fuzzy Match Corrections ---")
      print(merge_log, row.names = FALSE)
      message("-------------------------------\n")
    } else {
      message("\n--- No fuzzy matches found ---\n")
    }
    
    canonical[match(name, unique_names)]
  }) %>%
  mutate(respondent_id = sapply(name, digest)) %>%
  group_by(respondent_id, name) %>%
  summarise(response_count = n(), .groups = "drop") %>%
  arrange(name)