library(tidytuesdayR)
simpsons_characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_characters.csv')

library(ggplot2)
library(dplyr)
library(stringr)

search_words <- c("donut", "homer", "nuclear")

filtered_data <- simpsons_characters %>%
  filter(str_detect(normalized_name, paste(search_words, collapse = "|")))

word_counts <- filtered_data %>%
  mutate(category = case_when(
    str_detect(normalized_name, "donut") ~ "Donut",
    str_detect(normalized_name, "homer") ~ "Homer",
    str_detect(normalized_name, "nuclear") ~ "Nuclear"
  )) %>%
  count(category)

ggplot(word_counts, aes(x = category, y = n, fill = category)) +
  geom_col(color = "black", alpha = 0.8) +
  labs(
    title = "Occurrences of Donut, Homer, and Nuclear in Names",
    subtitle = "From simpsons_characters dataset",
    x = "Word",
    y = "Count"
  ) +
  scale_fill_manual(values = c("Donut" = "brown", "Homer" = "blue", "Nuclear" = "green")) +
  theme_minimal()
