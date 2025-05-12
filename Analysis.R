# 📦 Required Libraries
library(tidyverse)
library(tidytext)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(scales)

# 📊 Word Frequency Plot (Trump & Clinton only)
debate %>%
  filter(Speaker %in% c("Trump", "Clinton")) %>%
  unnest_tokens(word, Text) %>% # Tokenization
  anti_join(stop_words) %>%     # Remove stop words
  count(Speaker, word, sort = TRUE) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>%
  na.omit() %>%
  filter(n > 30) %>% # Only show words with frequency > 30
  ggplot(aes(reorder(word, n), n, fill = Speaker)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Score") +
  xlab("Words") +
  ggtitle("Word Frequency") +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 13),
    axis.title.y = element_text(face = "bold", size = 13)
  ) +
  scale_fill_manual(values = c("Trump" = "tomato", "Clinton" = "steelblue"))

# 📈 Sentiment Variation Over Time
debate %>%
  filter(Speaker %in% c("Trump", "Clinton")) %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(Speaker, index = Line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(index, sentiment, fill = Speaker)) +
  geom_col(show.legend = FALSE, width = 3) +
  facet_wrap(~Speaker, ncol = 18, scales = "free_x") +
  ggtitle("Sentiments Variation") +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 13),
    axis.title.y = element_text(face = "bold", size = 13)
  )

# 🧮 Top Positive & Negative Words by Speaker
debate %>%
  filter(Speaker %in% c("Trump", "Clinton")) %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(sentiment, Speaker) %>%
  count(word) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = Speaker)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  xlab("Words") +
  ylab("Frequency") +
  ggtitle("Word Usage") +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 13),
    axis.title.y = element_text(face = "bold", size = 13)
  )

# ☁️ Comparison Word Cloud (Positive vs Negative)
debate %>%
  filter(Speaker %in% c("Trump", "Clinton")) %>%
  unnest_tokens(word, Text) %>%
  mutate(word = gsub("problems", "problem", word)) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("red", "green"),  # Negative as red, Positive as green
    max.words = 100
  )
