# lab 6 text mining 

#install.packages("tidyverse")
#install.packages("gutenbergr")
#install.packages("tidytext")
#install.packages("reshape2")
#install.packages("wordcloud")
library(gutenbergr)
library(tidyverse)
library(tidytext)
library(dplyr)
library(wordcloud)

#looking in the books
edgar_df <- gutenberg_download(c(62, 78, 64, 1154, 551, 106, 106, 90, 605, 92, 123, 85, 1401, 58874, 3475, 59752, 3475, 59752, 363, 331, 552, 149, 369, 96, 554, 2020, 29405, 58904, 20802), meta_fields = "title")

# normalize text and turn untidy text into tidy text

# turning text df into tidy data
edgar_tidy <- edgar_df %>%
  unnest_tokens(word, text)

edgar_tidy

# remove stop words 
edgar_clean <- edgar_tidy %>%
  anti_join(get_stopwords())

# word count
edgar_count <- edgar_clean %>%
  count(word, sort = TRUE)
# sentiments
bing <- get_sentiments("bing")
  

# positive sentiments
edgar_pos<- edgar_count <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

# getting the positive and negitive sentiments
edgar_bing <- edgar_tidy %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

# graph of word count and sentiment from all of edgar's books
edgar_bing %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

# getting the idf and tf
edgar_idf <- edgar_tidy %>%
  bind_tf_idf(word, story, n)

edgar_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
