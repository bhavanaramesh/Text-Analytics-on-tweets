library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(tm)

filepath <- scan("Sentiment_Analysis_corpusdata.csv", what= "character", sep=NULL)
docs <- Corpus(VectorSource(filepath))
inspect(docs)
fileName <- trimws(docs)
tokens <- data_frame(text = fileName) %>% unnest_tokens(word, text) %>% anti_join(stop_words)

#using afinn
tokenss <- tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, score, sort = TRUE) %>%
  ungroup()

library(ggplot2)
library(igraph)
tokenss %>%
  group_by(score) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~score, scales = "free_y") +
  labs(y = "AFINN Sentiment scores for Apple watch series 4",
       x = NULL) +
  coord_flip() + ggtitle("Twitter Sentiment") + theme(plot.title = element_text(hjust = 1.0)) +
  scale_fill_gradient(low="red", high="green")

#write.csv(tokenss, file= "Score_AFINN.csv")





