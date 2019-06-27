library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(tm)


filepath <- scan("Sentiment_Analysis_corpusdata.csv", what= "character", sep=NULL)

docs <- Corpus(VectorSource(filepath))
inspect(docs)
fileName <- trimws(docs)
tokens <- data_frame(text = fileName) %>% unnest_tokens(word, text)

# using nrc
tokenss <- tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
tokenss %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Overall Public Emotions for Apple watch series 4",
       x = NULL) +
  coord_flip() + ggtitle("Twitter Sentiment") + theme(plot.title = element_text(hjust = 0.3)) 

#write.csv(tokenss, file= "SCORE_NRC_Sentiment.csv.csv")

