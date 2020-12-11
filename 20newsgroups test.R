# 20 NewsGroup test for R - Politics Mideast
# Reading in the text files
#https://cran.r-project.org/web/packages/readtext/vignettes/readtext_vignette.html
#install.packages("readtext")
library(readtext)

#Tokenization of the text 
library(dplyr)
library(tidytext)

a <- readtext::readtext(paste0("/Users/johnc.burns/Documents/Documents/PhD Year One/Material/Data/talk.politics.mideast/*"))
a$count <- 1:nrow(a)

txdf1 <- a %>% 
  unnest_tokens(word, text)

#How to deal with some of the weird words
custom_stop_words <- bind_rows(tibble(word = c("amp"), 
                                      lexicon = c("custom")),
                               stop_words)

#Removing stop words such as the, of, to
txdf2 <- txdf1 %>%
  dplyr::anti_join(custom_stop_words)

rm(txdf1)

#count the number of words frequency
txdf2 %>% dplyr::count(word, sort = TRUE)

# SENTIMENT ANALYSIS 
tidytext::get_sentiments("afinn")
tidytext::get_sentiments("bing")
tidytext::get_sentiments("nrc")

#Look into words related to fear 
nrc_fear <- tidytext::get_sentiments("nrc") %>%
  filter(sentiment == "fear")

txdf2 %>%
  inner_join(nrc_fear) %>%
  dplyr::count(word, sort = TRUE)

#Look into words related to negative
nrc_neg <- tidytext::get_sentiments("nrc") %>%
  filter(sentiment == "negative")

txdf2 %>%
  inner_join(nrc_neg) %>%
  dplyr::count(word, sort = TRUE)

#Look into words related to positive
nrc_pos <- tidytext::get_sentiments("nrc") %>%
  filter(sentiment == "positive")

txdf2 %>%
  inner_join(nrc_pos) %>%
  dplyr::count(word, sort = TRUE)

#Look into words related to joy
nrc_joy <- tidytext::get_sentiments("nrc") %>%
  filter(sentiment == "joy")

txdf2 %>%
  inner_join(nrc_joy) %>%
  dplyr::count(word, sort = TRUE)

#Get the sentiments of the documents
tweet_sentiment <- txdf2 %>%
  dplyr::inner_join(get_sentiments("bing")) %>%
  dplyr::count(count, sentiment) %>%
  tidyr::spread(sentiment, n, fill = 0) %>%
  dplyr::mutate(sentiment = positive - negative)

#Plot the sentiments 
ggplot(tweet_sentiment, aes(count, sentiment)) +
  geom_col(show.legend = FALSE)

#HOW WORD USAGE CHANGES OVER TIME 
#number of times a word appears in the tweets
txdf3_2 <- a %>%
  unnest_tokens(word, text) %>%
  group_by(count) %>%
  dplyr::count(word, sort = TRUE)

#Using the bind_tf_idf function
txdf2_words2 <- txdf3_2 %>%
  bind_tf_idf(word, count, n)

#Looking at terms in tweets with high tf-idf 
txdf2_words2 %>% 
  arrange(desc(tf_idf))

#get year from documents could use this with date 
year_term_counts <- txdf2_words2 %>%
  dplyr::group_by(count) %>%
  dplyr::mutate(time_total = sum(n))

#Get specific terms used in speeches and how they changed over time
library(ggplot2)
year_term_counts %>%
  dplyr::filter(word %in% c("war")) %>%
  ggplot(aes(count, n / time_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~word, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("%frequency of word in post")

