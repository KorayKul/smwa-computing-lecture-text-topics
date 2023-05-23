#' topic_modelling.R
#' 
#' contributors: 
#'
#' What this file does"
#' - Explores the stm package for topic modelling as applied to tidied tweets from Trump in early 2020
#'

# --- Library --- #
library(readr)
library(dplyr)
library(tibble)
library(tidytext)
library(textstem)
library(stm)
library(ggplot2)
# install.packages("Rtsne")
# install.packages("rsvd")
# install.packages("geometry")
library(Rtsne)
library(rsvd)
library(geometry)

# --- load the data --- # 
tidy_trump <- read_csv('data/tidy_trump.csv')

# --- Data Cleaning --- #
# Need stop word removal
# And lemmatize(changing endings 's -ed -ing)
tidy_trump<- tidy_trump %>% 
    # for simplicity stick to stopword list
    anti_join(stop_words) %>% 
    mutate(word = lemmatize_words(word))


# --- Establish a Vocab List --- #
# Keep any word that appears > 5 times
# Note: this is a little ad-hoc
# Should explore sensitivity to choice or use TF-IDF 

# set up what my vocab list will be
word_counts <- 
    tidy_trump %>% 
    group_by(word) %>% 
    count(sort= TRUE) %>% 
    filter(n > 5)


# Keep only those words in data
# only keep words from my vocab list
tidy_trump <- tidy_trump %>% 
    filter(word %in% word_counts$word)


# --- Create Doc-Term-Matrix --- #
doc_word_counts <- 
    tidy_trump %>% 
    group_by(id,word) %>% 
    count() %>% 
    ungroup()

# cast this to a matrix
dtm <- doc_word_counts %>%  
    cast_sparse(id,word, n)

# --- Model! --- #
# model
# word count together are topic,
topic_mod <- 
    stm(dtm,
        K = 10,
        seed = 1234567890)

# --- Explore Output --- #

labelTopics(topic_mod)

# top 10 words per topic visualized
# beta word to tweet, gama tweet to topic

td_beta <- tidy(topic_mod)

td_beta %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics")

# Suppose we want to assign human readable labels to topics:
td_beta <- 
    td_beta %>%
    mutate(topic_name = case_when(
        topic == 1 ~ "COVID", # ie. name it something meaningful,
        topic == 2 ~ "LABOUR % ECONOMY",
        topic == 3 ~ "BORDER PROTECTION",
        topic == 4 ~ "?????",
        topic == 5 ~ "DEMOCRATIC PARTY",
        topic == 6 ~ "IMPEACMENT",
        topic == 7 ~ "COVID 2",
        topic == 8 ~ "??",
        topic == 9 ~ "DEMOCRATIC PRIMARIES",
        TRUE ~ "FAKE NEWS"
        )
    )

# regraph!
td_beta %>%
    group_by(topic_name) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic_name))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic_name, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics")

# --- Assigning Topics to Tweets --- #
td_gamma <- tidy(topic_mod,
                 matrix = "gamma",
                 document_names = rownames((dtm)))


# give each tweet its most probably topic..
tweets_gamma <- 
    td_gamma %>%
    rename(id = document) %>%
    mutate(id = as.numeric(id)) %>%
    group_by(id) %>%
    slice_max(gamma) %>%
    select(-gamma)

# --- Topic Trends --- #
# trump_tweets <- read_csv("data/trump_early_2020_tweets.csv")


# How many topics should I use
topic_mod_2 <- stm(
    dtm,
    K=0,
    seed = 1234567890)
labelTopics(topic_mod_2)
n_tops <- c(5,10,15,20,25,30,35,40,45)

topic_search <- 
    searchK(dtm,
            K = n_tops,
            N = floor(0.1 * nrow(dtm)))

plot(topic_search)

# topic correlation
plot(topicCorr(topic_mod_2))
plot(topicCorr(topic_mod))











