library(tm)
library(stringi)
library(tidyverse)
library(textstem)
library(data.table)
library(tokenizers)

# After doing the data preprocessing, using ngram for predictions

# Seprating bigrams and trigrams into different words
bigram_df <- bigram_df %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")
trigram_df <- trigram_df %>%
  separate(trigram, into = c("word1", "word2", "word3"), sep = " ")

# Adding quadgrams to add more predictive power
quadgram_df <- sampled_df %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  count(word1, word2, word3, word4, sort = TRUE)
colnames(quadgram_df) <- c("word1", "word2", "word3", "word4", "frequency")
saveRDS(quadgram_df, "./output/quadgram_df.rds")

# Simple Prediction Function
next_word_pred <- function(input) {
  # Cleaning data before processing
  text <- tolower(input)
  text <- gsub("[^a-z ]", "", text)
  
  # Dividing the string into words
  words <- unlist(strsplit(text, "\\s+"))
  word_len <- length(words)
  
  if (word_len >= 3) {
    prev_three <- tail(words, 3)
    output <- quadgram_df %>%
      filter(word1 == prev_three[1], word2 == prev_three[2], word3 == prev_three[3]) %>%
      arrange(desc(frequency)) %>%
      slice_head(n = 3) %>%
      pull(word4)
    if (length(output) > 0) return(output)
  }
  
  if (word_len >= 2) {
    prev_two <- tail(words, 2)
    output <- trigram_df %>%
      filter(word1 == prev_two[1], word2 == prev_two[2]) %>%
      arrange(desc(frequency)) %>%
      slice_head(n = 3) %>%
      pull(word3)
    if (length(output) > 0) return(output)
  }
  
  if (word_len >= 1) {
    prev_one <- tail(words, 1)
    output <- bigram_df %>%
      filter(word1 == prev_one) %>%
      arrange(desc(frequency)) %>%
      slice_head(n = 3) %>%
      pull(word2)
    if (length(output) > 0) return(output)
  }
  
  return(head(unigram_df$unigram, 3))
}

next_word_pred("i love")
next_word_pred("first of")
next_word_pred("England is a")


# Using optimisation to speed up the predictions
# Converting dataframes to datatable, because dplyr is slower

unigram_dt <- as.data.table(unigram_df)
bigram_dt <- as.data.table(bigram_df)
trigram_dt <- as.data.table(trigram_df)
quadgram_dt <- as.data.table(quadgram_df)

setkey(bigram_dt, word1)
setkey(trigram_dt, word1, word2)
setkey(quadgram_dt, word1, word2, word3)

# Removing extremely rare combinations
summary(bigram_df$frequency)
summary(trigram_df$frequency)
summary(quadgram_df$frequency)

bigram_dt <- bigram_dt[frequency >= 2]
trigram_dt <- trigram_dt[frequency >= 2]
quadgram_dt <- quadgram_dt[frequency >= 2]

# Keeping only the top three predictions for a given combination of words
# bigram_dt <- bigram_dt[order(word1, -frequency)]
# bigram_dt <- bigram_dt[, .SD[1:3], by = word1]
# trigram_dt <- trigram_dt[order(word1, word2, -frequency)]
# trigram_dt <- trigram_dt[, .SD[1:3], by = .(word1, word2)]
# quadgram_dt <- quadgram_dt[order(word1, word2, word3, -frequency)]
# quadgram_dt <- quadgram_dt[, .SD[1:3], by = .(word1, word2, word3)]

# Vocabulary size (for smoothing denominator)
vocab <- nrow(unigram_dt)  

next_word_pred_opt <- function(input) {
  text <- tolower(input)
  text <- gsub("[^a-z ]", "", text)
  
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]  # remove empty strings
  word_len <- length(words)
  
  if (word_len >= 3) {
    prev_three <- tail(words, 3)
    prob_out <- quadgram_dt[J(prev_three[1], prev_three[2], prev_three[3])]
    if (nrow(prob_out) > 0) {
      # Laplace Smoothing
      prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
      output <- prob_out[order(-prob)][1:3, word4]
      output <- as.vector(na.omit(output))
      if (length(output) > 0) return(output)
    }
  }
  
  if (word_len >= 2) {
    prev_two <- tail(words, 2)
    prob_out <- trigram_dt[J(prev_two[1], prev_two[2])]
    if (nrow(prob_out) > 0) {
      # Laplace Smoothing
      prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
      output <- prob_out[order(-prob)][1:3, word3]
      output <- as.vector(na.omit(output))
      if (length(output) > 0) return(output)
    }
  }
  
  if (word_len >= 1) {
    prev_one <- tail(words, 1)
    prob_out <- bigram_dt[J(prev_one)]
    if (nrow(prob_out) > 0) {
      # Laplace Smoothing
      prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
      output <- prob_out[order(-prob)][1:3, word2]
      output <- as.vector(na.omit(output))
      if (length(output) > 0) return(output)
    }
  }
  
  return(head(unigram_dt$unigram, 3))
}

