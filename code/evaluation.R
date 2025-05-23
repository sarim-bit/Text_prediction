library(tm)
library(stringi)
library(tidyverse)
library(textstem)
library(data.table)
library(tokenizers)

# Evaluation of model

# Basic accuracy test - has not been used for final testing
# Sample test data: list of input phrases with actual next word
test_data <- data.frame(
  input = c("i love", "happy birthday", "at the", "let us", "new york"),
  actual = c("you", "to", "same", "go", "city"),
  stringsAsFactors = FALSE
)

# Function to check accuracy
evaluate_accuracy <- function(predict_function, test_data) {
  top1_correct <- 0
  top2_correct <- 0
  top3_correct <- 0
  
  n <- nrow(test_data)
  
  for (i in 1:n) {
    input_text <- test_data$input[i]
    actual_word <- test_data$actual[i]
    
    predicted <- tryCatch({
      predict_function(input_text)
    }, error = function(e) {
      character(0)
    })
    
    if (length(predicted) >= 1 && predicted[1] == actual_word) top1_correct <- top1_correct + 1
    if (actual_word %in% predicted[1:min(2, length(predicted))]) top2_correct <- top2_correct + 1
    if (actual_word %in% predicted[1:min(3, length(predicted))]) top3_correct <- top3_correct + 1
  }
  
  return(list(
    top1_accuracy = top1_correct / n,
    top2_accuracy = top2_correct / n,
    top3_accuracy = top3_correct / n
  ))
}


result <- evaluate_accuracy(next_word_pred, test_data)
print(result)

result <- evaluate_accuracy(next_word_pred_opt, test_data)
print(result)

# Perplexity Calculation
# Adding probablity with prediction to calculate perplexity 

# Base model with probability
next_word_pred_prob_all <- function(input) {
  # Clean and tokenize input
  text <- tolower(input)
  text <- gsub("[^a-z ]", "", text)
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]  
  word_len <- length(words)
  
  # Quadgram
  if (word_len >= 3) {
    prev_three <- tail(words, 3)
    prob_df <- quadgram_df %>%
      filter(word1 == prev_three[1], word2 == prev_three[2], word3 == prev_three[3]) %>%
      mutate(prob = frequency / sum(frequency)) %>%
      select(next_word = word4, prob)
    if (nrow(prob_df) > 0) return(prob_df)
  }
  
  # Trigram
  if (word_len >= 2) {
    prev_two <- tail(words, 2)
    prob_df <- trigram_df %>%
      filter(word1 == prev_two[1], word2 == prev_two[2]) %>%
      mutate(prob = frequency / sum(frequency)) %>%
      select(next_word = word3, prob)
    if (nrow(prob_df) > 0) return(prob_df)
  }
  
  # Bigram
  if (word_len >= 1) {
    prev_one <- tail(words, 1)
    prob_df <- bigram_df %>%
      filter(word1 == prev_one) %>%
      mutate(prob = frequency / sum(frequency)) %>%
      select(next_word = word2, prob)
    if (nrow(prob_df) > 0) return(prob_df)
  }
  
  # Fall back to unigram 
  prob_df <- unigram_df %>%
    mutate(prob = frequency / sum(frequency)) %>%
    select(next_word = unigram, prob)
  
  return(prob_df)
}

# Optimised Model with Probability
next_word_pred_opt_prob_all <- function(input) {
  text <- tolower(input)
  text <- gsub("[^a-z ]", "", text)
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]  
  word_len <- length(words)
  
  # Quadgram
  if (word_len >= 3) {
    prev_three <- tail(words, 3)
    prob_out <- quadgram_dt[J(prev_three[1], prev_three[2], prev_three[3])]
    if ((nrow(prob_out) > 0) && !(all(is.na(prob_out$word4)))) {
      prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
      return(prob_out[, .(next_word = word4, prob)])
    }
  }
  
  # Trigram
  if (word_len >= 2) {
    prev_two <- tail(words, 2)
    prob_out <- trigram_dt[J(prev_two[1], prev_two[2])]
    if ((nrow(prob_out) > 0) && !(all(is.na(prob_out$word3)))) {
      prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
      return(prob_out[, .(next_word = word3, prob)])
    }
  }
  
  # Bigram
  if (word_len >= 1) {
    prev_one <- tail(words, 1)
    prob_out <- bigram_dt[J(prev_one)]
    if ((nrow(prob_out) > 0) && !(all(is.na(prob_out$word2)))) {
      prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
      return(prob_out[, .(next_word = word2, prob)])
    }
  }
  
  # Fall back to unigram
  prob_out <- unigram_dt
  prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
  return(prob_out[, .(next_word = unigram, prob)])
}

# Function to calculate Perplexity
get_perplexity <- function(test_text, model_func, vocab_size) {
  # Clean and tokenize input text
  text <- tolower(test_text)
  text <- gsub("[^a-z ]", "", text)
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]
  
  if (length(words) < 2) {
    stop("Minimum two words are required.")
  }
  
  log_prob_sum <- 0
  total_predictions <- length(words) - 1
  
  for (i in 2:length(words)) {
    # Take up to 3 words before the current word for prediction
    context_start <- max(1, i - 3)
    context <- words[context_start:(i - 1)]
    context_text <- paste(context, collapse = " ")
    
    # Probability of predictions
    preds <- model_func(context_text)
    actual_next_word <- words[i]
    
    # Probability of actual word 
    prob <- preds$prob[preds$next_word == actual_next_word]
    
    # If not found, assign a small smoothing probability
    if (length(prob) == 0 || is.na(prob)) {
      prob <- 1 / vocab_size
    }
    
    log_prob_sum <- log_prob_sum + log(prob)
  }
  
  # Perplexity
  perplexity <- exp(-log_prob_sum / total_predictions)
  return(perplexity)
}

# Unit testing 
test_text <- "I love you"
next_word_pred_prob_all('I love')

get_perplexity(test_text, next_word_pred_opt_prob_all, vocab)
get_perplexity(test_text, next_word_pred_prob_all, vocab)

# Updated Accuracy function
get_accuracy <- function(test_text, model_func, top_n = 3) {
  text <- tolower(test_text)
  text <- gsub("[^a-z ]", "", text)
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]
  
  n <- length(words)
  if (n < 2) {
    stop("Minimum two words are required.")
  }
  
  correct <- 0
  total <- n - 1  # number of predictions
  
  for (i in 2:n) {
    start_index <- max(1, i - 3)
    context_words <- words[start_index:(i - 1)]
    context_str <- paste(context_words, collapse = " ")
    
    # Prediction
    preds <- model_func(context_str)
    
    # Check if actual word is in top N predictions
    actual_word <- words[i]
    predicted_words <- preds$next_word[1:min(nrow(preds), top_n)]
    
    if (actual_word %in% predicted_words) {
      correct <- correct + 1
    }
  }
  
  # top-N accuracy
  accuracy <- correct / total
  return(accuracy)
}


# Evaluation to combine accuracy, perplexity and time taken
evaluate_model <- function(sentences, model_func, vocab) {
  results <- data.frame(
    text = sentences,
    accuracy = numeric(length(sentences)),
    perplexity = numeric(length(sentences)),
    time_sec = numeric(length(sentences)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(sentences)) {
    sentence <- sentences[i]
    
    start_time <- Sys.time()
    acc <- get_accuracy(sentence, model_func)
    ppl <- get_perplexity(sentence, model_func, length(vocab))
    end_time <- Sys.time()
    
    results$accuracy[i] <- acc
    results$perplexity[i] <- ppl
    results$time_sec[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }
  
  return(results)
}


# Automated test set selection
get_test_sentences <- function(n = 10, seed = 123) {
  # Load the crude dataset
  data("crude", package = "tm")
  texts <- sapply(crude, as.character)
  
  # Tokenize into sentences
  all_sentences <- unlist(tokenize_sentences(texts))
  
  # Data cleaning
  all_sentences <- tolower(all_sentences)
  all_sentences <- gsub("[0-9]+", "", all_sentences)         
  all_sentences <- gsub("[^a-z ]", "", all_sentences)        
  all_sentences <- gsub("\\s+", " ", all_sentences)          
  all_sentences <- trimws(all_sentences)                     
  
  # Filter by character length and minimum number of words
  filtered_sentences <- all_sentences[
    nchar(all_sentences) >= 20 &
      nchar(all_sentences) <= 100 &
      sapply(strsplit(all_sentences, " "), length) >= 3
  ]
  
  # Random selection
  set.seed(seed)
  sample_sentences <- sample(filtered_sentences, size = min(n, length(filtered_sentences)))
  
  return(sample_sentences)
}
test_sentences <- unname(get_test_sentences(100, 123))

results_base <- evaluate_model(test_sentences, next_word_pred_prob_all, vocab)
head(results_base)
results_opt <- evaluate_model(test_sentences, next_word_pred_opt_prob_all, vocab)
head(results_opt)

# Final result
results_combined <- data.frame(
  Model = c("Base", "Optimised"),
  Accuracy = c(mean(results_base$accuracy), mean(results_opt$accuracy)),
  Perplexity = c(mean(results_base$perplexity), mean(results_opt$perplexity)),
  Time = c(mean(results_base$time_sec), mean(results_opt$time_sec))
)

results_combined
