library(tm)
library(tidyverse)
library(data.table)
library(tokenizers)
library(shiny)
library(DT)

unigram_df <- readRDS("./output/unigram_df.rds")
bigram_df <- readRDS("./output/bigram_df.rds")
trigram_df <- readRDS("./output/trigram_df.rds")
quadgram_df <- readRDS("./output/quadgram_df.rds")

bigram_df <- bigram_df %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")
trigram_df <- trigram_df %>%
  separate(trigram, into = c("word1", "word2", "word3"), sep = " ")

unigram_dt <- as.data.table(unigram_df)
bigram_dt <- as.data.table(bigram_df)
trigram_dt <- as.data.table(trigram_df)
quadgram_dt <- as.data.table(quadgram_df)

setkey(bigram_dt, word1)
setkey(trigram_dt, word1, word2)
setkey(quadgram_dt, word1, word2, word3)

bigram_dt <- bigram_dt[frequency >= 2]
trigram_dt <- trigram_dt[frequency >= 2]
quadgram_dt <- quadgram_dt[frequency >= 2]

vocab <- nrow(unigram_dt)

# Final Model for the application
next_word_pred_app <- function(input) {
  text <- tolower(input)
  text <- gsub("[^a-z ]", "", text)
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]  # remove empty strings
  word_len <- length(words)
  
  result <- data.table()
  
  # Try quadgram
  if (word_len >= 3) {
    prev_three <- tail(words, 3)
    prob_out <- quadgram_dt[J(prev_three[1], prev_three[2], prev_three[3])]
    if (nrow(prob_out) > 0 && !all(is.na(prob_out$word4))) {
      prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
      result <- prob_out[, .(next_word = word4, prob)]
    }
  }
  
  # If not enough predictions, try trigram
  if (nrow(result) < 3 && word_len >= 2) {
    prev_two <- tail(words, 2)
    prob_out <- trigram_dt[J(prev_two[1], prev_two[2])]
    if (nrow(prob_out) > 0 && !all(is.na(prob_out$word3))) {
      prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
      trigram_preds <- prob_out[, .(next_word = word3, prob)]
      result <- rbind(result, trigram_preds, fill = TRUE)
      result <- unique(result, by = "next_word")
    }
  }
  
  # Try bigram
  if (nrow(result) < 3 && word_len >= 1) {
    prev_one <- tail(words, 1)
    prob_out <- bigram_dt[J(prev_one)]
    if (nrow(prob_out) > 0 && !all(is.na(prob_out$word2))) {
      prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
      bigram_preds <- prob_out[, .(next_word = word2, prob)]
      result <- rbind(result, bigram_preds, fill = TRUE)
      result <- unique(result, by = "next_word")
    }
  }
  
  # Fallback to unigrams
  if (nrow(result) < 3) {
    prob_out <- unigram_dt
    prob_out[, prob := (frequency + 1) / (sum(frequency) + vocab)]
    unigram_preds <- prob_out[, .(next_word = unigram, prob)]
    result <- rbind(result, unigram_preds, fill = TRUE)
    result <- unique(result, by = "next_word")
  }
  
  # Return sorted predictions, top N only
  result <- result[order(-prob)]
  return(head(result, max(3, 10)))  # return at least `min_n`, or top 10
}

# UI
ui <- fluidPage(
  titlePanel("Word Predictor App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("input_text", "Enter text:", value = " "),
      numericInput("top_n", "Top N Predictions:", value = 5, min = 1, max = 20),
      actionButton("predict_btn", "Predict")
    ),
    
    mainPanel(
      h4("Predicted Next Words:"),
      dataTableOutput("prediction_table"),
      h5("Created by Sarim Rizvi")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observeEvent(input$predict_btn, {
    req(input$input_text)
    
    # Generate predictions using the optimized model
    preds <- next_word_pred_app(input$input_text)
    colnames(preds) <- c('Next Word', 'Probability')
    # Show top N predictions
    preds <- head(preds, input$top_n)
    
    output$prediction_table <- renderDataTable({
      datatable(preds, rownames = FALSE,
                options = list(pageLength = input$top_n))
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
