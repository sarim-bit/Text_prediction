library(tm)
library(ggplot2)
library(wordcloud)
library(SnowballC)
library(stringi)
library(tidyverse)
library(RColorBrewer)
library(textstem)

# Reading lines and storing for quicker access
twitter <- readLines(con <- file("./data/en_US/en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)
news <- readLines(con <- file("./data/en_US/en_US.news.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)
blogs <- readLines(con <- file("./data/en_US/en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)

# Basic Summary of the datasets
# No. of lines
total_lines <- c(length(blogs), length(news), length(twitter))

# No. of words
blogs_words <- sum(stri_count_words(blogs))
news_words <- sum(stri_count_words(news))
twitter_words <- sum(stri_count_words(twitter))

total_words <- c(blogs_words, news_words, twitter_words)

# Size of the datasets
blogs_size<-format(object.size(blogs), units="auto")
news_size<-format(object.size(news), units="auto")
twitter_size<-format(object.size(twitter), units="auto")

size<-c(blogs_size, news_size, twitter_size)

# Making a dataframe summary of the data
df <- data.frame(Lines=total_lines, Words=total_words, Size=size)
row.names(df)<-c("Blogs", "News", "Twitter")
df

# Data Analysis

# Sampling some data to reduce processing time
set.seed(2020)
sampleblogs <- sample(blogs, size = length(blogs)*0.05)
samplenews <- sample(news, size = length(news)*0.05)
sampletwitter <- sample(twitter, size = length(twitter)*0.05)
sampled_data <- sample(paste(sampleblogs, samplenews, sampletwitter), size = 10000)
head(sampled_data)
sampled_df <- tibble(id = 1:length(sampled_data), text = sampled_data)

# Tokenisation
# Unigram
unigram_df <- sampled_df %>%
  unnest_tokens(output = "word", input = text) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  count(word, sort = TRUE)
unigram_df <- as.data.frame(unigram_df)
colnames(unigram_df) <- c("unigram", "frequency")
head(unigram_df)

# Pictoral representation using Wordcloud
wordcloud(words = unigram_df$unigram, freq = unigram_df$frequency, min.freq = 500,
          random.order = FALSE, colors = brewer.pal(6, "Dark2"))

# Bigram
bigram_df <- sampled_df %>%
  unnest_tokens(output = "bigram", input = text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)
bigram_df <- as.data.frame(bigram_df)
colnames(bigram_df) <- c("bigram", "frequency")
head(bigram_df)

# Trigram
trigram_df <- sampled_df %>%
  unnest_tokens(output = "bigram", input = text, token = "ngrams", n = 3) %>%
  count(bigram, sort = TRUE)
trigram_df <- as.data.frame(trigram_df)
colnames(trigram_df) <- c("trigram", "frequency")
head(trigram_df)

# Plots of n-grams
ggplot(unigram_df[1:20,], aes(x = reorder(unigram, -frequency), y = frequency, fill = frequency)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Top 20 Unigrams",
       x = "Unigrams",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(bigram_df[1:20,], aes(x = reorder(bigram, -frequency), y = frequency, fill = frequency)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Top 20 Bigrams",
       x = "Bigrams",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position = "none")

ggplot(trigram_df[1:20,], aes(x = reorder(trigram, -frequency), y = frequency, fill = frequency)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Top 20 Trigrams",
       x = "Trigrams",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position = "none")

# Saving data for easy access later
write.csv(unigram_df, "unigram_df.csv", row.names = FALSE)
write.csv(bigram_df, "bigram_df.csv", row.names = FALSE)
write.csv(trigram_df, "trigram_df.csv", row.names = FALSE)


saveRDS(sampled_df, "./output/sampled_df.rds")
saveRDS(unigram_df, "./output/unigram_df.rds")
saveRDS(bigram_df, "./output/bigram_df.rds")
saveRDS(trigram_df, "./output/trigram_df.rds")

# Understanding word coverage
total_words <- sum(unigram_df$frequency)
unique_words <- nrow(unigram_df)

# Cumulative frequency
unigram_df$cum_freq <- cumsum(unigram_df$frequency)
unigram_df$coverage <- unigram_df$cum_freq / total_words
head(unigram_df)

# Words covering 50% and 90%
word_50 <- which.min(abs(unigram_df$coverage - 0.5))
word_90 <- which.min(abs(unigram_df$coverage - 0.9))

total_words #879341
cat("Words needed for 50% coverage:", word_50, "\n") #144
cat("Words needed for 90% coverage:", word_90, "\n") #7069

ggplot(unigram_df, aes(x = 1:nrow(unigram_df), y = coverage)) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = c(word_50, word_90), linetype = "dashed", color = "red") +
  labs(title = "Cumulative Word Coverage",
       x = "Top N Words", y = "Coverage (%)") +
  theme_minimal()

# Lemmatisation to see the impact on coverage
unigram_df$lemma <- lemmatize_words(unigram_df$unigram)
unigram_lemma_df <- unigram_df %>%
  group_by(lemma) %>%
  summarise(frequency = sum(frequency)) %>%
  arrange(desc(frequency)) %>%
  ungroup()

head(unigram_lemma_df)

total_words <- sum(unigram_lemma_df$frequency)
unigram_lemma_df$cum_freq <- cumsum(unigram_lemma_df$frequency)
unigram_lemma_df$coverage <- unigram_lemma_df$cum_freq / total_words

word_50 <- which.min(abs(unigram_lemma_df$coverage - 0.5))
word_90 <- which.min(abs(unigram_lemma_df$coverage - 0.9))

total_words #879341
cat("Lemmas needed for 50% coverage:", word_50, "\n") #104 words
cat("Lemmas needed for 90% coverage:", word_90, "\n") #4150 words

ggplot(unigram_lemma_df, aes(x = 1:nrow(unigram_lemma_df), y = coverage)) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = c(word_50, word_90), linetype = "dashed", color = "red") +
  labs(title = "Cumulative Coverage After Lemmatization",
       x = "Top N Lemmas", y = "Coverage") +
  theme_minimal()

# Identifying Foreign language words
library(words)
english_words <- tolower(words::words)

# Check how many words are outside this list
unigram_df$is_foreign <- !(unigram_df$unigram %in% english_words)
sum(unigram_df$is_foreign) # 41398 foreign language words 


