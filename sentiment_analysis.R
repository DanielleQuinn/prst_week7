# Run each line of code, in order
# To run a line of code: place your cursor on the line of code and click on Run in the upper right corner of this panel

# ---- Load the required packages ----
library(readxl)
library(tidytext)
library(textdata)
library(dplyr)
library(stopwords)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(tidyr)

# ---- Import the data ----
review_data <- read_excel("reviews.xlsx")

# Print the first six rows of the data
head(review_data)

# Print the number of observations
nrow(review_data)

# Print the complete text from the first review
review_data$review[1]

# ---- Tokenize the Data ----
tidy_review <- review_data %>% unnest_tokens(output = word, input = review)

# Print the first six rows of the tokenized data
head(tidy_review)

# ---- Exclude Stopwords ----
# Start with the tidy_review data
tidy_review %>% count(word, sort = TRUE)
  
# Exclude stopwords
cleaned_review <- tidy_review %>% anti_join(get_stopwords())

# Recount remaining words
cleaned_review %>% count(word, sort = TRUE)

# ---- Sentiment Analysis ----
# Extract all words from lexicon
bing <- get_sentiments("bing")

# Look at the first six rows of the lexicon
head(bing)

# Classify the sentiment of each word in the reviews
complete_review <- cleaned_review %>% left_join(bing) %>% drop_na()

# Look at the first six rows of the complete data
head(complete_review)

# Count how often each word appears
results_table <- complete_review %>% count(word, sentiment, sort = TRUE)

# Look at the first six rows of our results
head(results_table)

# Look at positive words
results_table %>% filter(sentiment == "positive")

# Look at negative words
results_table %>% filter(sentiment == "negative")

# ---- Data Visualization ----
# Create a bar chart
results_table %>% filter(n > 5) %>% mutate(n = ifelse(sentiment == "negative", -n, n), word = reorder(word, n)) %>% ggplot() + geom_col(aes(x = n, y = word, fill = sentiment)) + labs(x = "Contribution to Sentiment", y = "Word", fill = "Sentiment")

# ---- Word Clouds ----
# Create word cloud
cleaned_review %>% count(word) %>% with(wordcloud(word, n, max.words = 100))

# Create word cloud
results_table %>% acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("red", "green"), max.words = 100)

# What proportion of words from each customer were negative?
tidy_review %>% left_join(bing) %>% group_by(customer) %>% summarise(total = n(), negative = sum(sentiment == "negative", na.rm = TRUE), positive = sum(sentiment == "positive", na.rm = TRUE), proportion_negative = negative/total) %>% arrange(desc(proportion_negative))

# Pull the review from customer number 15
review_data %>% filter(customer == 15) %>% pull(review)
