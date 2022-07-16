###############################################################################
# Homework 2
###############################################################################

# Set Up ----------------------------------------------------------------------

baby_data <- read.csv("baby_reviews.csv"); baby_data
View(baby_data)
summary(baby_data)
str(baby_data)
class(baby_data) #data.frame
nrow(baby_data) #4978
ncol(baby_data) #3

# Section 1 -------------------------------------------------------------------
# Question 1
# How many baby_data does this dataset contain?
nrow(baby_data)
#4978


# Question 2
# What is the average review rating?
mean(baby_data$review_rating)
#4.227601


# Question 3
# nchar() is a handy function for counting the number of characters in text. 
# What is the average number of characters in a review?
mean(nchar(baby_data$review))
#441.8248


# Question 4
# Examine the relationship between review length (measured by number of 
# characters) and rating. Now, indicate whether the following statement is True 
# or False.
# Greater the length of the review, better the rating.
cor.test(nchar(baby_data$review), baby_data$review_rating)
#False


# Question 5
# The stringr library has a number of handy text search functions capable of 
# both literal search and pattern matching. The sample code that follows 
# specifies a pattern to identify a word and str_count to count the number of 
# such words in a set of text.
library(stringr)
str_count(string = 'Hmm, how many words are in this sentence?', pattern = '\\S+')

# Using the above code, find the median number of words in a review?
median(str_count(baby_data$review, pattern = "\\S+"))
#57


# Question 6
# How many words are in the longest review?
max(str_count(baby_data$review, pattern = "\\S+"))
#1041


# Question 7
# How many words are in the shortest review?
min(str_count(baby_data$review, pattern = "\\S+"))
#2


# Question 8
# Next, let us examine which words are used most frequently in the review. For
# this, you can use relevant functions from library(tidytext). Which of the
# following words are in the Top 10 list?
library(tidytext); library(dplyr)
baby_data %>%
  unnest_tokens(input = review, output = word) %>%
  select(word) %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  top_n(10)
#the, and, to, i, it, a , is, this, for, of


# Question 9
# Next, let us construct a Top 10 list after excluding stop words. For the
# purpose of this question use the set of stop words from library(tidytext):
# stop_words. Which of the following words are in this Top 10 list? (Are you
# surprised?)
baby_data %>%
  unnest_tokens(input = review, output = word) %>%
  select(word) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  top_n(10)
#baby, easy, love, seat, 34, time, stroller, months, son, bought


# Section 2 ---------------------------------------

# Question 1
# Let us use the dplyr and tidytext packages to explore the words used in the
# review. Use the unnest_tokens function from tidytext to tokenize the baby_data
# and the following dplyr functions to organize the data.
# What is the total number of words in all the baby_data?
baby_data %>%
  select(id, review) %>%
  unnest_tokens(output = word, input = review) %>%
  count()
#421790


# Question 2
# Now, let us explore valence of the words used in baby_data. Use the "bing"
# dictionary to classify words in baby_data into positive and negative. The bing
# dictionary of words can be accessed using tidytext::get_sentiments("bing") or
# bing.csv file accompanying this assignment. What is the total number of
# positive words in all the baby_data?
baby_data %>%
  unnest_tokens(output = word, input = review) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(sentiment) %>%
  count() %>%
  ungroup()
#24462  

# Question 3
# Among all the review words categorized as either positive or negative (using 
# the "bing" dictionary), what proportion are positive?
baby_data %>%
  select(id, review) %>%
  unnest_tokens(output = word, input = review) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(sentiment) %>%
  summarize(n = n()) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()
#Also works using previous question's numbers: 24462 / (24462 + 8739)
#0.737


# Question 4
# Now, let us examine the proportion of positive words in baby_data for each
# review rating. Of the five possible review ratings, which has the highest
# proportion of positive words.
baby_data %>%
  select(id, review, review_rating) %>%
  unnest_tokens(output = word, input = review) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(review_rating, sentiment) %>%
  summarize(n = n()) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()
#5


# Question 5
# Next, let us examine the emotions expressed in the baby_data using the "nrc"
# dictionary. You can access the "nrc" dictionary from nrc.csv file
# accompanying this assignment. How many words reflect surprise?
nrc <- read.csv("nrc.csv"); nrc
baby_data %>%
  unnest_tokens(output = word, input = review) %>%
  inner_join(nrc) %>%
  group_by(sentiment) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup()
#3815


# Question 6
# Based on the code run for the previous question, how many words reflect
# anticipation?
#8429


# Question 7
# The "afinn" dictionary scores the sentiment of words. Use this dictionary to
# determine the sentiment of each review. See class R code on how to access
# "afinn" dictionary. What is the minimum sentiment scores across all baby_data?
afinn <- read.csv("afinn.csv"); head(afinn)
afinn %>%
  group_by(value) %>%
  count() %>%
  ungroup()
baby_data %>%
  select(id, review) %>%
  unnest_tokens(output = word, input = review) %>%
  inner_join(afinn) %>%
  group_by(id) %>%
  summarize(baby_dataentiment = mean(value)) %>%
  summarize(min = min(baby_dataentiment)) %>%
  ungroup()
#-3


# Question 8
# Which of the following review ids have the lowest sentiment score identified 
# in Question 7? (If you are curious, you may also want to take a look at the
# review to understand the reason for the low sentiment score)
baby_data %>%
  select(id, review) %>%
  unnest_tokens(output = word, input = review) %>%
  inner_join(afinn) %>%
  group_by(id) %>%
  summarize(baby_dataentiment = mean(value)) %>%
  filter(baby_dataentiment == -3)
#91, 238, 2598


# Question 9
# What is the average sentiment score (based on the "afinn" dictionary)?
baby_data %>%
  select(id, review) %>%
  unnest_tokens(output = word, input = review) %>%
  inner_join(afinn) %>%
  group_by(id) %>%
  summarize(baby_dataentiment = mean(value)) %>%
  summarize(mean = mean(baby_dataentiment)) %>%
  ungroup()
#1.38


# Section 3 ---------------------------------------

# Preprocess the corpus of baby_data using functions from library(tm). Specifically,

# 1. Create a corpus from the variable 'review'
library(tm)
corpus <- Corpus(VectorSource(baby_data$review))

# 2. Use tm_map to
#    (a) transform text to lower case,
          corpus <- tm_map(corpus, FUN = content_transformer(tolower))

#    (b) remove punctuation,
         corpus <- tm_map(corpus, FUN = removePunctuation)
          
#    (c) remove English stopwords using the following dictionary tm::stopwords('english)
         corpus <- tm_map(corpus, FUN = removeWords, c(stopwords("english")))

#    (d) remove whitespace
         corpus <- tm_map(corpus, FUN = stripWhitespace)
         
# 3. Create a dictionary
dict <- findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(baby_data$review))),
                      lowfreq = 0)
dict_corpus <- Corpus(VectorSource(dict))
         
# 4. Use tm_map to stem words
corpus <- tm_map(corpus, FUN = stemDocument)

# 5. Create a DocumentTermMatrix
dtm <- DocumentTermMatrix(corpus)


# Question 1
# How many terms does the document term matrix contain?
dtm
#11730


# Question 2
# Inspect document 100 of the document term matrix. How many times does 
# "amazon" appear in this document?
inspect(dtm[100, "amazon"])
#1

# Question 3
# Now, let us reduce the number of terms to a more reasonable number by only
# keeping terms that appear in at least 10% of documents. Save the result as 
# "xdtm". How many terms remain after removing sparse terms?
xdtm <- removeSparseTerms(dtm, sparse = 0.9); xdtm
#47


# Question 4
# Transform the document term matrix, xdtm created in the previous question
# into a data frame. Use stemCompletion() to complete stemmed words by selecting
# the most prevalent match. In the resulting data frame, which term appears
# most frequently?
xdtm <- as.data.frame(as.matrix(xdtm))
colnames(xdtm) <- stemCompletion(x = colnames(xdtm), dictionary = dict_corpus,
                                 type = "prevalent")
colnames(xdtm) <- make.names(colnames(xdtm))
sort(colSums(xdtm), decreasing = T)
#use


# Question 5
# Attach the column containing the review rating to the dataframe created in
# the previous question. Which is the third (3rd) most frequently occurring
# word among baby_data with a rating of 5?
review_rating_xdtm <- cbind(baby_data$review_rating, xdtm)
sort(colSums(review_rating_xdtm[review_rating_xdtm$`baby_data$review_rating`==5,
                                -review_rating_xdtm$`baby_data$review_rating`]),
     decreasing = T)
#love


# Section 4 ---------------------------------------

# Question 1
# Now, let us use data on word frequencies to predict review rating. Split the
# dataset containing review rating and term frequencies into train and test
# samples. Use sample() to create a train sample with 70% of the data and a test
# sample with the remaining 30%. Use a seed of 1031. For a dataset called,
# baby_data, the following code will create the train and test samples.
set.seed(1031)
split <- sample(1:nrow(review_rating_xdtm), size = 0.7*nrow(review_rating_xdtm))
train <- review_rating_xdtm[split,]
test <- review_rating_xdtm[-split,]
# How many rows are in the test sample?
nrow(test)
#1494


# Question 2
# Use a CART model to predict review_rating using all other variables, i.e.,
# term frequencies. For the CART model, use rpart(). Now, indicate whether the
# following statement is True or False.
library(rpart); library(rpart.plot)

# Based on results of the CART model (and all else being equal), baby_data that
# contain the term "love" are rated higher than those that don't contain the
# term "love".
set.seed(100)
tree <- rpart(`baby_data$review_rating` ~ ., train)
rpart.plot(tree)
#True 

# Question 3
# Based on results of the CART model, reviews that contain the term "easier" are
# rated lower than those that don't contain the term "easier".
#False


# Question 4
# Based on results of the CART model, reviews that contain the term "perfect"
# are rated lower than those that don't contain the term "perfect".
#False


# Question 5
# Use a linear regression to predict review_rating using all other variables,
# i.e., term frequencies. Examine the results. From an earlier section
# (answer for question 4 in section 3), recall the most frequently occurring
# term in the document term matrix after removing sparse terms. Locate the most
# frequently occurring term in review in the regression results. Is this term
# predictive of review_rating? (answer to Section 3, question 4 = "use")
reg <- lm(`baby_data$review_rating` ~ ., train)
summary(reg)
#No


# Question 6
# What is the RMSE of the CART model on the test set?
pred_tree <- predict(tree, newdata = test)
rmse_tree <- sqrt(mean((pred_tree - test$`baby_data$review_rating`)^2))
rmse_tree
#1.114328


# Question 7
# What is the RMSE of the linear regression model on the test set?
pred_reg <- predict(reg, newdata = test)
rmse_reg <- sqrt(mean((pred_reg - test$`baby_data$review_rating`)^2))
rmse_reg
#1.11626