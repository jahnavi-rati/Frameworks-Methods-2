###############################################################################
# Homework 3
###############################################################################


# Verifications ---------------------------------------------------------------
R.Version()
packageVersion(pkg = "arules")
packageVersion(pkg = "recommenderlab")


# Section 1 -------------------------------------------------------------------

# Set Up 

library(arules)
library(arulesViz)
data("Groceries")
?Groceries


# Question 1
# Load the dataset called Groceries that accompanies the arules package by 
# executing data(Groceries). How many transactions does the transactions dataset 
# Groceries contain?
Groceries
#9835


# Question 2
# Which of the following are among the top 5 (five) frequently occurring items 
# in the dataset?
summary(Groceries)
#whole milk, yogurt


# Question 3
# Run a market basket analysis to generate a set of rules with the following 
# parameters: support=0.01 and confidence=0.01. How many rules were generated?
rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.01))
summary(rules)
#610


# Question 4
# Now, repeat the above market basket analysis but with support=0.001 and 
# confidence=0.001. How many rules were generated?
rules4 <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.001))
summary(rules4)
#41100


# Question 5
# Let us go back to the first market basket analysis with support of 0.01 and 
# confidence of 0.01. Among the rules generated, what is the value of the 
# largest lift? Type in the value of the largest lift.
summary(rules)
#3.3723


# Question 6
# In a grocery store setting, rules with just two items are more actionable than 
# ones that have many items. For instance, cross-promotions (e.g., when you buy 
# a carton of Brand A milk, take 50 cents of Brand X bread) or retail 
# merchandising decisions (e.g., place Brand A milk close to Brand X bread) are 
# easier to implement for two-item sets. Therefore, now you will generate a list 
# of rules for only two-items. Keep support and confidence at 0.01. How many 
# two-item rules are created? Note, a rule with only one-item should not be 
# included.
rules_2items <- apriori(Groceries, parameter = list(support = 0.01,
                                                    confidence = 0.01,
                                                    minlen = 2,
                                                    maxlen = 2))
summary(rules_2items)
#426


# Question 7
# Which of the following rules created from the analysis in the previous 
# question has the highest confidence?
inspect_2 <- inspect(rules_2items)
head(inspect_2[order(inspect_2$confidence, decreasing = T),])
#butter => whole milk


# Question 8
# What is support for the rule, soda => whole milk? Keep support and confidence 
# at 0.01. 
soda_whole_rules <- apriori(Groceries, parameter = list(support = 0.01,
                                                        confidence = 0.01),
                            appearance = list(lhs = "soda", rhs = "whole milk"),
                            minlen = 2)
inspect(soda_whole_rules)
#0.04006101


# Question 9
# The store manager contends that the support between soda and whole milk is not 
# of much value, because most people buy whole milk.  She goes on to say the 
# association between soda and whole milk is weak. Is she correct?
whole_soda_rules <- apriori(Groceries, parameter = list(support = 0.01,
                                                        confidence = 0.01),
                            appearance = list(lhs = "whole milk", rhs = "soda"),
                            minlen = 2)
inspect(whole_soda_rules)
library(RColorBrewer)
plot(rules_2items)
#yes, she is correct


# Question 10
# A shopper just picked up yogurt while shopping for groceries. What is the most 
# likely item, based on highest confidence, the shopper will also buy from the 
# grocery store? Assume the shopper only buys two items on this shopping trip. 
# Keep support and confidence at 0.01.
yogurt_and_rules <- apriori(Groceries, parameter = list(support = 0.01,
                                                        confidence = 0.01),
                            appearance = list(lhs = "yogurt"), minlen = 2)
inspect_yogurt_and <- inspect(yogurt_and_rules)
head(inspect_yogurt_and[order(inspect_yogurt_and$confidence, decreasing = T),])
#whole milk


# Section 2 ---------------------------------------

# Set Up 

data <- read.csv("product_ratings_data.csv")
class(data)
str(data)
View(data)
head(data)


# Question 1
# Read in the data, product_ratings_data.csv  Download product_ratings_data.csv
# using read.csv(). What format is the data in?
#Tall or long


# Question 2
# From the imported data, create a realRatingMatrix object called, 
# ratings_matrix. How many ratings does the realRatingMatrix contain?
library(recommenderlab)
ratings_matrix <- as(data, Class = "realRatingMatrix")
matrix <- as(ratings_matrix, "matrix")
ratings_matrix
#362105  


# Question 3
# What rating did u10023 give to prod_14?
matrix["u10023", "prod_14"]
#4


# Question 4
# Now, let us split the data into a train sample and a test sample. We will use 
# the sample() function with a seed of 1031 to create a train sample with 90% 
# of the data. Run the following code to create the train and test samples.
set.seed(1031)
split <- sample(nrow(ratings_matrix),size = 0.9*nrow(ratings_matrix))
train <- ratings_matrix[split,]
test <- ratings_matrix[-split,]

# How many rows are in the train sample? Answer the remaining questions in this 
# section using the train sample.
nrow(train)
#4500


# Question 5
# How many products (prod) did user 20150 (u20150) rate?
nProducts20150 <- rowCounts(train["u20150", ]); nProducts20150
# Can also do this:
# train["u20150", ]
# train_matrix <- as(train, "matrix")
# sum(!is.na(train_matrix["u20150", ]))
#44


# Question 6
# How many user ratings did product 25 (prod_25) receive?
nRatingsProd_25 <- colCounts(train[, "prod_25"]); nRatingsProd_25
#3745


# Question 7
# What is the most common rating in the train sample?
max(table(getRatings(train)))
#3


# Question 8
# What is the average rating for product 100 (prod_100) in the train sample?
colMeans(train)["prod_100"]
# Can also do this:
# colMeans(train[, "prod_100"])
#2.824492


# Question 9
# Now, normalize user ratings using the normalize() function from recommenderLab. 
# Use the defaults of method='center' and row=TRUE. What is the average rating 
# for product 100 (prod_100) in the normalized train sample?
train_norm <- normalize(train, method = "center", row = T); train_norm
colMeans(train_norm[, "prod_100"])
#0.08438464


# Question 10
# Using the normalized user ratings generated above, assess the cosine 
# similarity between the first five users in the train dataset (u395, u21174, 
# u9881, u18449, u8926). Which of the following pairs is most similar?
similarity(train_norm[1:5], method = "cosine")
#u395 and u21174
#u21174 and u9881
#u395 and u8926


# Section 3 ---------------------------------------

# Set Up
# Same data as section 2

data <- read.csv("product_ratings_data.csv")
class(data)
str(data)
View(data)
head(data)


# Question 1
# Construct a user-based collaborative filtering recommender using the train 
# data. Use defaults for the parameters in the Recommender function. Based on 
# this recommender, which of the following are in the list of top 5 recommended 
# products for u10139 in the test data?
recom_user <- Recommender(train, method = "UBCF"); recom_user
pred_user <- predict(recom_user, test, n = 5); pred_user
getList(pred_user)$u10139
#prod_72, prod_83


# Question 2
# Based on the recommender created above, what is the predicted rating of 
# product 10 (prod_10) by user 10139 (u10139) in the test data?
as(test, "matrix")["u10139","prod_10"]
pred_user2 <- predict(recom_user, test["u10139"], type = "ratings"); pred_user2
as(pred_user2, "list")$u10139["prod_10"]
#2.696542


# Question 3
# Construct an item-based collaborative filtering recommender using train data. 
# Use defaults for the parameters in the Recommender function. Based on this 
# recommender, which of the following are in the list of top 5 recommended 
# products for u10139 in the test data?
recom_item <- Recommender(train, method = "IBCF"); recom_item
pred_item <- predict(recom_item, test, n = 5); pred_item
getList(pred_item)$u10139
#prod_40, prod_45, prod_72


# Question 4
# Based on the recommender created in the previous question, what is the 
# predicted rating of product 10 (prod_10) by user 10139 (u10139) in the test 
# data?
as(test, "matrix")["u10139","prod_10"]
pred_item2 <- predict(recom_item, test["u10139"], type = "ratings"); pred_item2
as(pred_item2, "list")$u10139["prod_10"]
#2.856013


# Question 5
# The recommenderlab library offers a useful framework to cross-validate and 
# evaluate recommenders. Here, we are going to create an evaluation scheme 
# using ratings_matrix, the realRatingMatrix that we had before splitting into 
# a train and test dataset. The evaluationScheme() function below will do a 
# 80:20 split on the data, placing 80% of the data in the train sample. And, we 
# will give the recommender algorithm 30 items from the test set and hold out 
# the other items for computing the error. As with any random sampling 
# operation, it is important to set the seed right before creating the 
# evaluationScheme as done below.
set.seed(1031)
es <- evaluationScheme(ratings_matrix, method = "split", train = 0.8, 
                       given = 30)
# Now, evaluate accuracy of an item-based collaborative filtering recommender 
# using defaults. To do so, run the following code
recom_item5 <- Recommender(getData(es, "train"), method = "IBCF")
pred_ibcf5 <- predict(recom_item5, newdata = getData(es, "known"),
                     type = "ratings"); pred_ibcf5
accuracy_ibcf5 <- calcPredictionAccuracy(x = pred_ibcf5,
                                        data = getData(es, "unknown"))
# What is the RMSE for the item-based collaborative filtering recommender?
accuracy_ibcf5
#1.151257


# Question 6
# Now, evaluate the accuracy of the user-based collaborative filtering 
# recommender using defaults. To do so, modify the code in the previous question. 
# Note, there is no need to recreate the evaluation scheme. What is the RMSE for 
# the user-based collaborative filtering recommender?
recom_user6 <- Recommender(getData(es, "train"), method = "UBCF")
pred_ubcf6 <- predict(recom_user6, newdata = getData(es, "known"),
                      type = "ratings"); pred_ubcf6
accuracy_ubcf6 <- calcPredictionAccuracy(x = pred_ubcf6,
                                         data = getData(es, "unknown"))
accuracy_ubcf6
#1.2129814


# Question 7
# Next, evaluate the accuracy of another user-based collaborative filtering 
# recommender, with just one change from the previous question. Set the 
# parameter nn to 100. To learn more about the default nn, run:
recommenderRegistry$get_entries()$UBCF_realRatingMatrix
recom_user7 <- Recommender(getData(es, "train"), method = "UBCF", 
                           parameter = list(nn = 100))
pred_user7 <- predict(recom_user7, newdata = getData(es, "known"),
                      type = "ratings"); pred_user7
accuracy_user7 <- calcPredictionAccuracy(x = pred_user7,
                                         data = getData(es, "unknown"))
# What is the RMSE for this modified user-based collaborative filtering 
# recommender?
accuracy_user7
#1.1469487


# Question 8
# Finally, as a baseline for evaluating personalized recommenders, let us use a 
# non-personalized recommender that relies only on popularity not on similarity. 
# To do so, modify the code for the item-based recommender, replacing 'IBCF' by 
# 'POPULAR'. Note, there is no need to recreate the evaluation scheme.
recom_item8 <- Recommender(getData(es, "train"), method = "POPULAR")
pred_item8 <- predict(recom_item8, newdata = getData(es, "known"),
                      type = "ratings"); pred_item8
accuracy_item8 <- calcPredictionAccuracy(x = pred_item8,
                                         data = getData(es, "unknown"))
# What is the RMSE for this non-personalized recommender?
accuracy_item8
#1.1692988