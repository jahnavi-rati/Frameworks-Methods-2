###############################################################################
# Homework 1
###############################################################################

# Set Up ----------------------------------------------------------------------

survey <- read.csv("fastfood_survey.csv"); survey
View(survey)
summary(survey)
str(survey)
class(survey) #data.frame
nrow(survey) #662
ncol(survey) #21

# Section 1 -------------------------------------------------------------------
# Question 1
# How many variables are included in this dataset?
ncol(survey)
#21


# Question 2
# We will cluster the data based on the eleven fast-food restaurant 
# characteristics that respondents rated the importance of. These are the first
# eleven variables in the dataset. So, subset the data to only include the 
# first eleven columns of data. Call this data_cluster.

# For the first two sections of this assignment, we will only work with this
# reduced dataset.
# How many variables does data_cluster have?
data_cluster <- subset(survey, select = 1:11)
str(survey)
str(data_cluster)
#11


# Question 3
# Cluster analysis is particularly sensitive to missing values. Let us see if 
# this dataset has any missing values. How many missing values are there for 
# the variable cleanliness?
sum(is.na(data_cluster$cleanliness))
#23


# Question 4
# How many rows of data would be left if rows corresponding to missing values 
# on any of the eleven variables were removed? (Hint: You can use na.omit() but
# don't overwrite the original data)
data_cluster_na_omit <- na.omit(data_cluster)
nrow(data_cluster_na_omit)
sum(is.na(data_cluster_na_omit))
#556


# Question 5
# Let us impute the missing values. There are many packages and functions in R 
# to use for imputation. We are going to make use of the mice package with the 
# default method, predictive mean matching. To do this, run the following code. 
# (Setting the seed is critical to getting consistent results).
library(mice)
set.seed(1706)
data_cluster <- complete(mice(data_cluster, use.matcher = T))

# What is the imputed value of observation 10 for the variable cleanliness?
str(data_cluster)
data_cluster$cleanliness[10]
View(data_cluster)
#6


# Question 6
# Cluster analysis is sensitive to the scale of the variables, so we are going 
# to standardize the variables. Use the scale() function as illustrated here:
data_cluster_scaled <- scale(data_cluster)
data_cluster_scaled[1, "cleanliness"]
#0.3479976


# Section 2 ---------------------------------------

# Question 1
# Compute the Euclidean distance between all observations in data_cluster. How 
# many elements are in the distance matrix?
d <- dist(data_cluster_scaled, method = "euclidean")
length(d) 
#193131


# Question 2
# Conduct a Hierarchical cluster analysis using the method, 'ward.D2'. Plot the 
# dendrogram from this process. Let us see how well the dendrogram matches true
# distances. What is the Cophenetic correlation coefficient?
h_clusters <- hclust(d = d, method = "ward.D2")
plot(h_clusters)
cor(cophenetic(h_clusters), d)
#0.7903926  

# Question 3
# Based on the distances shown in the dendrogram alone, which is the best 
# cluster solution?
library(factoextra)
library(gridExtra)
grid.arrange(fviz_dend(x = h_clusters, k = 2),
             fviz_dend(x = h_clusters, k = 3),
             fviz_dend(x = h_clusters, k = 4),
             fviz_dend(x = h_clusters, k = 6),
             fviz_dend(x = h_clusters, k = 7)
             )
#2


# Question 4
# If one decided to go with a two-cluster solution, how many observations would 
# be in the smaller of the two clusters? (Save the cluster memberships in an 
# object as you will need it in a later question)
data_cluster2 <- cutree(tree = h_clusters, k = 2)
table(data_cluster2)
#41


# Question 5
# If one decided to go with a three-cluster solution, how many observations 
# would be in the smallest of the three clusters? (Save the cluster memberships 
# in an object as you will need it in a later question)
data_cluster3 <- cutree(tree = h_clusters, k = 3)
table(data_cluster3)
#41


# Question 6
# Next, we will cluster the data using k-means. Conduct k-means clustering to 
# generate a two-cluster solution. Since the choice of initial solution in 
# k-means has a random element, use a seed of 1706. Set max iterations to 100. 
# Do not set nstart. (Save the cluster memberships in an object as you will 
# need it in a later question). How many observations are in the smaller 
# cluster?
set.seed(1706)
km2 <- kmeans(x = data_cluster_scaled, centers = 2, iter.max = 100)
km2_clusters <- km2$cluster
table(km2_clusters)
#43


# Question 7
# Run another k-means clustering, but this time to generate a three-cluster 
# solution. As above, use a seed of 1706 and set max iterations to 100. Do not
# set nstart. (Save the cluster memberships in an object as you will need it in
# a later question). How many observations are in the smallest cluster?
set.seed(1706)
km3 <- kmeans(x = data_cluster_scaled, centers = 3, iter.max = 100)
km3_clusters <- km3$cluster
table(km3_clusters)
#41


# Question 8
# In the above k-means analyses, we set the number of clusters expected. Now, 
# let us examine a data driven approach to determining the number of clusters. 
# Compute the total within cluster sum of squares for cluster solutions from 1 
# to 10. Use a seed of 1706. Do not set nstart. What is the total within 
# cluster sum of squares for a three-cluster solution?
within_ss <- sapply(1:10, function(x){
  set.seed(1706)
  kmeans(x = data_cluster_scaled, centers = x, iter.max = 100)$tot.withinss
})
within_ss[3]
#3801.308


# Question 9
# For the three-cluster solution, what is the ratio of between sum of squares 
# and total sum of squares? (Express as a decimal.)
ratio_ss <- sapply(1:10, function(x){
  set.seed(1706)
  km <- kmeans(x = data_cluster_scaled, centers = x, iter.max = 100)
  km$betweenss / km$totss
})
ratio_ss[3]
#0.4435209


# Question 10
# Construct a line graph of clusters (on x-axis) against total within cluster 
# sum of squares (on y-axis). Based on this chart, which of the following are 
# good cluster solutions?
ggplot(data = data.frame(cluster = 1:10, within_ss), 
       aes(x = cluster, y = within_ss)) +
  geom_line(col = "blue") +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 10, 1))
#2, 3


# Question 11
# Next let us examine the Silhouette method, another data driven approach to 
# choosing number of clusters. What is the average silhouette width for a 2
# cluster solution? Use pam() from library(cluster) to compute silhouette width.
library(cluster)
pam(data_cluster_scaled, k = 2)$silinfo$avg.width
#0.5869224


# Question 12
# What is the average silhouette width for a 3 cluster solution?
pam(data_cluster_scaled, k = 3)$silinfo$avg.width
#0.1722077


# Question 13
# Examine average silhouette width for other cluster solutions. Based on this 
# criterion, which is the best cluster solution?
silhouette_width <- sapply(2:10, function(x){
  pam(data_cluster_scaled, k = x)$silinfo$avg.width
})
ggplot(data = data.frame(cluster = 2:10, silhouette_width), 
       aes(x = cluster, y = silhouette_width)) +
  geom_line(col = "blue") +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 10, 1))
#2


# Question 14
# Now, we will make use of a Model-based clustering technique. Use Mclust() 
# from library(mclust) to cluster the data. How many clusters has the model 
# decided to group the data into?
library(mclust)
m_clusters <- Mclust(data_cluster_scaled)
summary(m_clusters)
#3


# Question 15
# Now, use model-based clustering to force a two-cluster solution. (Save the 
# cluster memberships in an object as you will need it in a later question). 
# How many observations are in the smaller cluster?
m_clusters2 <- Mclust(data_cluster_scaled, G = 2)
summary(m_clusters2)
#171


# Question 16
# Now, let us compare the two-cluster solutions obtained from hierarchical 
# cluster to k-means. Specifically, compare the cluster assignments for 
# hierarchical cluster analysis to k-means for the two-cluster solution. For 
# how many observations do the cluster assignments differ?

# Note: each clustering method will assign group labels (1 or 2) to each data 
# point. Which label is applied is somewhat arbitrary. Imagine that k-means 
# assigned 90% of the data points to group 1 and 10% to group 2, and that the 
# hierarchical cluster flipped the labels. In this case, it would be reasonable 
# to say that the group 1 for k-means exactly matched the group 2 for the 
# hierarchical cluster, and likewise the group 2 for k-means exactly matched 
# the group 1 for the hierarchical cluster. Since we don't know whether the 
# labeling will be consistent, use the following rule:

# The number of cases that differ will be the minimum of a) the number of cases
# that match with the original labels and b) the number of cases that do not 
# match with the original labels.
table(data_cluster2)
table(km2_clusters)
min(sum(data_cluster2 == km2_clusters), 
    sum(data_cluster2 != km2_clusters))
#4


# Question 17
# Now, compare the two-cluster solutions for k-means to Model-based clustering. 
# Specifically, compare the cluster assignments for k-means to Model-based 
# clustering. For how many observations do the cluster assignments differ?

# Note: As in the previous question, use the following rule: 
# The number of cases that differ will be the minimum of a) the number of cases
# that match with the original labels and b) the number of cases that do not 
# match with the original labels.
table(km2_clusters)
summary(m_clusters2)
min(sum(m_clusters2$classification == km2_clusters), 
    sum(m_clusters2$classification != km2_clusters))
#128


# Section 3 ---------------------------------------

# Based on the previous section, it must be obvious that with cluster analysis, 
# multiple solutions are possible. Choice of the optimal number of clusters and 
# clustering technique should be based not only on data-driven metrics but also 
# understanding of the domain and the problem at hand.

# Having looked at the sizes of the clusters, the marketer felt the smaller 
# segment in the two-cluster solution was not actionable because there too 
# few customers. On the other hand, a three-cluster solution seemed more 
# promising because it had more balanced segment sizes. Since the cluster 
# selection methods also offered some support for a three-cluster solution, we 
# will use three clusters. We will use k-means clustering with three clusters. 
# Use a seed of 1706 and set maximum iterations to 100. Do not set nstart.

set.seed(1706)
km3 <- kmeans(x = data_cluster_scaled, centers = 3, iter.max = 100)
km3_clusters <- km3$cluster
table(km3_clusters)

# Next, we will examine the profile of the resulting clusters or segments in 
# terms of clustering variables and demographics. To do so, combine the cluster 
# memberships from three-cluster k-means with the original dataset, the one 
# with not only clustering variables but also demographic variables. You can 
# use cbind() to combine.

data <- cbind(km3_clusters, survey)
head(data)
View(data)

# Work for questions 1-4

library(cluster)
clusplot(data, km3_clusters, color = T, shade = F, labels = 2, lines = 0,
         main = "k-means Cluster Plot")

round(prop.table(table(data$km3_clusters,data$speed_of_service),1),2)*100
round(prop.table(table(data$km3_clusters,data$variety),1),2)*100
round(prop.table(table(data$km3_clusters,data$popularity_with_children),1),2)*100
round(prop.table(table(data$km3_clusters,data$cleanliness),1),2)*100
round(prop.table(table(data$km3_clusters,data$convenience),1),2)*100
round(prop.table(table(data$km3_clusters,data$taste),1),2)*100
round(prop.table(table(data$km3_clusters,data$price),1),2)*100
round(prop.table(table(data$km3_clusters,data$drive_through),1),2)*100
round(prop.table(table(data$km3_clusters,data$friendliness),1),2)*100
round(prop.table(table(data$km3_clusters,data$quality_of_fries),1),2)*100
round(prop.table(table(data$km3_clusters,data$taste_burgers),1),2)*100


# Question 1
# Compared to other clusters, Cluster 1 has the lowest value for:
#popularity with children, drive through


# Question 2
# Compared to other clusters, Cluster 2 has the highest value for:
#speed of service, variety, popularity with children, cleanliness, 
#convenience, taste, price, friendliness, quality of fries, taste of burgers


# Question 3
# Compared to other clusters, Cluster 3 has the highest value for:
#drive through


# Question 4
# Compared to other clusters, Cluster 3 has the lowest value for:
#speed of service, variety, cleanliness, convenience, taste, price, friendliness, 
#quality of fries, taste of burgers


# Work for questions 5-7

# Now, let us understand the demographic makeup of the customers that belong to 
# each cluster or market segment. To examine distributions of factor demographic 
# variables, you could use the table function. E.g.,
##  table(data$segment,data$demographicVariable)
# Since segment sizes are different, you could examine percentage of each group 
# in the segment by using prop.table as illustrated here:
  
round(prop.table(table(data$km3_clusters,data$dollars_avg_meal),1),2)*100
round(prop.table(table(data$km3_clusters,data$marital_status),1),2)*100
round(prop.table(table(data$km3_clusters,data$gender),1),2)*100
round(prop.table(table(data$km3_clusters,data$number_children),1),2)*100
round(prop.table(table(data$km3_clusters,data$own_rent),1),2)*100
round(prop.table(table(data$km3_clusters,data$dwelling),1),2)*100
round(prop.table(table(data$km3_clusters,data$occupation),1),2)*100
round(prop.table(table(data$km3_clusters,data$education),1),2)*100
round(prop.table(table(data$km3_clusters,data$age),1),2)*100
round(prop.table(table(data$km3_clusters,data$income),1),2)*100


# Question 5
# Compared to other clusters, Cluster 1:
#has the lowest percent home ownership, is the youngest


# Question 6
# Compared to other clusters, Cluster 2:
#has the smallest percent of singles, has the largest percent of females,
#has the most number of children, has the largest percentage stay at home moms, 
#has the least amount of education


# Question 7
# Compared to other clusters, cluster 3:
#spends the most when eating out, has the largest percentage of professionals


