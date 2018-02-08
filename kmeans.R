setwd("G:/R_Training/Kmeans")


library(cluster)
library(fpc)
##### Clustering with k-means ##### 

## Business Case: Finding Teen Market Segments ## 

## Exploring and preparing the data ## 
## We have a random sample of 30,000 US high school students

teens <- read.csv("snsdata.csv")

## 36 most frequent words used have been chosen to represent
## 5 categories of interests viz, extracurricular activities,
## fashion, religion, romance and anti-social behaviour
## The dataset also has 4 variables indicating personal 
## characteristics
str(teens)

## The data set is sampled evenly across four high school years
table(teens$gradyear)

## Notice that Gender has NA values
## Lets see the distribution of Males & Females
table(teens$gender)

## Notice that in spite of gender having NA the table() 
## function does not show the NAs
# So to look at missing data for gender variable
table(teens$gender, useNA = "ifany")
prop.table(table(teens$gender, useNA = "ifany"))

## Data Prep: Dummy Coding Missing Values
## One option is to reassign missing gender values to "unknown"
## Recall that dummy coding involves creating binary values
## So since we have 3 values for Gender - Male, Female, Unknown
## We create 2 new dummy coding variables - Female & Unknown

teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)

teens$unknown <- ifelse(is.na(teens$gender), 1, 0)

## check our recoding work
## Note that Gender Variable will be no use to us now
## If a person is not female or not Unknown, then its Male
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$unknown, useNA = "ifany")

## Lets take a look at the age variable
summary(teens$age)

## We notice that Age variable has NAs
## Also, we notice that the minimum age is 3
## And the maximum age is 106 which is not very likely
## a more resonable range would be at least 13 years
## and have not crossed 20

## So we first change them to NA
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)

## So now you see that the number of NAs have further increased, 
## which is obvious
summary(teens$age)

## We have categorized missing values for categorical variable into Unknown
## Now we need to treat numeric missing values
## We have 5523 missing Age Values
## Lets find the mean age
summary(teens$age)
mean(teens$age) # doesn't work
mean(teens$age, na.rm = TRUE) # works, we enforce removing NAs

## Let us see the age by cohort
## The mean age differs by roughly 1 Year per change in Graduation Age
## This is quite ok and reasonable
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

## Create a vector with the average age for each gradyear, 
## repeated by person
ave_age <- ave(teens$age, teens$gradyear,
               FUN = function(x) mean(x, na.rm = TRUE))

tail(ave_age)
head(ave_age, 20)
## Replace the missing values with the average age
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
## check the summary results to ensure missing values are eliminated
summary(teens$age)

## Selecting the features
interests <- teens[5:40]
head(interests)
summary(teens[5:40])
## Process of z-score standardization rescales features
interests_z <- as.data.frame(lapply(interests, scale))
head(interests_z)
summary(interests_z)

## Training a model on the data after z-score standardization
set.seed(1234)
teen_clusters <- stats::kmeans(interests_z, centers=5)

## Evaluating model performance ----

## look at the size of the clusters
teen_clusters$size
## We see 5 clusters have been produced
## The smallest cluster has some 600 (2%) teenagers and the
## largest cluster has some 20K (70%) teenagers

## This large gap between the small and large cluster is
## of concern but without examining, we will not know
## It might be the case that the cluster size 
## indicates something real; such as a big group of
## teenagers share similar interests OR
## it may be a random fluke caused by K-Means 

## Let us look at the cluster centers
teen_clusters$centers
write.csv(teen_clusters$centers, "centers.csv")
## Rows of the output are labeled 1 to 5 referring to each 
## of the clusters
## Numbers at each row indicate the cluster's average for the 
## respective interests

## Interpreting: The values are z-score standardized
## Positive values indicate above overall mean level
## Negative values indicate below overall mean level
## Say, The 5th row has highest value in Shopping column 
## across all columns
## means that cluster 5 has the highest average interest
## in Shopping

## Start examining the clusters
## In practice this invloves printing the clusters centers
## and searching thhrough them for any patterns or extreme values
## Interestingly you see that cluster 3 which has highest members have 
## lower than average of interest in all activities
## One possible reason is users created a profile on the social media
## but never posted any interests

## Based on studies donw on the centers, the marketing executives
## could see targeted advertisement to the right segments

## Can we distinguish between different cluster categories? 
## Atheletes, Cheerleaders/Princesses,Criminals, Brains etc?

## Further study on the clusters
# apply the cluster IDs to the original data frame
teen_clusters$cluster
teens$cluster <- teen_clusters$cluster

# You look at the first five records
teens[1:5, c("cluster", "age", "friends")]
teen_clusters$cluster
teens$cluster <- teen_clusters$cluster

# You can see the mean age by cluster
aggregate(data = teens, age ~ cluster, mean)

# You can see the mean number of friends by cluster
aggregate(data = teens, friends ~ cluster, mean)

# You can subset your data for marketing actions or further modeling
subsetdata <- teens[ which(teens$cluster==4), ]
write.csv(subsetdata, "data_cluster1.csv")
