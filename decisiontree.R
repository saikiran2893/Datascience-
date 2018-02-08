setwd("G:/R_Training/DEcision Tree")
getwd()


credit <- read.csv("credit.csv",stringsAsFactors = TRUE)

View(credit)

str(credit)

##table command used to check the number of occurances
##to explaore categorical variable
table(credit$checking_balance)
table(credit$savings_balance)

##to explore numeric variable
summary(credit$months_loan_duration)
summary(credit$amount)
summary(credit)

##to check outliers,remove outliers observations to build good model
boxplot(credit$amount)

boxplot(credit$months_loan_duration)

##to check the outcome value counts on dependant variable
table(credit$default)

set.seed(1234)

train_sample <- sample(1000,900)
train_sample

##creating the taining smapel with rownumbers

credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

View(credit_test)

View(credit_train)
## checking the dependant variabe counts in test and train
table(credit_train$default)
table(credit_test$default)


## to check data distibuted us sane among test and train dataset,if not ren runt he code without setseed
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

##TRAINING A MODEL

##we will use C5.0 algorithm in the C50 package to train our decion tree model


library("C50")
names(credit_train)

credit_model <- C50::C5.0(credit_train[-17],credit_train$default)
summary(credit_model)


## evaluating model performance
##apply model on test daata
credit_prediction <- predict(credit_model,credit_test)

##this creates a vector of predicted class values which can compare to the actual
##claSS VALUES

library(gmodels)
CrossTable(credit_test$default,credit_prediction, prop.chisq=FALSE, prop.r=FALSE,
           prop.c=FALSE, dnn=c("Actual Default","Predicted Default"))

CrossTable(credit_test$default,credit_prediction,dnn=c("Actual Default","Predicted Default"))

mean(credit_test$default==credit_prediction)

##IMproving the model
##you can boost the accuracy of the decisio tree
##boosting is done by combinig a number of weak performing learners and create
##a team and trail =10 is the best

credit_boost10 <- C50::C5.0(credit_train[-17],credit_train$default,trials = 10)

summary(credit_boost10)

##predict with new boosted model
credit_boos_predict <- predict(credit_boost10,credit_test)

mean(credit_test$default==credit_boos_predict)

CrossTable(credit_test$default,credit_boos_predict, prop.chisq=FALSE, prop.r=FALSE,
           prop.c=FALSE, dnn=c("Actual Default","Predicted Default"))

##removing least usage and built the model


