setwd("G:/R_Training/DEcision Tree")

getwd()

library(randomForest) ##rfmodel
library(caret) ##feature selection
library(e1071) ##model tuning
library(ROCR) ##model evaluation
source("data_preparation.R")

#separate features and class variables
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]
test.class.var

prop.table(table(train.data$credit.rating))
prop.table(table(test.data$credit.rating))



rf.model <- randomForest(credit.rating ~., data = train.data,importance = T,
                         mtry=5, nodesize=5, ntrees= 1000)



summary(rf.model)
##view moidel details

print(rf.model)


## to check the importance of variable / information on OOBE

randomForest::varImpPlot(rf.model, sort = T,n.var = 20,type =1)


##predict and evaliate values

rf.predictions <- predict(rf.model, test.data, type = "class")
rf.predictions <-as.numeric(rf.predictions)
head(rf.predictions)
##rf.predictions1 <- predict(rf.model, test.data, type = "class")

caret::confusionMatrix(data = rf.predictions, reference=test.class.var,
                       positive="1")

pred <- ROCR::prediction(rf.predictions,test.class.var)

perf <- ROCR::performance(prediction.obj = pred, measure = "tpr", x.measure = "fpr",colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

##checking accuracy for ROCR
auc <- ROCR::performance(pred,"auc")
auc <- unlist(slot(auc,"y.values"))
minauc <- round(auc, digits =2)
minauc
##Nmaing the value with the variable min(AUC)
minauct <- paste(c("min(AUC) = "),minauc,sep = "")
minauct
##Priniting AUC value in the chart
legend(0.5,0.5,c(minauct,"\n"),border="White",cex=1.2,box.col = "Red")

perf1 <- ROCR::performance(prediction.obj = pred, measure = "tpr", x.measure = "fpr",colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
plot(perf1)


##hyperparameter optimizations

nodesize.vals <- c(2,3,4,5)
ntree.vals <- c(200,500,1000,2000)
formula.new <- "creditrating ~ account.balance + savings +credit.amount+
                credit.duration.months+previous.credit.payment.status"

formula.new <- as.formula(formula.new)

##tuning.results <- tune.randomForest(formula.new,data=train.data,mtry=3,
                                    nodesize=nodesize.vals,
                                    ntree=ntree.vals)

print(tuning.results)

##to get best model and evaluate
rf.model.best <-tuning.results$best.model
rf.predictions.best <- predict(rf.model.best, test.feature.vars, type = "class")

confusionMatrix(data=rf.predictions.best, refrence=test.class.var, positive = "1")


