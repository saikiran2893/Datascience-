setwd("G:/R_Training/SVM")

library("e1071") ##for SVM
library("caret") ##for splitting sample randomly


bdf <- read.table("biodeg.csv", sep=";", header = F)

head(bdf,3)

##coloumn v42 is the dependant variable
##NRB as non biodegaradable
##RB as biodegaradable
## we decode NRB as 0 and RB as 1

levels(bdf$V42) <- c(0,1)
#Set seed

set.seed(123)
##split sample into 80:20 ratio
bdf_sampling_vector <- createDataPartition(bdf$V42, p = 0.80, list = FALSE)


bdf_train_set <- bdf[bdf_sampling_vector,]
bdf_test_set <- bdf[-bdf_sampling_vector,]

nrow(bdf_train_set)


##Explore the data

##notice that scale of various deatures are quite different
#Also,many features are sparse features
##It is good idea to scale features

model_lin <- svm(V42~., data=bdf_train_set, kernel = "linear", cost=10, scale=TRUE)

#c=10 is the soft margin classifier

summary(model_lin)


##The summary does not provide such info
##but the fitted attributes contains

mean(bdf_train_set[,42] == model_lin$fitted)

##This gives training accuracy

##confusion matrix

table(actual = bdf_train_set[,42],predictions = model_lin$fitted)

##using predict on the test data

test_predict <- predict(model_lin,bdf_test_set[,1:41])

mean(bdf_test_set[,42] == test_predict)

table(actual = bdf_test_set[,42],predictions = test_predict)

##The cost paramter plays an important role
##we can rerun the SVM with various cost paramter values
##and that inovless a trade_off in model bias and variance
runLinearSVMwithVariouscost <- function(Xcost)
{
  modellin <- svm(V42~., data=bdf_train_set, kernel = "linear",
                  cost =Xcost)
  train_accuracy <- signif(mean(bdf_train_set[,42] == modellin$fitted), digits= 3)

  ## signif to set decimal places
  
  test_predictions <- predict(model_lin,bdf_test_set[,1:41])
  
  test_accuracy <- signif(mean(bdf_test_set[,42] == test_predictions), digits= 3)
  
  return(list(trainin= train_accuracy, test=test_accuracy))
  }


cost <- c(0.01,0.1,1,10,100,1000)
##Run the runlinearSVMWithVariousCost()

linearPerformance <- sapply(cost, runLinearSVMwithVariouscost)
colnames(linearPerformance) <- cost
linearPerformance
