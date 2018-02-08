setwd("G:/R_Training")
getwd()

#loading file in to R
propertytraindata <- read.csv("PropertyTrainData.csv")

propertytestdata <- read.csv("PropertyTestData.csv")

##Exploring data
summary(propertytraindata)

View(propertytraindata)

##checking proce coloumn for normal distribution
#with histogram

hist(propertytraindata$Price)


#checking Price column for normal distribution
# with Shapiro MIlk test
#Ho is data is normally distributed
#Ha is data is NOT Normally distributed

shapiro.test(propertytraindata$Price)
##P-value is less than 0.05 so it not normally distributed so reject null hypotesis Ho
##Here Price is have skewness


##To reduce skewness do log transformation
mylogprice <- log(propertytraindata$Price)
hist(mylogprice)
summary(mylogprice)

shapiro.test(mylogprice)
#P-valueis greate than 0.05 so it is normally distributed

##Add log(price) values into the train dataset

propertytraindata$logprice <- as.numeric(mylogprice)
###Antilog or do exponentiql atalast to remove it as dependent variable

View(propertytraindata)

##check correlation
cor(propertytraindata[,-1])
##Rounding to 3 Decimal places for easy Viewing
cormat <- round(cor(propertytraindata[,-1]),3)
##write to the CSV file
write.csv(cormat,"corrmatrix.csv")

pairs(propertytraindata[,-1])

##to develop plots install GGally and run below commands
  library(GGally)
  ggpairs(propertytraindata[,-1])

##to plot between two variables
plot(propertytraindata$logprice, propertytraindata$Days)

plot(propertytraindata$logprice, propertytraindata$Area)

##lets propose the following regression model

reg_model <- lm(logprice ~ ., data=propertytraindata[,-1])
summary(reg_model)
plot(reg_model)

names(propertytraindata)
##new model without sea
reg_model_a <- lm(logprice ~ Area+Days+Distance+Flood+
                  Elevation+Sewer, data=propertytraindata[,-1])
summary(reg_model_a)
plot(reg_model_a)

##new model without sea and area as it not significant

reg_model_b <- lm(logprice ~., data=propertytraindata[,-c(1:3)])
summary(reg_model_b)

##check for multicollinerity
library("car")
car::vif(reg_model_b)
##remove the variable wich has highest value greter than 4

car::vif(reg_model_a)

##check heteroscedaticity
#Ha check for Breusch_pagan test for Heteroscadasicity;
#ho :Homoscedasticity(Variance of residual is constant)

library(lmtest)
lmtest::bptest(reg_model_b)


##check autocorrelation using DUrbin-Watson test
#ho: No autocorrelation ; Ha:AutoCOrrelation present
#How to rectify:Consider removing predictor variabes
lmtest::dwtest(reg_model_b)
summary(reg_model_b)


##fitting the model
predictedvalues <- predict(reg_model_b,newdata=propertytestdata)
predictedvalues
propertytestdata$predictedvalue <- exp(predictedvalues)
write.csv(propertytestdata,"predictedresult.csv")
