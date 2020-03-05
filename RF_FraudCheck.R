#fraud dataset

library(MASS)

# USe the set.seed function so that we get same results each time 
set.seed(123)
FraudCheck <- read.csv("D:/Data Science Course/Assignments/Random Forest/Fraud_check.csv")
hist(FraudCheck$Taxable.Income)
hist(FraudCheck$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","violet"))
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky else Good.
FCtemp= data.frame(FraudCheck,Risky_Good)
FC = FCtemp[,c(1:7)]
str(FC)
table(FC$Risky_Good)   # 476 good customers and 124 risky customers
# Data Partition
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]

# Building the model

library(randomForest)
set.seed(213)
rf <- randomForest(Risky_Good~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting
attributes(rf)

# Prediction and Confusion Matrix - Training data 

pred1 <- predict(rf, train)
head(pred1)
head(train$Risky_Good)
library(caret)
confusionMatrix(pred1, train$Risky_Good)   # 100 % accuracy on training data

# Prediction with test data - Test Data 

pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good) # 100 % accuracy on test data 
# Error Rate in Random Forest Model :
plot(rf)

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(Risky_Good~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1
pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$Risky_Good)  # 100 % accuracy on training data 
# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$Risky_Good) # 100 % accuracy on test data 
# no of nodes of trees
hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")
# Variable Importance :
varImpPlot(rf1)
varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
# Quantitative values 
importance(rf1)
varUsed(rf)   # which predictor variables are actually used in the random forest.
# Partial Dependence Plot 
partialPlot(rf1, train, Taxable.Income, "Good")
tr1 <- getTree(rf1, 2, labelVar = TRUE)
# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, FC$Risky_Good)
