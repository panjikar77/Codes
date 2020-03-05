
set.seed(123)
CompanyData <- read.csv("D:/Data Science Course/Assignments/Random Forest/Company_Data.csv")
hist(CompanyData$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))
highsales = ifelse(CompanyData$Sales<9, "No", "Yes")  # if greater than 8 then high sales else Low
CD = data.frame(CompanyData[2:11], highsales)
str(CD)
table(CD$highsales)
# Data Partition
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]
set.seed(213)

#Building the model

library(randomForest)
rf <- randomForest(highsales~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting
attributes(rf)
# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)
head(train$highsales)

library(caret)
confusionMatrix(pred1, train$highsales)   # 100 % accuracy on training data 
# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsales) # 82.61 % accuracy on test data 
# Error Rate in Random Forest Model :
plot(rf)
# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1
pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$highsales)  # 100 % accuracy on training data 
# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$highsales) # 84.35 % accuracy on test data 
# Confidence Interval is around 90 % # no of nodes of trees
hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")
# Variable Importance :
varImpPlot(rf1)
varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
# Quantitative values 
importance(rf1)
varUsed(rf)   # which predictor variables are actually used in the random forest.
# Partial Dependence Plot 
partialPlot(rf1, train, Price, "Yes")
# Extract single tree from the forest :
getTree(rf, 1, labelVar = TRUE)
# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, CD$highsales)
