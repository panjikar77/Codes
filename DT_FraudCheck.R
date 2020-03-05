# Reading and Viewing Data

data <- read.csv("D:/Data Science Course/Assignments/Decision Tree/Fraud_check.csv")
View(data)
colnames(data)
str(data)

# Categorising Data as Risky or Good

Risk <- ifelse(data$Taxable.Income <= 30000, "Good", "Risky")
FraudCheck <- data.frame(data[-3], Risk)
View(FraudCheck)

# Splitting Data into Train and Test Components

FraudCheck_train <- FraudCheck[1:420,]
FraudCheck_test <- FraudCheck[421:600,]

# Building model with C5.0
library(C50)
tree = C5.0(FraudCheck_train[,-6], FraudCheck_train$Risk)
plot(tree)

pred <- predict(tree, newdata = FraudCheck_test)
mean(pred == FraudCheck_test$Risk) # Accuracy = 85%

# Cross Table

library(gmodels)
CrossTable(FraudCheck_test$Risk, pred)
