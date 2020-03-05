
Salary_train <- read.csv("D:/Data Science Course/Assignments/Support Vector Machines/SalaryData_Train(1).csv")
Salary_test  <- read.csv("D:/Data Science Course/Assignments/Support Vector Machines/SalaryData_Test(1).csv")
head(Salary_train)
head(Salary_test)

library(kernlab)
Salary_classifier <- ksvm(Salary~ ., data = Salary_train,
                          kernel = "vanilladot")
Salary_classifier

# Predictions on testing dataset
Salary_predictions <- predict(Salary_classifier, Salary_test)

head(Salary_predictions)

table(Salary_predictions, Salary_test$Salary)


agreement <- Salary_predictions == Salary_test$Salary
table(agreement)
prop.table(table(agreement)) # Accuracy = 84.6%


## Improving model performance ---- kernel = polydot
Salary_classifier_poly <- ksvm(Salary ~ ., data = Salary_train, kernel = "polydot")
Salary_predictions_poly <- predict(Salary_classifier_poly, Salary_test)

agreement_poly <- Salary_predictions_poly == Salary_test$Salary
table(agreement_poly)
prop.table(table(agreement_poly)) # Accuracy = 84.6%

## Improving model performance ---- kernel = rbfdot
Salary_classifier_rbf <- ksvm(Salary ~ ., data = Salary_train, kernel = "rbfdot")
Salary_predictions_rbf <- predict(Salary_classifier_rbf, Salary_test)

agreement_rbf <- Salary_predictions_rbf == Salary_test$Salary
table(agreement_rbf)
prop.table(table(agreement_rbf)) # Accuracy = 85.4%

# Kernel trick 'rbfdot' gives higher accuracy compared to 'vanilladot' and 'polydot'
