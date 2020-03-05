forestfires = read.csv("D:/Data Science Course/Assignments/Support Vector Machines/forestfires.csv")
View(forestfires)

forest_train <- forestfires[1:350,]
forest_test  <- forestfires[351:517,]
library(kernlab)
forest_classifier <- ksvm(size_category~ ., data = forest_train,
                          kernel = "vanilladot")
forest_classifier

# predictions on testing dataset
forest_predictions <- predict(forest_classifier, forest_test)

head(forest_predictions)

table(forest_predictions, forest_test$size_category)


agreement <- forest_predictions == forest_test$size_category
table(agreement)
prop.table(table(agreement)) # Accuracy = 94.6%


## Improving model performance ---- kernel = polydot
forest_classifier_poly <- ksvm(size_category ~ ., data = forest_train, kernel = "polydot")
forest_predictions_poly <- predict(forest_classifier_poly, forest_test)

agreement_poly <- forest_predictions_poly == forest_test$size_category
table(agreement_poly)
prop.table(table(agreement_poly)) # Accuracy = 94.6%

## Improving model performance ---- kernel = tanhdot
forest_classifier_tanh <- ksvm(size_category ~ ., data = forest_train, kernel = "tanhdot")
forest_predictions_tanh <- predict(forest_classifier_tanh, forest_test)

agreement_tanh <- forest_predictions_tanh == forest_test$size_category
table(agreement_tanh)
prop.table(table(agreement_tanh)) # Accuracy = 67.7%


## Improving model performance ---- kernel = rbfdot
forest_classifier_rbf <- ksvm(size_category ~ ., data = forest_train, kernel = "rbfdot")
forest_predictions_rbf <- predict(forest_classifier_rbf, forest_test)

agreement_rbf <- forest_predictions_rbf == forest_test$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf)) # Accuracy = 76.0%

## Improving model performance ---- kernel = splinedot
forest_classifier_s <- ksvm(size_category ~ ., data = forest_train, kernel = "splinedot")
forest_predictions_s <- predict(forest_classifier_s, forest_test)

agreement_s <- forest_predictions_s == forest_test$size_category
table(agreement_s)
prop.table(table(agreement_s)) # Accuracy = 94.0%

# Kernel trick 'vanilladot' and 'polydot' give the highest accuracy.