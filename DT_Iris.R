data("iris")

# Splitting the data based on species 

iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50

# Splitting data into training and testing. As the species are in order 

iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# Building model using 'ctree' function from 'party' package

library(party)
tree <- ctree(Species~., data = iris_train)
windows()
plot(tree)

# Training accuracy

pred_train <- predict(tree,iris_train)
mean(iris_train$Species==pred) # 97.33% Accuracy

# Confusion Matrix
library(caret)
confusionMatrix(pred_train,iris_train$Species)

pred <- predict(tree, newdata = iris_test )
mean(iris_test$Species==pred) # 94.67% Accuracy

# Confusion Matrix
confusionMatrix(pred,iris_test$Species)

# Cross Table
library(gmodels)
CrossTable(pred, iris_test$Species)
