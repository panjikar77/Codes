 
set.seed(123) # Used to get same result each time
data(iris)
View(iris)

# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

library(randomForest)
rf <- randomForest(Species~., data=iris_train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting

attributes(rf)

# Prediction and Confusion Matrix - Training data 

pred1 <- predict(rf, iris_train)
head(iris_train$Species)

library(caret)

confusionMatrix(pred1, iris_train$Species)  # 100 % accuracy on training data 

head(pred1)

# Prediction with test data - Test Data 

pred2 <- predict(rf, iris_test)
confusionMatrix(pred2, iris_test$Species) # 94,67 % accuracy on test data 

# Visualization 
plot(rf,lwd=2)
legend("topright", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)
