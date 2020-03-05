
library("recommenderlab")
library(caTools)

#book rating data
Author <- read.csv("D:/Data Science Course/Assignments/Recommendation system/books.csv")

#metadata about the variable
str(Author)
View (Author)

#rating distribution
hist(Author$ratings...3.)

#the datatype should be realRatingMatrix inorder to build recommendation engine
Author_matrix <- as(Author, 'realRatingMatrix')

#Popularity based 

Author_recomm_model1 <- Recommender(Author_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(Author_recomm_model1, Author_matrix[1], n=5)
as(recommended_items1, "list")

?Recommender
## Popularity model recommends the same books for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

Author_recomm_model2 <- Recommender(Author_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(Author_recomm_model2, Author_matrix[1],n = 5)
as(recommended_items2, "list")

