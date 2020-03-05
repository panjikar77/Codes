glass <- read.csv("D:/Data Science Course/Assignments/KNN/glass.csv")
head(glass)

table(glass$Type)

summary(glass[c("Na", "Si", "Fe")])

norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))}

glass_n <- as.data.frame(lapply(glass[1:9], norm))
summary(glass_n[c("Na", "Si", "Fe")])
glass_n <- cbind(glass_n, glass[10])

library(corrplot)
corrplot(cor(glass_n))

library(caTools)
set.seed(123)
sample = sample.split(glass, SplitRatio = 0.6)

glass_train <- subset(glass_n, sample == TRUE)
glass_test <- subset(glass_n, sample == FALSE)

library(class)
library(gmodels)
glass_KNN <- knn(train = glass_train[1:9], test = glass_test[1:9], 
                 cl = glass_train$Type, k = 3)
CrossTable(glass_test$Type, glass_KNN, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE, prop.chisq = FALSE)

glass_KNN <- NULL
error <- NULL

for(i in 1:10){ 
  glass_KNN <- knn(train = glass_train[1:9], test = glass_test[1:9], 
                   cl = glass_train$Type, k = i)
  error[i] <- mean(glass_KNN != glass_test$Type)
}

plot(1:10, error, type = 'l') #Least Error at k=3


