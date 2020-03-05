zoo <- read.csv("D:/Data Science Course/Assignments/KNN/Zoo.csv")
View(zoo)

zoo$type <- factor(zoo$type,levels = c("1","2","3","4","5","6","7"),
                   labels = c("Type-1","Type-2","Type-3","Type-4","Type-5","Type-6","Type-7"))
table(zoo$type)

summary(zoo[c("airborne","aquatic","legs")])

norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))}

zoo_n <- as.data.frame(lapply(zoo[2:17], norm))
summary(zoo_n[c("airborne","aquatic","legs")])

zoo_train <-  zoo_n[1:80, 1:16]
zoo_test <-  zoo_n[81:101, 1:16]

zoo_train_labels <-  zoo[1:80, 18]
zoo_test_labels <-  zoo[81:101, 18]

library(class)
zoo_KNN <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=3)
plot(zoo_KNN, col = rainbow(7), main = "Classification of Animals", xlab = "Types of Animal")

library(gmodels)
CrossTable(zoo_test_labels, zoo_KNN, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE, prop.chisq = FALSE)

zoo_KNN <- NULL
error <- NULL

for(i in 1:10){ 
  zoo_KNN <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=i)
  error[i] <- mean(zoo_KNN != zoo_test_labels)
}

plot(1:10, error, type = 'l') #Least Error at k=3
