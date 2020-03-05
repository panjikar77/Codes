
###### Heirarchical Clustering ######
library(readxl)

Airlines <- read_xlsx("D:/Data Science Course/Assignments/Clustering/EastWestAirlines.xlsx", sheet = 2)

View(Airlines)
str(Airlines)

# Normalizing continuous columns to bring them under same scale
Airlines_n <- scale(Airlines[,2:12]) #excluding ID column before normalizing
str(Airlines_n)
summary(Airlines_n)

d <- dist(Airlines_n, method = "euclidean") # distance matrix

fit <- hclust(d, method="ward.D2")
cutree(fit, h=89)
plot(fit) # display dendrogram
plot(fit, hang=-1)

rect.hclust(fit, k=3, border="red")

groups <- cutree(fit, k=3) # cut tree into 5 clusters

membership <- as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime, membership)

View(final)

###### K-Means Clustering ######

library(animation)
km <- kmeans.ani(Airlines_n,5) #K-Means Animation
km$cluster
km$withinss
km$centers

wss <- (nrow(Airlines_n)-1)*sum(apply(Airlines_n, 2, var))
for (i in 2 : 10) wss[i] <- sum(kmeans(Airlines_n, centers = i)$withinss)
plot(1:10, wss, type = "b", xlab = "No of Clusters", ylab = "Avg Distance") #Elbow Plot

library(fpc)
set.seed(123)
km_7 <- kmeans(Airlines_n, 7, nstart = 49)
plotcluster(Airlines_n,km_8$cluster)
km_8
