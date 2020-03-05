###### Heirarchical Clustering ######
crime <- read.csv("D:/Data Science Course/Assignments/Clustering/crime_data.csv")

View(crime)
str(crime)

# Normalizing continuous columns to bring them under same scale
crime_n <- scale(crime[,2:5]) #excluding the state column before normalizing

d <- dist(crime_n, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")

fit1 <- hclust(d, method = "centroid")
fit2 <- hclust(d, method = "mcquitty")

d1 <- dist(crime_n, method = "manhattan")
fit3 <- hclust(d1, method="complete")

d2 <- dist(crime_n, method = "maximum")
fit4 <- hclust(d2, method="centroid")

plot(fit) # display dendrogram

plot(fit, hang=-1)
rect.hclust(fit, k=4, border="red")

plot(fit1, hang=-1)
plot(fit2, hang=-1)
plot(fit3, hang=-1)
plot(fit4, hang=-1)

groups <- cutree(fit, k=4) # cut tree into 5 clusters

membership <- as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime, membership)

View(final)

###### K-Means Clustering ######

library(animation)
km <- kmeans.ani(crime_n,5) #K-Means Animation
km$cluster
km$withinss
km$centers

wss <- (nrow(crime_n)-1)*sum(apply(crime_n, 2, var))
for (i in 2 : 10) wss[i] <- sum(kmeans(crime_n, centers = i)$withinss)
plot(1:10, wss, type = "b", xlab = "No of Clusters", ylab = "Avg Distance") #Elbow Plot

library(fpc)
set.seed(123)
km_8 <- kmeans(crime_n,4, nstart = 50)
plotcluster(crime_n,km_8$cluster)
km_8
