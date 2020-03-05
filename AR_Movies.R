movies <- read.csv("D:/Data Science Course/Assignments/Association Rules/my_movies.csv")
View(movies)

library(arules)
movies_rules <- apriori(as.matrix(movies[,6:15]), parameter = list(support = 0.02, confidence = 0.5, minlen = 2))
movies_rules
inspect(head(sort(movies_rules, by="lift")))

movies_rules2 <- apriori(as.matrix(movies[,6:15]), parameter = list(support = 0.002, confidence = 0.05, minlen = 2))
movies_rules
inspect(head(sort(movies_rules, by="lift")))

movies_rules3 <- apriori(as.matrix(movies[,6:15]), parameter = list(support = 0.02, confidence = 0.5, minlen = 5))
movies_rules
inspect(head(sort(movies_rules, by="lift")))

windows()
plot(movies_rules,method = "scatterplot")
plot(movies_rules,method = "grouped")
plot(movies_rules,method = "graph")
