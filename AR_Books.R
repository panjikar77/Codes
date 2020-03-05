#Read and view file
Books <- read.csv("D:/Data Science Course/Assignments/Association Rules/book.csv")
View(Books)

library(arules)
library(arulesViz)

Book_rules <- apriori(as.matrix(Books), parameter = list(support = 0.02, confidence = 0.5, minlen =5))
inspect(Book_rules[1:10])

Book_rules2 <- apriori(as.matrix(Books), parameter = list(support = 0.002, confidence = 0.05, minlen =5))
inspect(Book_rules2[1:10])

Book_rules3 <- apriori(as.matrix(Books), parameter = list(support = 0.02, confidence = 0.5, minlen =3))
inspect(Book_rules3[1:10])

windows()                      
plot(Book_rules,method = "scatterplot")
plot(Book_rules,method = "grouped")
plot(Book_rules,method = "graph")

