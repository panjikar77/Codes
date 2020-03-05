
groceries<-read.transactions("D:/Data Science Course/Assignments/Association Rules/groceries.csv",format="basket")
inspect(groceries[1:10])
class(groceries)

# count of each item from all the transactions 
itemFrequencyPlot(groceries,topN=20)

library(arules)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
inspect(groceries_rules[1:20])

groceries_rules2<-apriori(groceries,parameter = list(support = 0.001,confidence = 0.05,minlen=3))
inspect(groceries_rules2[1:10])

groceries_rules3<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=2))
inspect(groceries_rules3[1:10])

library(arulesViz)
windows()
plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")

groceries_rules <- sort(groceries_rules, by="lift")

inspect(rules[1:20])

