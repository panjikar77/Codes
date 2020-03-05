# Reading and Viewing Data

CD <- read.csv("D:/Data Science Course/Assignments/Decision Tree/Company_Data.csv")
View(CD)
colnames(CD)
hist(CD$Sales)

# Categorising Sales Data as High or not

High <- ifelse(CD$Sales <= 10, "No", "Yes")
CompanyData <- data.frame(CD, High)
CompanyData <- CompanyData[,2:12]
View(CompanyData)

# Splitting Data into Train and Test Components

CompanyData_train <- CompanyData[1:320,]
CompanyData_test <- CompanyData[321:400,]

# Building the model with 'ctree' function from 'party' package

library(party)
tree <- ctree(High ~., data = CompanyData_train)
plot(tree)

# In a Good Shelf Location 
# With Price <= 119, there's 80% chance of high sales. 
# With Price > 119 there's 30% chance of high sales.

# In a Medium or Bad Shelf Location
# With Price <= 87, there's 50% chance of high sales.
# With Price > 87 and average Age of local population > 48, there is zero percent chance of high sales.
# If Age <= 48 and Price <= 113, 40% chance of high sales. For Price > 113, 5% chance of high sales.

pred <- predict(tree, newdata = CompanyData_test)
mean(pred == CompanyData_test$High) # Accuracy = 85%

# Cross Table

library(gmodels)
CrossTable(CompanyData_test$High, pred)
