Bank = read.csv("D:/Data Science Course/Assignments/Logistic regression/bank-full.csv", sep = ";")

summary(Bank)
View(Bank)
str(Bank) #structure of dataset

attach(Bank)
plot(job,y)
plot(marital,y)
plot(education,y)

Bank$y =as.factor(Bank$y)
summary(Bank$y)
Bank$job = factor(Bank$job,levels = c("admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
                                      "blue-collar","self-employed","retired","technician","services"),labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
Bank$marital = factor(Bank$marital,levels = c( "married","divorced","single"),labels = c(0, 1, 2))

Bank$education = factor(Bank$education,levels = c( "unknown","secondary","primary","tertiary"),labels = c(0, 1, 2,3))
Bank$default = factor(Bank$default,levels = c("yes" , "no"),labels = c(0,1))
Bank$housing= factor(Bank$housing, levels = c("yes","no"),labels = c(0,1))
Bank$loan =factor(Bank$loan,levels = c("yes","no"),labels = c(0,1))
Bank$contact =factor(Bank$contact,levels = c("unknown","telephone","cellular"),labels = c(0,1,2))
Bank$month = factor(Bank$month,levels = c("jan", "feb", "mar", "apr" , "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), labels = c(0,1,2,3,4,5,6,7,8,9,10,11))
Bank$poutcome = factor(Bank$poutcome,levels = c("unknown","other","failure","success"),labels = c(0,1,2,3))
View(Bank)

logit = glm(y ~ age + factor(job)  + factor(marital) + factor(education) + factor(default) + balance + factor(housing) + factor(loan) + factor(month)
           + duration + pdays + previous +factor(poutcome),family= "binomial",data=Bank)

?glm 
summary(logit)
?logit


# Odds Ratio


# Confusion Matrix Table

prob=predict(logit,type=c("response"),Bank)
prob

confusion<-table(prob>0.7,Bank$y)
confusion


# Model Accuracy

Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy

