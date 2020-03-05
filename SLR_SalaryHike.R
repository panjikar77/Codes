#Read and View Data
Employee <- read.csv("D:/Data Science Course/Assignments/Simple linear regression/Salary_Data.csv")
View(Employee)

summary(Employee)
plot(Employee)
cor(Employee)

attach(Employee) 

############### Simle Linear Regression Model ###############
model1 = lm(Salary ~ YearsExperience)
summary(model1)

pred1 = predict(model1)

sqrt(mean(model1$residuals^2)) #RMSE

confint(model1, level = 0.95)
predict(model1, interval = "predict")

#Visualisation
library(ggplot2)
ggplot( data = Employee, aes( x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Employee, aes( x= YearsExperience, y=pred1))

############## Logarithmic Model [Y ~ log(X)] ##############

model2 = lm(Salary ~ log(YearsExperience))
summary(model2)

confint(model2, level = 0.95)
pred2 = predict(model2, interval = "predict")

sqrt(mean(model2$residuals^2)) #RMSE

############### Exponential Model [log(Y) ~ X] ############### 
model3 = lm(log(Salary) ~ YearsExperience)
summary(model3)

confint(model3, level = 0.95)
pred3 = exp(predict(model3, interval = "confidence"))

error = Salary - pred3
#error

sqrt(sum(error^2)/nrow(Employee))  #RMSE

############### Quadratic model ############### 

model4 = lm (log(Salary) ~ I((YearsExperience)^2) + YearsExperience)
summary(model4)

pred4 = exp(predict(model4))

error = Salary - pred4
#error

sqrt(sum(error^2)/nrow(Employee))  #RMSE

#Visualisation
ggplot( data = Employee, aes( x = I((YearsExperience)^2) + YearsExperience,
                              log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red', data = Employee, 
            aes(x= I((YearsExperience)^2) + YearsExperience, y= predict(model4)))
