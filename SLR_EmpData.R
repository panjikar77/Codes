#Read and View Data
EmpData <- read.csv("D:/Data Science Course/Assignments/Simple linear regression/emp_data.csv")
View(EmpData)

summary(EmpData)
plot(EmpData)
cor(EmpData)

attach(EmpData)

############### Simle Linear Regression Model ###############
model1 = lm(Churn_out_rate ~ Salary_hike)
summary(model1)

pred1 = predict(model1)

sqrt(mean(model1$residuals^2)) #RMSE

confint(model1, level = 0.95)
predict(model1, interval = "predict")

#Visualisation
library(ggplot2)
ggplot(data = EmpData, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = EmpData, aes(x= Salary_hike, y=pred1))

################ Logarithmic Model [Y ~ log(X)] ###############

model2 = lm(Churn_out_rate ~ log(Salary_hike))
summary(model2)

confint(model2, level = 0.95)
pred2 = predict(model2, interval = "predict")

sqrt(mean(model2$residuals^2)) #RMSE

################ Exponential Model [log(Y) ~ X] ###############

model3 = lm(log(Churn_out_rate) ~ Salary_hike)
summary(model3)

confint(model3, level = 0.95)
pred3 = exp(predict(model3, interval = "confidence"))

error = Churn_out_rate - pred3
#error

sqrt(sum(error^2)/nrow(EmpData))  #RMSE

################ Quadratic model ###############

model4 = lm ( log(Churn_out_rate) ~ I((Salary_hike)^2) + Salary_hike)
summary(model4)

pred4 = exp(predict(model4))

error = Churn_out_rate - pred4
#error

sqrt(sum(error^2)/nrow(EmpData))  #RMSE

#Visualisation
ggplot( data = EmpData, aes( x = I((Salary_hike)^2) + Salary_hike,
                           y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red', data = EmpData, 
            aes(x= I((Salary_hike)^2) + Salary_hike, 
                y= predict(model4)))
