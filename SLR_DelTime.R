#Read and View Data
DelTime <- read.csv("D:/Data Science Course/Assignments/Simple linear regression/delivery_time.csv")
View(DelTime)

summary(DelTime)
plot(DelTime)
cor(DelTime)

attach(DelTime)

############### Simle Linear Regression Model ###############

model1 = lm(Delivery.Time ~ Sorting.Time)
summary(model1)

pred1 = predict(model1)

sqrt(mean(model1$residuals^2)) #RMSE

confint(model1, level = 0.95)
predict(model1, interval = "predict")

#Visualisation
library(ggplot2)
ggplot(data = DelTime, aes(x = Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red', data = DelTime, aes(x= Sorting.Time, y=pred1))

################ Logarithmic Model [Y ~ log(X)] ################

model2 = lm(Delivery.Time ~ log(Sorting.Time))
summary(model2)

confint(model2, level = 0.95)
pred2 = predict(model2, interval = "predict")

sqrt(mean(model2$residuals^2)) #RMSE

################ Exponential Model [log(Y) ~ X] ################

model3 = lm(log(Delivery.Time) ~ Sorting.Time)
summary(model3)

confint(model3, level = 0.95)
pred3 = exp(predict(model3, interval = "confidence"))

error = Delivery.Time - pred3
#error

sqrt(sum(error^2)/nrow(DelTime))  #RMSE

################ Quadratic model ################

model4 = lm (log(Delivery.Time) ~ I((Sorting.Time)^2) + Sorting.Time)
summary(model4)

pred4 = exp(predict(model4))

error = DelTime$Delivery.Time - pred4
#error

sqrt(sum(error^2)/nrow(DelTime))  #RMSE

#Visualisation
ggplot(data = DelTime, aes( x = I((Sorting.Time)^2) + Sorting.Time,
                            y = log(Delivery.Time))) + 
  geom_point(color='blue') +
  geom_line(color='red', data = DelTime, 
            aes( x= I((Sorting.Time)^2) + Sorting.Time, y= predict(model4)))

