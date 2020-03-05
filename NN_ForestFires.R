library(neuralnet)  # regression
library(nnet) # classification 
library(NeuralNetTools)
library(plyr)
library(kernlab)
library(caret)
forestfires <- read.csv("D:/Data Science Course/Assignments/Neural Network/forestfires.csv")
a= NULL
b=NULL
#ff <- data.frame(a = 12:15,b = 16:19,r=20:23,d=24:27,e=27:30)
#forestfires <- subset(forestfires, select = -c(a,b))
str(forestfires)
View(forestfires)
forestfires$size_category <- as.numeric(revalue(forestfires$size_category,
                                     c("small"="0", "large"="1")))

forestfires$month <- as.numeric(revalue(forestfires$month,c('jan'='0','feb'='1','mar'='2','apr'='3','may'='4','jun'='5','jul'='6','aug'='7','sep'='8','oct'='9','nov'='10','dec'='11')))
forestfires$day <- as.numeric(revalue(forestfires$day,c('mon'='0','tue'='1','wed'='2','thu'='3','fri'='4','sat'='5','sun'='6')))
forestfires <- as.data.frame(forestfires)
attach(forestfires)
windows()
pairs(forestfires[-c(10:30)])
cor(forestfires[-c(10:30)])
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfires_norm<-as.data.frame(lapply(forestfires,FUN=normalize))
summary(forestfires_norm)
summary(forestfires$area)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(forestfires_norm), replace = TRUE, prob = c(0.7,0.3))
forestfires_train <- forestfires_norm[ind==1,]
forestfires_test  <- forestfires_norm[ind==2,]
# Creating a neural network model on training data

forestfires_model <- neuralnet(formula = area ~ temp+DMC+wind
                            +ISI+RH+DC,data = forestfires_train)
str(forestfires_model)
plot(forestfires_model, rep = "best")
summary(forestfires_model)

set.seed(12323)
model_results <- compute(forestfires_model,forestfires_test[1:12])
predicted_area <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_area,forestfires_test$area)
str_max <- max(forestfires$area)
str_min <- min(forestfires$area)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualarea_pred <- unnormalize(predicted_area,str_min,str_max)
head(Actualarea_pred)

set.seed(12345)
forestfires_model2 <- neuralnet(area~temp+DMC+wind
                                +ISI+RH+DC,data = forestfires_train,
                             hidden = 2)
plot(forestfires_model2 ,rep = "best")
summary(forestfires_model2)
model_results2<-compute(forestfires_model2,forestfires_test[1:12])
predicted_area2<-model_results2$net.result
cor(predicted_area2,forestfires_test$area)
plot(predicted_area2,forestfires_test$area)
par(mar = numeric(4), family = 'serif')
plotnet(forestfires_model2, alpha = 0.6)


####################################################
