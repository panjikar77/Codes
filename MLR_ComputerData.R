library(readr)
comp_data <- read.csv("D:/Data Science Course/Assignments/Multi linear regression/Computer_Data.csv")

View(comp_data)
# Removing unwanted colum no
comp_data <- comp_data[-1]


attach(comp_data)
summary(comp_data)

# Some variables are character, hence converted to dummy variable.
library(plyr)
comp_data$cd <- as.factor(revalue(comp_data$cd, c("yes" = 1, "no" = 0)))
comp_data$multi <- as.factor(revalue(comp_data$multi,c("yes" = 1, "no" = 0)))
comp_data$premium<- as.factor(revalue(comp_data$premium,c("yes" = 1, "no" = 0)))
View(comp_data)


###### Exploratory Data Analysis #######

#price
hist(price)
##skewness and kurtosis
library(moments)
skewness(price)
kurtosis(price)
boxplot(price)
# The data is posirively skewed and lepto kurtic or having higher peak than the normal
# There are some outliers

#speed
hist(speed)
skewness(speed)
kurtosis(speed)
boxplot(speed)
# Data is positively skewed and havimg lesser peak than the normal data

#hd
hist(hd)
skewness(hd)
kurtosis(hd)
boxplot(hd)
# Data is positively skewed and high peak compared to the normal data
# There are some outliers in the data

#ram
hist(ram)
skewness(ram)
kurtosis(ram)
boxplot(ram)
# Here the data is positively skewed and having higher peak there are some outliers which shows 
# There are some computers with having high ram like 16 GB, 24 GB and 32 GB
# Most common RAM capacity is 8 GB which is represented by median

#checking normality
qqnorm(price)
qqline(price)
# Data is normally distributed.

qqnorm(speed)
qqline(speed)
# Data is not normally distributed.

qqnorm(hd)
qqline(hd)
# Data is not normally distributed.

qqnorm(ram)
qqline(ram)
# Data is not normally distributed. .

qqnorm(screen)
qqline(screen)
# Data is not normally distributed.

# Since many data are rigid type, we use normalization method.

norm <- function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

normdata <- as.data.frame(lapply(comp_data[0:5],norm))
View(normdata)
data <- cbind(cd,multi,premium,ads,trend,normdata)
data <- data.frame(data)

library(plyr)
data$cd <- as.factor(revalue(comp_data$cd, c("yes" = 1, "no" = 0)))
data$multi <- as.factor(revalue(comp_data$multi,c("yes" = 1, "no" = 0)))
data$premium<- as.factor(revalue(comp_data$premium,c("yes" = 1, "no" = 0)))

View(data)
attach(data)
summary(data)
pairs(data)

# Corelation
cor(normdata)
# 0.62 correlation between RAM and price 

# Partial Corelation.
library(corpcor)
cor2pcor(cor(normdata))
# Here we can see that the price is depending mainly on the RAM size 

# HD and RAM multicolinerity.
##### Multinomial Linear Model #####

model <- lm(price~.,data=data)
summary(model)
# All the variables have significant pvalues ,error values are also less and 
# Multiple R-squared:  0.7756 ,goodmodel.

# Multicollinerity
vif(model)
# All value less than 10 ,no multicollinerity.

# Checking influence by vif
influence.measures(model)

## Plotting Influential measures 
library(car)
windows()
influenceIndexPlot(model,id.n=3) # Index plots for infuence measures
influencePlot(model,id.n=3) # A user friendly representation of the above

# Data points 1441 & 1701 have high influence on model
# Rebuilding model after omitting these values.
modelfinal <- lm (price~.,data = data[-c(1441,1701),])
summary(modelfinal)
# Multiple R-squared:  0.7777 value incresed , std error is less & p values are signficant.

pred <- predict(modelfinal)
pred
modelfinal$residuals  #error
error <- sum(modelfinal$residuals)
error   # total error
rmse <- sqrt((sum(error)^2)/nrow(comp_data))
rmse
# Confidence Interval
confint(modelfinal,level = .95)
predict(modelfinal,interval ="predict")

# Confident intervel are less and RMSE are less the model will be.

# Price= 3.315e-01+3.271e-01(ram)+1.359e-02(cd)+2.354e-02(multi)+1.146e-01(premium)+ 1.463e-04(ads)+-1.161e-02(trend)+
# 1.567e-01(speed)+ 3.517e-01(hd)+8.164e-02(screen)



# Evaluate model LINE assumptions 
plot(modelfinal)
# LINE assumptions has no viaolation.










