#import the data set

data <- read.csv(file.choose())

#remove the unwanted coloumn 

data$clarity <- NULL
names(data)
str(data)

# convert int variable into numeric 

data$price <- as.numeric(data$price)
str(data)

#to check outlier

boxplot(data$carat)
boxplot(data$price)

#treatment of outlier for carat 
summary(data$carat)
upper <- 1.0400 + 1.5 * IQR(data$carat)
upper

data$carat[data$carat > upper] <- upper
boxplot(data$carat)

# treatment of outlier for price 
summary(data$price)
upper <- 5324 + 1.5 * IQR(data$price)
upper

data$price[data$price > upper] <- upper
boxplot(data$price)

#data partition 

library(caret)
Train <- createDataPartition(data$price, p=0.70, list = FALSE)
training <- data[Train, ]
testing <- data[- Train,]

#model building 
cor(training)
model1 <- lm(price~carat , data = training)
summary(model1)
library(car)
vif(model1)
par(mfrow=c(2,2))
plot(model1)

#transformation 

hist(training$price)
hist(1/training$price)
hist(log(training$price))

#second model 

model2 <- step(lm(log(price)~carat, data = training), direction = "backward")
summary(model2)

#assumption

par(mfrow=c(2,2))
plot(model2)

library(lmtest)
dwtest(model2)
library(car)
ncvTest(model2)

#prediction

testing$fitted <- predict(model2,testing)
testing$orignal <- exp(testing$fitted)
