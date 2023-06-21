rm(list=ls())
library(rio)
library(car)
library(corrplot)
library(dplyr)


set.seed(123)

data = import("credit.csv")
colnames(data)=tolower(make.names(colnames(data)))
head(data)
data = subset(data, select=-c(ethnicity,gender,student,education,cards,rating,age))

data$married [data$married == "Yes"] = 1
data$married [data$married == "No"] = 0
data$married <- as.integer(data$married)

data %>% count(married)


head(data)


cor(data)



# Sampling
sample_data = data[sample(1:nrow(data),100),]
sample_data %>% count(married)
sample_data

attach(sample_data)

plot(sample_data)


# Probably gonna remove this, coz the correlation isn't good

cont=subset(sample_data, select=c("income","limit","balance"))

corelated=round(cor(cont),3)

round(cor(corelated),3)

corrplot(corelated, method="ellipse")

# Simple regression

regout1=lm(limit~income,data = sample_data)
summary(regout1)
coefficients(regout1)


regout2=lm(limit~married,data = sample_data)
summary(regout2)
coefficients(regout2)


regout3=lm(limit~balance,data = sample_data)
summary(regout3)
coefficients(regout3)


# Multiple regression with 2 independent variables

regout4=lm(limit~income+married,data = sample_data)
summary(regout4)
coefficients(regout4)


regout5=lm(limit~balance+married,data = sample_data)
summary(regout5)
coefficients(regout5)


regout6=lm(limit~balance+income,data = sample_data)
summary(regout6)
coefficients(regout6)

# Main Multiple Regression with 3 independent variables

regout7 = lm(limit ~ income + married + balance, data=sample_data)
summary(regout7)
coefficients(regout7)

# X1 * X2

sample_data$incbal <- sample_data$income*sample_data$balance

regout8 = lm(limit ~ income + balance + incbal, data=sample_data)
summary(regout8)
coefficients(regout8)

#

sample_data$income2 <- sample_data$income*sample_data$income
sample_data$balance2 <- sample_data$balance*sample_data$balance


regout9=lm(limit~income+income2,data = sample_data)
summary(regout9)
coefficients(regout9)

regout10=lm(limit~balance+balance2,data = sample_data)
summary(regout10)
coefficients(regout10)

#

plot(sample_data$balance, regout7$fitted.values, pch=19, 
     main = " Actual vs Fitted Values",xlab = 'Balance', ylab = 'Fitted Values')
abline(0,10,col='red',pch=20)

# Normality

qqnorm(regout7$residuals,pch=19,main="Normality Plot of the Residuals")
qqline(regout7$residuals,col='red',lwd=3)

hist(regout7$residuals, col='red', probability = TRUE,main = 'Histogram to determine Normality',xlab = 'Residuals')
```




#Equality of Variance
plot(regout7$fitted.values,regout7$residuals,pch=19,
     main="Residuals",xlab = 'Fitted Values', ylab = 'Residuals')
abline(0,0,col="red",lwd=3)
