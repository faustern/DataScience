
Advertising = read.csv("Advertising.csv")
head(Advertising)      
summary(Advertising)

# matrix scatter plot
pairs(Advertising, panel = panel.smooth)

#covariance matrix
cov(Advertising)

#correlation matrix
cor(Advertising)

# MLR
model1 = lm(Sales~TV + Radio + Newspaper, data = Advertising)
model1 = lm(Sales~., data = Advertising)
model1

summary(model1)

model2 = lm(Sales~TV + Radio, data = Advertising)
model2 = lm(Sales~., data = Advertising)
model2
summary(model2)

#R^2 = 89.2 %
#RSE = 1.681

anova(model2)

model_summary <- summary(model1)

#predicted values
model1$fitted.values
model2$fitted.values

#residuals
model2$residuals
resid(model2)

hist(residuals)

#plot resid vs. fitted plot
plot(model2$fitted, model2$residuals, xlab = "Fitted values", ylab = "Residuals")

#model diagnostic plots
par(mfrow = c(2,2))
plot(model2)

par(mfrow = c(1,1))
hist(model2$residuals)

model3 = lm(Sales~TV + Radio + TV*Radio, data = Advertising)
model3

#polynomial regression
model4 = lm(Sales~TV + I(TV*TV) + I(TV*TV*TV), data = Advertising)
model4

summary(model4)

new_data <- data.frame(TV = c(100, 200), Radio = c(20, 30), Newspaper = c(10, 20))
predicted_sales <- predict(model, new_data)
print(predicted_sales)

#Question 3

library(ISLR)

attach(Auto)
  
str(Auto)
?Auto

AutoNew = Auto[,-9]
AutoNew = AutoNew[,-8]

# MLR

model2.1 = lm(mpg~., data = AutoNew)
summary(model2.1)

pairs(AutoNew, panel = panel.smooth)

# 2.a
model_with_interaction <- lm(Sales ~ TV * Radio + Newspaper, data = advertising)
summary(model_with_interaction)

# 2.c
model_poly <- lm(Sales ~ poly(TV, 3) + poly(Radio, 3) + poly(Newspaper, 3), data = advertising)
summary(model_poly)

# 3.a
# import the csv file
Auto = read.csv("Auto.csv")
head(Auto)      
summary(Auto)

new_data <- data.frame(cylinders = c(100, 200), displacement = c(20, 30), horsepower = c(10, 20))
predicted_sales <- predict(model, new_data)
print(predicted_sales)

