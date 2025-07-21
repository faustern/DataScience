##Advertising dataset

#Load the dataset

Advertising = read.csv("Advertising.csv")

#explore the data

dim(Advertising)

names(Advertising)

sapply(Advertising, class)

str(Advertising)

# scatter

attach(Advertising)

plot(Sales~TV, xlab = "Expenditure on TV", ylab = "Sales", main = "Sales vs TV")
plot(Advertising$Sales~Advertising$TV)

#Sales vs Radio
plot(Sales~Radio, xlab = "Expenditure on Radio", ylab = "Sales", main = "Sales vs Radio")
plot(Advertising$Sales~Advertising$Radio)

#Sales vs Newspaper
plot(Sales~Newspaper, xlab = "Expenditure on Newspaper", ylab = "Sales", main = "Sales vs Newspaper")
plot(Advertising$Sales~Advertising$Newspaper)

#correlation coefficient
cor(Sales, TV)
cor(Sales, Radio)
cor(Sales, Newspaper)

#simple linear regression model
model1 = lm(Sales~TV, data = Advertising)
model1
model2 = lm(Sales~Radio, data = Advertising)
model2
model3 = lm(Sales~Newspaper, data = Advertising)
model3

#Assessing the accuracy of parameter estimates
summary(model1)
summary(model2)
summary(model3)

#confident interval
confint(model1)
confint(model2)
confint(model3)

plot(Sales~TV, xlab = "Expenditure on TV", ylab = "Sales", main = "Sales vs TV")
abline(a = 7.032594, b = 0.047537, col = "red")
plot(Sales~Radio, xlab = "Expenditure on Radio", ylab = "Sales", main = "Sales vs Radio")
abline(a = 9.3116, b = 0.2025, col = "red")
plot(Sales~Newspaper, xlab = "Expenditure on Newspaper", ylab = "Sales", main = "Sales vs Newspaper")
abline(a = 12.35141, b = 0.05469, col = "red")

#Anova Table
anova(model1)
anova(model2)
anova(model3)

#model diagnostic plots
par(mfrow = c(2,2))
plot(model1)
plot(model2)
plot(model3)

#prediction
predict(model1, list(TV = 100))
predict(model2, list(Radio = 100))
predict(model3, list(Newspaper = 100))


##Auto dataset

#Load the dataset
Auto = read.csv("Auto.csv")

#explore the data

dim(Auto)

names(Auto)

sapply(Auto, class)

str(Auto)

# scatter

attach(Auto)

#mpg and displacement

plot(mpg~displacement, xlab = "displacement", ylab = "mpg", main = "mpg and displacement")
plot(mpg~displacement)

#mpg and weight
plot(mpg~weight, xlab = "weight", ylab = "mpg", main = "mpg and weight")
plot(mpg~weight)

#mpg and acceleration
plot(mpg~acceleration, xlab = "acceleration", ylab = "mpg", main = "mpg and acceleration")
plot(mpg~acceleration)

#correlation coefficient
cor(mpg, displacement)
cor(mpg, weight)
cor(mpg, acceleration)

#simple linear regression model
model1 = lm(mpg~displacement, data = Auto)
model1
model2 = lm(mpg~weight, data = Auto)
model2
model3 = lm(mpg~acceleration, data = Auto)
model3

#Assessing the accuracy of parameter estimates
summary(model1)
summary(model2)
summary(model3)

#confident interval
confint(model1)
confint(model2)
confint(model3)

plot(mpg~displacement, xlab = "displacement", ylab = "mpg", main = "mpg and displacement")
abline(a = 35.188350, b = -0.060313, col = "red")
plot(mpg~weight, xlab = "weight", ylab = "mpg", main = "mpg and weight")
abline(a = 46.3173992, b = -0.0076766, col = "red")
plot(mpg~acceleration, xlab = "acceleration", ylab = "mpg", main = "mpg and acceleration")
abline(a = 4.8218, b = 1.2018, col = "red")

#Anova Table
anova(model1)
anova(model2)
anova(model3)

#model diagnostic plots
par(mfrow = c(2,2))
plot(model1)
plot(model2)
plot(model3)

#prediction
predict(model1, list(displacement = 100))
predict(model2, list(weight = 100))
predict(model3, list(acceleration = 100))

