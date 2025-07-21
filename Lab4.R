# 1.a
library(ISLR)
data("Default")
str(Default)
head(Default)
# 1.b
table(Default$default)
plot(Default$balance, Default$default)
# 1.c
model1 = glm(default ~ balance, data = Default, family = "binomial")
summary(model1)
# 1.d
new_data = data.frame(balance = 500)
predict(model1, newdata = new_data, type = "response")
# 1.e
model2 = glm(default ~ balance + income + student, data = Default, family = "binomial")
summary(model2)

# 2.a
Heart = read.csv('heart.csv')
str(Heart)
head(Heart)
# 2.b
model3 = glm(AHD ~ Sex + ChestPain + Ca, data = Heart, family = "binomial")
summary(model3)

# 3.a
data("Smarket")
str(Smarket)
head(Smarket)
# 3.b
model4 = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = "binomial")
summary(model4)
# 3.c
new_data = data.frame(Lag1 = value1, Lag2 = value2, Lag3 = value3, Lag4 = value4, Lag5 = value5, Volume = value)
predict(model4, newdata = new_data, type = "response")
# 3.d
predictions = predict(model4, type = "response")
pred_direction = ifelse(predictions > 0.5, "Up", "Down")
table(Predicted = pred_direction, Actual = Smarket$Direction)
# 3.e
mean(pred_direction != Smarket$Direction)
# 3.f
fp_rate = sum(pred_direction == "Up" & Smarket$Direction == "Down") / sum(Smarket$Direction == "Down")
fp_rate
# 3.g
fn_rate = sum(pred_direction == "Down" & Smarket$Direction == "Up") / sum(Smarket$Direction == "Up")
fn_rate

