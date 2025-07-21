# Q1
library(ISLR)
library(tree)
data("Carseats")
# 1.a
Carseats$HighSales <- ifelse(Carseats$Sales > 8, "Yes", "No")
Carseats$HighSales <- as.factor(Carseats$HighSales)
# 1.b
tree_model <- tree(HighSales ~ . - Sales, data = Carseats)
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0)
# 1.c
predicted <- predict(tree_model, Carseats, type = "class")
accuracy_full <- mean(predicted == Carseats$HighSales)
print(paste("Model accuracy on the full dataset:", accuracy_full))
# 1.d
set.seed(1)
train_index <- sample(1:nrow(Carseats), nrow(Carseats)/2)
train_data <- Carseats[train_index, ]
test_data <- Carseats[-train_index, ]
tree_carseats_train <- tree(HighSales ~ . - Sales, data=train_data)
summary(tree_carseats_train)
plot(tree_carseats_train)
text(tree_carseats_train, pretty=0)
# 1.e
predicted_test <- predict(tree_carseats_train, test_data, type = "class")
accuracy_test <- mean(predicted_test == test_data$HighSales)
print(paste("Model accuracy on the test dataset:", accuracy_test))
# 1.f
cv_tree <- cv.tree(tree_carseats_train, FUN = prune.misclass)
plot(cv_tree$size, cv_tree$dev, type="b", xlab="Tree Size", ylab="CV Misclassification Error")
# 1.g
best_size <- cv_tree$size[which.min(cv_tree$dev)]
print(paste("Best size of the tree:", best_size))
# 1.h
pruned_tree <- prune.misclass(tree_carseats_train, best = best_size)  # The best size is 13
summary(pruned_tree)
plot(pruned_tree)
text(pruned_tree, pretty = 0)
# 1.i
predicted_pruned <- predict(pruned_tree, test_data, type = "class")
accuracy_pruned <- mean(predicted_pruned == test_data$HighSales)
print(paste("Model accuracy of the pruned tree on the test dataset:", accuracy_pruned))
# 1.j
summary(pruned_tree)
# from left to right
# Terminal Node 1 (Leftmost):
# Prediction: "Yes"
# Condition Met: Price < 91.5
# Car seats with a price less than 91.5 are predicted to have high sales.
# Terminal Node 2:
# Prediction: "No"
# Condition Met: Price >= 91.5 and CompPrice < 121.5
# When the price is greater than or equal to 91.5 and the competitor's price is less than 121.5, the sales are predicted to be low.
# Terminal Node 3:
# Prediction: "No"
# Condition Met: Price >= 91.5, CompPrice >= 121.5, Price < 127, Advertising < 3.5, ShelveLoc: Bad
# For prices between 91.5 and 127, with low advertising expenditure and a bad shelf location, low sales are predicted.
# Terminal Node 4:
# Prediction: "Yes"
# Condition Met: Price >= 91.5, CompPrice >= 121.5, Price < 127, Advertising < 3.5, ShelveLoc != Bad
# With similar conditions but not a bad shelf location, high sales are predicted.
# Terminal Node 5:
# Prediction: "No"
# Condition Met: Price >= 91.5, CompPrice >= 121.5, Price < 127, Advertising >= 3.5
# For higher advertising expenditure, high sales are predicted.
# Terminal Node 6 :
# Prediction: "No"
# Condition Met: Price >= 91.5, CompPrice >= 121.5, Price >= 127, ShelveLoc: Bad or Medium
# For conditions where price even larger than 127, shelveloc is either bad nor medium, low sales are predicted.
# Terminal Node 7 :
# Prediction: "No"
# Condition Met: Price >= 91.5, CompPrice >= 121.5, Price >= 127, ShelveLoc != Bad or Medium, US: No
# For conditions where price even larger than 127, shelveloc is neither bad or medium , and not sold in US, low sales are predicted.
# Terminal Node 8 :
# Prediction: "Yes"
# Condition Met: Price >= 91.5, CompPrice >= 121.5, Price > 127, ShelveLoc != Bad or Medium, US: Yes
# For conditions where price even larger than 127, shelveloc is neither bad nor medium, and sold in US, high sales are predicted.


# Q2
# 2.a Use the same train set defined in Question 01.
# 2.b
reg_tree <- tree(Sales ~ ., data=train_data)
summary(reg_tree)
plot(reg_tree)
text(reg_tree, pretty=0)
# 2.c
cv_reg_tree <- cv.tree(reg_tree)
plot(cv_reg_tree$size, cv_reg_tree$dev, type="b", xlab="Tree Size", ylab="CV Error")
# 2.d
best_size_reg <- cv_reg_tree$size[which.min(cv_reg_tree$dev)]
print(paste("Best size of the regression tree:", best_size_reg))
# 2.e
pruned_reg_tree <- prune.tree(reg_tree, best=best_size_reg)
summary(pruned_reg_tree)
plot(pruned_reg_tree)
text(pruned_reg_tree, pretty=0)
# 2.f
predicted_reg <- predict(pruned_reg_tree, test_data)
mse <- mean((predicted_reg - test_data$Sales)^2)
print(paste("Mean Squared Error of the pruned regression tree:", mse))
# 2.g
summary(pruned_reg_tree)
# from left to right
# terminal node 1
# Split condition: low highsales
# Number of Observations: 5.652
# For conditions where low highsales, the number of sale observed is 5.652
# terminal node 2
# split condition: high highsales, shelveloc is neither bad or medium
# Number of Observations: 9.694
# For conditions where high highsales, shelve location is either bad or medium, the number of sale observed is 9.694
# terminal node 3
# split condition: high highsales, shelveloc != bad or medium
# Number of Observations: 11.300
# For conditions where high highsales, shelve location is neithor bad nor medium, the number of sale observed is 11.300


## Using advertising dataset
Advertising = read.csv("Advertising.csv")
set.seed(1)
train_index <- sample(1:nrow(Advertising), nrow(Advertising)/2)
train_ad_data <- Advertising[train_index, ]
test_ad_data <- Advertising[-train_index, ]
reg_ad_tree <- tree(Sales ~ ., data=train_ad_data)
summary(reg_ad_tree)
plot(reg_ad_tree)
text(reg_ad_tree, pretty=0)
cv_reg_ad_tree <- cv.tree(reg_ad_tree)
plot(cv_reg_ad_tree$size, cv_reg_ad_tree$dev, type="b", xlab="Tree Size", ylab="CV Error")
best_size_ad_reg <- cv_reg_ad_tree$size[which.min(cv_reg_ad_tree$dev)]
print(paste("Best size of the regression tree:", best_size_ad_reg))
pruned_reg_ad_tree <- prune.tree(reg_ad_tree, best=best_size_ad_reg)
summary(pruned_reg_ad_tree)
plot(pruned_reg_ad_tree)
text(pruned_reg_ad_tree, pretty=0)
predicted_reg_ad <- predict(pruned_reg_ad_tree, test_ad_data)
mse <- mean((predicted_reg_ad - test_ad_data$Sales)^2)
print(paste("Mean Squared Error of the pruned regression tree:", mse))
summary(pruned_reg_tree)
# from left to right
# terminal node 1
# Split condition: TV < 38.85
# Number of Observations: 7.156
# terminal node 2
# split condition: TV >= 38.85, Radio < 28.55
# Number of Observations: 10.620
# terminal node 3
# split condition: TV >= 38.85, Radio >= 28.55
# Number of Observations: 14.240
# terminal node 4
# split condition: TV >= 140.8, Radio < 10.05
# Number of Observations: 12.040
# terminal node 5
# split condition: TV >= 140.8, Radio >= 10.05, Radio < 26.85
# Number of Observations: 15.510
# terminal node 6
# split condition: TV >= 140.8, Radio >= 26.85, Radio < 35.95
# Number of Observations: 18.980
# terminal node 7
# split condition: TV >= 140.8, Radio >= 35.95
# Number of Observations: 23.520
