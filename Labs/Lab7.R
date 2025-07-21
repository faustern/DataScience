library(ISLR)
library(e1071)


# 1.a
data(Auto)
head(Auto)
median_mpg = median(Auto$mpg)
Auto$high_mileage = ifelse(Auto$mpg > median_mpg, 1, 0) # Create a new column 'high_mileage' where 1 = above median, 0 = below median
head(Auto)

# 1.b
# Define the predictor variables and the target variable
x = Auto[, -c(1,9)] # Exclude the 'mpg' and 'name' columns
y = as.factor(Auto$high_mileage)
set.seed(123)
cost_values = c(0.01, 0.1, 1, 10, 100)
cv_errors = sapply(cost_values, function(cost) {
  svm_model = svm(x, y, kernel = "linear", cost = cost, cross = 10)
  mean(svm_model$accuracies) # Calculate the cross-validation error
})
print(data.frame(Cost = cost_values, CV_Error = cv_errors))
# Comment: error of all cost value is the same and equal to 100 which means:
# choosing different cost does not affect the model


# 1.c
# Radial Kernel with different gamma and cost values
gamma_values = c(0.01, 0.1, 1)
cost_values = c(0.01, 0.1, 1)
radial_cv_errors = expand.grid(gamma = gamma_values, cost = cost_values)
radial_cv_errors$cv_error = NA
for (i in 1:nrow(radial_cv_errors)) {
  svm_model = svm(x, y, kernel = "radial", cost = radial_cv_errors$cost[i], gamma = radial_cv_errors$gamma[i], cross = 10)
  radial_cv_errors$cv_error[i] = mean(svm_model$accuracies)
}

# Polynomial Kernel with different degree and cost values
degree_values = c(2, 3, 4)
poly_cv_errors = expand.grid(degree = degree_values, cost = cost_values)
poly_cv_errors$cv_error = NA
for (i in 1:nrow(poly_cv_errors)) {
  svm_model = svm(x, y, kernel = "polynomial", cost = poly_cv_errors$cost[i], degree = poly_cv_errors$degree[i], cross = 10)
  poly_cv_errors$cv_error[i] = mean(svm_model$accuracies)
}


print(radial_cv_errors)
# with using radial kernel best model with smallest cv_error 43.62 when gamma 0.01 and cost 0.01
print(poly_cv_errors)
# with using poly kernel best model with smallest cv_error 49.47 when degree 2 and cost 0.01
# => best model is radial kernel with gamma 0.01 and cost 0.01