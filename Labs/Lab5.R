# Q1
# 1.a
library(ISLR) # Load the necessary library
data("Auto") # Load the Auto dataset
head(Auto) # View the first few rows

# 1.b
set.seed(2)
train_index = sample(1:nrow(Auto), nrow(Auto) / 2)
train_data = Auto[train_index, ]
validation_data = Auto[-train_index, ]

# model 1
model_1 = lm(mpg ~ poly(horsepower, 1), data = train_data)
pred_1 = predict(model_1, validation_data)
error_1 = mean((validation_data$mpg - pred_1)^2)

# model 2
model_2 = lm(mpg ~ poly(horsepower, 2), data = train_data)
pred_2 = predict(model_2, validation_data)
error_2 = mean((validation_data$mpg - pred_2)^2)

# model 3
model_3 = lm(mpg ~ poly(horsepower, 3), data = train_data)
pred_3 = predict(model_3, validation_data)
error_3 = mean((validation_data$mpg - pred_3)^2)

# Display the errors
errors = data.frame(Model = c(1, 2, 3), Validation_Error = c(error_1, error_2, error_3))
print(errors) # => model 3 is the best

# 1.c (part b with different seed set)
seeds = c(5, 8)
results = list()

for (s in seeds) {
  set.seed(s)
  train_index = sample(1:nrow(Auto), nrow(Auto) / 2)
  train_data = Auto[train_index, ]
  validation_data = Auto[-train_index, ]
  
  # model 1
  model_1 = lm(mpg ~ poly(horsepower, 1), data = train_data)
  pred_1 = predict(model_1, validation_data)
  error_1 = mean((validation_data$mpg - pred_1)^2)
  
  # model 2
  model_2 = lm(mpg ~ poly(horsepower, 2), data = train_data)
  pred_2 = predict(model_2, validation_data)
  error_2 = mean((validation_data$mpg - pred_2)^2)
  
  # model 3
  model_3 = lm(mpg ~ poly(horsepower, 3), data = train_data)
  pred_3 = predict(model_3, validation_data)
  error_3 = mean((validation_data$mpg - pred_3)^2)
  
  results[[as.character(s)]] = data.frame(Model = c(1, 2, 3), Validation_Error = c(error_1, error_2, error_3))
}
print(results)

#1.d
# Disadvantage of validation set approach: model's performance is evaluated on 
# a single split of the data => high variance in the model evaluation results (especially
# the dataset is small)

# 1.e
cv_error = rep(0, 10)
for (i in 1:10) {
  glm_fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv_error[i] = cv.glm(Auto, glm_fit)$delta[1]
}
print(cv_error) # 
which.min(cv_error) # => model with polynomial 7

# 1.f
cv_error_10fold = rep(0, 10)
for (i in 1:10) {
  glm_fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv_error_10fold[i] = cv.glm(Auto, glm_fit, K = 10)$delta[1]
}
print(cv_error_10fold)
which.min(cv_error_10fold)  # => model with polynomial 8

# Q2
# 2.1
advertising_data = read.csv("Advertising.csv")

# Function to calculate cross-validation error for a given formula
cross_validation_error = function(formula, data, k = 10) {
  set.seed(1)
  folds = sample(1:k, nrow(data), replace = TRUE)
  cv_errors = rep(0, k)
  
  for (i in 1:k) {
    train = data[folds != i, ]
    test = data[folds == i, ]
    
    # Fit the model on the training data
    model = lm(formula, data = train)
    
    # Predict on the test data
    predictions = predict(model, newdata = test)
    
    # Calculate mean squared error (MSE)
    mse = mean((test$Sales - predictions) ^ 2)
    cv_errors[i] = mse
  }
  
  # Return average cross-validation error
  return(mean(cv_errors))
}
predictor_combinations = list(
  c("TV"),
  c("Radio"),
  c("Newspaper"),
  c("TV", "Radio"),
  c("TV", "Newspaper"),
  c("Radio", "Newspaper"),
  c("TV", "Radio", "Newspaper")
)

# Initialize variables to store results
results = data.frame(Model = character(), CVError = numeric(), stringsAsFactors = FALSE)

# Loop through each combination of predictors
for (combination in predictor_combinations) {
  # Create the model formula
  predictors = paste(combination, collapse = " + ")
  formula = paste("Sales ~", predictors)
  # Calculate cross-validation error
  cv_error = cross_validation_error(as.formula(formula), advertising_data, k = 10)
  results = rbind(results, data.frame(Model = formula, CVError = cv_error, stringsAsFactors = FALSE))
}

print(results)
print(results[which.min(results$CVError), ]) # best model


# Q3
# 3.1
set.seed(1)
x = 10 * rexp(20)
print(x)
# 3.2
mean_x = mean(x)
print(mean_x)
# 3.3
bootstrap_means = replicate(1000, mean(sample(x, replace = TRUE)))
print(bootstrap_means)
# 3.4
hist(bootstrap_means, main = "Bootstrap Means", xlab = "Means", ylab = "Frequency", col = "lightblue", border = "black")
