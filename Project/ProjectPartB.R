library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(caret)
library(boot)
library(tree)
library(factoextra)
kidney_data = read.csv('KidneyData.csv') # Load the dataset
str(kidney_data) # Print the structure of the data before cleaning
# Preprocessing data step
# 1.1 Data cleaning
summary(kidney_data)
sum(is.na(kidney_data)) # Check for missing values => no missing values
# Remove the 'PatientID' column as it is an identifier and not useful for prediction
kidney_data = kidney_data %>% dplyr::select(-PatientID)
# Convert categorical variables to factors
kidney_data$Gender = as.factor(kidney_data$Gender)
kidney_data$SmokingStatus = as.factor(kidney_data$SmokingStatus)
str(kidney_data)
# 1.2: Data Normalization/Scaling
# Identify continuous variables that need to be scaleds
summary(kidney_data)
continuous_columns = c('Age', 'BloodPressure', 'BloodSugar', 'Cholesterol', 'BMI',
                       'ElectricConductivity', 'pH', 'DissolvedOxygen', 'Turbidity',
                       'TotalDissolvedSolids', 'NitriteLevel', 'NitrateLevel',
                       'LeadConcentration', 'ArsenicConcentration', 'Humidity')
kidney_data[continuous_columns] = scale(kidney_data[continuous_columns]) # Scale the continuous variables
str(kidney_data)
summary(kidney_data)
# 1.3 Data splitting
set.seed(123)
train_index = sample(1:nrow(kidney_data), 0.5 * nrow(kidney_data))
train_data = kidney_data[train_index, ]
test_data = kidney_data[-train_index, ]
str(train_data) # Display the structure of the training data after cleaning and pre-processing
# B.1. Build a logistic regression model incorporating polynomial terms
# After first building, significant factor contribute to model
#v
# Build the logistic regression model with polynomial with support from cross validation
# Define cross-validation settings
selected_data <- train_data[, c("BloodPressure", "ElectricConductivity", "DissolvedOxygen",
                                "Turbidity", "TotalDissolvedSolids", "KidneyDisease")]
# Ensure the target variable is a factor
selected_data$KidneyDisease <- factor(selected_data$KidneyDisease, levels = c(0, 1))
head(selected_data)
# Initialize a vector to store cross-validation error
cv_error <- rep(0, 10)
# Loop over polynomial degrees from 1 to 10
for (i in 1:10) {
  # Fit a logistic regression model with polynomial terms of degree i
  glm.fit <- glm(KidneyDisease ~ poly(BloodPressure, i) + poly(ElectricConductivity, i) +
                   poly(DissolvedOxygen, i) + poly(Turbidity, i) + poly(TotalDissolvedSolids, i),
                 data = train_data, family = binomial)
  # Perform cross-validation on the training data (10-fold CV)
  cv_error[i] <- cv.glm(train_data, glm.fit, K = 10)$delta[1]
}
print(data.frame(degree = 1:10, cv_error = cv_error))
# Plot the training and test errors for comparison
cv_results <- data.frame(degree = 1:10, cv_error = cv_error)
ggplot(cv_results, aes(x = degree)) +
  geom_line(aes(y = cv_error, color = "CV Error")) +
  geom_point(aes(y = cv_error, color = "CV Error")) +
  labs(title = "Cross-Validation for Different Polynomial Degrees",
       x = "Polynomial Degree",
       y = "Error") +
  theme_minimal() +
  scale_color_manual("", values = c("CV Error" = "red"))
# Find the degree with the least error for both training and test sets
best_degree <- which.min(cv_error)
best_degree
# Fit the final best model on the training data using the best degree
best_model <- glm(KidneyDisease ~ poly(BloodPressure, best_degree) +
                    poly(ElectricConductivity, best_degree) +
                    poly(DissolvedOxygen, best_degree) +
                    poly(Turbidity, best_degree) +
                    poly(TotalDissolvedSolids, best_degree),
                  data = train_data, family = binomial)
summary(best_model)
# Return the best model and the degree
cat("The best polynomial degree is:", best_degree, "\n")
summary(best_model)
predictions = predict(best_model, newdata=test_data, type="response") # Make predictions on
the test set
predicted_classes = ifelse(predictions > 0.5, 1, 0) # Convert probabilities to binary outcomes (0
or 1)
confusion_matrix = table(Predicted = predicted_classes, Actual = test_data$KidneyDisease) #
Confusion matrix to evaluate the model
print(confusion_matrix)
accuracy = mean(predicted_classes == test_data$KidneyDisease)
cat("Model accuracy: ", accuracy, "\n")
print(coef(best_model))
# B.3
# Use decision tree model to answer the research question.
#Clearly outline and explain each step of the process involved
tree_model <- tree(KidneyDisease ~ BloodPressure + ElectricConductivity
                   + DissolvedOxygen + Turbidity + TotalDissolvedSolids,
                   data = selected_data)
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0)
predictions <- predict(tree_model, newdata = test_data, type = "class") # Type class returns
class predictions (0 or 1)
confusion_matrix <- table(Predicted = predictions, Actual = test_data$KidneyDisease)
print(confusion_matrix)
# Calculate accuracy
accuracy <- mean(predictions == test_data$KidneyDisease)
cat("Model accuracy: ", accuracy, "\n")
# Perform cross-validation to find the best number of terminal nodes
cv_tree <- cv.tree(tree_model, FUN = prune.misclass) # Use misclassification as the criterion
# Plot the cross-validation results
plot(cv_tree$size, cv_tree$dev, type = "b",
     xlab = "Number of Terminal Nodes",
     ylab = "Deviance",
     main = "Cross-Validation for Tree Pruning")
# Find the best size (minimum deviance)
optimal_size <- which.min(cv_tree$dev)
optimal_size
# Prune the tree to the optimal size
pruned_tree <- prune.misclass(tree_model, best = cv_tree$size[optimal_size])
# Plot the pruned tree
plot(pruned_tree)
text(pruned_tree, pretty = 0)
# Print a summary of the pruned tree
summary(pruned_tree)
pruned_tree
predictions <- predict(pruned_tree, newdata = test_data, type = "class") # Type class returns
class predictions (0 or 1)
confusion_matrix <- table(Predicted = predictions, Actual = test_data$KidneyDisease)
print(confusion_matrix)
# Calculate accuracy
accuracy <- mean(predictions == test_data$KidneyDisease)
cat("Model accuracy: ", accuracy, "\n")
# Plot the pruned tree (optional)
plot(pruned_tree)
text(pruned_tree, pretty = 0)
selected_data <- kidney_data[, c("BloodPressure", "ElectricConductivity", "DissolvedOxygen",
                                 "Turbidity", "TotalDissolvedSolids")]
summary(selected_data)
# B.5 + 6
### LOGISTIC REGRESSION MODEL WITH DEGREE 1 ###
# Fit logistic regression model with degree 1 (linear)
logistic_model_deg1 <- glm(KidneyDisease ~ BloodPressure + ElectricConductivity +
                             DissolvedOxygen + Turbidity + TotalDissolvedSolids,
                           data = train_data, family = binomial)
# Make predictions on the test set
logistic_prob_deg1 <- predict(logistic_model_deg1, newdata = test_data, type = "response")
logistic_pred_deg1 <- ifelse(logistic_prob_deg1 > 0.5, 1, 0)
summary(logistic_model_deg1)
# Evaluate accuracy
accuracy_deg1 <- mean(logistic_pred_deg1 == test_data$KidneyDisease)
cat("Logistic Regression (Degree 1) Accuracy: ", accuracy_deg1, "\n")
### LOGISTIC REGRESSION MODEL WITH DEGREE 3 ###
# Fit logistic regression model with degree 3 (polynomial)
logistic_model_deg3 <- glm(KidneyDisease ~ poly(BloodPressure, 3) +
                             poly(ElectricConductivity, 3) +
                             poly(DissolvedOxygen, 3) +
                             poly(Turbidity, 3) +
                             poly(TotalDissolvedSolids, 3),
                           data = train_data, family = binomial)
summary(logistic_model_deg3)
# Make predictions on the test set
logistic_prob_deg3 <- predict(logistic_model_deg3, newdata = test_data, type = "response")
logistic_pred_deg3 <- ifelse(logistic_prob_deg3 > 0.5, 1, 0)
# Evaluate accuracy
accuracy_deg3 <- mean(logistic_pred_deg3 == test_data$KidneyDisease)
cat("Logistic Regression (Degree 3) Accuracy: ", accuracy_deg3, "\n")
### DECISION TREE MODEL ###
# Fit the decision tree model
tree_model <- tree(KidneyDisease ~ BloodPressure + ElectricConductivity + DissolvedOxygen
                   + Turbidity + TotalDissolvedSolids,
                   data = train_data)
# Prune the tree based on cross-validation
cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
optimal_size <- which.min(cv_tree$dev)
pruned_tree <- prune.misclass(tree_model, best = cv_tree$size[optimal_size])
# Make predictions on the test set
tree_pred <- predict(pruned_tree, newdata = test_data, type = "class")
# Evaluate accuracy
accuracy_tree <- mean(tree_pred == test_data$KidneyDisease)
cat("Decision Tree Accuracy: ", accuracy_tree, "\n")
### EVALUATING USING ROC CURVES AND AUC ###
# Logistic Regression Degree 1 ROC and AUC
logistic_roc_deg1 <- roc(test_data$KidneyDisease, logistic_prob_deg1)
logistic_auc_deg1 <- auc(logistic_roc_deg1)
cat("Logistic Regression (Degree 1) AUC: ", logistic_auc_deg1, "\n")
# Logistic Regression Degree 3 ROC and AUC
logistic_roc_deg3 <- roc(test_data$KidneyDisease, logistic_prob_deg3)
logistic_auc_deg3 <- auc(logistic_roc_deg3)
cat("Logistic Regression (Degree 3) AUC: ", logistic_auc_deg3, "\n")
# Decision Tree ROC and AUC
tree_prob <- predict(pruned_tree, newdata = test_data, type = "vector")[, 2] # Probability for
class 1
tree_roc <- roc(test_data$KidneyDisease, tree_prob)
tree_auc <- auc(tree_roc)
cat("Decision Tree AUC: ", tree_auc, "\n")
# Plot ROC curves for comparison
plot(logistic_roc_deg1, col = "blue", main = "ROC Curve Comparison")
plot(logistic_roc_deg3, col = "green", add = TRUE)
plot(tree_roc, col = "red", add = TRUE)
legend("bottomright", legend = c("Logistic Regression (Degree 1)", "Logistic Regression
(Degree 3)", "Decision Tree"),
       col = c("blue", "green", "red"), lwd = 2)
### SUMMARY OF RESULTS ###
cat("\nModel Comparison Summary:\n")
cat("Logistic Regression (Degree 1) Accuracy: ", accuracy_deg1, " | AUC: ", logistic_auc_deg1,
    "\n")
cat("Logistic Regression (Degree 3) Accuracy: ", accuracy_deg3, " | AUC: ", logistic_auc_deg3,
    "\n")
cat("Decision Tree Accuracy: ", accuracy_tree, " | AUC: ", tree_auc, "\n")
# B.7
pca_result <- prcomp(selected_data, center = TRUE, scale. = TRUE)
summary(pca_result)
# Plot the variance explained by each principal component (scree plot)
plot(pca_result$sdev^2 / sum(pca_result$sdev^2), type = "b",
     xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     main = "Scree Plot")
cat("PCA Loadings (contributions of original features to principal components):\n")
print(pca_result$rotation)
biplot(pca_result, scale = 0, main = "Biplot of Principal Components and Original Features")
# Use all the principal components for K-means clustering
pca_data <- pca_result$x # Extract the principal components from PCA
# Determine the optimal number of clusters (K) using the Elbow Method
### Apply K-means clustering to the first two principal components ###
set.seed(123) # For reproducibility
kmeans_model <- kmeans(pca_data, centers = 3, nstart = 25)
# Plot the PCA results with clusters
plot(pca_data, col = kmeans_model$cluster, pch = 19, xlab = "PC1", ylab = "PC2",
     main = "K-means Clustering on PCA-reduced Data")
# Add the cluster centers to the plot
points(kmeans_model$centers, col = 1:3, pch = 8, cex = 2)

