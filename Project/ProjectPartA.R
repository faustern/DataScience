# 1. Data preprocessing
# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(caret)

kidney_data = read.csv('KidneyData.csv') # Load the dataset
str(kidney_data) # Print the structure of the data before cleaning

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
# Identify continuous variables that need to be scaled
continuous_columns = c('Age', 'BloodPressure', 'BloodSugar', 'Cholesterol', 'BMI', 
                        'ElectricConductivity', 'pH', 'DissolvedOxygen', 'Turbidity',
                        'TotalDissolvedSolids', 'NitriteLevel', 'NitrateLevel', 
                        'LeadConcentration', 'ArsenicConcentration', 'Humidity')
kidney_data[continuous_columns] = scale(kidney_data[continuous_columns]) # Scale the continuous variables
str(kidney_data)

# 1.3 Data splitting
set.seed(123)
train_index = sample(1:nrow(kidney_data), 0.5 * nrow(kidney_data))
train_data = kidney_data[train_index, ]
test_data = kidney_data[-train_index, ]
str(train_data) # Display the structure of the training data after cleaning and pre-processing

# 2. Data exploration
# Correlation Matrix to identify relationships between features
correlation_matrix = cor(train_data %>% select_if(is.numeric))
corrplot(correlation_matrix, method = "color", tl.col = "black", tl.srt = 45, addCoef.col = "black", title = "Correlation Matrix of Training Data")

# 2.1: Histograms of Continuous Variables
train_data %>%
  dplyr::select(all_of(continuous_columns)) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  ggtitle("Histograms of Continuous Variables")

# 2.2: Boxplots to check for outliers and distribution
train_data %>%
  dplyr::select(continuous_columns) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(y = Value, x = Variable)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  ggtitle("Boxplot of Continuous Variables") +
  coord_flip()

# 2.3: Identify the most significant risk factors using correlation with target variable
# Calculate correlation with the target variable
correlation_with_target = cor(train_data %>% select_if(is.numeric), as.numeric(train_data$KidneyDisease))
print(correlation_with_target)


# 3. Model build
# 3.1 Build the logistic regression model
formula = KidneyDisease ~ .
model = glm(formula, family=binomial, data=train_data)
print(summary(model)) # Summary of the model

# 3.2 Model Evaluation on Test Data
predictions = predict(model, newdata=test_data, type="response") # Make predictions on the test set
predicted_classes = ifelse(predictions > 0.5, 1, 0) # Convert probabilities to binary outcomes (0 or 1)
confusion_matrix = table(Predicted = predicted_classes, Actual = test_data$KidneyDisease) # Confusion matrix to evaluate the model
print(confusion_matrix)
accuracy = mean(predicted_classes == test_data$KidneyDiseas)
cat("Model accuracy: ", accuracy, "\n")
print(coef(model))

# 3.3. Model improvement
# After first running, z-index < 0.05 or high coeffecient => significant factor contribute to model
# BloodPressure, ElectricConductivity, pH, DissolvedOxygen, Turbidity, TotalDissolvedSolids
# Build the logistic regression model
improved_formula = KidneyDisease ~ BloodPressure + ElectricConductivity + pH + DissolvedOxygen + Turbidity + TotalDissolvedSolids
improved_model = glm(improved_formula, family=binomial, data=train_data)
print(summary(improved_model))
summary(improved_model) # Summary of the model
improved_predictions = predict(improved_model, newdata=test_data, type="response") # Make predictions on the test set
improved_predicted_classes = ifelse(improved_predictions > 0.5, 1, 0) # Convert probabilities to binary outcomes (0 or 1)
improved_confusion_matrix = table(Predicted = improved_predicted_classes, Actual = test_data$KidneyDisease) # Confusion matrix to evaluate the model
print(improved_confusion_matrix)
improved_accuracy = mean(improved_predicted_classes == test_data$KidneyDiseas)
cat("Improved model accuracy: ", improved_accuracy, "\n")
print(coef(improved_model))
