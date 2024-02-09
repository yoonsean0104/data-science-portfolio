# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(readr)

# Load the dataset
nfl_data <- read_csv("path_to_nfl_data.csv")

# Data preprocessing
# Assuming 'Player', 'Touchdowns', and other relevant features are columns in your dataset
nfl_data <- nfl_data %>%
  select(Player, Touchdowns, other_relevant_features) %>%
  filter(!is.na(Touchdowns))

# Split the data into training and testing sets
set.seed(123)
trainingIndex <- createDataPartition(nfl_data$Touchdowns, p = .8, 
                                     list = FALSE, 
                                     times = 1)
nfl_train <- nfl_data[trainingIndex,]
nfl_test <- nfl_data[-trainingIndex,]

# Model fitting using Random Forest
set.seed(123)
touchdown_model <- randomForest(Touchdowns ~ ., data = nfl_train, ntree = 100)

# Model prediction
predictions <- predict(touchdown_model, nfl_test)

# Evaluate the model
actual_vs_predicted <- data.frame(Actual = nfl_test$Touchdowns, Predicted = predictions)
print(actual_vs_predicted)

# Model performance metrics
model_performance <- postResample(pred = predictions, obs = nfl_test$Touchdowns)
print(model_performance)
