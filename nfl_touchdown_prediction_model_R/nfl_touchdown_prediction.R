# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(readr)

# Load the dataset
nfl_data <- read_csv("path_to_nfl_data.csv")

# Data preprocessing
nfl_data <- nfl_data %>%
  mutate(PlayedGame = if_else(is.na(PlayedGame), 0, 1), # Convert NA to 0 for players who didn't play
         LinemenFormationChange = if_else(Position == "Quarterback" & LinemenFormationChange == 1, 1, 0), # Flag for QBs with linemen change
         SeasonTypeFactor = if_else(SeasonType == "Playoff", 2, 1), # Higher weight for playoff games
         InjuryFactor = if_else(PlayedGame == 0, 0.5, 1)) %>% # Reduce weight for games player missed
  select(Player, Touchdowns, Team, SeasonType, PlayedGame, LinemenFormationChange, SeasonTypeFactor, InjuryFactor, other_relevant_features) %>%
  filter(!is.na(Touchdowns))

# Creating a weighted touchdown variable considering SeasonType and Injury
nfl_data <- nfl_data %>%
  mutate(WeightedTouchdowns = Touchdowns * SeasonTypeFactor * InjuryFactor)

# Split the data into training and testing sets
set.seed(123)
trainingIndex <- createDataPartition(nfl_data$WeightedTouchdowns, p = .8, 
                                     list = FALSE, 
                                     times = 1)
nfl_train <- nfl_data[trainingIndex,]
nfl_test <- nfl_data[-trainingIndex,]

# Model fitting using Random Forest
set.seed(123)
touchdown_model <- randomForest(WeightedTouchdowns ~ . -Player -Touchdowns, data = nfl_train, ntree = 100)

# Model prediction
predictions <- predict(touchdown_model, nfl_test)

# Evaluate the model
actual_vs_predicted <- data.frame(Actual = nfl_test$WeightedTouchdowns, Predicted = predictions)
print(actual_vs_predicted)

# Model performance metrics
model_performance <- postResample(pred = predictions, obs = nfl_test$WeightedTouchdowns)
print(model_performance)

