# Load necessary libraries
library(tidyverse) # For data manipulation and visualization
library(dplyr)     # For data manipulation
library(dbplyr)    # For interacting with databases through dplyr syntax
library(caret)     # For data splitting and model training
library(randomForest) # For the random forest algorithm
library(lubridate) # For easier date manipulation
library(DBI)       # For database connection

# Database connection
# Replace 'path_to_air_quality_database.sqlite' with your actual database path
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "path_to_air_quality_database.sqlite")

# Fetching and preprocessing data
air_quality_data <- tbl(con, "air_quality") %>%
  # Assuming 'Date' and 'PM2_5' are among the columns, along with other relevant features
  select(-matches("unnecessary_columns")) %>% # Exclude unnecessary columns
  filter(Date >= as.Date('2020-01-01')) %>%   # Filter records from 2020 onwards
  collect() %>% # Convert database table to a local tibble
  mutate(Date = as.Date(Date), # Ensure 'Date' is in Date format
         Year = year(Date), 
         Month = month(Date), 
         Day = day(Date)) %>%
  select(-Date) %>% # Remove 'Date' column to avoid redundancy
  na.omit() # Remove rows with NA values

# Splitting data into training and testing sets
set.seed(123) # For reproducibility
training_samples <- air_quality_data$PM2_5 %>% 
  createDataPartition(p = 0.8, list = FALSE) # 80% training data
train_data <- air_quality_data[training_samples, ]
test_data <- air_quality_data[-training_samples, ]

# Enhancing model fitting process
set.seed(123) # Ensure reproducibility in model results
rf_model <- randomForest(PM2_5 ~ ., data = train_data, ntree = 100, 
                         importance = TRUE) # Enable calculation of variable importance

# Model prediction and evaluation
predictions <- predict(rf_model, test_data)

# Comparing actual vs predicted values
actual_vs_predicted <- data.frame(Actual = test_data$PM2_5, Predicted = predictions)
print(head(actual_vs_predicted))

# Model performance metrics
model_performance <- postResample(pred = predictions, obs = test_data$PM2_5)
print(model_performance)

# Displaying variable importance
var_importance <- varImp(rf_model, scale = FALSE) # Extract variable importance scores
print(var_importance)

# Disconnect from the database
dbDisconnect(con)
