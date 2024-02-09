# Load necessary libraries
library(tidyverse)
library(dplyr)
library(dbplyr)
library(caret)
library(randomForest)
library(lubridate)
library(DBI)

# Assume connection to a database containing air quality data
# Replace with actual database connection details
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "path_to_air_quality_database.sqlite")

# Using dbplyr to interact with the database
tbl(con, "air_quality") %>%
  select(-matches("unnecessary_columns")) %>%
  filter(Date >= as.Date('2020-01-01')) %>%
  collect() -> air_quality_data

# Preprocess data with dplyr
air_quality_data <- air_quality_data %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
  select(-Date) %>%
  na.omit()

# Splitting data into training and testing sets
set.seed(123)
training_samples <- air_quality_data$PM2_5 %>% createDataPartition(p = 0.8, list = FALSE)
train_data <- air_quality_data[training_samples, ]
test_data <- air_quality_data[-training_samples, ]

# Model fitting using Random Forest
set.seed(123)
rf_model <- randomForest(PM2_5 ~ ., data = train_data, ntree = 100)

# Model prediction
predictions <- predict(rf_model, test_data)

# Evaluate the model
actual_vs_predicted <- data.frame(Actual = test_data$PM2_5, Predicted = predictions)
head(actual_vs_predicted)

# Model performance metrics
model_performance <- postResample(pred = predictions, obs = test_data$PM2_5)
print(model_performance)

# Disconnect from the database
dbDisconnect(con)
