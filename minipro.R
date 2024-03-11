# Load necessary libraries
install.packages("tidyverse")
install.packages("caret")

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(caret)

# Load the dataset (replace 'weatherdataset.csv' with your actual dataset)
weather_data <- read.csv("weatherdataset.csv")

# Explore the dataset
summary(weather_data)
str(weather_data)

# Data preprocessing
# Convert date to a usable format
weather_data$Date <- as.Date(weather_data$Date, format = "%Y-%m-%d")

# Split the data into training and testing sets
set.seed(123)
split_index <- createDataPartition(weather_data$Temperature, p = 0.8, list = FALSE)
train_data <- weather_data[split_index, ]
test_data <- weather_data[-split_index, ]

# Build a linear regression model
model <- lm(Temperature ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate the model
rmse <- sqrt(mean((test_data$Temperature - predictions)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Visualize actual vs. predicted values
plot(test_data$Temperature, predictions, main = "Actual vs. Predicted Temperature",
     xlab = "Actual Temperature", ylab = "Predicted Temperature", col = "blue", xlim = c(0, 40), ylim = c(0, 40))

# Add a diagonal line for reference
abline(0, 1, col = "red")

# Interpret the model coefficients
summary(model)

