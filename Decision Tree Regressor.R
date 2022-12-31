library(rsample)     # Data splitting.
library(dplyr)       # Data wrangling.
library(rpart)       # Performing regression trees.
library(rpart.plot)  # Plotting regression trees.
library(caret)       # bagging
library(readxl)

# Import the dataset.
avocado <- read_excel("avocado.xlsx")

# Drop the geography column.
data = avocado[, 1:12]

# Splitting the dataset in a 0.7 ratio by default order by years.
avocado_train = data[1:23131,]
avocado_test = data[23132:33045,]

# Regressor.
m1 <- rpart(formula = average_price ~ ., data = avocado_train, method = "anova")
print(m1)

# Summary of the decision tree regressor.
summary(m1)

# Predicting prices for the test split.
predictions <- predict(m1, avocado_test, type = 'vector')

# Summarizing accuracy.
# Calculating the Root Mean Squared Error.
RMSE(predictions, avocado_test$average_price)

# Calculating the Mean Squared Error.
mse <- mean((avocado_test$average_price - predictions)^2)
print(mse)

# Calculating the Mean Absolute Error.
MAE = function(actual, predicted) {
  mean(abs(actual - predicted))
}

print(MAE(avocado_test$average_price, predictions))
