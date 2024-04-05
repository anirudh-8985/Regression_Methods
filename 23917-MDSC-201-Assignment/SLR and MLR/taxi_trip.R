# Multiple Linear Regression
df = read.csv("taxi_trip.csv")

# Exploring the data
summary(df)

# Checking for null values
if (any(is.na(df))) {
  print("Null values found in the df.")
} else {
  print("No null values found in the df.")
}

# Splitting the df into Training and Testing set

library(caTools)
set.seed(123)
split = sample.split(df$total_fare, SplitRatio = 0.8)
print(split)

training_set = subset(df, split== TRUE)
test_set = subset(df, split == FALSE)

# Fitting the Multiple Linear Regression Model using Training Set

regressor = lm(formula = total_fare ~ ., data = training_set)
print(regressor)

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)

print(y_pred)
print(test_set$total_fare)

# Checking the accuracy using Mean Squared error
mae = mean(abs(y_pred - test_set$total_fare))
print(paste("Mean Absolute Error (MAE):", mae))
