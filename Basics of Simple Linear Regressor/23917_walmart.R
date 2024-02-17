# Importing the Dataset
df <- read.csv("walmart.csv")  

# Exploring the data
summary(df)

# Checking for null values
if (any(is.na(df))) {
  print("Null values found in the dataset.")
} else {
  print("No null values found in the dataset.")
}

# Splitting the Dataset into Training and Testing sets
library(caTools)
set.seed(123)
split <- sample.split(df$Weekly_Sales, SplitRatio = 2/3)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == FALSE)

# Fitting the Simple Linear Regression Model using Training Set
regressor <- lm(Weekly_Sales ~ ., data = training_set)
print(summary(regressor))

# Predicting the Test Set Results
y_pred <- predict(regressor, newdata = test_set)
print(y_pred)

# Visualizing the Training Set Results
library(ggplot2)

ggplot() + 
  geom_point(data = training_set, aes(x = Temperature, y = Weekly_Sales), colour = "red") +
  geom_line(data = training_set, aes(x = Temperature, y = predict(regressor)), colour = "blue") +
  ggtitle("Temperature Vs Weekly Sales (Training Set Results)") +
  xlab("Temperature") +
  ylab("Weekly Sales")

# Visualizing the Testing Set Results
ggplot() + 
  geom_point(data = test_set, aes(x = Temperature, y = Weekly_Sales), colour = "red") +
  geom_line(data = test_set, aes(x = Temperature, y = y_pred), colour = "blue") +
  ggtitle("Temperature Vs Weekly Sales (Testing Set Results)") +
  xlab("Temperature") +
  ylab("Weekly Sales")

