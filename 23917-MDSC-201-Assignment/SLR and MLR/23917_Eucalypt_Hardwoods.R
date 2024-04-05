#`Importing dataset
dataset = read.csv("bmi.csv")

library(caTools)
set.seed(123)
split = sample.split(dataset$weight, SplitRatio = 2/3)
print(split)

training_set = subset(dataset, split== TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting the SLR Model using Training Set

regressor = lm(formula = weight ~ height , data = training_set)
print(regressor)

# Predicting the Test Set Results
y_test_pred = predict(regressor, newdata = test_set)

print(y_test_pred)

#Training set results plot
y_train_pred = predict(regressor, newdata = training_set)

library(ggplot2)
ggplot() + 
  geom_point(aes(x= training_set$height, 
                 y = training_set$weight),
             colour = "red") +
  geom_line(aes(x= training_set$height, 
                y = y_train_pred),
            colour = "blue") +
  ggtitle("weight Vs height (Training Set Results)") +
  xlab("weight") +
  ylab("height")

#Testing set results plot
ggplot() + 
  geom_point(aes(x= test_set$height, 
                 y = test_set$weight),
             colour = "red") +
  geom_line(aes(x= test_set$height, 
                y = y_test_pred),
            colour = "blue") +
  ggtitle("weight Vs height (Testing Set Results)") +
  xlab("height") +
  ylab("weight")

#Calculation of Residuals
train_residual = y_train_pred - training_set$weight
test_residual = y_test_pred - test_set$weight

length(train_residual)
#Plotting Residual line
ggplot() +
  geom_point(aes(x = 1:length(train_residual),
                 y = train_residual),
             colour = "black") + 
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "red")

ggplot() +
  geom_point(aes(x = 1:length(test_residual),
                 y = test_residual),
             colour = "black") + 
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "red")
