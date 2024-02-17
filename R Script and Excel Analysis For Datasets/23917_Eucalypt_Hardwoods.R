#`Importing dataset
dataset = read.csv("eucalypt_hardwoods.csv")

library(caTools)
set.seed(123)
split = sample.split(dataset$hardness, SplitRatio = 2/3)
print(split)

training_set = subset(dataset, split== TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting the SLR Model using Training Set

regressor = lm(formula = hardness ~ density , data = training_set)
print(regressor)

# Predicting the Test Set Results
y_test_pred = predict(regressor, newdata = test_set)

print(y_test_pred)

#Training set results plot
y_train_pred = predict(regressor, newdata = training_set)

library(ggplot2)
ggplot() + 
  geom_point(aes(x= training_set$density, 
                 y = training_set$hardness),
             colour = "red") +
  geom_line(aes(x= training_set$density, 
                y = y_train_pred),
            colour = "blue") +
  ggtitle("Hardness Vs Density (Training Set Results)") +
  xlab("Hardness") +
  ylab("Density")

#Testing set results plot
ggplot() + 
  geom_point(aes(x= test_set$density, 
                 y = test_set$hardness),
             colour = "red") +
  geom_line(aes(x= test_set$density, 
                y = y_test_pred),
            colour = "blue") +
  ggtitle("Hardness Vs Density (Testing Set Results)") +
  xlab("Density") +
  ylab("Hardness")

#Calculation of Residuals
train_residual = y_train_pred - training_set$hardness
test_residual = y_test_pred - test_set$hardness

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
