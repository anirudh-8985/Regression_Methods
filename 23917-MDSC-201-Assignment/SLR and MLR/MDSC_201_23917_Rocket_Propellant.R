#Importing dataset
dataset = read.csv("The Rocket propellant Data.csv")

library(caTools)
set.seed(123)
split = sample.split(dataset$Shear.strength, SplitRatio = 2/3)
print(split)

training_set = subset(dataset, split== TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting the SLR Model using Training Set
regressor = lm(formula = Shear.strength ~ Age.of.propellant , data = training_set)
print(regressor)

# Predicting the Test Set Results
y_test_pred = predict(regressor, newdata = test_set)

print(y_test_pred)

#Training set results plot
y_train_pred = predict(regressor, newdata = training_set)

library(ggplot2)
ggplot() + 
  geom_point(aes(x= training_set$Age.of.propellant, 
                 y = training_set$Shear.strength),
             colour = "red") +
  geom_line(aes(x= training_set$Age.of.propellant, 
                y = predict(regressor, newdata = training_set)),
            colour = "blue") +
  ggtitle("Shear.strengh Vs Age.of.propellant (Training Set Results)") +
  xlab("Age.of.propellant") +
  ylab("Shear.strengh")

#Testing set results plot
ggplot() + 
  geom_point(aes(x= test_set$Age.of.propellant, 
                 y = test_set$Shear.strength),
             colour = "red") +
  geom_line(aes(x= test_set$Age.of.propellant, 
                y = y_test_pred),
            colour = "blue") +
  ggtitle("Shear.strengh Vs Age.of.propellant (Testing Set Results)") +
  xlab("Age.of.propellant") +
  ylab("Shear.strength")

#Calculation of Residuals
train_residual = y_train_pred - training_set$Shear.strength
test_residual = y_test_pred - test_set$Shear.strength

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


