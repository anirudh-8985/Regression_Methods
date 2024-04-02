data <- read.csv("f:/23917-MDSC-201_final_lab/Problem 2/CGPA.csv")


Y <- data[, 3]
X <- data[, -3]

fit <- lm(Y ~ ., data = data)

summary_fit <- summary(fit)
print(summary_fit)       # fitting the MLR
print("p values are")    # explicitly printing p values
print(summary_fit$coefficients[,4])
