
data = read.csv("f:/23917-MDSC-201_final_lab/Problem 1/Song_count.csv")

X = data[,1]
Y = data[,2]

correlation_coeff = cor(X, Y)

print(paste("Correlation Coeff:", correlation_coeff))

fit = lm(Y ~ X)

plot_data = data.frame(X = X, Y = Y)

intercept=coef(fit)[1]
slope=coef(fit)[2]

library(ggplot2)

p = ggplot(plot_data, aes(x = X, y = Y)) +
  geom_point() +
  geom_abline(intercept = intercept, slope = slope, color = "red") +
  labs(title = "Scatter plot for fitted line", x = "X", y = "Y") +
  theme_minimal()

print(p)

