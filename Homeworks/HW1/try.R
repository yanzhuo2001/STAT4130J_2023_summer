library(MASS)
data("Boston")

# Create an empty vector to store the results
significant_models <- vector("logical", length = 13)

# Fit simple linear regression models for each predictor
for (i in 2:13) {
  predictor <- Boston[, i]
  model <- lm(crim ~ predictor, data = Boston)
  
  # Check if the p-value for the predictor is less than 0.05
  if (summary(model)$coefficients[2, "Pr(>|t|)"] < 0.05) {
    significant_models[i] <- TRUE
  }
}

# Print the predictors with a statistically significant association
significant_predictors <- names(Boston)[significant_models]
cat("Predictors with a statistically significant association:\n")
cat(significant_predictors, sep = ", ")

# Fit multiple regression model using all features
model <- lm(crim ~ ., data = Boston)

# Obtain the p-values for each feature
p_values <- summary(model)$coefficients[, "Pr(>|t|)"]

# Identify features with significant associations (rejecting the null hypothesis)
significant_features <- names(Boston)[p_values < 0.05]

# Print the features with significant associations
cat("Features with a significant association (rejecting the null hypothesis):\n")
cat(significant_features, sep = ", ")




# Fit simple linear regression models for each predictor
simple_reg_coeffs <- vector("numeric", length = 13)
for (i in 2:13) {
  predictor <- Boston[, i]
  model <- lm(crim ~ predictor, data = Boston)
  simple_reg_coeffs[i] <- coef(model)[2]  # Store the regression coefficient
}

# Fit multiple regression model using all features
multiple_reg_coeffs <- coef(lm(crim ~ ., data = Boston))

# Create a scatterplot
plot(simple_reg_coeffs, multiple_reg_coeffs[-1], 
     xlab = "Simple Regression Coefficients", ylab = "Multiple Regression Coefficients",
     main = "Comparison of Regression Coefficients",
     xlim = c(-10, 10), ylim = c(-10, 10),
     col = "steelblue", pch = 16)

# Add a line of equality
abline(a = 0, b = 1, col = "red", lty = 2)

# Add labels for each predictor with an offset
text(simple_reg_coeffs, multiple_reg_coeffs[-1], labels = names(Boston)[-1], pos = 4, cex = 0.8, offset = 0.5)

# Add gridlines
grid()

# Add legend
legend("topleft", legend = "Regression Coefficients", col = "steelblue", pch = 16, cex = 0.8)






# Select variables of interest
variables_of_interest <- c("zn", "dis", "rad", "black", "medv")

# Create an empty vector to store the p-values for the cubic model
p_values_cubic <- vector("numeric", length = length(variables_of_interest))

# Fit cubic models for each variable
for (i in 1:length(variables_of_interest)) {
  predictor <- Boston[, variables_of_interest[i]]
  
  # Fit the cubic model
  cubic_model <- lm(crim ~ predictor + I(predictor^2) + I(predictor^3), data = Boston)
  
  # Extract the p-value for the cubic term
  p_value <- summary(cubic_model)$coefficients[4, "Pr(>|t|)"]
  
  # Store the p-value
  p_values_cubic[i] <- p_value
  
  # Print the p-value for the cubic term
  cat("Variable:", variables_of_interest[i], "\t p-value for cubic term:", p_value, "\n")
}

# Identify variables with a significant cubic relationship
significant_cubic_predictors <- variables_of_interest[p_values_cubic < 0.05]

# Print the variables with a significant cubic relationship
cat("\nVariables with a significant cubic relationship:\n")
cat(significant_cubic_predictors, sep = ", ")

