# Load necessary libraries
rm(list=ls())
library(tidyverse)
library(ranger)
library(caret)
library(data.table)
library(dplyr)
library(ggplot2)

# Load the data
data <- fread("E:/GAIA_BD/final_77777777777/Incidence/Incidence2/Incidence_covariate_new12.csv")

hist(data$NO)
data[, log_NO := log(NO + 1)]  # Adding 1 to avoid issues with log(0)
hist(data$log_NO)

# Inspect the data
str(data)
summary(data)

# Handle missing values if necessary
data <- na.omit(data)

# Define the dependent and independent variables
dependent_var <- "log_NO"
independent_vars <- setdiff(names(data), c(dependent_var,"NO", "hexID", "NumSpecies", "slope_IC", "country", "geometry", "LON", "LAT", "BM"))

# Prepare the data for modeling
set.seed(88779)
train_index <- createDataPartition(data[[dependent_var]], p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Build the ranger model
ranger_model <- ranger(as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + "))), 
                       data = train_data, importance = 'impurity')


# Print the model summary
print(ranger_model)
importance <- importance(ranger_model)

# Plot variable importance
importance_df <- data.frame(Variable = names(importance), Importance = importance)
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variables", y = "Importance")

# Make predictions on the test set
predictions <- predict(ranger_model, data = test_data)$predictions

# Calculate RMSE
rmse <- sqrt(mean((predictions - test_data[[dependent_var]])^2))
cat("RMSE: ", rmse, "\n")

# Fine-tuning the model using caret
tune_grid <- expand.grid(mtry = c(2, 4, 6, 8, 10), splitrule = "variance", min.node.size = c(1, 3, 5))
control <- trainControl(method = "cv", number = 5, savePredictions = "final")
set.seed(88779)
ranger_tuned <- train(as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + "))), 
                      data = train_data, method = "ranger", tuneGrid = tune_grid, trControl = control, importance = 'impurity')

print(ranger_tuned)

# Extract the optimal model
best_model <- ranger_tuned$finalModel

# Cross validation RMSE and R^2 check
cv_results <- ranger_tuned$results
cat("Cross-Validation RMSE:", min(cv_results$RMSE), "\n")
cat("Cross-Validation R-squared:", max(cv_results$Rsquared), "\n")


# Evaluate the tuned model on the test set
tuned_predictions <- predict(ranger_tuned, newdata = test_data)

# Calculate RMSE for tuned model
tuned_rmse <- sqrt(mean((tuned_predictions - test_data[[dependent_var]])^2))
cat("Tuned RMSE: ", tuned_rmse, "\n")

# # Plot variable importance for tuned model
tuned_importance <- importance(best_model)
tuned_importance_df <- data.frame(Variable = names(tuned_importance), Importance = tuned_importance)
ggplot(tuned_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = "Tuned Model Variable Importance", x = "Variables", y = "Importance")

##########################variable importance############################
ggplot(tuned_importance_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = "Tuned Model Variable Importance", x = "Predictor Variables", y = "Variable Importance") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme(
    axis.title.x = element_text(size = 13),  
    axis.title.y = element_text(size = 13),  
    plot.title = element_text(size = 16, face = "bold")  
  )

test_actuals <- test_data[[dependent_var]]

# Plot fitted vs actual values
fitted_vs_actual <- data.frame(Fitted = tuned_predictions, Actual = test_actuals)

# Calculate R² and RMSE
rsq <- cor(fitted_vs_actual$Actual, fitted_vs_actual$Fitted)^2
cat("R2: ", rsq, "\n")
rmse <- sqrt(mean((fitted_vs_actual$Fitted - fitted_vs_actual$Actual)^2))

# Calculate MAE
mae <- mean(abs(fitted_vs_actual$Fitted - fitted_vs_actual$Actual))
cat("MAE: ", mae, "\n")
# Calculate RMSE for tuned model
tuned_rmse <- sqrt(mean((tuned_predictions - test_data[[dependent_var]])^2))
cat("Tuned RMSE: ", tuned_rmse, "\n")


################################################################################
############################cross validation####################################
################################################################################
library(ggplot2)
library(MASS)  
library(ggExtra)


# Create a data frame for plotting with Actual vs. Predicted values
cv_plot_data <- data.frame(
  Actual = test_data[[dependent_var]],
  Predicted = tuned_predictions
)

# # Find min and max values for Actual and Predicted
x_min <- min(cv_plot_data$Predicted, na.rm = TRUE)
x_max <- max(cv_plot_data$Predicted, na.rm = TRUE)
y_min <- min(cv_plot_data$Actual, na.rm = TRUE)
y_max <- max(cv_plot_data$Actual, na.rm = TRUE)

# Set the range to cover both axes
range_min <- min(x_min, y_min)
range_max <- max(x_max, y_max)


# Calculate linear regression model and metrics
lm_model <- lm(Actual ~ Predicted, data = cv_plot_data)
rmse <- sqrt(mean(lm_model$residuals^2))
r_sq <- summary(lm_model)$r.squared
mae <- mean(abs(lm_model$residuals))
print(rmse)
print(r_sq)
print(mae)


# Create the base plot with scatter points and density visualization
p <- ggplot(cv_plot_data, aes(x = Predicted, y = Actual)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 4) +  # Scatter plot 추가
  geom_smooth(method = "lm", se = FALSE, size = 1.2, aes(linetype = "Trendline", color = "Trendline")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  coord_fixed(ratio = 1, xlim = c(range_min, range_max), ylim = c(range_min, range_max)) +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 27),
    legend.position = "none",
    legend.key.size = unit(0.5, "cm"),
    legend.spacing = unit(0.1, "cm"),
    legend.text = element_text(size = 25),
  ) +
  guides(
    linetype = "none",
    color = "none"
   
  ) +
  scale_linetype_manual(
    values = c("Trendline" = "solid", "Reference Line" = "dashed"),
    name = "",
    breaks = c("Trendline", "Reference Line"),
    labels = c("Trendline", "Reference Line")
  ) +
  scale_color_manual(
    values = c("Trendline" = "firebrick4", "Reference Line" = "black"),
    name = "",
    breaks = c("Trendline", "Reference Line"),
    labels = c("Trendline", "Reference Line")
  ) +
  annotate(  "text", x = 11, y = 4,
             label = paste(
               sprintf("%-5s = %6.2f", "R²", r_sq),
               sprintf("%-5s = %6.2f", "RMSE", rmse),
               sprintf("%-5s = %6.2f", "MAE", mae),
               sep = "\n"
             ),
             size = 9, hjust = 0, vjust = 1, colour = "black"
  )

# Add marginal density plots with shape outline
p_marginal <- ggMarginal(p, type = "density", fill = "skyblue", color = "black")

# Save the plot
jpeg("E:/GAIA_BD/The Biodiversity Knowledge Paradox/figure/RandomForest2/rf_incidence_final7890123.jpg", res = 600, width = 10, height = 10, units = "in")
print(p_marginal)
dev.off()
