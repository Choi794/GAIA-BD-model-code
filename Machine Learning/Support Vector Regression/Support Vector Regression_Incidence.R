# Load necessary libraries
rm(list=ls())
library(e1071)
library(caret)
library(data.table)

# Load the data
data <- fread("E:/GAIA_BD/final_77777777777/Incidence/Incidence2/Incidence_covariate_new12.csv")

hist(data$NO)
data[, log_NO := log(NO + 1)]  # Adding 1 to avoid issues with log(0)
hist(data$log_NO)

# Define the dependent and independent variables
dependent_var <- "log_NO"
independent_vars <- setdiff(names(data), c(dependent_var,"NO", "hexID", "slope_IC", "NumSpecies", "LAT", "LON", "country", "BM", "geometry"))

# Remove rows with missing values only in the columns we need
data <- data[complete.cases(data[, .SD, .SDcols = c(dependent_var, independent_vars)]), ]

# Convert character columns to factors to ensure consistency
for (col in names(data)) {
  if (is.character(data[[col]])) {
    data[[col]] <- as.factor(data[[col]])
  }
}

# Prepare the data for modeling
set.seed(88779)
train_index <- createDataPartition(data[[dependent_var]], p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Ensure test_data has the same structure as train_data
test_data <- test_data[, c(independent_vars, dependent_var), with = FALSE]

# Standardize the independent variables
standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# 독립 변수 표준화
for (col in independent_vars) {
  train_data[[col]] <- standardize(train_data[[col]])
  test_data[[col]] <- standardize(test_data[[col]])  # 동일한 평균과 표준편차 사용
}


# Define a grid for tuning the SVR model with caret's expected parameters
tune_grid <- expand.grid(
  sigma = c(0.01, 0.05, 0.1),  # Sigma (equivalent to gamma in e1071)
  C = 2^(2:5)                   # Cost parameter
)

# Set up cross-validation
control <- trainControl(method = "cv", number = 5, savePredictions = "all")  # 5-fold cross-validation

# Train and fine-tune the SVR model using caret
set.seed(88779)
svr_tuned <- train(
  as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + "))),
  data = train_data,
  method = "svmRadial",
  trControl = control,
  tuneGrid = tune_grid,
  
)

# Print the best model and its parameters
print(svr_tuned)
best_svr_model <- svr_tuned$finalModel

predictions <- svr_tuned$pred

# Make predictions on the test set using the tuned SVR model
best_predictions <- predict(svr_tuned, newdata = test_data)

rsq <- cor(test_data[[dependent_var]], best_predictions)^2
rmse <- sqrt(mean((best_predictions - test_data[[dependent_var]])^2))
mae <- mean(abs(best_predictions - test_data[[dependent_var]]))
cat("RMSE:", rmse, "\n")
cat("R-squared:", rsq, "\n")
cat("MAE:", mae, "\n")

################################################################################
############################cross validation####################################
################################################################################

library(ggplot2)
library(MASS)  # For kde2d
library(ggExtra)


# Create a data frame for plotting with Actual vs. Predicted values
cv_plot_data <- data.frame(
  Actual = test_data[[dependent_var]],
  Predicted = best_predictions
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
jpeg("E:/GAIA_BD/The Biodiversity Knowledge Paradox/figure/RandomForest2/svr_incidence_final.jpg", res = 600, width = 10, height = 10, units = "in")
print(p_marginal)
dev.off()
