# Load necessary libraries
library(data.table)
library(xgboost)
library(dplyr)

# Load the data
rm(list=ls())
data <- fread("E:/GAIA_BD/final_77777777777/Incidence/Incidence2/Incidence_covariate_new12.csv")

hist(data$NO)
data[, log_NO := log(NO + 1)]  # Adding 1 to avoid issues with log(0)
hist(data$log_NO)

dependent_var <- "log_NO"
excluded_vars <- c("hexID", "slope_IC", "NumSpecies", "LAT", "LON", "country", "BM", "geometry", "NO")
independent_vars <- setdiff(names(data), c(dependent_var, excluded_vars))

data <- na.omit(data)

set.seed(88779)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, independent_vars, with = FALSE]), label = train_data[[dependent_var]])
test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, independent_vars, with = FALSE]), label = test_data[[dependent_var]])

# Setting up grid for hyperparameter tuning
grid <- expand.grid(
  eta = c(0.01, 0.05, 0.1),  # 학습률
  max_depth = c(4, 6, 8),    # 트리 깊이
  min_child_weight = c(1, 5, 10),
  subsample = c(0.6, 0.8, 1),
  colsample_bytree = c(0.6, 0.8, 1)
)

# Initialize variables to store tuning results
best_params <- list()
best_rmse <- Inf

# Grid Search
for (i in 1:nrow(grid)) {
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = grid$eta[i],
    max_depth = grid$max_depth[i],
    min_child_weight = grid$min_child_weight[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i]
  )
  
  # Model training (evaluated with cross validation)
  set.seed(88779)
  cv_results <- xgb.cv(
    params = params,
    data = train_matrix,
    nrounds = 100,
    nfold = 5,
    metrics = "rmse",
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  # If the optimal RMSE is low, update the parameters
  mean_rmse <- min(cv_results$evaluation_log$test_rmse_mean)
  if (mean_rmse < best_rmse) {
    best_rmse <- mean_rmse
    best_params <- params
  }
}

# Train the final model with optimal parameters
final_model <- xgboost(
  params = best_params,
  data = train_matrix,
  nrounds = 100,
  verbose = 1
)

# Evaluate the final model on test data
preds <- predict(final_model, test_matrix)
test_actuals <- test_data[[dependent_var]]

# Calculating evaluation metrics (RMSE, R-squared, MAE)
rmse <- sqrt(mean((preds - test_actuals)^2))
rsq <- cor(preds, test_actuals)^2
mae <- mean(abs(preds - test_actuals))

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
  Actual = test_actuals,
  Predicted = preds
)

# # Find min and max values for Actual and Predicted
x_min <- min(cv_plot_data$Actual, na.rm = TRUE)
x_max <- max(cv_plot_data$Actual, na.rm = TRUE)
y_min <- min(cv_plot_data$Predicted, na.rm = TRUE)
y_max <- max(cv_plot_data$Predicted, na.rm = TRUE)

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
jpeg("E:/GAIA_BD/The Biodiversity Knowledge Paradox/figure/RandomForest2/xgboost_incidence_final.jpg", res = 600, width = 10, height = 10, units = "in")
print(p_marginal)
dev.off()
