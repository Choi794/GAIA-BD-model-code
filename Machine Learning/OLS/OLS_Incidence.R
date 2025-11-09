rm(list=ls())
library(tidyverse)
library(caret)
library(dplyr)
library(data.table)

# Set working directory and load the data
setwd("E:/GAIA_BD/final_77777777777/Incidence/Incidence2")
data <- fread("Incidence_covariate_new12.csv")

hist(data$NO)
data[, log_NO := log(NO + 1)]  # Adding 1 to avoid issues with log(0)
hist(data$log_NO)

# Drop unwanted columns
data <- data %>% dplyr::select(-hexID, -slope_IC, -NumSpecies, -LAT, -LON, -country, -BM, -geometry, -NO)

# Remove rows with missing values
data <- na.omit(data)

# setting seed to generate a 
# reproducible random sampling
set.seed(88779) 

# creating training data as 80% of the dataset
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# training the model by assigning sales column

model <- lm(log_NO ~., data = train_data)

# predicting the target variable
predictions2 <- predict(model, test_data)

rmse2 <- sqrt(mean((predictions2 - test_data$log_NO)^2))
rsq2 <- cor(predictions2, test_data$log_NO)^2
mae2 <- mean(abs(predictions2 - test_data$log_NO))
print(rmse2)
print(rsq2)
print(mae2)

################################################################################
############################cross validation####################################
################################################################################
library(ggplot2)
library(MASS)  
library(ggExtra)

# Create a data frame for plotting with Actual vs. Predicted values
cv_plot_data <- data.frame(
  Actual = test_data$log_NO,
  Predicted = predictions2
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
  geom_smooth(method = "lm", se = FALSE, size = 1.2, aes(linetype = "Trendline", color = "Trendline"), fullrange =TRUE) +
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
  annotate(  "text", x = 11, y = 3,
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
jpeg("E:/GAIA_BD/The Biodiversity Knowledge Paradox/figure/RandomForest2/OLS_incidence_final.jpg", res = 600, width = 10, height = 10, units = "in")
print(p_marginal)
dev.off()

