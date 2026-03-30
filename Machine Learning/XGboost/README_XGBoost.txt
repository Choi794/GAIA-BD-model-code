## Overview
This script evaluates predictive performance for biodiversity data using an Extreme Gradient Boosting (XGBoost) regression model.
The model predicts the log-transformed response variable log(NO + 1) from environmental and socioeconomic predictors. Model performance is assessed using a train-test split, hyperparameter tuning with cross-validation, evaluation metrics, and visualization.

The same workflow is applied to both incidence-based and abundance-based datasets.

## System Requirements

Tested on:
- Windows 11
- R 4.2

Required packages:
- xgboost
- data.table
- dplyr
- ggplot2
- MASS
- ggExtra

## Installation
Install the required R packages:

install.packages(c("xgboost", "data.table", "dplyr", "ggplot2", "MASS", "ggExtra"))

Installation takes approximately 5–10 minutes on a standard desktop.

## Input Data Requirements
The script requires a .csv file containing the following columns:
- NO : Response variable 
- hexID : Identifier for the spatial unit 
- slope_IC : Inventory completeness-related variable 
- NumSpecies : Number of species 
- LAT : Latitude 
- LON : Longitude 
- country : Country identifier 
- BM : Biome or related grouping variable 
- geometry : Spatial geometry information 
- predictor variables : Environmental and socioeconomic covariates used in the regression model 

The input file used in the current script is:
`Incidence_covariate_new12.csv`

A parallel analysis is conducted for abundance-based data using:
`Abundance_covariate_new12.csv`

Note: The script creates a new response variable, log_NO, defined as log(NO + 1).

## Demo
To run the script:
1. Open R or RStudio
2. Set the working directory to the script location
3. Run the script:

To fully reproduce the results, run both scripts:

- Incidence data:
```r
source("XGBoost_Incidence.R")
```
- Abundance data:
```r
source("XGBoost_Abundance.R")
```

Example input files:
- `Incidence_covariate_new12.csv`
- `Abundance_covariate_new12.csv`

Expected output:
- evaluation metrics printed in the console
- predicted vs. observed plots

Note: Please update file paths in the script before running.
The demo runs in approximately 20–30 minutes on a standard desktop.

## Method Description
### 1. Data Preparation
The input dataset is loaded using fread() from the data.table package.
A new response variable is created:
`log_NO = log(NO + 1)`
A value of 1 is added before log transformation to avoid undefined values when NO = 0.
The following columns are removed before model fitting:
- hexID 
- slope_IC 
- NumSpecies 
- LAT 
- LON 
- country 
- BM 
- geometry 
- NO 

Rows with missing values are removed using na.omit().
All columns remaining after the removal of the variables listed above are used as predictors in the XGBoost model.

### 2. Train-Test Split
The cleaned dataset is randomly split into:
- 80% training data
- 20% testing data

A fixed random seed is used:
`set.seed(88779)`
This ensures reproducibility of the train-test split across runs and environments.

### 3. Data Formatting for XGBoost
The training and testing datasets are converted into matrix format using `xgb.DMatrix.`
This step is required because XGBoost operates on matrix-based input structures.

### 4. Hyperparameter Tuning

Hyperparameter tuning is performed using a grid search approach combined with 5-fold cross-validation (xgb.cv).

The tuning grid includes:

`eta = c(0.01, 0.05, 0.1)`
`max_depth = c(4, 6, 8)`
`min_child_weight = c(1, 5, 10)`
`subsample = c(0.6, 0.8, 1)`
`colsample_bytree = c(0.6, 0.8, 1)`

The optimal parameter set is selected based on the lowest cross-validated RMSE.
Early stopping is applied to prevent overfitting.

### 5. Model Training
The final XGBoost model is trained using the optimal hyperparameters obtained from cross-validation.

### 6. Model Evaluation
The trained model is evaluated on the test dataset.

The following evaluation metrics are calculated:
- RMSE
- R²
- MAE

All metrics are computed on the log-transformed scale.

### 7. Visualization
A scatter plot of predicted vs. observed values is generated.

The plot includes:
- scatter points
- a fitted regression trendline (for visualization purposes)
- a 1:1 reference line
- annotated summary statistics
- marginal density plots
The final figure is saved as a high-resolution JPEG file.

## Output
The script produces the following outputs:
Console output:
- RMSE
- R²
- MAE

Incidence-based analysis:
xgboost_incidence_final.jpg

Abundance-based analysis:
xgboost_abundance_final.jpg

## Example Data
`Incidence_covariate_new12.csv`
`Abundance_covariate_new12.csv`

## Expected Output
Running the script on the input data will generate:
- evaluation metrics printed in the console
- predicted vs. observed plot

## Important Notes
### 1. Reproducibility
The train-test split and model tuning are controlled by a fixed random seed.

Due to the stochastic nature of the XGBoost algorithm, results may vary slightly across runs or computing environments. However, given the fixed seed and large dataset size, such variations are minimal and do not materially affect the results.

### 2. Model Type
The model used in this script is XGBoost regression (xgboost package), a gradient boosting ensemble method.
XGBoost uses sequential tree boosting to improve predictive performance.

### 3. Interpretation of Results
The model predicts log(NO + 1) rather than the original NO.
Therefore:
- all evaluation metrics are calculated on the log-transformed scale
- the observed-versus-predicted plot is shown on the log-transformed scale

### 4. Missing Data Handling
Rows with missing values are removed before model fitting.

### 5. Evaluation Strategy
The current workflow uses:
a single 80/20 train-test split
5-fold cross-validation for hyperparameter tuning

Thus, final performance is assessed on the held-out test dataset after model tuning.

### 6. File Paths
The script uses absolute file paths in fread() and in the jpeg() output path.
These paths may need to be modified before running the script on another computer or in a different directory structure.

### 7. Incidence and Abundance Data
A parallel script is provided for abundance-based data.
The underlying methodology and workflow are identical, with only the input dataset and output file name differing.
________________________________________

## Usage
The script is intended to be run in R after setting the correct working directory and ensuring that the required packages are installed.

Example workflow:
1. Place `Incidence_covariate_new12.csv` or `Abundance_covariate_new12.csv` in the working directory
2. Update file paths if necessary 
3. Run the script in R or RStudio 

The script will then:
- load and preprocess the data
- split the data into training and test sets
- convert data into matrix format
- perform hyperparameter tuning using cross-validation
- train the final model
- generate predictions
- calculate evaluation metrics
- save the final figure

To apply the model to new data:
- Replace the input dataset with your own data
- Ensure the same column structure
- Update file paths in the script

## Reproducibility
To reproduce manuscript results:

1. Run incidence analysis:
```r
source("XGBoost_Incidence.R")
```
2. Run abundance analysis:
```r
source("XGBoost_Abundance.R")
```
3. Outputs:
   - `xgboost_incidence_final.jpg`
   - `xgboost_abundance_final.jpg`


## Summary
This workflow provides a reproducible and scalable framework for modeling biodiversity data using XGBoost regression, incorporating preprocessing, matrix transformation, hyperparameter tuning, prediction, performance evaluation, and visualization. The same approach is consistently applied to both incidence-based and abundance-based datasets.

## License
This code is released under the MIT License.

## Code Availability
The code used to generate the results in this study is available at:
https://github.com/Choi794/GAIA-BD-model-code