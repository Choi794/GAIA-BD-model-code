## Overview
This script evaluates predictive performance for biodiversity data using a Support Vector Regression (SVR) model.
The model predicts the log-transformed response variable log(NO + 1) from environmental and socioeconomic predictors. Model performance is assessed using a train-test split, hyperparameter tuning with cross-validation, evaluation metrics, and visualization.

The same workflow is applied to both incidence-based and abundance-based datasets.

## System Requirements

Tested on:
- Windows 11
- R 4.2

Required packages:
- e1071
- caret
- data.table
- ggplot2
- MASS
- ggExtra

## Installation

Install the required R packages:

install.packages(c("e1071", "caret", "data.table", "ggplot2", "MASS", "ggExtra"))

Installation takes approximately 5–10 minutes on a standard desktop.

## Input Data Requirements
The script requires a `.csv` file containing the following columns:
- NO : Response variable
- hexID : Identifier for the spatial unit
- slope_IC : Inventory completeness-related variable
- NumSpecies : Number of species
- LAT : Latitude
- LON : Longitude
- country : Country identifier
- BM : Biome or related grouping variable
- geometry : Spatial geometry information
- predictor variables : Environmental and socioeconomic covariates used in the SVR model

The input file used in the incidence-based analysis is:
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
source("SVR_Incidence.R")
```

- Abundance data:
```r
source("SVR_Abundance.R")
```

Example input files:
- `Incidence_covariate_new12.csv`
- `Abundance_covariate_new12.csv`

Expected output:
- evaluation metrics printed in the console
- predicted vs. observed plots

Note: Please update file paths in the script before running.
The demo runs in approximately 10–20 minutes on a standard desktop.

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
All columns remaining after the removal of the variables listed above are used as predictors in the SVR model.


### 2. Train-Test Split
The cleaned dataset is randomly split into:
- 80% training data
- 20% testing data

A fixed random seed is used:
`set.seed(88779)`
This ensures reproducibility of the train-test split across runs and environments.

### 3. Data Standardization
Predictor variables are standardized prior to model fitting.
This step is included because SVR is sensitive to the scale of predictor variables.

### 4. Hyperparameter Tuning and Model Fitting
The SVR model is fitted using the caret package with method svmRadial.
Hyperparameter tuning is performed using 5-fold cross-validation.

The tuning grid includes:
`sigma = c(0.01, 0.05, 0.1)`
`C = 2^(2:5)`

The best-performing tuned model is selected and used for final predictions.

### 5. Model Evaluation
The following metrics are calculated on the test dataset:
- RMSE
- R²
- MAE
Metrics are computed on the log-transformed scale.

### 6. Visualization
The script creates a scatter plot comparing predicted and observed values.

The figure includes:
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
`svr_incidence_final.jpg`

Abundance-based analysis:
`svr_abundance_final.jpg`

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
As a result, the same split and tuning procedure will be reproduced when the script is run under the same data and software conditions.

### 2. Model Type
The model used in this script is Support Vector Regression implemented through caret using the svmRadial method.
This workflow differs from OLS and Random Forest in that it uses a kernel-based regression approach.

### 3. Interpretation of Results
The model predicts log(NO + 1) rather than the original NO.
Therefore:
- all evaluation metrics are calculated on the log-transformed scale
- the observed-versus-predicted plot is shown on the log-transformed scale

### 4. Missing Data Handling
Rows with missing values are removed before model fitting.
As a result, the number of observations used in the analysis may be smaller than the number of rows in the original input file.

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
- standardize predictor variables
- tune the SVR model using cross-validation
- generate predictions
- calculate evaluation metrics
- save the final figure

To apply the model to new data:
- Replace the input dataset with your own data
- Ensure the same column structure
- Update file paths in the script

## Reproducibility
To reproduce the results reported in the manuscript:

1. Run incidence analysis:
```r
source("SVR_Incidence.R")
```

2. Run abundance analysis:
```r
source("SVR_Abundance.R")
```

3. The script will generate:
   - `svr_incidence_final.jpg`
   - `svr_abundance_final.jpg`

## Summary
This workflow provides a reproducible approach to evaluating a Support Vector Regression model for biodiversity data using preprocessing, train-test splitting, predictor standardization, model tuning, prediction, performance evaluation, and visualization. The same framework is applied to both incidence-based and abundance-based datasets.

## License
This code is released under the MIT License.

## Code Availability
The code used to generate the results in this study is available at:
https://github.com/Choi794/GAIA-BD-model-code