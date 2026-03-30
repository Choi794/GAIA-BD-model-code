## Overview
This script evaluates predictive performance for biodiversity data using a Random Forest regression model implemented with the ranger package.
The model predicts the log-transformed response variable log(NO + 1) from environmental and socioeconomic predictors. Model performance is assessed using a train-test split, hyperparameter tuning with cross-validation, evaluation metrics, variable importance analysis, and visualization.

The same workflow is applied to both incidence-based and abundance-based datasets.

## System Requirements

Tested on:
- Windows 11
- R 4.2

Required packages:
- ranger
- caret
- tidyverse
- data.table
- dplyr
- ggplot2
- MASS
- ggExtra

## Installation
Install the required R packages:

install.packages(c("ranger", "caret", "tidyverse", "data.table", "dplyr", "ggplot2", "MASS", "ggExtra"))

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
source("Random Forest_Incidence.R")
```
- Abundance data:
```r
source("Random Forest_Abundance.R")
```

Example input files:
- `Incidence_covariate_new12.csv`
- `Abundance_covariate_new12.csv`

Expected output:
- evaluation metrics printed in the console
- variable importance plots
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
All columns remaining after the removal of the variables listed above are used as predictors in the regression model.

### 2. Train-Test Split
The cleaned dataset is randomly split into:
- 80% training data 
- 20% testing data 

A fixed random seed is used:
`set.seed(88779)`
This ensures reproducibility of the train-test split across runs and environments.

### 3. Random Forest Model
A Random Forest regression model is fitted using the ranger package with impurity-based variable importance.
Predictions are generated for the test dataset.

### 4. Hyperparameter Tuning
The model is tuned using the caret package with 5-fold cross-validation.
The tuning grid includes:
`mtry = c(2, 4, 6, 8, 10)`
`splitrule = "variance"`
`min.node.size = c(1, 3, 5)`

The best-performing model is selected and used for final predictions.

### 5. Model Evaluation
The following metrics are calculated:
- RMSE
- R²
- MAE

Metrics are computed on the log-transformed scale.
Both cross-validation performance and test-set performance are reported.

### 6. Variable Importance
Variable importance is computed using impurity-based importance.
Both initial and tuned model importance values are visualized.

### 7. Visualization
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
- cross-validation RMSE and R²
- Figures

Incidence-based analysis:
`rf_incidence_final.jpg`

Abundance-based analysis:
`rf_abundance_final.jpg`

## Example Data
`Incidence_covariate_new12.csv`
`Abundance_covariate_new12.csv`

## Expected Output
Running the script produces:
- evaluation metrics printed in the console
- variable importance plots
- predicted vs. observed plot

## Important Notes
### 1. Reproducibility
The train-test split and model tuning use a fixed random seed.

Due to the stochastic nature of the Random Forest algorithm, results may vary slightly across runs or computing environments. However, given the fixed seed and large dataset size, such variations are minimal and do not materially affect the results.

### 2. Model Type
This script uses a Random Forest regression model.

Compared to OLS, this approach captures non-linear relationships and interactions among predictors.

### 3. Interpretation of Results
All predictions and evaluation metrics are based on:

log(NO + 1)

### 4. Missing Data Handling
Rows with missing values are removed prior to modeling.

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
- fit the Random Forest model  
- perform hyperparameter tuning using cross-validation  
- generate predictions  
- calculate evaluation metrics  
- produce variable importance results  
- save the final figure  

To apply the model to new data:
- Replace the input dataset with your own data
- Ensure the same column structure
- Update file paths in the script

## Reproducibility
To reproduce the results reported in the manuscript:

1. Run incidence analysis:
```r
source("Random Forest_Incidence.R")
```

2. Run abundance analysis:
```r
source("Random Forest_Abundance.R")
```

3. The script will generate:
   - `rf_incidence_final.jpg`
   - `rf_abundance_final.jpg`

## Summary
This workflow provides a reproducible and scalable framework for modeling biodiversity data using Random Forest regression, incorporating preprocessing, model tuning, evaluation, and visualization. The same approach is applied consistently to both incidence and abundance datasets.

## License
This code is released under the MIT License.

## Code Availability
The code used to generate the results in this study is available at:
https://github.com/Choi794/GAIA-BD-model-code