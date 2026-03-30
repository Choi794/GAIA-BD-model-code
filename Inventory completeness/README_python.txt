## Overview

This script estimates inventory completeness for biodiversity data using a species accumulation approach.
For each spatial unit (hexagon), species richness is accumulated based on random permutations of observation order, and completeness is approximated from the asymptotic behavior of the accumulation curve.

## System Requirements

Tested on:
- Windows 11
- Python 3.12

Required packages:
- pandas
- numpy

Non-standard hardware:
- None

## Installation

Install the required Python packages:

`pip install pandas numpy`
Installation takes approximately 5–10 minutes on a standard desktop computer.

## Input Data Requirements

Each input file must be a .csv file containing the following columns:

- hexID : Identifier for the spatial unit (hexagon)
- species : Species name or identifier
- LAT : Latitude of the observation
- LON : Longitude of the observation
- NumObs : Number of observations for each species
- kingdom : Taxonomic kingdom of the species

Each input file is expected to correspond to a single hexagon (hexID), as the dataset is pre-split by spatial unit prior to processing.

Note: Only hexID, species, and NumObs are directly used in the current computation. Other columns are retained for metadata purposes.

## Demo
To run the script:
1. Open a terminal or command prompt
2. Make sure Python 3.12 and the required packages are installed
3. Ensure that `file_list.txt` and the example input files are in the working directory
4. Run the script:
```bash 
python Inventory_completeness.py 0 3
```   

Example input files:
`hexID_all3314.csv`
`hexID_all13104.csv`
`hexID_all23835.csv`

Expected output:
`hexID_all3314_inventory_completeness.csv`
`hexID_all13104_inventory_completeness.csv`
`hexID_all23835_inventory_completeness.csv`

The demo runs in approximately 1–5 minutes on a standard desktop computer.

## Method Description
### 1. Data Preparation

For each hexagon:

Species-level observation counts (`NumObs`) are aggregated.
Observations are expanded into a binary matrix in which:
- rows represent individual observations
- columns represent species
- presence is encoded as 1

###  2. Species Accumulation Curve
The order of observations is randomly permuted
Species richness is accumulated sequentially
This process is repeated 100 times (num_permutations = 100)
The mean species accumulation curve is computed across permutations

### 3. Inventory Completeness Estimation

Inventory completeness is approximated using the slope of the final 10% of the species accumulation curve:

The last 10% of the curve is treated as the asymptotic region
The slope is calculated as:

slope = (richness_end − richness_at_90%_of_samples) / (number_of_samples − index_90)

where index_90 corresponds to the sample index at 90% of the total sampling effort.

A lower slope indicates higher sampling completeness (i.e., fewer new species being discovered)

## Output

For each input file, a `.csv` file is generated.

**File naming convention:**

Output files follow the pattern:

    <input_filename>_inventory_completeness.csv

For example:

    hexID_all13071_inventory_completeness.csv

**Columns:**

- hexID : Spatial unit identifier  
- slope : Estimated completeness proxy  
- NumObs : Total number of observations  
- NumSpecies : Total number of species  

Each output file corresponds to a single hexagon.

For downstream analyses, individual output files can be combined into a single dataset (e.g., by concatenating all files) to obtain completeness estimates across all spatial units.

## Example Data

The following sample input files are provided for testing the script:

- `hexID_all3314.csv`
- `hexID_all13104.csv`
- `hexID_all23835.csv`

These files follow the required input format described above.

## Expected Output

Running the script on the example data will generate the following output files:

- hexID_all3314_inventory_completeness.csv  
- hexID_all13104_inventory_completeness.csv  
- hexID_all23835_inventory_completeness.csv  

These outputs contain the estimated completeness metrics for each corresponding hexagon.

The output values may vary slightly due to random permutations, but differences are minimal.

## Important Notes
### 1. Randomness and Reproducibility

The species accumulation curve is computed using random permutations of observation order.

As a result, outputs may vary slightly between runs
No random seed is fixed in the current implementation

However:

Each estimate is based on 100 permutations
Given the typically large number of observations per hexagon,
variation across runs is minimal and does not materially affect the results

### 2. Interpretation of Completeness
The reported value (slope) is not a direct completeness percentage
It is a proxy metric derived from the asymptotic behavior of the accumulation curve

Interpretation of raw output:

Lower slope → higher completeness
Higher slope → lower completeness

### 3. Post-processing of Completeness Metric

For interpretability, the final completeness metric used in downstream analyses is defined as:

completeness = 1 − slope

This transformation was applied outside of this script and is therefore not reflected in the output files.

Under this definition:

Higher values indicate higher completeness
Lower values indicate lower completeness

Users should note that the raw output (slope) and the transformed completeness metric have opposite interpretations.

### 4. Minimum Data Requirement

- Hexagons with fewer than 10 total records are excluded from analysis  

### 5. Computational Considerations

- The method expands observations into individual records, which may increase memory usage for large datasets  
- Processing is performed file-by-file using a list of input files (`file_list.txt`)  

### 6. Rationale for Per-hexagon Processing

The data are processed separately for each hexagon (hexID), and results are generated as individual output files.

This design enables efficient handling of large-scale datasets by:

- Reducing memory usage during computation  
- Allowing parallel or batch processing across multiple files  
- Simplifying error tracking and re-processing for specific spatial units  

The resulting per-hexagon outputs can be combined in a post-processing step to create a complete dataset for downstream analyses.

________________________________________

## Usage

The script is executed with command-line arguments specifying the range of files to process:

```bash
python script.py start_index end_index
```

**Example:**

```bash 
python Inventory_completeness.py 0 3
```   


In this example, files at index 0, 1, and 2 in `file_list.txt` will be processed.

Where:

file_list.txt contains the list of input file names (one per line), e.g.:

hexID_all3314.csv
hexID_all13104.csv
hexID_all23835.csv

start_index and end_index define the subset of files to process

To apply the script to new data:
- Prepare one `.csv` file per hexagon
- Ensure that each file contains the required columns
- Add the filenames to `file_list.txt`
- Run the script with the appropriate start and end indices

## Reproducibility

To reproduce the results reported in the manuscript:

1. Prepare the split input files, one `.csv` file per hexagon
2. Create `file_list.txt` listing the input files to process
3. Run the script, for example:
```bash 
python Inventory_completeness.py 0 3
```   
## Summary
This workflow provides a scalable and robust approach to estimating inventory completeness across large biodiversity datasets using species accumulation curves and permutation-based averaging.

## License
This code is released under the MIT License.

## Code Availability
The code used to generate the results in this study is available at:
https://github.com/Choi794/GAIA-BD-model-code