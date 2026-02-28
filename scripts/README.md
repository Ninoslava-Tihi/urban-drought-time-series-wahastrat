# Scripts Directory

This directory contains all R scripts used for data processing, model implementation, validation, and figure generation in the study:

**"Time Series Forecasting of Climatic Drivers for Urban Drought Monitoring in Sustainable Smart Cities."**

## Overview of Scripts

### 1_Holdout_Analysis.R
Performs data import and preprocessing of the aggregated monthly climatic dataset.  
Main steps include:
- Loading the dataset (`/monthly-data.xlsx`)
- Converting Year–Month information into a continuous Date index
- Data validation and structural checks
Implements classical time series forecasting models using:
- (S)ARIMA
- Holt–Winters (ETS)

The script applies an 80/20 holdout validation strategy and computes:
- RMSE
- MAE
- MAPE

### 2_Rolling_Origin_Analysis.R
Performs rolling-origin time series cross-validation with expanding training windows.
Outputs include:
- Mean and standard deviation of forecast errors
- Performance comparison across 48 validation folds

### 3_Figures.R
Generates all figures included in the manuscript:
- Figure 1: Monthly aggregated climatic time series (2014–2020)
- Figure 2: Climatological monthly averages
- Figure 3: Observed vs. forecasted temperature under holdout validation

## Reproducibility Notes

- All scripts assume the dataset is located in `/monthly-data.xlsx`.
- Required R packages include: `dplyr`, `ggplot2`, `readxl`, `stringr`, `tibble`, `patchwork`, and `forecast`.
- Scripts are designed to be executed sequentially (01 → 03).

## Computational Environment

The analysis was conducted using R (version 4.x or later).  
All code is fully reproducible given the provided dataset and package dependencies.
