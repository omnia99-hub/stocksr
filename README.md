
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stocksr

<!-- badges: start -->

[![R-CMD-check](https://github.com/omnia99-hub/stocksr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/omnia99-hub/stocksr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## The **stocksr** R package provides tools for cleaning, transforming, analyzing, and modeling sector-level stock market data, specifically focused on the Energy and Healthcare sectors using S&P 500 daily price data (2010–2020). It supports financial data analysis, time-series forecasting, and feature engineering for sector-specific insights.

## Overview

This package provides a comprehensive framework to: - Load and clean raw
stock data from Excel files - Segment data by market sectors (e.g.,
Energy, Healthcare) - Compute key technical indicators (e.g., MACD, RSI,
Bollinger Bands) - Construct machine learning models such as Random
Forests - Compare model performance across different market sectors

### Data Preparation

- `load_and_preprocess_data()`: Load data and organize by sectors.
- `calculate_sector_index()`: Aggregate individual stock prices into
  sector-level index series.

### Feature Engineering

- `calculate_technical_indicators()`: Compute popular technical
  indicators (RSI, MACD, etc.) for each sector.
- `prepare_data_for_lstm()`: Format data into 3D sequences for LSTM
  input.

### Modeling

- `build_random_forest_model()`: Train a Random Forest model to predict
  stock movements based on engineered features.
- `build_lstm_model()`: Train a LSTM to predict stock movements based on
  engineered features.

## Installation

You can install the development version of stocksr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("omnia99-hub/stocksr")
```

## Example

``` r
devtools::load_all(".")
#> ℹ Loading stocksr
#> Warning: package 'testthat' was built under R version 4.4.2
#> Warning: package 'caret' was built under R version 4.4.3
#> Loading required package: ggplot2
#> Warning: package 'ggplot2' was built under R version 4.4.3
#> Loading required package: lattice
#> Warning: package 'randomForest' was built under R version 4.4.3
#> randomForest 4.7-1.2
#> Type rfNews() to see new features/changes/bug fixes.
#> 
#> Attaching package: 'randomForest'
#> 
#> The following object is masked from 'package:ggplot2':
#> 
#>     margin
#> Warning: package 'torch' was built under R version 4.4.3
#> Warning: package 'zoo' was built under R version 4.4.3
#> 
#> Attaching package: 'zoo'
#> 
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
#> Warning: package 'TTR' was built under R version 4.4.3
#> Warning: package 'ranger' was built under R version 4.4.3
#> 
#> Attaching package: 'ranger'
#> 
#> The following object is masked from 'package:randomForest':
#> 
#>     importance
#> 
#> The following object is masked from 'package:stocksr':
#> 
#>     importance
library(stocksr)
```

``` r
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:randomForest':
#> 
#>     combine
#> The following object is masked from 'package:stocksr':
#> 
#>     filter
#> The following object is masked from 'package:testthat':
#> 
#>     matches
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

# Step 1: Load and preprocess raw CSV data

``` r
data(df_cleaned2)
sectors <- load_and_preprocess_data(df_cleaned2)
#> [1] "Energy"     "Healthcare"
head(sectors$Healthcare,5)
#>         Date johnson_&_johnson unitedhealth_group eli_lilly  pfizer
#> 1 2010-01-01             64.41              30.48     35.71 17.2432
#> 2 2010-01-04             64.68              31.53     35.82 17.9447
#> 3 2010-01-05             63.93              31.48     35.19 17.6887
#> 4 2010-01-06             64.45              31.79     35.45 17.6318
#> 5 2010-01-07             63.99              33.01     35.27 17.5655
#>   merck_&_company
#> 1         34.8418
#> 2         35.2900
#> 3         35.4330
#> 4         35.9098
#> 5         35.9670
head(sectors$Energy,5)
#>         Date exxon_mobil chevron conocophillips schlumberger eog_res.
#> 1 2010-01-01       68.19   76.99        38.9317        65.09   48.650
#> 2 2010-01-04       69.15   79.06        40.0828        67.11   50.190
#> 3 2010-01-05       69.42   79.62        40.1209        67.30   49.575
#> 4 2010-01-06       70.02   79.63        40.4106        68.80   49.740
#> 5 2010-01-07       69.80   79.33        40.2505        69.51   49.275
```

# Step 2: Calculate sector index

``` r
health_care <- calculate_sector_index(sectors[["Healthcare"]])
head(health_care)
#>         Date sector_index
#> 1 2010-01-01     36.53700
#> 2 2010-01-04     37.05294
#> 3 2010-01-05     36.74434
#> 4 2010-01-06     37.04632
#> 5 2010-01-07     37.16050
#> 6 2010-01-08     37.11312
energy <- calculate_sector_index(sectors[["Energy"]])
head(energy)
#>         Date sector_index
#> 1 2010-01-01     59.57034
#> 2 2010-01-04     61.11856
#> 3 2010-01-05     61.20718
#> 4 2010-01-06     61.72012
#> 5 2010-01-07     61.63310
#> 6 2010-01-08     61.95324
```

# Step 3: Generate technical indicators

``` r
features_healthcare <- calculate_technical_indicators(health_care)
head(features_healthcare)
#>         Date sector_index log_price       returns   log_returns      MA5
#> 1 2010-03-11     37.01920  3.611437  0.0054477870  0.0054330015 36.90919
#> 2 2010-03-12     36.92878  3.608991 -0.0024425163 -0.0024455041 36.88744
#> 3 2010-03-15     37.09344  3.613440  0.0044588530  0.0044489417 36.93448
#> 4 2010-03-16     37.25966  3.617911  0.0044811158  0.0044711055 37.02394
#> 5 2010-03-17     37.24888  3.617622 -0.0002893209 -0.0002893628 37.10999
#> 6 2010-03-18     37.60126  3.627038  0.0094601502  0.0094156832 37.22640
#>       EMA5     MA10    EMA10     MA20    EMA20     MA50    EMA50        MACD
#> 1 36.88436 36.81487 36.81657 36.65597 36.78370 37.03687 37.03687 -0.06109699
#> 2 36.89916 36.84770 36.83697 36.69313 36.79752 37.04471 37.03264 -0.02846755
#> 3 36.96392 36.87476 36.88360 36.73853 36.82570 37.04552 37.03502  0.03305842
#> 4 37.06250 36.91520 36.95198 36.78124 36.86703 37.05583 37.04383  0.11676145
#> 5 37.12463 36.96571 37.00596 36.81093 36.90340 37.05988 37.05187  0.17856724
#> 6 37.28351 37.06780 37.11420 36.85105 36.96986 37.06869 37.07342  0.30076921
#>   MACD_signal volatility5 volatility20    lag_1 log_return_lag_1    lag_2
#> 1 -0.26048563 0.006717456  0.005906557 36.81862     0.0001700371 36.81236
#> 2 -0.21408202 0.003837536  0.005629483 37.01920     0.0054330015 36.81862
#> 3 -0.16465393 0.003492003  0.005674915 36.92878    -0.0024455041 37.01920
#> 4 -0.10837085 0.003396218  0.005614687 37.09344     0.0044489417 36.92878
#> 5 -0.05098324 0.003477397  0.005459812 37.25966     0.0044711055 37.09344
#> 6  0.01936725 0.004632110  0.005755304 37.24888    -0.0002893628 37.25966
#>   log_return_lag_2    lag_5 log_return_lag_5 day_of_week month
#> 1    -0.0012450019 36.58036    -0.0044591082         Thu   Mar
#> 2     0.0001700371 37.03756     0.0124210485         Fri   Mar
#> 3     0.0054330015 36.85822    -0.0048538726         Mon   Mar
#> 4    -0.0024455041 36.81236    -0.0012450019         Tue   Mar
#> 5     0.0044489417 36.81862     0.0001700371         Wed   Mar
#> 6     0.0044711055 37.01920     0.0054330015         Thu   Mar

features_energy <- calculate_technical_indicators(energy)
head(features_energy)
#>         Date sector_index log_price       returns   log_returns      MA5
#> 1 2010-03-11     58.62140  4.071100 -0.0031615112 -0.0031665193 58.59075
#> 2 2010-03-12     58.64934  4.071576  0.0004766178  0.0004765042 58.65267
#> 3 2010-03-15     58.39976  4.067312 -0.0042554614 -0.0042645416 58.61432
#> 4 2010-03-16     58.81604  4.074415  0.0071281115  0.0071028266 58.65877
#> 5 2010-03-17     59.38454  4.084034  0.0096657306  0.0096193163 58.77422
#> 6 2010-03-18     58.84438  4.074896 -0.0090959701 -0.0091375910 58.81881
#>       EMA5     MA10    EMA10     MA20    EMA20     MA50    EMA50       MACD
#> 1 58.44340 57.83743 58.03536 57.38825 57.78901 58.56181 58.56181 0.02443072
#> 2 58.51205 58.06203 58.14699 57.49370 57.87095 58.54339 58.56525 0.12197988
#> 3 58.47462 58.21721 58.19295 57.58667 57.92131 58.48902 58.55876 0.16259946
#> 4 58.58842 58.36021 58.30624 57.63362 58.00652 58.44120 58.56885 0.24961815
#> 5 58.85380 58.55180 58.50229 57.72242 58.13776 58.39448 58.60083 0.39252944
#> 6 58.85066 58.70478 58.56449 57.76649 58.20506 58.33871 58.61039 0.42589089
#>     MACD_signal volatility5 volatility20    lag_1 log_return_lag_1    lag_2
#> 1 -0.6294061883 0.007975557  0.009655032 58.80732     0.0036374480 58.59380
#> 2 -0.4791289742 0.003017808  0.009477097 58.62140    -0.0031665193 58.80732
#> 3 -0.3507832877 0.003143403  0.009568001 58.64934     0.0004765042 58.62140
#> 4 -0.2307030004 0.004723745  0.008218529 58.39976    -0.0042645416 58.64934
#> 5 -0.1060565119 0.006171314  0.008338556 58.81604     0.0071028266 58.39976
#> 6  0.0003329681 0.007778297  0.008589103 59.38454     0.0096193163 58.81604
#>   log_return_lag_2    lag_5 log_return_lag_5 day_of_week month
#> 1     3.925407e-05 57.31458    -2.685062e-03         Thu   Mar
#> 2     3.637448e-03 58.33972     1.772812e-02         Fri   Mar
#> 3    -3.166519e-03 58.59150     4.306470e-03         Mon   Mar
#> 4     4.765042e-04 58.59380     3.925407e-05         Tue   Mar
#> 5    -4.264542e-03 58.80732     3.637448e-03         Wed   Mar
#> 6     7.102827e-03 58.62140    -3.166519e-03         Thu   Mar
```

# Step 4: Train the random forest model

``` r
results_healtcare <- build_random_forest_model(features_healthcare, sector_name = "Healthcare")
#> 
#> --- Building Random Forest model for Healthcare sector ---
#> Model performance:
#> RMSE: 0.8883 
#> MAE: 0.4969 
#> R<U+00B2>: 0.9993
results_healtcare$prediction_plot
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r
results_healtcare$importance_plot
```

<img src="man/figures/README-unnamed-chunk-7-2.png" width="100%" />

``` r

results_energy <- build_random_forest_model(features_energy, sector_name = "Energy")
#> 
#> --- Building Random Forest model for Energy sector ---
#> Model performance:
#> RMSE: 0.805 
#> MAE: 0.5125 
#> R<U+00B2>: 0.9967
results_energy$prediction_plot
```

<img src="man/figures/README-unnamed-chunk-7-3.png" width="100%" />

``` r
results_energy$importance_plot
```

<img src="man/figures/README-unnamed-chunk-7-4.png" width="100%" /> \#
Step 5: Train the LSTM model

``` r
lstm_results_healthcare <- build_lstm_model(health_care, sector_name = "Healthcare")
#> 
#> --- Building LSTM model for Healthcare sector ---
#> Error building LSTM model:
lstm_results_energy <- build_lstm_model(energy, sector_name = "Energy")
#> 
#> --- Building LSTM model for Energy sector ---
#> Error building LSTM model:
```

## Notes

Input data must have daily stock prices with appropriate company names.

The calculate_technical_indicators function generates lagged features,
moving averages, volatility, and MACD indicators needed for modeling.

build_lstm_model and build_random_forest_model internally handle
training and performance evaluation.

## Output

Each modeling function returns:

Root Mean Squared Error (RMSE)

Mean Absolute Error (MAE)

R-squared (R²)

Trained model object

## Conclusion

The stocksr package provides a fast and easy workflow to simulate
sector-level stock forecasting, focusing only on Energy and Healthcare
sectors.

## Information

- The code in this repository was written by Shalu Shalu, Akhila Akkala,
  Omnia Dafalla, and Amanda Wijesinghe.
