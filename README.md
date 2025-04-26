
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stocksr

<!-- badges: start -->

[![R-CMD-check](https://github.com/omnia99-hub/stocksr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/omnia99-hub/stocksr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## The **`stocksr`** R package is designed to facilitate the cleaning, transformation, analysis, and modeling of sector-level stock market data, specifically leveraging S&P 500 daily price data (2010–2020). It is useful for financial data scientists, analysts, and students exploring time-series modeling, technical indicators, and sector-based financial insights.

## Overview

This package provides a comprehensive framework to: - Load and clean raw
stock data from Excel files - Segment data by market sectors (e.g.,
Technology, Healthcare) - Compute key technical indicators (e.g., MACD,
RSI, Bollinger Bands) - Construct machine learning models such as Random
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
#> Warning: package 'caret' was built under R version 4.4.3
#> Loading required package: ggplot2
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
#> The following object is masked from 'package:testthat':
#> 
#>     matches
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(readxl)
```

# Step 1: Load and preprocess raw CSV data

``` r
data(df_cleaned2)
sectors <- load_and_preprocess_data(df_cleaned2)
# Cleanly preview first few rows of each sector
head(sectors$Energy,5)
#>         Date chevron conocophillips schlumberger
#> 1 2010-01-01   76.99        38.9317        65.09
#> 2 2010-01-04   79.06        40.0828        67.11
#> 3 2010-01-05   79.62        40.1209        67.30
#> 4 2010-01-06   79.63        40.4106        68.80
#> 5 2010-01-07   79.33        40.2505        69.51
```

# Step 2: Calculate sector index for Technology

``` r
health_care <- calculate_sector_index(sectors[["Health Care"]])
head(health_care)
#>         Date sector_index
#> 1 2010-01-01      17.2432
#> 2 2010-01-04      17.9447
#> 3 2010-01-05      17.6887
#> 4 2010-01-06      17.6318
#> 5 2010-01-07      17.5655
#> 6 2010-01-08      17.7077
```

# Step 3: Generate technical indicators

``` r
features <- calculate_technical_indicators(health_care)
head(features)
#>         Date sector_index log_price      returns  log_returns      MA5     EMA5
#> 1 2010-03-11      16.3900  2.796671  0.008162487  0.008129354 16.39950 16.39347
#> 2 2010-03-12      16.1909  2.784449 -0.012147651 -0.012222037 16.32366 16.32594
#> 3 2010-03-15      16.3616  2.794937  0.010542959  0.010487770 16.30660 16.33783
#> 4 2010-03-16      16.3616  2.794937  0.000000000  0.000000000 16.31228 16.34575
#> 5 2010-03-17      16.3142  2.792036 -0.002897027 -0.002901232 16.32366 16.33524
#> 6 2010-03-18      16.3332  2.793200  0.001164630  0.001163952 16.31230 16.33456
#>       MA10    EMA10     MA20    EMA20     MA50    EMA50      MACD MACD_signal
#> 1 16.49904 16.49320 16.67821 16.69417 17.34120 17.34120 -1.699582   -1.696744
#> 2 16.45448 16.43823 16.64408 16.64624 17.32015 17.29609 -1.740930   -1.705582
#> 3 16.40803 16.42430 16.61848 16.61913 17.28849 17.25944 -1.671640   -1.698793
#> 4 16.37580 16.41290 16.59669 16.59461 17.26195 17.22423 -1.598514   -1.678737
#> 5 16.36537 16.39495 16.57488 16.56790 17.23560 17.18855 -1.546158   -1.652222
#> 6 16.35590 16.38373 16.55118 16.54555 17.21095 17.15500 -1.478517   -1.617481
#>   volatility5 volatility20   lag_1 log_return_lag_1   lag_2 log_return_lag_2
#> 1 0.008136585  0.008459427 16.2573     -0.004657807 16.3332     -0.006937163
#> 2 0.007645080  0.008774497 16.3900      0.008129354 16.2573     -0.004657807
#> 3 0.009872557  0.009207111 16.1909     -0.012222037 16.3900      0.008129354
#> 4 0.009307919  0.009185786 16.3616      0.010487770 16.1909     -0.012222037
#> 5 0.009102659  0.009186506 16.3616      0.000000000 16.3616      0.010487770
#> 6 0.008165983  0.009139948 16.3142     -0.002901232 16.3616      0.000000000
#>     lag_5 log_return_lag_5 day_of_week month
#> 1 16.4279     0.0005723611         Thu   Mar
#> 2 16.5701     0.0086187575         Fri   Mar
#> 3 16.4469    -0.0074628568         Mon   Mar
#> 4 16.3332    -0.0069371635         Tue   Mar
#> 5 16.2573    -0.0046578075         Wed   Mar
#> 6 16.3900     0.0081293541         Thu   Mar
```

# Step 4: Train a Forecast random forest model

``` r
results <- build_random_forest_model(features, sector_name = "Health Care")
#> 
#> --- Building Random Forest model for Health Care sector ---
#> Model performance:
#> RMSE: 0.3124 
#> MAE: 0.1952 
#> R<U+00B2>: 0.9981
```

## Information

- The code in this repository was written by Shalu Shalu, Akhila Akkala,
  Omnia Dafalla, and Amanda Wijesinghe.
