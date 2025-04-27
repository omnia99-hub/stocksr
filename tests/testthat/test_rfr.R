library(testthat)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(randomForest)
library(caret)
library(TTR)
library(zoo)
library(ranger)

# Test suite for Random Forest Energy and Healthcare Sector Analysis

# Functions under test (copied directly from main script with minor modifications)
load_and_preprocess_data <- function(df_cleaned2) {
  dates <- df_cleaned2[[1]]

  # Only include Energy and Healthcare sectors
  sector_mapping <- list(
    "exxon mobil" = "Energy",
    "chevron" = "Energy",
    "conocophillips" = "Energy",
    "schlumberger" = "Energy",
    "eog res." = "Energy",
    "johnson & johnson" = "Health Care",
    "unitedhealth group" = "Health Care",
    "eli lilly" = "Health Care",
    "pfizer" = "Health Care",
    "merck & company" = "Health Care"
  )

  sectors <- list()
  sector_names <- unique(unlist(sector_mapping))

  for (sector_name in sector_names) {
    sector_companies <- names(sector_mapping)[sapply(sector_mapping, function(x) x == sector_name)]

    sector_df <- data.frame(Date = dates)

    for (company in sector_companies) {
      if (company %in% colnames(df_cleaned2)) {
        sector_df[[company]] <- df_cleaned2[[company]]
      }
    }

    if (ncol(sector_df) > 1) {
      sectors[[sector_name]] <- sector_df
    }
  }

  return(sectors)
}

calculate_sector_index <- function(sector_df) {
  sector_df$Date <- as.Date(sector_df$Date)
  company_cols <- setdiff(colnames(sector_df), "Date")

  sector_df <- sector_df %>%
    mutate(across(all_of(company_cols), ~na.approx(., na.rm = FALSE))) %>%
    tidyr::fill(all_of(company_cols), .direction = "downup")

  index_df <- sector_df %>%
    mutate(sector_index = rowMeans(select(., all_of(company_cols)), na.rm = TRUE)) %>%
    select(Date, sector_index)

  return(index_df)
}

calculate_technical_indicators <- function(df) {
  df_copy <- df
  df_copy$Date <- as.Date(df_copy$Date)
  df_copy$log_price <- log(df_copy$sector_index)
  df_copy$returns <- c(NA, diff(df_copy$sector_index) / lag(df_copy$sector_index, 1)[-1])
  df_copy$log_returns <- c(NA, diff(df_copy$log_price))

  # Moving averages
  ma_windows <- c(5, 10, 20, 50)
  for (w in ma_windows) {
    df_copy[[paste0("MA", w)]] <- SMA(df_copy$sector_index, n = w)
    df_copy[[paste0("EMA", w)]] <- EMA(df_copy$sector_index, n = w)
  }

  # MACD
  macd <- MACD(df_copy$sector_index, nFast = 12, nSlow = 26, nSig = 9)
  df_copy$MACD <- macd[, "macd"]
  df_copy$MACD_signal <- macd[, "signal"]

  # Volatility
  df_copy$volatility5 <- rollapply(df_copy$log_returns, width = 5, FUN = sd, fill = NA, align = "right")
  df_copy$volatility20 <- rollapply(df_copy$log_returns, width = 20, FUN = sd, fill = NA, align = "right")

  # Lagged features
  for (lag in c(1, 2, 5)) {
    df_copy[[paste0("lag_", lag)]] <- lag(df_copy$sector_index, lag)
    df_copy[[paste0("log_return_lag_", lag)]] <- lag(df_copy$log_returns, lag)
  }

  # Time features
  df_copy$day_of_week <- lubridate::wday(df_copy$Date, label = TRUE)
  df_copy$month <- lubridate::month(df_copy$Date, label = TRUE)

  # Remove NAs
  df_copy <- df_copy %>%
    drop_na() %>%
    filter_all(all_vars(!is.infinite(.)))

  return(df_copy)
}

build_random_forest_model <- function(sector_data, sector_name) {
  # Suppress outputs for testing
  old_cat <- cat
  old_print <- print
  assign("cat", function(...) invisible(NULL), envir = environment())
  assign("print", function(...) invisible(NULL), envir = environment())

  # Prepare data
  features <- sector_data %>%
    select(-Date, -sector_index, -returns, -log_price)

  target <- sector_data$sector_index

  # Split data (80% train, 20% test)
  set.seed(42)
  train_index <- createDataPartition(target, p = 0.8, list = FALSE)
  x_train <- features[train_index, ]
  x_test <- features[-train_index, ]
  y_train <- target[train_index]
  y_test <- target[-train_index]

  # Train model
  rf_model <- randomForest(
    x = x_train,
    y = y_train,
    ntree = 100, # Reduced for testing
    mtry = floor(sqrt(ncol(x_train))),
    importance = TRUE
  )

  # Predictions
  y_pred <- predict(rf_model, x_test)

  # Metrics
  mse <- mean((y_test - y_pred)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(y_test - y_pred))
  r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

  # Get importance
  imp <- randomForest::importance(rf_model)
  imp_df <- data.frame(
    Feature = rownames(imp),
    Importance = imp[, "%IncMSE"]
  ) %>%
    arrange(desc(Importance))

  # Restore print/cat functions
  assign("cat", old_cat, envir = environment())
  assign("print", old_print, envir = environment())

  return(list(
    model = rf_model,
    rmse = rmse,
    mae = mae,
    r2 = r2,
    importance = imp_df
  ))
}

compare_sectors <- function(sector_models) {
  # Create comparison dataframe
  comparison <- data.frame(
    Sector = names(sector_models),
    RMSE = sapply(sector_models, function(x) ifelse(is.na(x$rmse), NA, x$rmse)),
    MAE = sapply(sector_models, function(x) ifelse(is.na(x$mae), NA, x$mae)),
    R2 = sapply(sector_models, function(x) ifelse(is.na(x$r2), NA, x$r2))
  ) %>%
    filter(!is.na(R2)) %>%
    arrange(desc(R2))

  if (nrow(comparison) > 0) {
    # Print which sector performs better
    if (nrow(comparison) == 2) {
      better_sector <- comparison$Sector[1]
      worse_sector <- comparison$Sector[2]
      cat("\nBetter performing sector:", better_sector, "with R² =", round(comparison$R2[1], 3), "\n")
      cat("Worse performing sector:", worse_sector, "with R² =", round(comparison$R2[2], 3), "\n")
    }
  } else {
    cat("No valid models to compare\n")
  }

  return(comparison)
}

# Create a mock dataset for testing
create_mock_data <- function() {
  set.seed(123)
  dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")

  # Only include Energy and Healthcare companies
  companies <- c(
    "exxon mobil", "chevron", "conocophillips", "schlumberger", "eog res.",
    "johnson & johnson", "unitedhealth group", "eli lilly", "pfizer", "merck & company"
  )

  # Create mock stock data frame
  mock_stocks <- data.frame(Date = dates, check.names = FALSE)

  for (company in companies) {
    # Different volatility and trends for Energy vs Healthcare
    volatility <- ifelse(company %in% c("exxon mobil", "chevron", "conocophillips", "schlumberger", "eog res."),
                         0.025, 0.015)  # Higher volatility for Energy

    trend <- ifelse(company %in% c("exxon mobil", "chevron", "conocophillips", "schlumberger", "eog res."),
                    0.0004, 0.0006)  # Stronger trend for Healthcare

    # Start price between 50 and 500
    start_price <- runif(1, 50, 500)
    # Random walk with drift
    prices <- numeric(length(dates))
    prices[1] <- start_price

    for (i in 2:length(dates)) {
      # Daily return with sector-specific volatility and trend
      daily_return <- rnorm(1, trend, volatility) + 0.3 * (prices[i-1] / prices[max(1, i-2)] - 1)
      prices[i] <- prices[i-1] * (1 + daily_return)
    }

    # Ensure no negative prices
    prices <- pmax(prices, 0.01)

    # Add to data frame
    mock_stocks[[company]] <- prices
  }

  return(mock_stocks)
}

# Create the mock data for tests
stocksr <- create_mock_data()

# TESTS BELOW HERE
test_that("Data loading and preprocessing works", {
  sectors <- load_and_preprocess_data(stocksr)

  # Check if sectors are correctly identified
  expect_true("Energy" %in% names(sectors))
  expect_true("Health Care" %in% names(sectors))
  expect_equal(length(sectors), 2)  # Only 2 sectors

  # Check structure of Energy sector dataframe
  energy_sector <- sectors[["Energy"]]
  expect_true("Date" %in% colnames(energy_sector))
  expect_true("exxon mobil" %in% colnames(energy_sector))
  expect_true("chevron" %in% colnames(energy_sector))

  # Check structure of Health Care sector dataframe
  healthcare_sector <- sectors[["Health Care"]]
  expect_true("Date" %in% colnames(healthcare_sector))
  expect_true("johnson & johnson" %in% colnames(healthcare_sector))
  expect_true("pfizer" %in% colnames(healthcare_sector))

  # Check if dates are maintained
  expect_equal(as.character(energy_sector$Date[1]), "2020-01-01")
})

test_that("Sector index calculation works", {
  sectors <- load_and_preprocess_data(stocksr)
  energy_sector <- sectors[["Energy"]]
  healthcare_sector <- sectors[["Health Care"]]

  energy_index <- calculate_sector_index(energy_sector)
  healthcare_index <- calculate_sector_index(healthcare_sector)

  # Check structure
  expect_equal(ncol(energy_index), 2)
  expect_equal(ncol(healthcare_index), 2)
  expect_true("Date" %in% colnames(energy_index))
  expect_true("sector_index" %in% colnames(healthcare_index))

  # Check that index values are reasonable
  expect_true(all(energy_index$sector_index > 0, na.rm = TRUE))
  expect_true(all(healthcare_index$sector_index > 0, na.rm = TRUE))
})

test_that("Technical indicators calculation works", {
  sectors <- load_and_preprocess_data(stocksr)
  energy_sector <- sectors[["Energy"]]
  healthcare_sector <- sectors[["Health Care"]]

  energy_index <- calculate_sector_index(energy_sector)
  healthcare_index <- calculate_sector_index(healthcare_sector)

  energy_df <- calculate_technical_indicators(energy_index)
  healthcare_df <- calculate_technical_indicators(healthcare_index)

  # Check that important columns are created
  expected_columns <- c(
    "Date", "sector_index", "log_price", "returns", "log_returns",
    "MA5", "MA10", "MA20", "MA50", "EMA5", "EMA10", "EMA20", "EMA50",
    "MACD", "MACD_signal", "volatility5", "volatility20",
    "lag_1", "lag_2", "lag_5",
    "log_return_lag_1", "log_return_lag_2", "log_return_lag_5",
    "day_of_week", "month"
  )

  for (col in expected_columns) {
    expect_true(col %in% colnames(energy_df),
                info = paste("Column", col, "is missing in Energy data"))
    expect_true(col %in% colnames(healthcare_df),
                info = paste("Column", col, "is missing in Healthcare data"))
  }

  # Check that NAs are handled
  expect_true(all(!is.na(energy_df)))
  expect_true(all(!is.na(healthcare_df)))

  # Check that time features are correctly formatted
  expect_true(is.factor(energy_df$day_of_week))
  expect_true(is.factor(healthcare_df$month))
})

test_that("Random Forest model building works for both sectors", {
  # Use a smaller subset for faster testing
  sectors <- load_and_preprocess_data(stocksr)
  energy_sector <- sectors[["Energy"]]
  healthcare_sector <- sectors[["Health Care"]]

  # Use only 2022 data for faster testing
  energy_subset <- energy_sector %>%
    filter(Date >= as.Date("2022-01-01"))
  healthcare_subset <- healthcare_sector %>%
    filter(Date >= as.Date("2022-01-01"))

  energy_index <- calculate_sector_index(energy_subset)
  healthcare_index <- calculate_sector_index(healthcare_subset)

  energy_df <- calculate_technical_indicators(energy_index)
  healthcare_df <- calculate_technical_indicators(healthcare_index)

  # Skip if too few rows after preprocessing
  if (nrow(energy_df) < 30 || nrow(healthcare_df) < 30) {
    skip("Not enough data points after preprocessing")
  }

  energy_results <- build_random_forest_model(energy_df, "Energy")
  healthcare_results <- build_random_forest_model(healthcare_df, "Health Care")

  # Check model structure
  expect_true("model" %in% names(energy_results))
  expect_true("rmse" %in% names(healthcare_results))
  expect_true("r2" %in% names(energy_results))
  expect_true("importance" %in% names(healthcare_results))

  # Check model performance
  expect_true(energy_results$rmse >= 0)
  expect_true(healthcare_results$mae >= 0)
  expect_true(energy_results$r2 <= 1)
  expect_true(healthcare_results$r2 <= 1)
})

# Custom test to check extreme value handling
test_that("Handles extreme values appropriately", {
  # Create a copy of our mock data with some extreme values
  extreme_data <- stocksr

  # Insert some extreme values
  energy_cols <- grep("exxon|chevron", names(extreme_data), value = TRUE)
  healthcare_cols <- grep("johnson|pfizer", names(extreme_data), value = TRUE)

  row_idx <- sample(nrow(extreme_data), 5)

  for (col in energy_cols) {
    extreme_data[row_idx, col] <- 9999999
  }

  # Process this data
  sectors <- load_and_preprocess_data(extreme_data)
  energy_sector <- sectors[["Energy"]]
  healthcare_sector <- sectors[["Health Care"]]

  energy_index <- calculate_sector_index(energy_sector)
  healthcare_index <- calculate_sector_index(healthcare_sector)

  # The sector index should handle these values without error
  energy_df <- calculate_technical_indicators(energy_index)
  healthcare_df <- calculate_technical_indicators(healthcare_index)

  # Data should still be processable
  expect_true(nrow(energy_df) > 0)
  expect_true(nrow(healthcare_df) > 0)
})


# Do not use test_file() at the end, as that's causing the "path does not exist" error
message("All Energy and Healthcare sector tests completed.")
