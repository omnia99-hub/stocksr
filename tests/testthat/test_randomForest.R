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

# Test suite for Random Forest S&P 500 Sector Analysis

# Functions under test (copied directly from main script )
load_and_preprocess_data <- function(df_cleaned2) {
  dates <- df_cleaned2[[1]]

  sector_mapping <- list(
    "microsoft" = "Technology",
    "apple" = "Technology",
    "nvidia" = "Technology",
    "broadcom" = "Technology",
    "oracle" = "Technology",
    "adobe (nas)" = "Technology",
    "salesforce" = "Technology",
    "cisco systems" = "Technology",
    "qualcomm" = "Technology",
    "intel" = "Technology",
    "texas instruments" = "Technology",
    "advanced micro devices" = "Technology",
    "jp morgan chase & co." = "Financial",
    "visa 'a'" = "Financial",
    "mastercard" = "Financial",
    "bank of america" = "Financial",
    "citigroup" = "Financial",
    "wells fargo & co" = "Financial",
    "morgan stanley" = "Financial",
    "exxon mobil" = "Energy",
    "chevron" = "Energy",
    "conocophillips" = "Energy",
    "schlumberger" = "Energy",
    "eog res." = "Energy",
    "johnson & johnson" = "Health Care",
    "unitedhealth group" = "Health Care",
    "eli lilly" = "Health Care",
    "pfizer" = "Health Care",
    "merck & company" = "Health Care",
    "amazon.com" = "Consumer Discretionary",
    "tesla" = "Consumer Discretionary",
    "home depot" = "Consumer Discretionary",
    "mcdonald's" = "Consumer Discretionary",
    "nike 'b'" = "Consumer Discretionary",
    "alphabet a" = "Communication Services",
    "walt disney" = "Communication Services",
    "netflix" = "Communication Services",
    "comcast a" = "Communication Services",
    "t-mobile us" = "Communication Services"
  )

  sectors <- list()
  sector_names <- unique(unlist(sector_mapping))

  for (sector_name in sector_names) {
    sector_companies <- names(sector_mapping)[sapply(sector_mapping,
                                     function(x) x == sector_name)] #nolint

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

  sectors
}

calculate_sector_index <- function(sector_df) {
  sector_df$Date <- as.Date(sector_df$Date)
  company_cols <- setdiff(colnames(sector_df), "Date")

  sector_df <- sector_df %>%
    mutate(across(all_of(company_cols), ~ na.approx(., na.rm = FALSE))) %>%
    tidyr::fill(all_of(company_cols), .direction = "downup")

  index_df <- sector_df %>%
    mutate(sector_index = rowMeans(select(., all_of(company_cols)),
                                   na.rm = TRUE)) %>%
    select(Date, sector_index)

  index_df
}

calculate_technical_indicators <- function(df) {
  df_copy <- df
  df_copy$Date <- as.Date(df_copy$Date)
  df_copy$log_price <- log(df_copy$sector_index)
  df_copy$returns <- c(NA, diff(df_copy$sector_index) /
                         lag(df_copy$sector_index, 1)[-1])
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
  df_copy$volatility5 <- rollapply(df_copy$log_returns, width = 5,
                                   FUN = sd, fill = NA, align = "right")
  df_copy$volatility20 <- rollapply(df_copy$log_returns, width = 20,
                                    FUN = sd, fill = NA, align = "right")

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

  df_copy
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

  list(
    model = rf_model,
    rmse = rmse,
    mae = mae,
    r2 = r2,
    importance = imp_df
  )
}

# Create a mock dataset for testing
create_mock_data <- function() {
  set.seed(123)
  dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
  companies <- c(
    "microsoft", "apple", "nvidia", "broadcom", "oracle",
    "jp morgan chase & co.", "visa 'a'", "mastercard",
    "exxon mobil", "chevron", "conocophillips",
    "johnson & johnson", "unitedhealth group",
    "amazon.com", "tesla", "home depot",
    "alphabet a", "walt disney", "netflix"
  )

  # Create mock stock data frame
  mock_stocks <- data.frame(Date = dates)

  for (company in companies) {
    # Start price between 50 and 500
    start_price <- runif(1, 50, 500)
    # Random walk with drift
    prices <- numeric(length(dates))
    prices[1] <- start_price

    for (i in 2:length(dates)) {
      # Daily return between -3% and 3% with some autocorrelation
      daily_return <- rnorm(1, 0.0005, 0.02) + 0.3 *
        (prices[i - 1] / prices[max(1, i - 2)] - 1)
      prices[i] <- prices[i - 1] * (1 + daily_return)
    }

    # Ensure no negative prices
    prices <- pmax(prices, 0.01)

    # Add to data frame
    mock_stocks[[company]] <- prices
  }

  mock_stocks
}

# Create the mock data for tests
stocksr <- create_mock_data()

# TESTS BELOW HERE
test_that("Data loading and preprocessing works", {
  sectors <- load_and_preprocess_data(stocksr)

  # Check if sectors are correctly identified
  expect_true("Technology" %in% names(sectors))
  expect_true("Financial" %in% names(sectors))
  expect_true("Energy" %in% names(sectors))

  # Check structure of sector dataframes
  tech_sector <- sectors[["Technology"]]
  expect_true("Date" %in% colnames(tech_sector))
  expect_true("microsoft" %in% colnames(tech_sector))
  expect_true("apple" %in% colnames(tech_sector))

  # Check if dates are maintained
  expect_equal(as.character(tech_sector$Date[1]), "2020-01-01")
})

test_that("Sector index calculation works", {
  sectors <- load_and_preprocess_data(stocksr)
  tech_sector <- sectors[["Technology"]]

  index_df <- calculate_sector_index(tech_sector)

  # Check structure
  expect_equal(ncol(index_df), 2)
  expect_true("Date" %in% colnames(index_df))
  expect_true("sector_index" %in% colnames(index_df))

  # Check that index values are reasonable
  expect_true(all(index_df$sector_index > 0, na.rm = TRUE))
})

test_that("Technical indicators calculation works", {
  sectors <- load_and_preprocess_data(stocksr)
  tech_sector <- sectors[["Technology"]]
  index_df <- calculate_sector_index(tech_sector)

  tech_df <- calculate_technical_indicators(index_df)

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
    expect_true(col %in% colnames(tech_df),
      info = paste("Column", col, "is missing")
    )
  }

  # Check that NAs are handled
  expect_true(all(!is.na(tech_df)))

  # Check that time features are correctly formatted
  expect_true(is.factor(tech_df$day_of_week))
  expect_true(is.factor(tech_df$month))
})

test_that("Random Forest model building works", {
  # Use a smaller subset for faster testing
  sectors <- load_and_preprocess_data(stocksr)
  tech_sector <- sectors[["Technology"]]
  # Use only 2022 data for faster testing
  tech_sector_subset <- tech_sector %>%
    filter(Date >= as.Date("2022-01-01"))

  index_df <- calculate_sector_index(tech_sector_subset)
  tech_df <- calculate_technical_indicators(index_df)

  # Skip if too few rows after preprocessing
  if (nrow(tech_df) < 30) {
    skip("Not enough data points after preprocessing")
  }

  model_results <- build_random_forest_model(tech_df, "Technology")

  # Check model structure
  expect_true("model" %in% names(model_results))
  expect_true("rmse" %in% names(model_results))
  expect_true("mae" %in% names(model_results))
  expect_true("r2" %in% names(model_results))
  expect_true("importance" %in% names(model_results))

  # Check model performance
  expect_true(model_results$rmse >= 0)
  expect_true(model_results$mae >= 0)
  expect_true(model_results$r2 <= 1)

  # Check importance dataframe
  expect_equal(ncol(model_results$importance), 2)
  expect_true("Feature" %in% colnames(model_results$importance))
  expect_true("Importance" %in% colnames(model_results$importance))
})

# Custom test to check extreme value handling
test_that("Handles extreme values appropriately", {
  # Create a copy of our mock data with some extreme values
  extreme_data <- stocksr

  # Insert some extreme values
  tech_cols <- grep("microsoft|apple|nvidia", names(extreme_data), value = TRUE)
  row_idx <- sample(nrow(extreme_data), 5)

  for (col in tech_cols) {
    extreme_data[row_idx, col] <- 9999999
  }

  # Process this data
  sectors <- load_and_preprocess_data(extreme_data)
  tech_sector <- sectors[["Technology"]]
  index_df <- calculate_sector_index(tech_sector)

  # The sector index should handle these values without error
  tech_df <- calculate_technical_indicators(index_df)

  # Data should still be processable
  expect_true(nrow(tech_df) > 0)
})

# Do not use test_file() at the end
message("All tests completed.")
