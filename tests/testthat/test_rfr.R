library(testthat)
library(dplyr)
library(ggplot2)
library(lubridate)
library(randomForest)
library(caret)
library(TTR)
library(zoo)


# Test suite for Random Forest Energy and Healthcare Sector Analysis

# Functions under test (copied directly from main script with minor
# modifications)
load_and_preprocess_data <- function(df_cleaned2) {
  dates <- df_cleaned2[[1]]

  # Only include Energy and Healthcare sectors
  sector_mapping <- list(
    "exxon_mobil" = "Energy",
    "chevron" = "Energy",
    "conocophillips" = "Energy",
    "schlumberger" = "Energy",
    "eog_res." = "Energy",
    "johnson_&_johnson" = "Healthcare",
    "unitedhealth_group" = "Healthcare",
    "eli_lilly" = "Healthcare",
    "pfizer" = "Healthcare",
    "merck_&_company" = "Healthcare"
  )

  sectors <- list()
  sector_names <- unique(unlist(sector_mapping))

  for (sector_name in sector_names) {
    sector_companies <- names(sector_mapping)[sapply(sector_mapping,
                                          function(x) x == sector_name)]#nolint

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
    mutate(across(all_of(company_cols), ~na.approx(., na.rm = FALSE))) %>%
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
  df_copy$returns <-
    c(NA, diff(df_copy$sector_index) / lag(df_copy$sector_index, 1)[-1])
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
    ntree = 500, # Reduced for testing
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


  list(
    model = rf_model,
    rmse = rmse,
    mae = mae,
    r2 = r2,
    importance = imp_df
  )
}

# Mock data for testing
create_mock_dataframe <- function() {
  set.seed(123)
  dates <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day")

  mock_df <- data.frame(
    Date = dates,
    "exxon_mobil" = rnorm(length(dates), mean = 100, sd = 10),
    "chevron" = rnorm(length(dates), mean = 90, sd = 8),
    "conocophillips" = rnorm(length(dates), mean = 85, sd = 9),
    "johnson_&_johnson" = rnorm(length(dates), mean = 150, sd = 12),
    "unitedhealth_group" = rnorm(length(dates), mean = 140, sd = 11),
    check.names = FALSE
  )

  mock_df
}

# Test suite for load_and_preprocess_data function
test_that("load_and_preprocess_data works correctly", {
  mock_df <- create_mock_dataframe()

  # Call the function
  sectors <- load_and_preprocess_data(mock_df)

  # Assertions
  expect_type(sectors, "list")
  expect_true("Energy" %in% names(sectors))
  expect_true("Healthcare" %in% names(sectors))

  # Check Energy sector
  energy_sector <- sectors[["Energy"]]
  expect_true("Date" %in% colnames(energy_sector))
  expect_true(all(c("exxon_mobil", "chevron", "conocophillips")
                  %in% colnames(energy_sector)))

  # Check Healthcare sector
  healthcare_sector <- sectors[["Healthcare"]]
  expect_true("Date" %in% colnames(healthcare_sector))
  expect_true(all(c("johnson_&_johnson", "unitedhealth_group")
                  %in% colnames(healthcare_sector)))
})

# Test suite for calculate_sector_index function
test_that("calculate_sector_index computes correct sector index", {
  mock_df <- create_mock_dataframe()

  sectors <- load_and_preprocess_data(mock_df)
  energy_sector <- sectors[["Energy"]]

  # Call the function
  index_df <- calculate_sector_index(energy_sector)

  # Assertions
  expect_s3_class(index_df, "data.frame")
  expect_true(all(c("Date", "sector_index") %in% colnames(index_df)))
  expect_type(index_df$sector_index, "double")

  # Check that sector index is computed correctly
  company_cols <- setdiff(colnames(energy_sector), "Date")
  expected_index <- rowMeans(energy_sector[, company_cols], na.rm = TRUE)
  expect_equal(index_df$sector_index, expected_index, tolerance = 1e-6)
})

# Test suite for calculate_technical_indicators function
test_that("calculate_technical_indicators generates expected features", {
  mock_df <- create_mock_dataframe()

  sectors <- load_and_preprocess_data(mock_df)
  energy_sector <- sectors[["Energy"]]
  index_df <- calculate_sector_index(energy_sector)

  # Call the function
  tech_df <- calculate_technical_indicators(index_df)

  # Assertions
  expected_columns <- c(
    "Date", "sector_index", "log_price", "returns", "log_returns",
    "MA5", "MA10", "MA20", "MA50", "EMA5", "EMA10", "EMA20", "EMA50",
    "MACD", "MACD_signal", "volatility5", "volatility20",
    "lag_1", "lag_2", "lag_5",
    "log_return_lag_1", "log_return_lag_2", "log_return_lag_5",
    "day_of_week", "month"
  )

  expect_true(all(expected_columns %in% colnames(tech_df)))
})

# Test suite for build_random_forest_model function
test_that("build_random_forest_model generates valid model", {
  mock_df <- create_mock_dataframe()

  sectors <- load_and_preprocess_data(mock_df)
  energy_sector <- sectors[["Energy"]]
  index_df <- calculate_sector_index(energy_sector)
  tech_df <- calculate_technical_indicators(index_df)

  # Suppress print output during testing
  suppressWarnings({
    model_results <- build_random_forest_model(tech_df, "Energy")
  })

  # Assertions
  expect_type(model_results, "list")
  expect_s3_class(model_results$model, "randomForest")

  # Check model performance metrics
  expect_type(model_results$rmse, "double")
  expect_type(model_results$mae, "double")
  expect_type(model_results$r2, "double")

  # Check importance dataframe
  expect_s3_class(model_results$importance, "data.frame")
  expect_true(all(c("Feature", "Importance") %in%
                    colnames(model_results$importance)))
})
