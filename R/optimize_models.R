#' Optimize Random Forest Hyperparameters
#'
#' Tunes the `mtry` hyperparameter of a Random Forest model using time-series
#' cross-validation via the `caret` package. The model is trained using 80% of
#' the data chronologically, and evaluated on the remaining 20%.
#'
#' @param sector_data A data frame with lagged features, technical indicators,
#' `sector_index`, and `Date`.
#' @param sector_name A character string for labeling outputs and console prints
#'
#' @return A named list with:
#' \describe{
#'   \item{model}{The best tuned `randomForest` model.}
#'   \item{best_params}{The best `mtry` value found during tuning.}
#'   \item{rmse}{Root Mean Squared Error on the test set.}
#'   \item{mae}{Mean Absolute Error on the test set.}
#'   \item{r2}{R-squared on the test set.}
#' }
#'
#' @importFrom caret train trainControl
#' @importFrom dplyr select
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume df_sector is already preprocessed
#' rf_optimized <- optimize_random_forest(df_sector, "Financial")
#' print(rf_optimized$best_params)
#' }
optimize_random_forest <- function(sector_data, sector_name) {
  cat("\n--- Optimizing Random Forest for", sector_name, "sector ---\n")

  # Define features and target
  model_data <- sector_data %>%
    select(-Date, -sector_index, -returns)

  target <- sector_data$sector_index

  # Split data chronologically
  train_size <- floor(nrow(sector_data) * 0.8)

  x_train <- model_data[1:train_size, ]
  x_test <- model_data[(train_size + 1):nrow(model_data), ]
  y_train <- target[1:train_size]
  y_test <- target[(train_size + 1):length(target)]

  # Create training data with target
  train_data <- x_train
  train_data$sector_index <- y_train

  # Set up cross-validation
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = floor(nrow(x_train) * 0.6),
    horizon = floor(nrow(x_train) * 0.2),
    fixedWindow = TRUE,
    allowParallel = TRUE
  )

  # Define parameter grid
  param_grid <- expand.grid(
    mtry = c(floor(sqrt(ncol(x_train))), floor(ncol(x_train) / 3),
             floor(ncol(x_train) / 2))
  )

  # Train model with parameter tuning
  set.seed(42)
  rf_tune <- train(
    sector_index ~ .,
    data = train_data,
    method = "rf",
    trControl = train_control,
    tuneGrid = param_grid,
    ntree = 100,
    importance = TRUE
  )

  # Best parameters
  cat("Best mtry parameter:", rf_tune$bestTune$mtry, "\n")

  # Evaluate best model
  best_rf <- rf_tune$finalModel
  y_pred <- predict(best_rf, x_test)

  # Calculate metrics
  mse <- mean((y_test - y_pred)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(y_test - y_pred))
  r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

  cat("Optimized model - RMSE:", round(rmse, 4),
      ", MAE:", round(mae, 4),
      ", R²:", round(r2, 4), "\n")

  list(
    model = best_rf,
    best_params = rf_tune$bestTune,
    rmse = rmse,
    mae = mae,
    r2 = r2
  )
}
#' Optimize LSTM Hyperparameters
#'
#' Performs a grid search to find the best set of hyperparameters for an LSTM
#' model using validation loss. The model is trained on scaled sequence data
#' prepared by `prepare_data_for_lstm()`, and the best performing model is
#' evaluated on test data.
#'
#' @param sector_data A data frame with `Date`, `sector_index`, and
#' lagged/technical features.
#' @param sector_name A character string used for output messages.
#' @param time_steps Integer. Number of time steps to include in each input
#' sequence (default is 10).
#'
#' @return A named list containing:
#' \describe{
#'   \item{model}{The best trained LSTM model.}
#'   \item{best_params}{A data frame of the best hyperparameter combination.}
#'   \item{rmse}{Root Mean Squared Error on the test set.}
#'   \item{mae}{Mean Absolute Error on the test set.}
#'   \item{r2}{R-squared on the test set.}
#' }
#'
#' @importFrom keras keras_model_sequential layer_lstm layer_dropout layer_dense
#' @importFrom keras compile fit
#' @importFrom keras optimizer_adam callback_early_stopping
#' @export
#'
#' @examples
#' \dontrun{
#' # Suppose df_sector is a processed sector data frame
#' lstm_result <- optimize_lstm(df_sector, "Technology", time_steps = 10)
#' print(lstm_result$best_params)
#' }
optimize_lstm <- function(sector_data, sector_name, time_steps = 10) {
  cat("\n--- Optimizing LSTM for", sector_name, "sector ---\n")

  # Prepare data for LSTM
  prepared_data <- prepare_data_for_lstm(sector_data, time_steps)

  # Split data chronologically
  n <- nrow(prepared_data$x)
  train_size <- floor(n * 0.8)

  x_train <- prepared_data$x[1:train_size, , ]
  x_test <- prepared_data$x[(train_size + 1):n, , ]
  y_train <- prepared_data$y[1:train_size]
  y_test <- prepared_data$y[(train_size + 1):n]

  n_features <- dim(x_train)[3]

  # Hyperparameter grid
  hyperparameters <- list(
    units1 = c(50, 100),
    units2 = c(50, 100),
    dropout = c(0.2, 0.3),
    learning_rate = c(0.001, 0.0005)
  )

  # Prepare grid
  grid <- expand.grid(hyperparameters)

  best_model <- NULL
  best_val_loss <- Inf

  # Grid search
  for (i in seq_len(nrow(grid))) {
    cat("Testing hyperparameter set", i, "of", nrow(grid), "\n")

    # Create model with current hyperparameters
    model <- keras_model_sequential()
    model %>%
      layer_lstm(units = grid$units1[i],
                 input_shape = c(time_steps, n_features),
                 return_sequences = TRUE) %>%
      layer_dropout(rate = grid$dropout[i]) %>%
      layer_lstm(units = grid$units2[i]) %>%
      layer_dropout(rate = grid$dropout[i]) %>%
      layer_dense(units = 1)

    # Compile model
    model %>% compile(
      optimizer = optimizer_adam(learning_rate = grid$learning_rate[i]),
      loss = "mse",
      metrics = c("mae")
    )

    # Early stopping
    early_stopping <- callback_early_stopping(
      monitor = "val_loss",
      patience = 10,
      restore_best_weights = TRUE
    )

    # Fit model
    history <- model %>% fit(
      x_train, y_train,
      epochs = 50,  # Reduced epochs for grid search
      batch_size = 32,
      validation_split = 0.2,
      callbacks = list(early_stopping),
      verbose = 0
    )

    # Check if this model is better
    val_loss <- min(history$metrics$val_loss)
    if (val_loss < best_val_loss) {
      best_val_loss <- val_loss
      best_model <- model
      best_params <- grid[i, ]
    }
  }

  cat("Best hyperparameters:\n")
  print(best_params)

  # Evaluate best model
  y_pred_scaled <- best_model %>% predict(x_test)

  # Inverse transform predictions and actual values
  y_pred <- (y_pred_scaled * prepared_data$target_sd) +
    prepared_data$target_mean
  y_actual <- (y_test * prepared_data$target_sd) + prepared_data$target_mean

  # Calculate metrics
  mse <- mean((y_actual - y_pred)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(y_actual - y_pred))
  r2 <- 1 - sum((y_actual - y_pred)^2) / sum((y_actual - mean(y_actual))^2)

  cat("Optimized model - RMSE:", round(rmse, 4),
      ", MAE:", round(mae, 4),
      ", R²:", round(r2, 4), "\n")

  return(list(
    model = best_model,
    best_params = best_params,
    rmse = rmse,
    mae = mae,
    r2 = r2
  ))
}
