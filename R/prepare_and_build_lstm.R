#' Prepare Data for LSTM Model
#'
#' Transforms time-series data into 3D sequences suitable for LSTM models,
#' using a sliding window approach. Scales both the features and the target
#' variable.
#'
#' @param data A data frame with columns `Date`, `sector_index`, `returns`,
#'             and lagged features or technical indicators.
#' @param time_steps Integer. The number of previous time steps to use in each
#' input sequence (default is 10).
#'
#' @return A list containing:
#' \describe{
#'   \item{x}{A 3D array of shape (samples, time_steps, features).}
#'   \item{y}{A numeric vector of scaled target values.}
#'   \item{target_mean}{The mean of the original `sector_index`, for
#'   inverse transformation.}
#'   \item{target_sd}{The standard deviation of `sector_index`, for
#'   inverse transformation.}
#' }
#'
#' @importFrom dplyr select
#' @export
#'
#' @examples
#' \dontrun{
#' lstm_ready <- prepare_data_for_lstm(df_with_indicators, time_steps = 15)
#' dim(lstm_ready$x)  # [samples, time_steps, features]
#' }
prepare_data_for_lstm <- function(data, time_steps = 10) {
  # Features and target
  features <- data %>%
    select(-Date, -sector_index, -returns)

  # Scale the features
  features_scaled <- scale(features)

  # Scale the target variable
  target <- data$sector_index
  target_mean <- mean(target)
  target_sd <- sd(target)
  target_scaled <- (target - target_mean) / target_sd

  # Create sequences
  x_sequences <- list()
  y_sequences <- list()

  for (i in 1:(length(target_scaled) - time_steps)) {
    x_sequences[[i]] <- features_scaled[i:(i + time_steps - 1), ]
    y_sequences[[i]] <- target_scaled[i + time_steps]
  }

  # Convert to arrays
  x_array <- array(0, dim = c(length(x_sequences), time_steps,
                              ncol(features_scaled)))
  y_array <- array(0, dim = c(length(y_sequences)))

  for (i in seq_along(x_sequences)) {
    x_array[i, , ] <- as.matrix(x_sequences[[i]])
    y_array[i] <- y_sequences[[i]]
  }

  return(list(
    x = x_array,
    y = y_array,
    target_mean = target_mean,
    target_sd = target_sd
  ))
}

#' Build LSTM Model for Sector Index Forecasting
#'
#' Trains an LSTM (Long Short-Term Memory) neural network to predict sector
#' index values using previously prepared and windowed input data. If the
#'  `keras` package is not installed
#' or model training fails, it falls back to a simple moving average baseline.
#'
#' @param sector_data A data frame with `Date`, `sector_index`, `returns`, and
#' lagged/technical features.
#' @param sector_name String. The sector name, used in titles and console
#' outputs.
#' @param time_steps Integer. Number of time steps to include in each sequence
#' (default is 10).
#'
#' @return A list containing:
#' \describe{
#'   \item{model}{The trained LSTM Keras model object, or `"moving_average"`
#'   if fallback used.}
#'   \item{rmse}{Root Mean Squared Error on test data.}
#'   \item{mae}{Mean Absolute Error on test data.}
#'   \item{r2}{R-squared value on test data.}
#' }
#'
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom ggplot2 scale_color_manual theme_minimal
#' @importFrom keras keras_model_sequential layer_lstm layer_dense
#' @importFrom keras compile fit
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume df_sector is processed with technical indicators and lag features
#' lstm_result <- build_lstm_model(df_sector, "Technology", time_steps = 10)
#'
#' # Metrics
#' lstm_result$rmse
#' lstm_result$r2
#'
#' # If fallback used:
#' if (lstm_result$model == "moving_average") {
#'   cat("Used fallback model.\n")
#' }
#' }
build_lstm_model <- function(sector_data, sector_name, time_steps = 10) {
  cat("\n--- Building LSTM model for", sector_name, "sector ---\n")

  # Prepare data for LSTM
  prepared_data <- prepare_data_for_lstm(sector_data, time_steps)

  # Split data chronologically
  n <- nrow(prepared_data$x)
  train_size <- floor(n * 0.8)

  x_train <- prepared_data$x[1:train_size, , ]
  x_test <- prepared_data$x[(train_size + 1):n, , ]
  y_train <- prepared_data$y[1:train_size]
  y_test <- prepared_data$y[(train_size + 1):n]

  # Get feature dimension
  n_features <- dim(x_train)[3]

  # Check if keras is available and tensorflow backend is working
  if (!requireNamespace("keras", quietly = TRUE)) {
    cat("Keras package is not available. Using a simple time series
        model instead.\n")

    # Simple moving average model as fallback
    y_pred <- rep(NA, length(y_test))
    for (i in seq_along(y_test)) {
      if (i <= 5) {
        # For the first few predictions, use the mean of available values
        y_pred[i] <- mean(y_train[(length(y_train) - 4):length(y_train)])
      } else {
        # Use moving average of previous 5 predictions
        y_pred[i] <- mean(y_pred[(i - 5):(i - 1)])
      }
    }

    # Un-scale the predictions
    y_pred <- (y_pred * prepared_data$target_sd) + prepared_data$target_mean
    y_actual <- (y_test * prepared_data$target_sd) + prepared_data$target_mean

    # Calculate metrics
    mse <- mean((y_actual - y_pred)^2, na.rm = TRUE)
    rmse <- sqrt(mse)
    mae <- mean(abs(y_actual - y_pred), na.rm = TRUE)
    r2 <- 1 - sum((y_actual - y_pred)^2, na.rm = TRUE) /
      sum((y_actual - mean(y_actual, na.rm = TRUE))^2, na.rm = TRUE)

    cat("Using fallback moving average model instead of LSTM.\n")
    cat("Root Mean Squared Error:", round(rmse, 4), "\n")
    cat("Mean Absolute Error:", round(mae, 4), "\n")
    cat("R² Score:", round(r2, 4), "\n")

    # Plot actual vs predicted
    plot_data <- data.frame(
      Time = seq_along(y_actual),
      Actual = y_actual,
      Predicted = as.vector(y_pred)
    )

    p <- ggplot(plot_data, aes(x = plot_data$Time)) +
      geom_line(aes(y = plot_data$Actual, color = "Actual")) +
      geom_line(aes(y = plot_data$Predicted, color = "Predicted")) +
      labs(title = paste(sector_name, "Sector - Moving Average Model
                         Predictions"),
           x = "Time", y = "Sector Index") +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
      theme_minimal()

    print(p)

    # Return results
    return(list(
      model = "moving_average",
      rmse = rmse,
      mae = mae,
      r2 = r2
    ))
  }

  # If keras is available, try to build a simple model with tryCatch
  tryCatch({
    # Use a very simple keras model structure
    model <- keras::keras_model_sequential()

    # Add a simple LSTM layer with dense output
    model$add(keras::layer_lstm(units = 50,
                                input_shape = c(time_steps, n_features)))
    model$add(keras::layer_dense(units = 1))

    # Compile with basic settings
    model$compile(loss = "mse", optimizer = "adam")

    # Fit the model with minimal options
    history <- model$fit(
      x_train, y_train,
      batch_size = 32,
      epochs = 50,
      validation_split = 0.2,
      verbose = 1
    )

    # Make predictions
    y_pred_scaled <- model$predict(x_test)

    # Inverse transform predictions and actual values
    y_pred <- (y_pred_scaled * prepared_data$target_sd) +
      prepared_data$target_mean
    y_actual <- (y_test * prepared_data$target_sd) + prepared_data$target_mean

    # Calculate metrics
    mse <- mean((y_actual - y_pred)^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(y_actual - y_pred))
    r2 <- 1 - sum((y_actual - y_pred)^2) / sum((y_actual - mean(y_actual))^2)

    cat("LSTM Root Mean Squared Error:", round(rmse, 4), "\n")
    cat("LSTM Mean Absolute Error:", round(mae, 4), "\n")
    cat("LSTM R² Score:", round(r2, 4), "\n")

    # Plot actual vs predicted
    plot_data <- data.frame(
      Time = seq_along(y_actual),
      Actual = y_actual,
      Predicted = as.vector(y_pred)
    )

    p <- ggplot(plot_data, aes(x = plot_data$Time)) +
      geom_line(aes(y = plot_data$Actual, color = "Actual")) +
      geom_line(aes(y = plot_data$Predicted, color = "Predicted")) +
      labs(title = paste(sector_name, "Sector - LSTM Predictions"),
           x = "Time", y = "Sector Index") +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
      theme_minimal()

    print(p)

    return(list(
      model = model,
      rmse = rmse,
      mae = mae,
      r2 = r2
    ))
  }, error = function(e) {
    cat("Error building LSTM model:", e$message, "\n")
    cat("Falling back to moving average model.\n")

    # Simple moving average model as fallback
    y_pred <- rep(NA, length(y_test))
    for (i in seq_along(y_test)) {
      if (i <= 5) {
        # For the first few predictions, use the mean of available values
        y_pred[i] <- mean(y_train[(length(y_train) - 4):length(y_train)])
      } else {
        # Use moving average of previous 5 predictions
        y_pred[i] <- mean(y_pred[(i - 5):(i - 1)])
      }
    }

    # Un-scale the predictions
    y_pred <- (y_pred * prepared_data$target_sd) + prepared_data$target_mean
    y_actual <- (y_test * prepared_data$target_sd) + prepared_data$target_mean

    # Calculate metrics
    mse <- mean((y_actual - y_pred)^2, na.rm = TRUE)
    rmse <- sqrt(mse)
    mae <- mean(abs(y_actual - y_pred), na.rm = TRUE)
    r2 <- 1 - sum((y_actual - y_pred)^2, na.rm = TRUE) /
      sum((y_actual - mean(y_actual, na.rm = TRUE))^2, na.rm = TRUE)

    cat("Root Mean Squared Error:", round(rmse, 4), "\n")
    cat("Mean Absolute Error:", round(mae, 4), "\n")
    cat("R² Score:", round(r2, 4), "\n")

    # Plot actual vs predicted
    plot_data <- data.frame(
      Time = seq_along(y_actual),
      Actual = y_actual,
      Predicted = as.vector(y_pred)
    )

    p <- ggplot(plot_data, aes(x = plot_data$Time)) +
      geom_line(aes(y = plot_data$Actual, color = "Actual")) +
      geom_line(aes(y = plot_data$Predicted, color = "Predicted")) +
      labs(title = paste(sector_name, "Sector - Moving Average
                         Model Predictions"),
           x = "Time", y = "Sector Index") +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
      theme_minimal()

    print(p)

    # Return results
    list(
      model = "moving_average",
      rmse = rmse,
      mae = mae,
      r2 = r2
    )
  })
}
