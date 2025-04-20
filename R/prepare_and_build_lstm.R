#' Build LSTM Model for Sector Index Forecasting
#'
#' Trains an LSTM (Long Short-Term Memory) neural network to predict sector
#' index values using previously prepared and windowed input data. If the
#' `keras` package is not installed or model training fails, it falls back to a
#' simple moving average baseline.
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
#' @importFrom ggplot2 ggplot aes geom_line labs scale_color_manual theme_minimal
#' @importFrom keras keras_model_sequential layer_lstm layer_dense compile fit
#' @export
#'
#' @examples
#' \dontrun{
#' lstm_result <- build_lstm_model(df_sector, "Technology", time_steps = 10)
#' lstm_result$rmse
#' lstm_result$r2
#' }
build_lstm_model <- function(sector_data, sector_name, time_steps = 10) {
  cat("\n--- Building LSTM model for", sector_name, "sector ---\n")
  prepared_data <- prepare_data_for_lstm(sector_data, time_steps)

  n <- nrow(prepared_data$x)
  train_size <- floor(n * 0.8)

  x_train <- prepared_data$x[1:train_size, , ]
  x_test <- prepared_data$x[(train_size + 1):n, , ]
  y_train <- prepared_data$y[1:train_size]
  y_test <- prepared_data$y[(train_size + 1):n]

  n_features <- dim(x_train)[3]

  if (!requireNamespace("keras", quietly = TRUE)) {
    cat("Keras not available. Using fallback moving average model.\n")

    y_pred <- rep(NA, length(y_test))
    for (i in seq_along(y_test)) {
      y_pred[i] <- if (i <= 5) {
        mean(y_train[(length(y_train) - 4):length(y_train)])
      } else {
        mean(y_pred[(i - 5):(i - 1)])
      }
    }

    y_pred <- (y_pred * prepared_data$target_sd) + prepared_data$target_mean
    y_actual <- (y_test * prepared_data$target_sd) + prepared_data$target_mean

    mse <- mean((y_actual - y_pred)^2, na.rm = TRUE)
    rmse <- sqrt(mse)
    mae <- mean(abs(y_actual - y_pred), na.rm = TRUE)
    r2 <- 1 - sum((y_actual - y_pred)^2, na.rm = TRUE) /
      sum((y_actual - mean(y_actual, na.rm = TRUE))^2, na.rm = TRUE)

    cat("RMSE:", round(rmse, 4), ", MAE:", round(mae, 4), ", R2:", round(r2, 4), "\n")

    plot_data <- data.frame(
      Time = seq_along(y_actual),
      Actual = y_actual,
      Predicted = as.vector(y_pred)
    )

    p <- ggplot(plot_data, aes(x = Time)) +
      geom_line(aes(y = Actual, color = "Actual")) +
      geom_line(aes(y = Predicted, color = "Predicted")) +
      labs(title = paste(sector_name, "- Moving Average Predictions"),
           x = "Time", y = "Sector Index") +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
      theme_minimal()

    print(p)

    return(list(model = "moving_average", rmse = rmse, mae = mae, r2 = r2))
  }

  tryCatch({
    model <- keras_model_sequential() %>%
      layer_lstm(units = 50, input_shape = c(time_steps, n_features)) %>%
      layer_dense(units = 1)

    model %>% compile(loss = "mse", optimizer = "adam")

    model %>% fit(
      x_train, y_train,
      batch_size = 32, epochs = 50,
      validation_split = 0.2, verbose = 1
    )

    y_pred_scaled <- model %>% predict(x_test)
    y_pred <- (y_pred_scaled * prepared_data$target_sd) + prepared_data$target_mean
    y_actual <- (y_test * prepared_data$target_sd) + prepared_data$target_mean

    mse <- mean((y_actual - y_pred)^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(y_actual - y_pred))
    r2 <- 1 - sum((y_actual - y_pred)^2) / sum((y_actual - mean(y_actual))^2)

    cat("LSTM RMSE:", round(rmse, 4), ", MAE:", round(mae, 4), ", R2:", round(r2, 4), "\n")

    plot_data <- data.frame(
      Time = seq_along(y_actual),
      Actual = y_actual,
      Predicted = as.vector(y_pred)
    )

    p <- ggplot(plot_data, aes(x = Time)) +
      geom_line(aes(y = Actual, color = "Actual")) +
      geom_line(aes(y = Predicted, color = "Predicted")) +
      labs(title = paste(sector_name, "- LSTM Predictions"),
           x = "Time", y = "Sector Index") +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
      theme_minimal()

    print(p)

    return(list(model = model, rmse = rmse, mae = mae, r2 = r2))
  }, error = function(e) {
    cat("LSTM failed. Fallback model used.\n")
    return(build_lstm_model(sector_data, sector_name, time_steps))  # Recursive fallback
  })
}
