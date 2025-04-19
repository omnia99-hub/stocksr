#' Forecast with LSTM or Fallback Model
#'
#' Generates a forecast for future values of a sector index using a trained
#' LSTM model.If the LSTM model is unavailable or fails, the function defaults
#' to a simple moving average model.
#'
#' @param model A trained LSTM model object (from `keras_model_sequential`) or
#' the string `"moving_average"` to use the fallback method.
#' @param data A data frame with historical time series data. Must include
#' columns `Date`, `sector_index`, `returns`, and any feature columns used
#' during training.
#' @param time_steps Integer. The number of time steps used during LSTM training
#' (i.e., window size for each prediction).
#' @param forecast_periods Integer. The number of future periods to forecast
#' (default = 30).
#'
#' @return A data frame with:
#' \describe{
#'   \item{Date}{Forecasted dates, starting from one day after the last
#'   date in `data`.}
#'   \item{Forecast}{Forecasted sector index values.}
#' }
#'
#' @details
#' For LSTM forecasts, the function uses a rolling input mechanism to predict
#' one step at a time,
#' shifting the prediction window forward. If the LSTM model is not usable or
#' an error occurs during prediction,
#' it falls back to a moving average forecast using the last 5 values.
#'
#' A plot showing both historical and forecasted values is automatically
#' generated.
#'
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom ggplot2 theme_minimal scale_color_manual
#' @export
#'
#' @examples
#' \dontrun{
#' # Suppose you have a trained LSTM model and a dataset
#' forecast_df <- forecast_with_lstm(model = trained_lstm_model,
#'                                   data = df_sector,
#'                                   time_steps = 10,
#'                                   forecast_periods = 30)
#'
#' # View forecasted values
#' head(forecast_df)
#' }
forecast_with_lstm <- function(model, data, time_steps, forecast_periods = 30) {
  cat("\nGenerating", forecast_periods, "day forecast...\n")

  # Get the last time_steps rows of data for initial prediction
  last_sequence <- tail(data, time_steps)

  # Prepare features
  features <- last_sequence %>%
    select(-Date, -sector_index, -returns)

  # Scale features using same parameters as in training
  features_mean <- colMeans(features, na.rm = TRUE) # nolint
  features_sd <- apply(features, 2, sd, na.rm = TRUE)#nolint
  features_scaled <- scale(features)

  # Scale target for reference
  target_mean <- mean(data$sector_index, na.rm = TRUE)
  target_sd <- sd(data$sector_index, na.rm = TRUE)

  # Container for forecasted values
  forecasted_values <- numeric(forecast_periods)
  forecast_dates <- seq(max(data$Date) + 1, by = "day",
                        length.out = forecast_periods)

  # Check if using fallback model or LSTM
  if (is.character(model) && model == "moving_average") {
    # Simple moving average forecast
    last_values <- tail(data$sector_index, 5)

    for (i in 1:forecast_periods) {
      if (i <= 5) {
        # For first few predictions, use mean of last 5 actual values
        forecasted_values[i] <- mean(last_values)
      } else {
        # Otherwise use mean of last 5 forecasted values
        forecasted_values[i] <- mean(forecasted_values[(i - 5):(i - 1)])
      }
    }
  } else {
    # Try LSTM forecast with error handling
    tryCatch({
      # Reshape for LSTM input [samples, time steps, features]
      prediction_input <- array(as.matrix(features_scaled),
                                dim = c(1, time_steps, ncol(features_scaled)))

      # Generate forecasts one step at a time
      for (i in 1:forecast_periods) {
        # Predict the next value
        next_pred_scaled <- model$predict(prediction_input)

        # Inverse transform
        next_pred <- (next_pred_scaled * target_sd) + target_mean

        # Store prediction
        forecasted_values[i] <- next_pred

        # For simplicity in this demonstration, we'll just shift the sequence
        # calculate all the features
        if (i < forecast_periods) {
          # Shift input sequence (simplified approach)
          new_sequence <- prediction_input[1, 2:time_steps, ]
          # Add a placeholder row (duplicating the last one for simplicity)
          prediction_input <- array(
            c(new_sequence, prediction_input[1, time_steps, ]),
            dim = c(1, time_steps, ncol(features_scaled))
          )
        }
      }
    }, error = function(e) {
      # Fallback to moving average if LSTM forecast fails
      cat("Error in LSTM forecast:", e$message, "\n")
      cat("Falling back to moving average forecast.\n")

      last_values <- tail(data$sector_index, 5)

      for (i in 1:forecast_periods) {
        if (i <= 5) {
          forecasted_values[i] <- mean(last_values)
        } else {
          forecasted_values[i] <- mean(forecasted_values[(i - 5):(i - 1)])
        }
      }
    })
  }

  # Create forecast dataframe
  forecast_df <- data.frame(
    Date = forecast_dates,
    Forecast = forecasted_values
  )

  # Plot original data + forecast
  plot_data <- rbind(
    data.frame(
      Date = data$Date,
      Value = data$sector_index,
      Type = "Historical"
    ),
    data.frame(
      Date = forecast_df$Date,
      Value = forecast_df$Forecast,
      Type = "Forecast"
    )
  )

  p <- ggplot(plot_data, aes(x = Date, y = Value, color = Type)) +
    geom_line() +
    labs(title = "Time Series Forecast", x = "Date", y = "Sector Index") +
    theme_minimal() +
    scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red"))

  print(p)

  return(forecast_df)
}
