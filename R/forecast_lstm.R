#' Forecast with LSTM or Moving Average Fallback
#'
#' Generates future sector index forecasts using a trained LSTM model.
#' If the model is unavailable or fails, falls back to moving average.
#'
#' @param model A trained LSTM model or "moving_average" fallback.
#' @param data A data frame with columns: Date, sector_index, returns, etc.
#' @param time_steps Integer. Time steps used in training (e.g., 10).
#' @param forecast_periods Integer. Days to forecast (default: 30).
#'
#' @return A data frame with Date and Forecast columns.
#' @export
#'
#' @examples
#' \dontrun{
#' forecast_df <- forecast_with_lstm(model, df_sector, 10, 30)
#' head(forecast_df)
#' }
forecast_with_lstm <- function(model, data, time_steps, forecast_periods = 30) {
  cat("\nGenerating", forecast_periods, "day forecast...\n")

  target_mean <- mean(data$sector_index, na.rm = TRUE)
  target_sd <- sd(data$sector_index, na.rm = TRUE)

  forecast_dates <- seq(
    from = max(data$Date) + 1,
    by = "day",
    length.out = forecast_periods
  )

  if (is.character(model) && model == "moving_average") {
    values <- tail(data$sector_index, 5)
    forecasted_values <- moving_avg_forecast(values, forecast_periods)
  } else {
    forecasted_values <- try_lstm_forecast(
      model, data, time_steps, forecast_periods, target_mean, target_sd
    )
  }

  forecast_df <- data.frame(Date = forecast_dates, Forecast = forecasted_values)
  plot_forecast(data, forecast_df)

  return(forecast_df)
}
#' Internal: Moving Average Forecast
#' @keywords internal
moving_avg_forecast <- function(last_values, forecast_periods) {
  forecasted <- numeric(forecast_periods)
  for (i in 1:forecast_periods) {
    if (i <= 5) {
      forecasted[i] <- mean(last_values)
    } else {
      forecasted[i] <- mean(forecasted[(i - 5):(i - 1)])
    }
  }
  return(forecasted)
}
#' Internal: LSTM Rolling Forecast
#' @keywords internal
try_lstm_forecast <- function(model, data, time_steps, forecast_periods,
                              target_mean, target_sd) {
  features <- tail(data, time_steps) %>%
    dplyr::select(-Date, -sector_index, -returns)

  scaled <- scale(features)
  input <- array(as.matrix(scaled), dim = c(1, time_steps, ncol(scaled)))
  predictions <- numeric(forecast_periods)

  for (i in 1:forecast_periods) {
    next_scaled <- model$predict(input)
    predictions[i] <- (next_scaled * target_sd) + target_mean

    if (i < forecast_periods) {
      seq_shift <- input[1, 2:time_steps, ]
      input <- array(
        c(seq_shift, input[1, time_steps, ]),
        dim = c(1, time_steps, ncol(scaled))
      )
    }
  }

  return(predictions)
}
#' Internal: Plot Forecast Results
#' @keywords internal
plot_forecast <- function(history, forecast_df) {
  plot_data <- rbind(
    data.frame(Date = history$Date,
               Value = history$sector_index,
               Type = "Historical"),
    data.frame(Date = forecast_df$Date,
               Value = forecast_df$Forecast,
               Type = "Forecast")
  )

  p <- ggplot(plot_data, aes(x = Date, y = Value, color = Type)) +
    geom_line() +
    labs(title = "Time Series Forecast",
         x = "Date", y = "Sector Index") +
    theme_minimal() +
    scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red"))

  print(p)
}
