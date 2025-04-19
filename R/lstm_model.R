#' Build and evaluate LSTM model for sector index forecasting
#'
#' Trains an LSTM model using Keras to predict the sector index based on
#' technical indicators. Falls back to a moving average model if Keras is
#' unavailable or fails.
#'
#' @param sector_data                      A data frame with Date, sector_index,
#'                                        returns, and engineered features.
#' @param sector_name                      Sector name for plot labeling.
#' @param time_steps                       Number of time steps in the input
#'                                        sequence (default = 10).
#'
#' @return A list with:
#' \describe{
#'   \item{model}{                         Keras model object or
#'                                        "moving_average" string.}
#'   \item{rmse}{                          Root Mean Squared Error.}
#'   \item{mae}{                           Mean Absolute Error.}
#'   \item{r2}{                            R-squared value.}
#' }
#'
#' @importFrom dplyr select
#' @importFrom keras keras_model_sequential layer_lstm layer_dense compile fit
#' @importFrom ggplot2 ggplot aes geom_line labs scale_color_manual
#' @importFrom ggplot2 theme_minimal
#' @export

build_lstm_model <- function(sector_data,
                             sector_name,
                             time_steps = 10) {
  cat("\n--- Building LSTM model for", sector_name, "sector ---\n")

  features <- dplyr::select(
    sector_data, -Date, -sector_index, -returns
  )

  features_scaled <- scale(features)

  target <- sector_data$sector_index
  target_mean <- mean(target)
  target_sd <- sd(target)
  target_scaled <- (target - target_mean) / target_sd

  x_seq <- list()
  y_seq <- list()

  for (i in 1:(nrow(features_scaled) - time_steps)) {
    x_seq[[i]] <- features_scaled[i:(i + time_steps - 1), ]
    y_seq[[i]] <- target_scaled[i + time_steps]
  }

  x_array <- array(
    unlist(x_seq),
    dim = c(
      length(x_seq),
      time_steps,
      ncol(features_scaled)
    )
  )
  y_array <- unlist(y_seq)

  n <- nrow(x_array)
  train_size <- floor(n * 0.8)

  x_train <- x_array[1:train_size, , ]
  x_test <- x_array[(train_size + 1):n, , ]
  y_train <- y_array[1:train_size]
  y_test <- y_array[(train_size + 1):n]

  if (!requireNamespace("keras", quietly = TRUE)) {
    cat("Keras not available. Using fallback model.\n")
    y_pred <- rep(mean(y_train), length(y_test))
    y_pred <- y_pred * target_sd + target_mean
    y_actual <- y_test * target_sd + target_mean

    rmse <- sqrt(mean((y_actual - y_pred)^2))
    mae <- mean(abs(y_actual - y_pred))
    r2 <- 1 - sum((y_actual - y_pred)^2) /
      sum((y_actual - mean(y_actual))^2)

    return(list(
      model = "moving_average",
      rmse = rmse,
      mae = mae,
      r2 = r2
    ))
  }

  tryCatch({
    model <- keras::keras_model_sequential()
    model %>%
      keras::layer_lstm(
        units = 50,
        input_shape = c(time_steps, ncol(features_scaled))
      ) %>%
      keras::layer_dense(units = 1)

    model %>% keras::compile(
      loss = "mse",
      optimizer = "adam"
    )

    model %>% keras::fit(
      x_train,
      y_train,
      epochs = 50,
      batch_size = 32,
      validation_split = 0.2,
      verbose = 0
    )

    y_pred_scaled <- model %>% predict(x_test)
    y_pred <- (y_pred_scaled * target_sd) + target_mean
    y_actual <- (y_test * target_sd) + target_mean

    rmse <- sqrt(mean((y_actual - y_pred)^2))
    mae <- mean(abs(y_actual - y_pred))
    r2 <- 1 - sum((y_actual - y_pred)^2) /
      sum((y_actual - mean(y_actual))^2)

    plot_data <- data.frame(
      time = seq_along(y_actual),
      actual = y_actual,
      predicted = as.vector(y_pred)
    )

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = time)
    ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$actual, color = "Actual")
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$predicted, color = "Predicted")
      ) +
      ggplot2::labs(
        title = paste(sector_name, "Sector - LSTM Predictions"),
        x = "Time",
        y = "Sector Index"
      ) +
      ggplot2::scale_color_manual(
        values = c("Actual" = "blue", "Predicted" = "red")
      ) +
      ggplot2::theme_minimal()

    print(p)

    return(list(
      model = model,
      rmse = rmse,
      mae = mae,
      r2 = r2
    ))

  }, error = function(e) {
    cat("LSTM failed:", e$message, "\nUsing fallback model.\n")
    y_pred <- rep(mean(y_train), length(y_test))
    y_pred <- y_pred * target_sd + target_mean
    y_actual <- y_test * target_sd + target_mean

    rmse <- sqrt(mean((y_actual - y_pred)^2))
    mae <- mean(abs(y_actual - y_pred))
    r2 <- 1 - sum((y_actual - y_pred)^2) /
      sum((y_actual - mean(y_actual))^2)

    return(list(
      model = "moving_average",
      rmse = rmse,
      mae = mae,
      r2 = r2
    ))
  })
}
