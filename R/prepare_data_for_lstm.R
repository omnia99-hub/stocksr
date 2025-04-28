#' Prepare Data for LSTM Model
#'
#' Prepares and scales a dataset for input into an LSTM model. It creates lagged
#' features, scales the features and target, and arranges the data into
#' sequences suitable for LSTM training.
#'
#' @description
#' This function takes a dataset with a `sector_index` column, computes simple
#' returns, creates lagged variables, removes missing values, and scales the
#' features and target. It then builds sequential batches for LSTM training and
#' returns them as torch tensors.
#'
#' @details
#' - Calculates simple returns.
#' - Creates lagged features (`lag_1`, `lag_2`, `lag_5`).
#' - Standardizes features and target using mean and standard deviation.
#' - Splits data into input sequences of length `time_steps` and corresponding
#'   targets.
#' - Converts data into torch tensors.
#'
#' @param data A data frame containing at least a `sector_index` and `Date`
#' column.
#' @param time_steps (int, default = 10) Number of time steps in each input
#' sequence.
#'
#' @return
#' A list with the following elements:
#' - `x`: Input features as a torch tensor of shape (samples, time_steps,
#'    features).
#' - `y`: Target values as a torch tensor of shape (samples,).
#' - `target_mean`: Mean of the original target variable.
#' - `target_sd`: Standard deviation of the original target variable.
#' - `feature_names`: Names of the selected features.
#' - `dates`: Corresponding dates for the target values (after time_steps).
#'
#' @importFrom torch torch_tensor torch_float32
#' @importFrom stats filter
#'
#'
#' @export


prepare_data_for_lstm <- function(data, time_steps = 10) {
  # Calculate simple returns
  data$returns <- c(NA, diff(data$sector_index) / lag(data$sector_index, 1)[-1])
  data$returns[1] <- 0  # Set first value to 0 instead of NA

  # Create lagged features
  data$lag_1 <- dplyr::lag(data$sector_index, 1)
  data$lag_2 <- dplyr::lag(data$sector_index, 2)
  data$lag_5 <- dplyr::lag(data$sector_index, 5)

  # Remove NAs
  data <- data[!is.na(data$lag_5), ]

  # Prepare features
  features_df <- data %>%
    select(returns, lag_1, lag_2, lag_5)

  # Scale features
  features_mat <- as.matrix(features_df)
  features_scaled <- scale(features_mat)

  # Scale target
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

  # Convert to torch tensors
  x_array <- array(0, dim = c(length(x_sequences), time_steps,
                              ncol(features_scaled)))
  y_array <- array(0, dim = c(length(y_sequences)))

  for (i in seq_along(x_sequences)) {
    x_array[i, , ] <- as.matrix(x_sequences[[i]])
    y_array[i] <- y_sequences[[i]]
  }

  # Convert arrays to torch tensors
  x_tensor <- torch::torch_tensor(x_array, dtype = torch::torch_float32())
  y_tensor <- torch::torch_tensor(y_array, dtype = torch::torch_float32())

  return(list(
    x = x_tensor,
    y = y_tensor,
    target_mean = target_mean,
    target_sd = target_sd,
    feature_names = colnames(features_df),
    dates = data$Date[(time_steps + 1):nrow(data)]  # Save dates for plotting
  ))
}
