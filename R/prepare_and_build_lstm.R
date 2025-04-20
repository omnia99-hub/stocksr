#' Prepare Data for LSTM Model
#'
#' Converts time-series data into 3D arrays for LSTM training.
#'
#' @param data A data frame with Date, sector_index, returns, and features.
#' @param time_steps Integer. Number of time steps in each input sequence.
#'
#' @return A list containing:
#' \describe{
#'   \item{x}{A 3D array of shape (samples, time_steps, features).}
#'   \item{y}{A numeric vector of scaled target values.}
#'   \item{target_mean}{Mean of the original sector_index (for rescaling).}
#'   \item{target_sd}{Standard deviation of sector_index (for rescaling).}
#' }
#'
#' @importFrom dplyr select
#' @export
prepare_data_for_lstm <- function(data, time_steps = 10) {
  features <- dplyr::select(data, -Date, -sector_index, -returns)
  scaled_features <- scale(features)

  target <- scale(data$sector_index)
  target_mean <- attr(target, "scaled:center")
  target_sd <- attr(target, "scaled:scale")

  x_list <- list()
  y_list <- list()

  for (i in 1:(nrow(data) - time_steps)) {
    x_list[[i]] <- scaled_features[i:(i + time_steps - 1), ]
    y_list[[i]] <- target[i + time_steps]
  }

  x_array <- array(unlist(x_list),
                   dim = c(length(x_list), time_steps, ncol(features)))
  y_array <- unlist(y_list)

  return(list(
    x = x_array,
    y = y_array,
    target_mean = target_mean,
    target_sd = target_sd
  ))
}
