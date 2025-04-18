#' Optimize random forest hyperparameters
#'
#' Tunes the mtry parameter using time-series cross-validation.
#'
#' @param sector_data Input data with features and sector_index target.
#' @param sector_name Sector name for output labeling.
#'
#' @return A list with best model, best parameters, RMSE, MAE, and R².
#'
#' @importFrom caret train trainControl
#' @importFrom dplyr select
#' @export
optimize_random_forest <- function(sector_data, sector_name) {
  model_data <- dplyr::select(sector_data, -Date, -sector_index, -returns)
  target <- sector_data$sector_index
  train_size <- floor(nrow(sector_data) * 0.8)
  x_train <- model_data[1:train_size, ]
  y_train <- target[1:train_size]
  x_test <- model_data[(train_size + 1):nrow(model_data), ]
  y_test <- target[(train_size + 1):length(target)]

  train_data <- x_train
  train_data$sector_index <- y_train

  control <- caret::trainControl(
    method = "timeslice",
    initialWindow = floor(nrow(x_train) * 0.6),
    horizon = floor(nrow(x_train) * 0.2),
    fixedWindow = TRUE, allowParallel = TRUE
  )

  grid <- expand.grid(mtry = c(
    floor(sqrt(ncol(x_train))), floor(ncol(x_train) / 3),
    floor(ncol(x_train) / 2)
  ))

  rf_tune <- caret::train(
    sector_index ~ .,
    data = train_data,
    method = "rf", trControl = control,
    tuneGrid = grid, ntree = 100, importance = TRUE
  )

  best_rf <- rf_tune$finalModel
  y_pred <- predict(best_rf, x_test)
  mse <- mean((y_test - y_pred)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(y_test - y_pred))
  r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

  return(list(
    model = best_rf, best_params = rf_tune$bestTune,
    rmse = rmse, mae = mae, r2 = r2
  ))
}
