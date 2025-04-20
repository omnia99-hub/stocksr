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
#' rf_optimized <- optimize_random_forest(df_sector, "Financial")
#' print(rf_optimized$best_params)
#' }
optimize_random_forest <- function(sector_data, sector_name) {
  cat("\n--- Optimizing Random Forest for", sector_name, "sector ---\n")

  model_data <- sector_data %>%
    select(-Date, -sector_index, -returns)
  target <- sector_data$sector_index
  train_size <- floor(nrow(sector_data) * 0.8)

  x_train <- model_data[1:train_size, ]
  x_test <- model_data[(train_size + 1):nrow(model_data), ]
  y_train <- target[1:train_size]
  y_test <- target[(train_size + 1):length(target)]

  train_data <- x_train
  train_data$sector_index <- y_train

  train_control <- trainControl(
    method = "timeslice",
    initialWindow = floor(nrow(x_train) * 0.6),
    horizon = floor(nrow(x_train) * 0.2),
    fixedWindow = TRUE,
    allowParallel = TRUE
  )

  param_grid <- expand.grid(
    mtry = c(floor(sqrt(ncol(x_train))), floor(ncol(x_train) / 3), floor(ncol(x_train) / 2))
  )

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

  cat("Best mtry parameter:", rf_tune$bestTune$mtry, "\n")

  best_rf <- rf_tune$finalModel
  y_pred <- predict(best_rf, x_test)

  mse <- mean((y_test - y_pred)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(y_test - y_pred))
  r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

  cat("Optimized model - RMSE:", round(rmse, 4),
      ", MAE:", round(mae, 4),
      ", R2:", round(r2, 4), "\n")

  list(
    model = best_rf,
    best_params = rf_tune$bestTune,
    rmse = rmse,
    mae = mae,
    r2 = r2
  )
}
