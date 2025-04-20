#' Build random forest model for sector index
#'
#' Trains and evaluates a random forest model to predict sector index values
#' based on engineered features. It performs an 80/20 time-based split,
#' computes RMSE, MAE, and R², and returns feature importance.
#'
#' @param sector_data A data frame with engineered features. Must include
#'        'sector_index', 'Date', and optionally 'returns'.
#' @param sector_name A string giving the sector name for reporting.
#'
#' @return A list with:
#' \describe{
#'   \item{model}{Trained random forest model.}
#'   \item{rmse}{Root mean squared error.}
#'   \item{mae}{Mean absolute error.}
#'   \item{r2}{R-squared performance.}
#'   \item{feature_importance}{Data frame of ranked feature importance.}
#' }
#'
#' @importFrom randomForest randomForest importance
#' @importFrom dplyr mutate arrange select
#' @importFrom ggplot2 ggplot aes geom_line labs scale_color_manual
#' @importFrom ggplot2 theme_minimal
#' @export
build_random_forest_model <- function(sector_data, sector_name) {
  model_data <- dplyr::select(sector_data,
                              -Date, -sector_index, -returns)
  target <- sector_data$sector_index
  train_size <- floor(nrow(sector_data) * 0.8)
  x_train <- model_data[1:train_size, ]
  x_test <- model_data[(train_size + 1):nrow(model_data), ]
  y_train <- target[1:train_size]
  y_test <- target[(train_size + 1):length(target)]

  rf_model <- randomForest::randomForest(
    x = x_train,
    y = y_train,
    ntree = 100,
    mtry = floor(sqrt(ncol(x_train))),
    nodesize = 5,
    importance = TRUE
  )

  y_pred <- predict(rf_model, x_test)
  mse <- mean((y_test - y_pred)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(y_test - y_pred))
  r2 <- 1 - sum((y_test - y_pred)^2) /
    sum((y_test - mean(y_test))^2)

  importance_df <- randomForest::importance(rf_model) %>%
    as.data.frame() %>%
    dplyr::mutate(Feature = rownames(.)) %>%
    dplyr::arrange(desc(IncNodePurity))

  return(list(
    model = rf_model,
    rmse = rmse,
    mae = mae,
    r2 = r2,
    feature_importance = importance_df
  ))
}
