#' Build Random Forest Model for Sector Index
#'
#' Trains and evaluates a Random Forest regression model to predict a sector's
#' index based on engineered features. The function performs an 80/20 time-based
#' split for training and testing, computes performance metrics (RMSE, MAE, R²),
#' and visualizes predicted vs actual values. It also returns the feature
#' importance rankings.
#'
#' @param sector_data A data frame containing the engineered features, including
#'        `sector_index`, `returns`, lagged features, and technical indicators.
#'        The data must have a `Date` column and a `sector_index` column as the
#'        target.
#' @param sector_name A character string giving the name of the sector, used in
#'        print outputs and plot title.
#'
#' @return A named list containing:
#' \describe{
#'   \item{model}{The trained Random Forest model object.}
#'   \item{rmse}{Root Mean Squared Error on the test set.}
#'   \item{mae}{Mean Absolute Error on the test set.}
#'   \item{r2}{R-squared score on the test set.}
#'   \item{feature_importance}{A data frame showing feature importance ranked
#'   by IncNodePurity.}
#' }
#'
#'
#' @importFrom randomForest importance
#' @importFrom dplyr mutate arrange select
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom ggplot2 theme_minimal scale_color_manual
#' @export
#'
#' @examples
#' result <- build_random_forest_model(df_tech, "Technology")
#'
#' # View RMSE
#' result$rmse
#'
#' # View top features
#' head(result$feature_importance)
build_random_forest_model <- function(sector_data, sector_name) {
  cat("\n--- Building Random Forest model for", sector_name, "sector ---\n")

  # Define features and target
  model_data <- sector_data %>%
    select(-Date, -sector_index, -returns)

  target <- sector_data$sector_index

  # Split data chronologically (time series split)
  train_size <- floor(nrow(sector_data) * 0.8)

  x_train <- model_data[1:train_size, ]
  x_test <- model_data[(train_size + 1):nrow(model_data), ]
  y_train <- target[1:train_size]
  y_test <- target[(train_size + 1):length(target)]

  # Train Random Forest model
  set.seed(42)  # For reproducibility
  rf_model <- randomForest::randomForest(
    x = x_train,
    y = y_train,
    ntree = 100,
    mtry = floor(sqrt(ncol(x_train))),
    nodesize = 5,
    importance = TRUE
  )

  # Get predictions
  y_pred <- predict(rf_model, x_test)

  # Calculate metrics
  mse <- mean((y_test - y_pred)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(y_test - y_pred))
  r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

  cat("Root Mean Squared Error:", round(rmse, 4), "\n")
  cat("Mean Absolute Error:", round(mae, 4), "\n")
  cat("R² Score:", round(r2, 4), "\n")

  # Feature importance
  importance_df <- importance(rf_model) %>%
    as.data.frame() %>%
    mutate(Feature = rownames(.)) %>%
    arrange(desc(IncNodePurity))

  cat("\nTop 5 Important Features:\n")
  print(head(importance_df, 5))

  # Plot actual vs predicted
  plot_data <- data.frame(
    Time = seq_along(y_test),
    Actual = y_test,
    Predicted = y_pred
  )

  p <- ggplot(plot_data, aes(x = plot_data$Time)) +
    geom_line(aes(y = plot_data$Actual, color = "Actual")) +
    geom_line(aes(y = plot_data$Predicted, color = "Predicted")) +
    labs(title = paste(sector_name, "Sector - Random Forest Predictions"),
         x = "Time", y = "Sector Index") +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_minimal()

  # Save the plot
  print(p)

  # Return model and performance metrics
  list(
    model = rf_model,
    rmse = rmse,
    mae = mae,
    r2 = r2,
    feature_importance = importance_df
  )
}
