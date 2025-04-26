#' Build Random Forest Model for Sector Index
#'
#' Trains and evaluates a Random Forest regression model to predict a sector's
#' index based on engineered features. The function performs an 80/20 time-based
#' split for training and testing, computes performance metrics (RMSE, MAE, R2),
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
#' @importFrom randomForest randomForest importance
#' @importFrom dplyr mutate arrange select
#' @importFrom ggplot2 ggplot aes geom_line labs geom_bar coord_flip
#' @importFrom ggplot2 theme_minimal scale_color_manual
#' @importFrom caret createDataPartition
#' @export
#'
#' @examples
#' sectors <- load_and_preprocess_data(stocksr)
#' df <- calculate_sector_index(sectors[["Energy"]])
#' df_tech <- calculate_technical_indicators(df)
#' result <- build_random_forest_model(df_tech, "Energy")
#' result$rmse
#' head(result$feature_importance)
build_random_forest_model <- function(sector_data, sector_name) {
  cat("\n--- Building Random Forest model for", sector_name, "sector ---\n")

  # Prepare data
  features <- sector_data %>%
    select(-Date, -sector_index, -returns, -log_price)

  target <- sector_data$sector_index

  # Split data (80% train, 20% test)
  set.seed(42)
  train_index <- createDataPartition(target, p = 0.8, list = FALSE)
  x_train <- features[train_index, ]
  x_test <- features[-train_index, ]
  y_train <- target[train_index]
  y_test <- target[-train_index]

  # Train model
  rf_model <- randomForest(
    x = x_train,
    y = y_train,
    ntree = 500,
    mtry = floor(sqrt(ncol(x_train))),
    importance = TRUE # This is crucial for importance calculation
  )

  # Predictions
  y_pred <- predict(rf_model, x_test)

  # Metrics
  mse <- mean((y_test - y_pred)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(y_test - y_pred))
  r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

  cat("Model performance:\n")
  cat("RMSE:", round(rmse, 4), "\n")
  cat("MAE:", round(mae, 4), "\n")
  cat("R<U+00B2>:", round(r2, 4), "\n")

  # Plot
  plot_data <- data.frame(
    Date = sector_data$Date[-train_index],
    Actual = y_test,
    Predicted = y_pred
  )

  p <- ggplot(plot_data, aes(x = Date)) +
    geom_line(aes(y = Actual, color = "Actual"), linewidth = 0.8) +
    geom_line(aes(y = Predicted, color = "Predicted"),
      linewidth = 0.8, alpha = 0.7
    ) +
    labs(
      title = paste(sector_name, "Sector - Random Forest Predictions"),
      subtitle = paste("R<U+00B2> =", round(r2, 3)),
      x = "Date",
      y = "Sector Index"
    ) +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_minimal()

  #print(p)

  # Get importance - corrected method
  imp <- randomForest::importance(rf_model)
  imp_df <- data.frame(
    Feature = rownames(imp),
    Importance = imp[, "%IncMSE"] # Using %IncMSE for importance
  ) %>%
    arrange(desc(Importance))

  # Plot feature importance
  imp_plot <- ggplot(head(imp_df, 10), aes(
    x = reorder(Feature, Importance),
    y = Importance
  )) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Top 10 Important Features -", sector_name)) +
    theme_minimal()

  #print(imp_plot)

  list(
    model = rf_model,
    rmse = rmse,
    mae = mae,
    r2 = r2,
    importance = imp_df,
    prediction_plot = p,
    importance_plot = imp_plot
  )
}
