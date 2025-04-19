#' Run Full Random Forest Analysis Pipeline
#'
#' Executes the complete analysis pipeline using the Random Forest algorithm for
#' all sectors. It includes loading and preprocessing the stock dataset,
#' computing sector indices and technical indicators, training a Random Forest
#' model per sector, and comparing model performance across sectors. It also
#' visualizes feature importance for the best-performing sector.
#'
#' @return None. The function prints progress updates, comparison metrics,
#' and plots. It optionally stores model metrics in `sector_models` and plots
#' feature importance.
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip labs theme_minimal
#' @export
#'
#' @examples
#' \dontrun{
#' # Make sure the 'stocksr' dataset is loaded before calling
#' main_rfr()
#' }

main_rfr <- function() {
  # Use the already loaded stocksr dataset
  cat("Using the stocksr dataset that's already loaded...\n")

  # Load and preprocess data
  sectors <- load_and_preprocess_data(stocksr)

  # Dictionary to store models and performance metrics
  sector_models <- list()

  # Process each sector
  for (sector_name in names(sectors)) {
    cat("\nProcessing", sector_name, "sector...\n")

    # Calculate sector index
    sector_df <- sectors[[sector_name]]
    index_df <- calculate_sector_index(sector_df)

    # Calculate technical indicators
    tech_df <- calculate_technical_indicators(index_df)

    # Build model
    model_metrics <- build_random_forest_model(tech_df, sector_name)

    # Store metrics
    sector_models[[sector_name]] <- model_metrics

    # Optionally optimize model
    # optimized_model <- optimize_random_forest(tech_df, sector_name)
    # sector_models[[sector_name]] <- optimized_model
  }

  # Compare sector models
  comparison <- compare_sectors(sector_models)

  # Feature importance analysis
  best_sector <- comparison$Sector[1]
  cat("\nAnalyzing feature importance for best sector:", best_sector, "\n")

  importance_df <- sector_models[[best_sector]]$feature_importance

  # Plot feature importance
  p <- ggplot(head(importance_df, 10),
              aes(x = reorder(Feature, IncNodePurity), y = IncNodePurity)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = paste("Top 10 Important Features -", best_sector, "Sector"),
         x = "Feature", y = "Importance")

  # Save the plot
  #png(paste0(best_sector, "_feature_importance.png"), width = 800, height = 600)
  # print(p)


  cat("\nAnalysis complete! Results saved as PNG files.\n")
}
#' Run Full LSTM Analysis Pipeline
#'
#' Executes the complete analysis pipeline using LSTM neural networks for all
#' sectors. The pipeline includes loading and preprocessing stock data,
#' computing sector indices and technical indicators, training LSTM models,
#' evaluating performance, and optionally generating short-term forecasts.
#'
#' @return None. The function prints progress updates, model evaluation metrics,
#' and forecast plots.
#'
#' @importFrom ggplot2 ggplot aes geom_line labs
#' @importFrom ggplot2 scale_color_manual theme_minimal
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure 'stocksr' is loaded and preprocessed
#' main_lstm()
#' }
main_lstm <- function() {
  # Use the already loaded stocksr dataset
  cat("Using the stocksr dataset that's already loaded...\n")

  # Load and preprocess data
  sectors <- load_and_preprocess_data(stocksr)

  # Dictionary to store models and performance metrics
  sector_models <- list()

  # Process each sector
  for (sector_name in names(sectors)) {
    cat("\nProcessing", sector_name, "sector...\n")

    # Calculate sector index
    sector_df <- sectors[[sector_name]]
    index_df <- calculate_sector_index(sector_df)

    # Calculate technical indicators
    tech_df <- calculate_technical_indicators(index_df)

    # Build LSTM model
    model_metrics <- build_lstm_model(tech_df, sector_name)

    # Store metrics
    sector_models[[sector_name]] <- model_metrics

    # Optional: Generate forecast for this sector
    forecast_df <- forecast_with_lstm(
      model_metrics$model,
      tech_df,
      time_steps = 10,
      forecast_periods = 30
    )

    # Optional: Optimize model
    # optimized_model <- optimize_lstm(tech_df, sector_name)
    # sector_models[[sector_name]] <- optimized_model
  }

  # Compare sector models
  comparison <- compare_sectors(sector_models)

  cat("\nAnalysis complete!\n")
}

