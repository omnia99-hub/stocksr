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
#' main_rfr()
#' }
main_rfr <- function() {
  cat("Using the stocksr dataset that's already loaded...\n")
  sectors <- load_and_preprocess_data(stocksr)
  sector_models <- list()

  for (sector_name in names(sectors)) {
    cat("\nProcessing", sector_name, "sector...\n")
    sector_df <- sectors[[sector_name]]
    index_df <- calculate_sector_index(sector_df)
    tech_df <- calculate_technical_indicators(index_df)
    model_metrics <- build_random_forest_model(tech_df, sector_name)
    sector_models[[sector_name]] <- model_metrics
  }

  comparison <- compare_sectors(sector_models)

  best_sector <- comparison$Sector[1]
  cat("\nAnalyzing feature importance for best sector:", best_sector, "\n")

  importance_df <- sector_models[[best_sector]]$feature_importance
  p <- ggplot(head(importance_df, 10),
              aes(x = reorder(Feature, IncNodePurity), y = IncNodePurity)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = paste("Top 10 Important Features -", best_sector, "Sector"),
         x = "Feature", y = "Importance")

  # Uncomment to print the plot
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
#' @importFrom ggplot2 ggplot aes geom_line labs scale_color_manual
#' theme_minimal
#' @export
#'
#' @examples
#' \dontrun{
#' main_lstm()
#' }
main_lstm <- function() {
  cat("Using the stocksr dataset that's already loaded...\n")
  sectors <- load_and_preprocess_data(stocksr)
  sector_models <- list()

  for (sector_name in names(sectors)) {
    cat("\nProcessing", sector_name, "sector...\n")
    sector_df <- sectors[[sector_name]]
    index_df <- calculate_sector_index(sector_df)
    tech_df <- calculate_technical_indicators(index_df)
    model_metrics <- build_lstm_model(tech_df, sector_name)
    sector_models[[sector_name]] <- model_metrics

    forecast_df <- forecast_with_lstm(
      model_metrics$model,
      tech_df,
      time_steps = 10,
      forecast_periods = 30
    )
  }

  comparison <- compare_sectors(sector_models)
  cat("\nAnalysis complete!\n")
}

#' Run the Full Parallelized Random Forest Pipeline
#'
#' Loads and processes the dataset, computes technical indicators, trains
#' Random Forest models in parallel for all sectors, and compares results.
#'
#' @param file_path Path to the Excel or CSV file containing the raw stock data.
#'
#' @return None. Prints results and returns performance summary.
#'
#' @importFrom parallel makeCluster parLapply stopCluster clusterExport
#'  detectCores
#' @export
#'
#' @examples
#' \dontrun{
#' main_parallel_rfr("data/sp500_data.xlsx")
#' }
main_parallel_rfr <- function(file_path) {
  sectors <- load_and_preprocess_data(file_path)
  cl <- parallel::makeCluster(parallel::detectCores() - 1)

  parallel::clusterExport(cl, varlist = c(
    "calculate_sector_index",
    "calculate_technical_indicators",
    "build_random_forest_model"
  ), envir = environment())

  sector_models <- parallel::parLapply(cl, names(sectors),
                                       function(sector_name) {
    sector_df <- sectors[[sector_name]]
    index_df <- calculate_sector_index(sector_df)
    tech_df <- calculate_technical_indicators(index_df)
    build_random_forest_model(tech_df, sector_name)
  })

  names(sector_models) <- names(sectors)
  parallel::stopCluster(cl)
  compare_sectors(sector_models)
}
