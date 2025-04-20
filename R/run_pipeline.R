#' Run Full LSTM Analysis Pipeline
#'
#' Executes the LSTM modeling pipeline on all sectors: preprocesses,
#' trains models, evaluates, and forecasts.
#'
#' @return None. Prints progress and shows forecast plot.
#'
#' @importFrom ggplot2 ggplot aes geom_line labs scale_color_manual
#' @importFrom ggplot2 theme_minimal
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
      model = model_metrics$model,
      data = tech_df,
      time_steps = 10,
      forecast_periods = 30
    )

    print(head(forecast_df))
  }

  comparison <- compare_sectors(sector_models)
  print(comparison)

  cat("\nLSTM analysis complete.\n")
}
#' Run Parallel Random Forest Pipeline
#'
#' Loads stock data, splits by sector, computes features, trains models
#' in parallel, and compares performance.
#'
#' @param file_path File path to the stock dataset (Excel or CSV).
#' @return None. Outputs progress and comparison metrics.
#'
#' @importFrom parallel makeCluster parLapply stopCluster clusterExport
#' @importFrom parallel detectCores
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

  sector_models <- parallel::parLapply(
    cl, names(sectors),
    function(sector) {
      df <- sectors[[sector]]
      index_df <- calculate_sector_index(df)
      tech_df <- calculate_technical_indicators(index_df)
      build_random_forest_model(tech_df, sector)
    }
  )

  names(sector_models) <- names(sectors)
  parallel::stopCluster(cl)

  comparison <- compare_sectors(sector_models)
  print(comparison)
}
