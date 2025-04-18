#' Run the full analysis pipeline (parallelized)
#'
#' Loads data, computes features, trains models in parallel, and compares them.
#'
#' @return None. Prints results and returns performance summary.
#'
#' @importFrom parallel makeCluster parLapply stopCluster clusterExport
#' @importFrom parallel detectCores
#' @export
main <- function() {
  file_path <- "your_path/S&P500_data.xlsx"
  sectors <- load_and_preprocess_data(file_path)
  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  parallel::clusterExport(cl, varlist = c(
    "calculate_sector_index",
    "calculate_technical_indicators",
    "build_random_forest_model"
  ), envir = environment())

  sector_models <- parallel::parLapply(
    cl, names(sectors),
    function(sector_name) {
      sector_df <- sectors[[sector_name]]
      index_df <- calculate_sector_index(sector_df)
      tech_df <- calculate_technical_indicators(index_df)
      build_random_forest_model(tech_df, sector_name)
    }
  )

  names(sector_models) <- names(sectors)
  parallel::stopCluster(cl)
  compare_sectors(sector_models)
}
