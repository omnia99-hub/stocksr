#' Compare model performance across sectors
#'
#' Summarizes RMSE, MAE, and R² for each sector's model.
#'
#' @param sector_models A list of model evaluation results per sector.
#'
#' @return A data frame comparing model performance.
#'
#' @importFrom dplyr arrange
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal labs
#' @importFrom gridExtra grid.arrange
#' @export
compare_sectors <- function(sector_models) {
  df <- data.frame(
    Sector = names(sector_models),
    RMSE = sapply(sector_models, function(x) x$rmse),
    MAE = sapply(sector_models, function(x) x$mae),
    R2 = sapply(sector_models, function(x) x$r2)
  )
  df <- dplyr::arrange(df, dplyr::desc(R2))
  return(df)
}
