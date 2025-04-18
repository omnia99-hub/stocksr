#' Calculate sector index
#'
#' Computes the average daily price across all companies in a sector.
#'
#' @param sector_df A data frame with Date and stock prices by company.
#'
#' @return A data frame with Date and sector_index columns.
#'
#' @importFrom dplyr mutate select
#' @export
calculate_sector_index <- function(sector_df) {
  sector_df$Date <- as.Date(sector_df$Date)
  index_df <- dplyr::mutate(
    sector_df,
    sector_index = rowMeans(dplyr::select(sector_df, -Date), na.rm = TRUE)
  )
  return(index_df[, c("Date", "sector_index")])
}
