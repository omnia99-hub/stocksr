#' Calculate Sector Index
#'
#' Computes the average daily stock price across all companies in a given sector
#'
#' @param sector_df A data frame containing stock price data for companies in a
#' single sector.The data frame must include a `Date` column and one column per
#' company.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{Date}{The date of the observation.}
#'   \item{sector_index}{The average stock price across all companies in the
#'   sector for each date.}
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom dplyr all_of
#' @export
#'
#' @examples
#' # Example sector data frame
#' example_sector <- data.frame(
#'   Date = as.Date("2024-01-01") + 0:4,
#'   company_a = c(100, 102, 101, NA, 105),
#'   company_b = c(98, 100, 99, 101, 103),
#'   company_c = c(95, 97, 96, 98, 100)
#' )
#'
#' # Calculate sector index
#' sector_index_df <- calculate_sector_index(example_sector)
#'
#' # View the result
#' print(sector_index_df)
calculate_sector_index <- function(sector_df) {
  # Convert dates to proper datetime
  sector_df$Date <- as.Date(sector_df$Date)

  # Calculate the mean price as sector index
  company_cols <- setdiff(colnames(sector_df), "Date")

  index_df <- sector_df %>%
    mutate(sector_index = rowMeans(select(., all_of(company_cols)),
                                   na.rm = TRUE))

  # Keep only date and sector_index
  index_df <- index_df %>%
    select(Date, sector_index)

  index_df
}
