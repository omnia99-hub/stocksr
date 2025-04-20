#' Load and preprocess S&P 500 data
#'
#' Loads an Excel file of S&P 500 prices and groups companies into sectors.
#' Each sector is returned as a data frame with 'Date' and company columns.
#'
#' @param file_path Path to the Excel file with the stock price data.
#'
#' @return A named list of data frames (one per sector).
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr desc
#' @importFrom magrittr %>%
#' @importFrom utils head tail
#' @importFrom stats predict complete.cases sd reorder
#' @importFrom stats time

#' @export
#'
#' @examples
#' \dontrun{
#' file_path <- "data/sp500_daily_2010_2020.xlsx"
#' sectors_data <- load_and_preprocess_data(file_path)
#' names(sectors_data)
#' }
load_and_preprocess_data <- function(file_path) {
  stocksr <- readxl::read_excel(file_path)
  dates <- stocksr[[1]]

  sector_mapping <- list(
    "microsoft" = "Technology",
    "apple" = "Technology",
    "nvidia" = "Technology",
    "broadcom" = "Technology",
    "oracle" = "Technology",
    "adobe (nas)" = "Technology",
    "salesforce" = "Technology",
    "cisco systems" = "Technology",
    "qualcomm" = "Technology",
    "intel" = "Technology",
    "texas instruments" = "Technology",
    "advanced micro devices" = "Technology",
    "jp morgan chase & co." = "Financial",
    "visa 'a'" = "Financial",
    "mastercard" = "Financial",
    "bank of america" = "Financial",
    "citigroup" = "Financial",
    "wells fargo & co" = "Financial",
    "morgan stanley" = "Financial",
    "exxon mobil" = "Energy",
    "chevron" = "Energy",
    "conocophillips" = "Energy",
    "schlumberger" = "Energy",
    "eog res." = "Energy",
    "johnson & johnson" = "Health Care",
    "unitedhealth group" = "Health Care",
    "eli lilly" = "Health Care",
    "pfizer" = "Health Care",
    "merck & company" = "Health Care",
    "amazon.com" = "Consumer Discretionary",
    "tesla" = "Consumer Discretionary",
    "home depot" = "Consumer Discretionary",
    "mcdonald's" = "Consumer Discretionary",
    "nike 'b'" = "Consumer Discretionary",
    "alphabet a" = "Communication Services",
    "walt disney" = "Communication Services",
    "netflix" = "Communication Services",
    "comcast a" = "Communication Services",
    "t-mobile us" = "Communication Services"
  )

  sectors <- list()
  sector_names <- unique(unlist(sector_mapping))

  for (sector in sector_names) {
    sector_companies <- names(sector_mapping)[
      sapply(sector_mapping, function(x) x == sector)
    ]

    sector_df <- data.frame(Date = dates)

    for (company in sector_companies) {
      if (company %in% colnames(stocksr)) {
        sector_df[[company]] <- stocksr[[company]]
      }
    }

    if (ncol(sector_df) > 1) {
      sectors[[sector]] <- sector_df
    }
  }

  return(sectors)
}
