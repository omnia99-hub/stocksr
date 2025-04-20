#' Load and preprocess S&P 500 data
#'
#' This function loads an Excel file containing historical stock price data
#' for S&P 500 companies. It then groups the companies into predefined sectors
#' (e.g., Technology, Financial, Energy) based on a mapping and returns a
#' named list of data frames, one per sector. Each data frame includes
#' a 'Date' column and columns for the companies in that sector.
#'
#' The Excel file should contain a `Date` column as the first column, followed
#' by one column per company with daily closing prices or returns.
#'
#' @param file_path Path to the Excel file containing the stock data.
#'
#' @return A named list of data frames. Each data frame contains the 'Date'
#'         column and the columns corresponding to companies in a given sector.
#'
#' @importFrom readxl read_excel
#' @importFrom stats predict complete.cases sd
#' @importFrom dplyr desc
#' @importFrom magrittr %>%
#' @importFrom utils head tail
#' @importFrom stats reorder

#' @export
#'
#' @examples
#'
#' # Load and preprocess data
#' file_path <- "data/sp500_daily_2010_2020.csv"
#' sectors_data <- load_and_preprocess_data(file_path)
#'
#' # View names of all sectors
#' names(sectors_data)
load_and_preprocess_data <- function(file_path) {
  stocksr <- readxl::read_excel(file_path)
  dates <- stocksr[[1]]

  # Create a mapping of companies to sectors
  sector_mapping <- list(
    # Technology
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

    # Financial
    "jp morgan chase & co." = "Financial",
    "visa 'a'" = "Financial",
    "mastercard" = "Financial",
    "bank of america" = "Financial",
    "citigroup" = "Financial",
    "wells fargo & co" = "Financial",
    "morgan stanley" = "Financial",

    # Energy
    "exxon mobil" = "Energy",
    "chevron" = "Energy",
    "conocophillips" = "Energy",
    "schlumberger" = "Energy",
    "eog res." = "Energy",

    # Health Care
    "johnson & johnson" = "Health Care",
    "unitedhealth group" = "Health Care",
    "eli lilly" = "Health Care",
    "pfizer" = "Health Care",
    "merck & company" = "Health Care",

    # Consumer Discretionary
    "amazon.com" = "Consumer Discretionary",
    "tesla" = "Consumer Discretionary",
    "home depot" = "Consumer Discretionary",
    "mcdonald's" = "Consumer Discretionary",
    "nike 'b'" = "Consumer Discretionary",

    # Communication Services
    "alphabet a" = "Communication Services",
    "walt disney" = "Communication Services",
    "netflix" = "Communication Services",
    "comcast a" = "Communication Services",
    "t-mobile us" = "Communication Services"

  )

  # Group companies by sector
  sectors <- list()
  sector_names <- unique(unlist(sector_mapping))

  for (sector in sector_names) {
    # Get companies in this sector
    sector_companies <- names(sector_mapping)[sapply(sector_mapping,
                                                     function(x) x == sector)]

    # Create a dataframe for this sector
    sector_df <- data.frame(Date = dates)

    for (company in sector_companies) {
      if (company %in% colnames(stocksr)) {
        sector_df[[company]] <- stocksr[[company]]
      }
    }

    if (ncol(sector_df) > 1) {  # Check if we found any companies
      sectors[[sector]] <- sector_df
    }
  }

  sectors
}
