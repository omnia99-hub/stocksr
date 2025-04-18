#' Load and preprocess S&P 500 data
#'
#' Loads Excel file of stock prices and groups companies by sector.
#'
#' @param file_path Path to the Excel file.
#'
#' @return A named list of data frames, each for a sector.
#'
#' @importFrom readxl read_excel
#' @importFrom stats predict complete.cases sd
#' @importFrom dplyr desc
#' @importFrom magrittr %>%
#' @export
load_and_preprocess_data <- function(file_path) {
  sp500_data <- readxl::read_excel(file_path)
  dates <- sp500_data[[1]]
  sector_mapping <- list(
    "MICROSOFT" = "Technology", "APPLE" = "Technology",
    "NVIDIA" = "Technology", "BROADCOM" = "Technology",
    "ORACLE" = "Technology", "ADOBE (NAS)" = "Technology",
    "SALESFORCE" = "Technology", "CISCO SYSTEMS" = "Technology",
    "QUALCOMM" = "Technology", "INTEL" = "Technology",
    "TEXAS INSTRUMENTS" = "Technology",
    "ADVANCED MICRO DEVICES" = "Technology",
    "JP MORGAN CHASE & CO." = "Financial", "VISA 'A'" = "Financial",
    "MASTERCARD" = "Financial", "BANK OF AMERICA" = "Financial",
    "CITIGROUP" = "Financial", "WELLS FARGO & CO" = "Financial",
    "MORGAN STANLEY" = "Financial", "EXXON MOBIL" = "Energy",
    "CHEVRON" = "Energy", "CONOCOPHILLIPS" = "Energy",
    "SCHLUMBERGER" = "Energy", "EOG RES." = "Energy",
    "JOHNSON & JOHNSON" = "Health Care",
    "UNITEDHEALTH GROUP" = "Health Care", "ELI LILLY" = "Health Care",
    "PFIZER" = "Health Care", "MERCK & COMPANY" = "Health Care",
    "AMAZON.COM" = "Consumer Discretionary", "TESLA" = "Consumer Discretionary",
    "HOME DEPOT" = "Consumer Discretionary",
    "MCDONALD'S" = "Consumer Discretionary",
    "NIKE 'B'" = "Consumer Discretionary",
    "ALPHABET A" = "Communication Services",
    "WALT DISNEY" = "Communication Services",
    "NETFLIX" = "Communication Services",
    "COMCAST A" = "Communication Services",
    "T-MOBILE US" = "Communication Services"
  )
  sectors <- list()
  for (sector in unique(unlist(sector_mapping))) {
    companies <- names(sector_mapping)[unlist(sector_mapping) == sector]
    sector_df <- data.frame(Date = dates)
    for (company in companies) {
      if (company %in% colnames(sp500_data)) {
        sector_df[[company]] <- sp500_data[[company]]
      }
    }
    if (ncol(sector_df) > 1) {
      sectors[[sector]] <- sector_df
    }
  }
  return(sectors)
}
