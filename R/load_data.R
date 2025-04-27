#' Load and preprocess S&P 500 data
#'
#' Loads an Excel file of S&P 500 prices and groups companies into sectors.
#' Each sector is returned as a data frame with 'Date' and company columns.
#'
#' @param df_cleaned2 A data frame of cleaned S&P 500 stock data
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
#' @examples
#'
#' # Load and preprocess data
#' data(df_cleaned2)
#' sectors_data <- load_and_preprocess_data(df_cleaned2)
#'
#' # View names of all sectors
#' names(sectors_data)
load_and_preprocess_data <- function(df_cleaned2) {
  dates <- df_cleaned2[[1]]

  # Only include Energy and Healthcare sectors
  sector_mapping <- list(
    "exxon_mobil" = "Energy",
    "chevron" = "Energy",
    "conocophillips" = "Energy",
    "schlumberger" = "Energy",
    "eog_res." = "Energy",
    "johnson_&_johnson" = "Healthcare",
    "unitedhealth_group" = "Healthcare",
    "eli_lilly" = "Healthcare",
    "pfizer" = "Healthcare",
    "merck_&_company" = "Healthcare"
  )

  sectors <- list()
  sector_names <- unique(unlist(sector_mapping))
  print(sector_names)

  for (sector in sector_names) {
    sector_companies <- names(sector_mapping)[
      sapply(sector_mapping, function(x) x == sector)
    ]

    sector_df <- data.frame(Date = dates)

    for (company in sector_companies) {
      if (company %in% colnames(df_cleaned2)) {
        sector_df[[company]] <- df_cleaned2[[company]]
      }
    }

    if (ncol(sector_df) > 1) {
      sectors[[sector]] <- sector_df
    }
  }

  sectors
}
