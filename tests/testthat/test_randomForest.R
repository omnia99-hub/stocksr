library(testthat)
library(dplyr)
library(randomForest)
library(caret)
library(TTR)
library(zoo)


create_mock_data <- function() {
  set.seed(123)
  dates <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day")
  mock_stocks <- data.frame(Date = dates)
  mock_stocks$microsoft <- cumsum(rnorm(length(dates), 0.1, 1)) + 100
  mock_stocks$apple <- cumsum(rnorm(length(dates), 0.1, 1)) + 120
  return(mock_stocks)
}

stocksr <- create_mock_data()


load_and_preprocess_data_df <- function(df_cleaned2) {
  dates <- df_cleaned2[[1]]
  sector_mapping <- list(
    "microsoft" = "Technology",
    "apple" = "Technology"
  )
  sectors <- list()
  sector_names <- unique(unlist(sector_mapping))

  for (sector_name in sector_names) {
    sector_companies <- names(sector_mapping)[
      sapply(sector_mapping, function(x) x == sector_name)
    ]

    sector_df <- data.frame(Date = dates)

    for (company in sector_companies) {
      if (company %in% colnames(df_cleaned2)) {
        sector_df[[company]] <- df_cleaned2[[company]]
      }
    }

    if (ncol(sector_df) > 1) {
      sectors[[sector_name]] <- sector_df
    }
  }

  return(sectors)
}

calculate_sector_index <- function(sector_df) {
  sector_df$Date <- as.Date(sector_df$Date)
  company_cols <- setdiff(colnames(sector_df), "Date")
  sector_df <- sector_df %>%
    mutate(across(all_of(company_cols), ~na.approx(., na.rm = FALSE))) %>%
    tidyr::fill(all_of(company_cols), .direction = "downup")

  index_df <- sector_df %>%
    mutate(sector_index = rowMeans(select(., all_of(company_cols)),
                                   na.rm = TRUE)) %>%
    select(Date, sector_index)

  return(index_df)
}

test_that("load_and_preprocess_data_df returns correct sectors", {
  sectors <- load_and_preprocess_data_df(stocksr)
  expect_true("Technology" %in% names(sectors))
})

test_that("calculate_sector_index creates index", {
  tech_sector <- load_and_preprocess_data_df(stocksr)[["Technology"]]
  index_df <- calculate_sector_index(tech_sector)
  expect_equal(ncol(index_df), 2)
  expect_true("sector_index" %in% colnames(index_df))
})
