#' Calculate Technical Indicators
#'
#' Enhances a sector index time series by computing common technical indicators,
#' including moving averages (SMA, EMA), volatility, Relative Strength Index
#' (RSI), and lagged features. Useful for feature engineering in financial
#' modeling and machine learning.
#'
#' @param df A data frame with columns:
#'   \describe{
#'     \item{Date}{A date column.}
#'     \item{sector_index}{A numeric column for sector-level price/index.}
#'   }
#'
#' @return A data frame with the original columns plus:
#' \describe{
#'   \item{returns}{Daily returns.}
#'   \item{MA5, MA10, MA20}{Simple Moving Averages.}
#'   \item{EMA5, EMA10}{Exponential Moving Averages.}
#'   \item{volatility5, volatility10}{Rolling standard deviations.}
#'   \item{RSI}{Relative Strength Index.}
#'   \item{lag_1 to lag_5}{Lagged index values.}
#'   \item{returns_lag_1 to returns_lag_5}{Lagged return values.}
#' }
#'
#' @importFrom TTR SMA EMA RSI
#' @importFrom zoo rollapply
#' @importFrom dplyr lag
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' set.seed(42)
#' dates <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = 100)
#' index <- cumsum(rnorm(100, mean = 0.1, sd = 1))
#' df <- data.frame(Date = dates, sector_index = index)
#' calculate_technical_indicators(df)
calculate_technical_indicators <- function(df) {
  df_copy <- df
  df_copy$Date <- as.Date(df_copy$Date)

  df_copy$returns <- c(NA, diff(df_copy$sector_index) /
                         lag(df_copy$sector_index, 1)[-1])

  df_copy$MA5 <- SMA(df_copy$sector_index, n = 5)
  df_copy$MA10 <- SMA(df_copy$sector_index, n = 10)
  df_copy$MA20 <- SMA(df_copy$sector_index, n = 20)

  df_copy$EMA5 <- EMA(df_copy$sector_index, n = 5)
  df_copy$EMA10 <- EMA(df_copy$sector_index, n = 10)

  df_copy$volatility5 <- rollapply(df_copy$sector_index, 5, sd,
                                   align = "right", fill = NA)
  df_copy$volatility10 <- rollapply(df_copy$sector_index, 10, sd,
                                    align = "right", fill = NA)

  df_copy$RSI <- RSI(df_copy$sector_index, n = 14)

  for (lag in 1:5) {
    df_copy[[paste0("lag_", lag)]] <- lag(df_copy$sector_index, lag)
    df_copy[[paste0("returns_lag_", lag)]] <- lag(df_copy$returns, lag)
  }

  df_copy <- df_copy %>% drop_na()
  return(df_copy)
}
