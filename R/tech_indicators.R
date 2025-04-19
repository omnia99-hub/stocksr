#' Calculate Technical Indicators
#'
#' Enhances a sector index time series by computing common technical indicators,
#' including moving averages (SMA, EMA), volatility, Relative Strength Index
#' (RSI), and lagged features. Useful for feature engineering in financial
#' modeling and machine learning.
#'
#' @param df A data frame with at least two columns:
#'   \describe{
#'     \item{Date}{A date column.}
#'     \item{sector_index}{A numeric column representing sector-level index
#'     or price.}
#'   }
#'
#' @return A data frame with the original columns plus additional columns:
#' \describe{
#'   \item{returns}{Daily returns based on sector index.}
#'   \item{MA5, MA10, MA20}{Simple Moving Averages over 5, 10, and 20 days.}
#'   \item{EMA5, EMA10}{Exponential Moving Averages over 5 and 10 days.}
#'   \item{volatility5, volatility10}{Rolling standard deviation (volatility).}
#'   \item{RSI}{Relative Strength Index computed over a 14-day window.}
#'   \item{lag_1 to lag_5}{Lagged values of sector index from previous 1 to 5
#'   days.}
#'   \item{returns_lag_1 to returns_lag_5}{Lagged values of daily returns.}
#' }
#'
#' @importFrom TTR SMA EMA RSI
#' @importFrom zoo rollapply
#' @importFrom dplyr lag
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' # Simulated example of sector index data
#' set.seed(42)
#' sample_dates <- seq.Date(from = as.Date("2024-01-01"),
#'                          by = "day", length.out = 100)
#' sample_index <- cumsum(rnorm(100, mean = 0.1, sd = 1))
#'
#' df <- data.frame(Date = sample_dates, sector_index = sample_index)
#'
#' # Calculate technical indicators
#' df_with_indicators <- calculate_technical_indicators(df)
#'
#' # View first few rows
#' head(df_with_indicators)
calculate_technical_indicators <- function(df) {
  # Make a copy to avoid warnings
  df_copy <- df

  # Ensure Date is properly formatted
  df_copy$Date <- as.Date(df_copy$Date)

  # Calculate returns
  df_copy$returns <- c(NA, diff(df_copy$sector_index) /
                         lag(df_copy$sector_index, 1)[-1])

  # Calculate moving averages
  df_copy$MA5 <- SMA(df_copy$sector_index, n = 5)
  df_copy$MA10 <- SMA(df_copy$sector_index, n = 10)
  df_copy$MA20 <- SMA(df_copy$sector_index, n = 20)

  # Calculate exponential moving averages
  df_copy$EMA5 <- EMA(df_copy$sector_index, n = 5)
  df_copy$EMA10 <- EMA(df_copy$sector_index, n = 10)

  # Calculate volatility (rolling standard deviation)
  df_copy$volatility5 <- rollapply(df_copy$sector_index, width = 5,
                                   FUN = sd, align = "right", fill = NA)
  df_copy$volatility10 <- rollapply(df_copy$sector_index, width = 10,
                                    FUN = sd, align = "right", fill = NA)

  # Calculate Relative Strength Index (RSI)
  df_copy$RSI <- RSI(df_copy$sector_index, n = 14)

  # Create lagged features (previous days)
  for (lag in 1:5) {
    df_copy[[paste0("lag_", lag)]] <- lag(df_copy$sector_index, lag)
    df_copy[[paste0("returns_lag_", lag)]] <- lag(df_copy$returns, lag)
  }

  # Drop rows with NA values
  df_copy <- df_copy %>%
    drop_na()

  df_copy
}
