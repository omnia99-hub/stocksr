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
#' @importFrom TTR SMA EMA RSI MACD
#' @importFrom zoo rollapply
#' @importFrom dplyr lag filter_all all_vars
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
  df_copy$log_price <- log(df_copy$sector_index)
  df_copy$returns <- c(NA, diff(df_copy$sector_index) /
                         lag(df_copy$sector_index, 1)[-1])
  df_copy$log_returns <- c(NA, diff(df_copy$log_price))

  # Moving averages
  ma_windows <- c(5, 10, 20, 50)
  for (w in ma_windows) {
    df_copy[[paste0("MA", w)]] <- SMA(df_copy$sector_index, n = w)
    df_copy[[paste0("EMA", w)]] <- EMA(df_copy$sector_index, n = w)
  }

  # MACD
  macd <- TTR::MACD(df_copy$sector_index, nFast = 12, nSlow = 26, nSig = 9)
  df_copy$MACD <- macd[, "macd"]
  df_copy$MACD_signal <- macd[, "signal"]

  # Volatility
  df_copy$volatility5 <- rollapply(df_copy$log_returns,
    width = 5,
    FUN = sd, fill = NA, align = "right"
  )
  df_copy$volatility20 <- rollapply(df_copy$log_returns,
    width = 20,
    FUN = sd, fill = NA, align = "right"
  )

  # Lagged features
  for (lag in c(1, 2, 5)) {
    df_copy[[paste0("lag_", lag)]] <- lag(df_copy$sector_index, lag)
    df_copy[[paste0("log_return_lag_", lag)]] <- lag(df_copy$log_returns, lag)
  }

  # Time features
  df_copy$day_of_week <- lubridate::wday(df_copy$Date, label = TRUE)
  df_copy$month <- lubridate::month(df_copy$Date, label = TRUE)

  # Remove NAs
  df_copy <- df_copy %>%
    drop_na() %>%
    dplyr::filter_all(dplyr::all_vars(!is.infinite(.)))

  df_copy
}
