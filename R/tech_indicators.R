#' Calculate technical indicators
#'
#' Adds moving averages, RSI, volatility, and lagged features to a time series.
#'
#' @param df A data frame with Date and sector_index.
#'
#' @return A data frame with technical indicators added.
#'
#' @importFrom TTR SMA EMA RSI
#' @importFrom zoo rollapply
#' @importFrom dplyr lag
#' @export
calculate_technical_indicators <- function(df) {
  df$Date <- as.Date(df$Date)
  df$returns <- c(NA, diff(df$sector_index) / lag(df$sector_index, 1)[-1])
  df$MA5 <- TTR::SMA(df$sector_index, n = 5)
  df$MA10 <- TTR::SMA(df$sector_index, n = 10)
  df$MA20 <- TTR::SMA(df$sector_index, n = 20)
  df$EMA5 <- TTR::EMA(df$sector_index, n = 5)
  df$EMA10 <- TTR::EMA(df$sector_index, n = 10)
  df$volatility5 <- zoo::rollapply(df$sector_index, 5, sd, fill = NA)
  df$volatility10 <- zoo::rollapply(df$sector_index, 10, sd, fill = NA)
  df$RSI <- TTR::RSI(df$sector_index, n = 14)
  for (i in 1:5) {
    df[[paste0("lag_", i)]] <- dplyr::lag(df$sector_index, i)
    df[[paste0("returns_lag_", i)]] <- dplyr::lag(df$returns, i)
  }
  df <- df[complete.cases(df), ]
  return(df)
}
