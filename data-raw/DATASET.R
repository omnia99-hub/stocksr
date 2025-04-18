# Load CSV dataset
stocksr <- readr::read_csv(
  "C:/Users/omnia/Downloads/S&P 500 daily dataset 2010-2020.csv"
)

# Clean column names: lowercase and replace spaces with underscores
library(tidyverse)
stocksr <- stocksr %>%
  rename_with(~ str_replace_all(tolower(.), "\\s+", "_"))

# Rename first column to 'date'
library(dplyr)
stocksr <- rename(stocksr, date = `#name?`)

# Save raw cleaned dataset
usethis::use_data(stocksr, overwrite = TRUE)

# Summary of columns
df <- data.frame(
  columns = names(stocksr),
  class = sapply(stocksr, class),
  nas = sapply(stocksr, function(x) sum(is.na(x))),
  unique_vals = sapply(stocksr, function(x) length(unique(x)))
)

# Identify columns with more than 366 missing values
col_name <- df$columns[df$nas > 366]
print(col_name)

# Drop columns with too many missing values
df_cleaned2 <- stocksr[, !(names(stocksr) %in% col_name), drop = FALSE]

# Count remaining missing values
sapply(df_cleaned2, function(x) sum(is.na(x)))

# Replace remaining NAs with 0
df_cleaned2[is.na(df_cleaned2)] <- 0

# Check column classes
sapply(df_cleaned2, class)

# Convert date column
library(lubridate)
df_cleaned2$date <- as.Date(df_cleaned2$date, format = "%d/%m/%Y")
df_cleaned2$date <- ymd(df_cleaned2$date)

# Save final cleaned dataset
usethis::use_data(df_cleaned2, overwrite = TRUE)
