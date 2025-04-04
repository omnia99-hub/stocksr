## code to prepare `DATASET` dataset goes here
stocksr <- readr::read_csv("C:\\Users\\omnia\\Downloads\\S&P 500 daily dataset 2010-2020.csv")

library(tidyverse)
stocksr <- stocksr %>%
  rename_with(~ str_replace_all(tolower(.), "\\s+", "_"))
library(dplyr)
stocksr<- rename(stocksr, date = `#name?`)

usethis::use_data(stocksr, overwrite = TRUE)

df <- data.frame(
  columns = names(stocksr),
  Class = sapply(stocksr, class),
  NAs = sapply(stocksr,function(x) sum(is.na(x))),
  Unique_Values = sapply(stocksr,function(x) length(unique(x)))
)

col_name <- df$columns[df$NAs > 366]
print(col_name)

df_cleaned2 <- stocksr[, !(names(stocksr) %in% col_name), drop = FALSE]



sapply(df_cleaned2, function(x) sum(is.na(x)))

df_cleaned2[is.na(df_cleaned2)] <- 0

sapply(df_cleaned2,class)

library(lubridate)
df_cleaned2$date <- as.Date(df_cleaned2$date, format = "%d/%m/%Y")

df_cleaned2$date <- ymd(df_cleaned2$date)


usethis::use_data(df_cleaned2, overwrite = TRUE)
