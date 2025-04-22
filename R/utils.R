# Suppress global variable notes during R CMD check
utils::globalVariables(c(
  "stocksr", "Date", "returns", "sector_index", "R2", "IncNodePurity",
  ".", "%>%", "sd", "desc", "complete.cases", "Sector", "RMSE", "theme",
  "Value", "Type", "Feature", ".data", "Time", "Actual", "Predicted", "across",
  "log_price", "Importance", "lag_1", "lag_2", "lag_5"
))
