# Suppress global variable notes during R CMD check
utils::globalVariables(c(
  "stocksr", "Date", "returns", "sector_index", "R²", "IncNodePurity",
  ".", "%>%","sd", "desc", "complete.cases", "Sector", "RMSE", "theme",
  "Value", "Type", "Feature"
))
