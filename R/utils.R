# Suppress global variable notes during R CMD check
utils::globalVariables(c(
  "Date", "returns", "sector_index", "R2", "IncNodePurity", ".", "%>%",
  "sd", "desc", "complete.cases", ".data", "actual", "predicted","time"
))
