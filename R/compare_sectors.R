#' Compare Model Performance Across Sectors
#'
#' This function summarizes and visualizes the performance of models built
#' for different sectors. It prints and plots RMSE and R² for each sector
#' and returns a sorted comparison data frame.
#'
#' @param sector_models A named list of model evaluation results for each sector
#' .Each element in the list should be a list with numeric components `rmse`,
#' `mae`, and `r2`, typically returned by a model-building function.
#'
#' @return A data frame containing performance metrics (`RMSE`, `MAE`, and `R2`)
#'         for each sector, sorted in descending order of `R2`.
#'
#' @importFrom dplyr arrange
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal labs element_text
#' @importFrom gridExtra grid.arrange
#' @export
#'
#' @examples
#' \dontrun{
#' # Suppose you have results from multiple models
#' models <- list(
#'   Technology = list(rmse = 20.5, mae = 15.2, r2 = 0.89),
#'   Financial = list(rmse = 25.1, mae = 18.3, r2 = 0.81),
#'   Energy = list(rmse = 30.2, mae = 22.8, r2 = 0.75)
#' )
#'
#' # Compare them
#' sector_summary <- compare_sectors(models)
#' print(sector_summary)
#' }

compare_sectors <- function(sector_models) {
  # Create comparison dataframe
  comparison <- data.frame(
    Sector = names(sector_models),
    RMSE = sapply(sector_models, function(x) x$rmse),
    MAE = sapply(sector_models, function(x) x$mae),
    R2 = sapply(sector_models, function(x) x$r2)
  )

  # Sort by R²
  comparison <- comparison %>%
    arrange(desc(R2))

  cat("\n--- Sector Model Performance Comparison ---\n")
  print(comparison)

  # Plot comparison
  p1 <- ggplot(comparison, aes(x = reorder(Sector, -RMSE), y = RMSE)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(title = "RMSE by Sector", x = "Sector") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p2 <- ggplot(comparison, aes(x = reorder(Sector, R2), y = R2)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    theme_minimal() +
    labs(title = "R2 Score by Sector", x = "Sector") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Save the combined plot
  grid.arrange(p1, p2, nrow = 2)

  comparison
}
