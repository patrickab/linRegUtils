#' Generate residual plots for a target variable & subvector of a dataset
#'
#' @title Generate Residual Plots
#'
#' @description
#' This function generates residual plots for a specified target variable and subvector of
#' variables in a dataset. If no subvector is specified, the function iterates over all
#' variables in the dataset, creating residual plots for numeric variables and ignoring
#' non-numeric variables.
#'
#' @param dataset The dataset to generate the residual plots for.
#' @param target_variable The target variable (dependent variable) for the residual plots.
#' @param dataset_subvector (Optional) The subvector of variables to generate residual plots for.
#'
#' @examples
#' generate_residual_plots(mtcars, "mpg", c("cyl", "disp"))
#'
#' @export
generate_residual_plots <- function(dataset, target_variable, dataset_subvector = NULL) {
  # iterate over entire dataset, if no subvector is specified
  if (is.null(dataset_subvector)) {
    dataset_subvector <- names(dataset)
  }
  # iterate over subvector
  for (variable in dataset_subvector) {
    if (is.numeric(dataset[[variable]])) {
      # create scatterplot if variable is numerical
      lm_model <- lm(dataset[[target_variable]] ~ dataset[[variable]], data = dataset)
      residuals <- resid(lm_model)

      plot(dataset[[variable]], residuals,
           xlab = variable,
           ylab = "Residuals",
           main = paste("Residual Plot:", variable))
      abline(h = 0, col = "red")
    } else {
      # skip non-numerical variables
      cat("Skipping non-numerical variable:", variable, "\n")
    }
  }
}
