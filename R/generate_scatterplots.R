#' Generate scatterplots for a target variable & subvector of a dataset
#'
#' @title Generate Scatterplots with Regression Lines
#'
#' @description
#' This function generates scatterplots with regression lines for a specified target variable
#' and subvector of variables in a dataset. If no subvector is specified, the function iterates
#' over all variables in the dataset, creating scatterplots for numeric variables and ignoring
#' non-numeric variables.
#'
#' @param dataset The dataset to generate the scatterplots for.
#' @param target_variable The target variable (dependent variable) for the scatterplots.
#' @param dataset_subvector (Optional) The subvector of variables to generate scatterplots for.
#'
#' @examples
#' generate_scatterplots(mtcars, "mpg", c("cyl", "disp"))
#'
#' @export
generate_scatterplots <- function(dataset, target_variable, dataset_subvector = NULL) {
  # iterate over entire dataset, if no subvector is specified
  if (is.null(dataset_subvector)) {
    dataset_subvector <- names(dataset)
  }
  # iterate over subvector
  for (variable in dataset_subvector) {
    if (is.numeric(dataset[[variable]])) {
      # create scatterplot if variable is numerical
      lm_model <- lm(dataset[[target_variable]] ~ dataset[[variable]], data = dataset)
      plot(dataset[[variable]], dataset[[target_variable]],
           xlab = variable,
           ylab = target_variable,
           main = paste("Scatterplot with Regression Line:", variable))
      abline(a = coef(lm_model)[1], b = coef(lm_model)[2], col = "red")
    } else {
      # skip non-numerical variables
      cat("Skipping non-numerical variable:", variable, "\n")
    }
  }
}
