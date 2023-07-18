#' Generate QQ plots for a subvector of a dataset
#'
#' @title Generate QQ Plots
#'
#' @description
#' This function generates QQ plots for a specified subvector of a dataset. If no
#' subvector is specified, the function iterates over all variables in the dataset,
#' creating QQ plots for numeric variables and ignoring non-numeric variables.
#'
#' @param dataset The dataset to generate the QQ plots for.
#' @param dataset_subvector (Optional) The subvector of variables to generate QQ plots for.
#'
#' @examples
#' generate_qqplots(mtcars, c("mpg", "cyl", "disp"))
#'
#' @export
generate_qqplots <- function(dataset, dataset_subvector = NULL) {
  # iterate over entire dataset, if no subvector is specified
  if (is.null(dataset_subvector)) {
    dataset_subvector <- names(dataset)
  }
  # iterate over subvector
  for (variable in dataset_subvector) {
    if (is.numeric(dataset[[variable]])) {
      # create qq-plot if variable is numerical
      qqnorm(dataset[[variable]], main = paste("Normal Q-Q Plot:", variable))
      qqline(dataset[[variable]], col = "red")
    } else {
      # skip non-numerical variables
      cat("Skipping non-numerical variable:", variable, "\n")
    }
  }
}
