#' Influence Diagnostics and Plot for Linear Models
#'
#' This function calculates a specified influence measure (Cook's Distance, DFFITS, or Hadi's Influence Measure)
#' for a given linear model and dataset, and either returns the calculated measure or generates a plot.
#'
#' @param model An object of class lm representing the linear model.
#' @param measure A character string specifying which influence measure to calculate. Options are "cooks_distance", "dffits", and "hadis_influence".
#' @param return_type A character string specifying the return type. Options are "measure" to return the calculated influence measure, or "plot" to generate a plot.
#' @param X A numeric matrix representing the design matrix (including the intercept term). (Required)
#' @param y A numeric vector representing the response variable. (Required)
#'
#' @return Either a numeric vector containing the specified influence measure or a plot object, depending on the return_type argument.
#' @importFrom graphics abline
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 10  # number of observations
#' p <- 2   # number of predictors (excluding intercept)
#'
#' # Generate random data
#' X <- matrix(rnorm(n * p), n, p)
#' y <- rnorm(n)
#'
#' # Add intercept term to the design matrix
#' X <- cbind(1, X)
#'
#' # Fit linear model
#' model <- lm(y ~ X - 1)
#'
#' # Calculate Cook's Distance and plot the results
#' influence_diagnostics_plot(model, "cooks_distance", "plot", X, y)
#'
#' # Return DFFITS values
#' dffits_values <- influence_diagnostics_plot(model, "dffits", "measure", X, y)
#' print(dffits_values)
influence_diagnostics_plot <- function(model, measure = "cooks_distance", return_type = "plot", X, y) {
    check_for_errors(X, y)

    if (measure == "cooks_distance") {
        result <- calculate_cooks_distance(X, y)
        plot_title <- "Cook's Distance"
        plot_threshold <- 4 / length(result)
    } else if (measure == "dffits") {
        result <- calculate_dffits(X, y)
        plot_title <- "DFFITS"
        plot_threshold <- 2 * sqrt(2 / length(result))
    } else if (measure == "hadis_influence") {
        result <- calculate_hadis_influence(X, y)
        plot_title <- "Hadi's Influence Measure"
        plot_threshold <- 2
    } else {
        stop("Invalid measure specified. Choose 'cooks_distance', 'dffits', or 'hadis_influence'.")
    }

    if (return_type == "measure") {
        return(result)
    } else if (return_type == "plot") {
        plot(result, type = "h", main = plot_title, ylab = plot_title, xlab = "Observation Index")
        abline(h = plot_threshold, col = "red")
    } else {
        stop("Invalid return_type specified. Choose 'measure' or 'plot'.")
    }
}
