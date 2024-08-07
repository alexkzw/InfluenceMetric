#' Calculate Hadi's Influence Measure
#'
#' This function calculates Hadi's Influence Measure for each observation in the
#' dataset, which is a measure of the influence of each observation on the
#' fitted regression model.
#'
#' @param X A numeric matrix representing the design matrix (including the intercept term).
#' @param y A numeric vector representing the response variable.
#'
#' @return A numeric vector containing Hadi's Influence Measure for each observation.
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
#' # Calculate Hadi's Influence Measure
#' hadis_influence <- calculate_hadis_influence(X, y)
#'
#' # Print Hadi's Influence Measure
#' print(hadis_influence)
calculate_hadis_influence <- function(X, y) {
    check_for_errors(X, y)

    n <- nrow(X)
    p <- ncol(X)

    # Calculate hat matrix H
    X_inv <- solve(t(X) %*% X)
    H <- X %*% X_inv %*% t(X)
    h_ii <- diag(H)

    # Calculate regression coefficients and fitted values
    beta_hat <- X_inv %*% t(X) %*% y
    y_hat <- X %*% beta_hat
    residuals <- y - y_hat

    # Calculate sum of squared errors (SSE)
    SSE <- sum(residuals^2)

    # Calculate normalised residuals
    d_i <- residuals / sqrt(SSE)

    # Calculate Hadi's Influence Measure
    hadis_influence <- numeric(n)
    for (i in 1:n) {
        part1 <- h_ii[i] / (1 - h_ii[i])
        part2 <- ((p + 1) / (1 - h_ii[i])) * (d_i[i]^2 / (1 - d_i[i]^2))
        hadis_influence[i] <- part1 + part2
    }

    return(hadis_influence)
}
