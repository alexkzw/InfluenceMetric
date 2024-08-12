#' Calculate DFFITS Measure
#'
#' This function calculates DFFITS for each observation in the dataset, which
#' is a measure of the influence of each observation on the fitted regression model.
#'
#' @param X A numeric matrix representing the design matrix (including the intercept term).
#' @param y A numeric vector representing the response variable.
#'
#' @return A numeric vector containing DFFITS for each observation.
#'
#' @keywords internal
calculate_dffits <- function(X, y) {
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

    # Calculate mean squared error
    mse <- sum(residuals^2) / (n - p)

    # Calculate standardized residuals
    standardized_residuals <- residuals / sqrt(mse * (1 - h_ii))

    # Calculate DFFITS values
    dffits_values <- standardized_residuals * sqrt(h_ii / (1 - h_ii))

    return(dffits_values)
}
