calculate_dffits <- function(X, y) {
    check_for_errors(X, y)

    n <- nrow(X)
    p <- ncol(X)

    # Calculate the hat matrix H
    X_inv <- solve(t(X) %*% X)
    H <- X %*% X_inv %*% t(X)
    h_ii <- diag(H)

    # Calculate the regression coefficients and fitted values
    beta_hat <- X_inv %*% t(X) %*% y
    y_hat <- X %*% beta_hat
    residuals <- y - y_hat

    # Calculate the mean squared error
    mse <- sum(residuals^2) / (n - p)

    # Calculate the standardized residuals
    standardized_residuals <- residuals / sqrt(mse * (1 - h_ii))

    # Calculate the DFFITS values
    dffits_values <- standardized_residuals * sqrt(h_ii / (1 - h_ii))

    return(dffits_values)
}
