#' Calculate Cook's Distance Measure
#'
#' This function calculates Cook's Distance Measure for each observation in the
#' dataset, which is a measure of the influence of each observation on the
#' fitted regression model.
#'
#' @param X A numeric matrix representing the design matrix (including the intercept term).
#' @param y A numeric vector representing the response variable.
#'
#' @return A numeric vector containing Cook's Distance Measure for each observation.
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
#' # Calculate Cook's Distance
#' cooks_distances <- calculate_cooks_distance(X, y)
#'
#' # Print the Cook's Distances
#' print(cooks_distances)
#'
#' # Plot Cook's Distances
#' plot(cooks_distances, type = "h", main = "Cook's Distance",
#' ylab = "Cook's Distance", xlab = "Observation Index")
#' # Add a reference line for influential points
#' abline(h = 4 / (n - p - 1), col = "red")
calculate_cooks_distance <- function(X, y){

    #validate inputs
    check_for_errors(X, y)

    n <- nrow(X)
    p <- ncol(X)

    X_inv <- solve(t(X) %*% X)
    beta_hat <- X_inv %*% t(X) %*% y
    y_hat <- X %*% beta_hat
    residuals <- y - y_hat

    H <- X %*% X_inv %*% t(X)
    h_ii <- diag(H)

    sigma_squared <- sum(residuals^2) / (n - p)

    cooks_distance <- numeric(n)
    for (i in 1:n) {
        r_i <- residuals[i]
        h_ii <- H[i, i]

        cooks_distance[i] <- (r_i^2 / (p * sigma_squared)) * (h_ii / (1 - h_ii)^2)
    }

    return(cooks_distance)
}
