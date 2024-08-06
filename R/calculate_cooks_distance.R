calculate_cooks_distance <- function(X, y){
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
