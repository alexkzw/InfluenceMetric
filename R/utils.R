#helper function to check for errors
check_for_errors <- function(X, y) {
    if (any(is.na(X))) stop("NA values in X")
    if (any(is.na(y))) stop("NA values in y")
    if (any(is.infinite(X))) stop("Inf values in X")
    if (any(is.infinite(y))) stop("Inf values in y")
    if (!is.matrix(X)) stop("X should be a matrix")
    if (!is.numeric(y)) stop("y should be a numeric vector")
    if (nrow(X) != length(y)) stop("Dimensions of X and y do not match")
}
