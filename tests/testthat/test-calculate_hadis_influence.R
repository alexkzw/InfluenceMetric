# Define test cases for calculate_hadis_influence
test_that("calculate_hadis_influence works as intended", {
    # Test 1: Normal case with valid inputs
    set.seed(123)
    n <- 10
    p <- 2
    X <- matrix(rnorm(n * p), n, p)
    y <- rnorm(n)
    X <- cbind(1, X)
    hadis_influence <- calculate_hadis_influence(X, y)
    expect_equal(length(hadis_influence), n)
    expect_false(any(is.na(hadis_influence)))

    # Test 2: NA values in X
    X_with_na <- X
    X_with_na[1, 1] <- NA
    expect_error(calculate_hadis_influence(X_with_na, y), "NA values in X")

    # Test 3: NA values in y
    y_with_na <- y
    y_with_na[1] <- NA
    expect_error(calculate_hadis_influence(X, y_with_na), "NA values in y")

    # Test 4: Inf values in X
    X_with_inf <- X
    X_with_inf[1, 1] <- Inf
    expect_error(calculate_hadis_influence(X_with_inf, y), "Inf values in X")

    # Test 5: Inf values in y
    y_with_inf <- y
    y_with_inf[1] <- Inf
    expect_error(calculate_hadis_influence(X, y_with_inf), "Inf values in y")

    # Test 6: Wrong format for X (not a matrix)
    X_wrong_format <- as.data.frame(X)
    expect_error(calculate_hadis_influence(X_wrong_format, y), "X should be a matrix")

    # Test 7: Wrong format for y (not a numeric vector)
    y_wrong_format <- as.character(y)
    expect_error(calculate_hadis_influence(X, y_wrong_format), "y should be a numeric vector")

    # Test 8: Wrong dimensions (number of rows in X not equal to length of y)
    X_wrong_dimensions <- X[-1, ]
    expect_error(calculate_hadis_influence(X_wrong_dimensions, y), "Dimensions of X and y do not match")
})
