# Define test cases for influence_diagnostics_plot
test_that("influence_diagnostics_plot works as intended", {
    # Test 1: Normal case with valid inputs - Cook's Distance, return as plot
    set.seed(123)
    n <- 10
    p <- 2
    X <- matrix(rnorm(n * p), n, p)
    y <- rnorm(n)
    X <- cbind(1, X)
    model <- lm(y ~ X - 1)
    expect_silent(influence_diagnostics_plot(model, "cooks_distance", "plot", X, y))

    # Test 2: Normal case with valid inputs - DFFITS, return as measure
    dffits_values <- influence_diagnostics_plot(model, "dffits", "measure", X, y)
    expect_equal(length(dffits_values), n)
    expect_false(any(is.na(dffits_values)))

    # Test 3: Normal case with valid inputs - Hadi's Influence Measure, return as plot
    expect_silent(influence_diagnostics_plot(model, "hadis_influence", "plot", X, y))

    # Test 4: NA values in X
    X_with_na <- X
    X_with_na[1, 1] <- NA
    expect_error(influence_diagnostics_plot(model, "cooks_distance", "measure",
                                            X_with_na, y), "NA values in X")

    # Test 5: NA values in y
    y_with_na <- y
    y_with_na[1] <- NA
    expect_error(influence_diagnostics_plot(model, "dffits", "measure", X,
                                            y_with_na), "NA values in y")

    # Test 6: Inf values in X
    X_with_inf <- X
    X_with_inf[1, 1] <- Inf
    expect_error(influence_diagnostics_plot(model, "hadis_influence", "measure",
                                            X_with_inf, y), "Inf values in X")

    # Test 7: Inf values in y
    y_with_inf <- y
    y_with_inf[1] <- Inf
    expect_error(influence_diagnostics_plot(model, "cooks_distance", "measure",
                                            X, y_with_inf), "Inf values in y")

    # Test 8: Wrong format for X (not a matrix)
    X_wrong_format <- as.data.frame(X)
    expect_error(influence_diagnostics_plot(model, "dffits", "measure",
                                            X_wrong_format, y), "X should be a matrix")

    # Test 9: Wrong format for y (not a numeric vector)
    y_wrong_format <- as.character(y)
    expect_error(influence_diagnostics_plot(model, "hadis_influence", "measure",
                                            X, y_wrong_format), "y should be a numeric vector")

    # Test 10: Wrong dimensions (number of rows in X not equal to length of y)
    X_wrong_dimensions <- X[-1, ]
    expect_error(influence_diagnostics_plot(model, "cooks_distance", "measure",
                                            X_wrong_dimensions, y),
                 "Dimensions of X and y do not match")

    # Test 11: Invalid measure specified
    expect_error(influence_diagnostics_plot(model, "invalid_measure", "measure",
                                            X, y), "Invalid measure specified.
                 Valid options are: 'cooks_distance', 'dffits', 'hadis_influence'.")

    # Test 12: Invalid return_type specified
    expect_error(influence_diagnostics_plot(model, "cooks_distance", "invalid_type",
                                            X, y), "Invalid return_type specified.
                 Valid options are: 'measure', 'plot'.")
})
