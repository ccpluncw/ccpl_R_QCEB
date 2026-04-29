# Tests for buildQCEswitchThreshold -- shape, rule validation, value
# constraints (especially randomIntBetween's exact-2 rule), JSON round-trip.

test_that("fixed rule with single value produces 2-key list", {
    th <- buildQCEswitchThreshold(values = 5, rule = "fixed")
    expect_equal(th$values, 5)
    expect_equal(th$rule, "fixed")
})

test_that("randomFromList accepts multiple values", {
    th <- buildQCEswitchThreshold(values = c(3, 5, 7), rule = "randomFromList")
    expect_equal(th$values, c(3, 5, 7))
    expect_equal(th$rule, "randomFromList")
})

test_that("randomIntBetween accepts exactly 2 values", {
    th <- buildQCEswitchThreshold(values = c(3, 7), rule = "randomIntBetween")
    expect_equal(th$values, c(3, 7))
    expect_equal(th$rule, "randomIntBetween")
})

test_that("default rule is 'fixed'", {
    th <- buildQCEswitchThreshold(values = 10)
    expect_equal(th$rule, "fixed")
})

test_that("invalid rule throws", {
    expect_error(
        buildQCEswitchThreshold(values = 5, rule = "monteCarlo"),
        "rule option must take on one of the following values"
    )
})

test_that("non-numeric values throws", {
    expect_error(
        buildQCEswitchThreshold(values = c("a", "b"), rule = "randomFromList"),
        "values option must be a numeric vector"
    )
})

test_that("empty values throws", {
    expect_error(
        buildQCEswitchThreshold(values = numeric(0), rule = "fixed"),
        "values option must be a numeric vector"
    )
})

test_that("missing values throws", {
    expect_error(
        buildQCEswitchThreshold(rule = "fixed"),
        "values option is required"
    )
})

test_that("randomIntBetween with 1 value throws", {
    expect_error(
        buildQCEswitchThreshold(values = 5, rule = "randomIntBetween"),
        "requires exactly 2 values"
    )
})

test_that("randomIntBetween with 3 values throws", {
    expect_error(
        buildQCEswitchThreshold(values = c(1, 5, 10), rule = "randomIntBetween"),
        "requires exactly 2 values"
    )
})

test_that("non-string rule throws", {
    expect_error(
        buildQCEswitchThreshold(values = 5, rule = c("fixed", "randomFromList")),
        "rule option must be a single string"
    )
})

test_that("JSON round-trip preserves shape", {
    th <- buildQCEswitchThreshold(values = c(3, 7), rule = "randomIntBetween")
    json <- jsonlite::toJSON(th, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    # values is a numeric vector → JSON array of numbers
    expect_equal(unlist(parsed$values), c(3, 7))
    # rule is scalar → wrapped as 1-element array per QCEP convention
    expect_equal(parsed$rule[[1]], "randomIntBetween")
})
