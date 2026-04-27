# Tests for buildQCEshowIfCompound -- kind validation, conditions list shape,
# nesting, and JSON round-trip.

test_that("compound 'all' wraps conditions correctly", {
    c <- buildQCEshowIfCompound("all", list(
        buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"),
        buildQCEshowIfCondition("over18", "equals", "Yes", "Response")
    ))
    expect_false(is.null(c$all))
    expect_null(c$any)
    expect_equal(length(c$all), 2L)
    expect_equal(c$all[[1]]$stimRef, "smoker")
    expect_equal(c$all[[2]]$stimRef, "over18")
})

test_that("compound 'any' wraps conditions correctly", {
    c <- buildQCEshowIfCompound("any", list(
        buildQCEshowIfCondition("vapes",    "equals", "Yes", "Response"),
        buildQCEshowIfCondition("chewsTob", "equals", "Yes", "Response")
    ))
    expect_false(is.null(c$any))
    expect_null(c$all)
    expect_equal(length(c$any), 2L)
})

test_that("nested compounds work", {
    c <- buildQCEshowIfCompound("all", list(
        buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"),
        buildQCEshowIfCompound("any", list(
            buildQCEshowIfCondition("vapes",    "equals", "Yes", "Response"),
            buildQCEshowIfCondition("chewsTob", "equals", "Yes", "Response")
        ))
    ))
    expect_equal(length(c$all), 2L)
    expect_false(is.null(c$all[[2]]$any))
    expect_equal(length(c$all[[2]]$any), 2L)
})

test_that("invalid kind throws", {
    expect_error(
        buildQCEshowIfCompound("AND", list(
            buildQCEshowIfCondition("x", "wasShown")
        )),
        "kind option must take on one of the following values"
    )
})

test_that("non-list conditions arg throws", {
    expect_error(
        buildQCEshowIfCompound("all", "not a list"),
        "conditions option must be a list of conditions"
    )
})

test_that("empty conditions list throws", {
    expect_error(
        buildQCEshowIfCompound("all", list()),
        "must contain at least one condition"
    )
})

test_that("malformed child (no stimRef/operator/all/any) throws", {
    expect_error(
        buildQCEshowIfCompound("all", list(
            list(foo = "bar")
        )),
        "is not a valid showIf condition",
        fixed = TRUE
    )
})

test_that("non-list child throws", {
    expect_error(
        buildQCEshowIfCompound("all", list(
            "not a list"
        )),
        "must be a list",
        fixed = TRUE
    )
})

test_that("hand-rolled child missing operator is rejected", {
    expect_error(
        buildQCEshowIfCompound("all", list(
            list(stimRef = "x")  # missing operator
        )),
        "is not a valid showIf condition",
        fixed = TRUE
    )
})

test_that("JSON round-trip preserves compound + nesting", {
    c <- buildQCEshowIfCompound("all", list(
        buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"),
        buildQCEshowIfCompound("any", list(
            buildQCEshowIfCondition("vapes",    "equals", "Yes", "Response"),
            buildQCEshowIfCondition("chewsTob", "equals", "Yes", "Response")
        ))
    ))
    json <- jsonlite::toJSON(c, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_equal(length(parsed$all), 2L)
    expect_equal(parsed$all[[1]]$stimRef[[1]], "smoker")
    expect_equal(length(parsed$all[[2]]$any), 2L)
})
