# Tests for buildQCEshowIfCondition -- operator validation, value/field
# requirements, warning behavior, and JSON round-trip.

test_that("value-op condition produces 4-key list", {
    c <- buildQCEshowIfCondition("smoker", "equals", "Yes", "Response")
    expect_equal(c$stimRef, "smoker")
    expect_equal(c$operator, "equals")
    expect_equal(c$value, "Yes")
    expect_equal(c$field, "Response")
})

test_that("wasShown produces 2-key list (no value/field)", {
    c <- buildQCEshowIfCondition("instructions", "wasShown")
    expect_equal(c$stimRef, "instructions")
    expect_equal(c$operator, "wasShown")
    expect_null(c$value)
    expect_null(c$field)
})

test_that("wasNotShown produces 2-key list (no value/field)", {
    c <- buildQCEshowIfCondition("optional", "wasNotShown")
    expect_equal(c$operator, "wasNotShown")
    expect_null(c$value)
    expect_null(c$field)
})

test_that("invalid operator throws with descriptive error", {
    expect_error(
        buildQCEshowIfCondition("x", "equls", "y", "z"),
        "operator option must take on one of the following values"
    )
})

test_that("value-op without value throws teaching error", {
    expect_error(
        buildQCEshowIfCondition("smoker", "equals", value = NULL, field = "Response"),
        "compares a stimulus value, so 'value' and 'field' are required",
        fixed = TRUE
    )
})

test_that("value-op without field throws teaching error", {
    expect_error(
        buildQCEshowIfCondition("smoker", "equals", value = "Yes", field = NULL),
        "compares a stimulus value, so 'value' and 'field' are required",
        fixed = TRUE
    )
})

test_that("non-string stimRef throws", {
    expect_error(
        buildQCEshowIfCondition(c("a", "b"), "equals", "y", "z"),
        "stimRef option must be a single string"
    )
})

test_that("non-string operator throws", {
    expect_error(
        buildQCEshowIfCondition("x", c("equals", "notEquals"), "y", "z"),
        "operator option must be a single string"
    )
})

test_that("non-string field throws", {
    expect_error(
        buildQCEshowIfCondition("x", "equals", "y", c("a", "b")),
        "field option must be a single string"
    )
})

test_that("all 9 operators accepted", {
    valueOps <- c("equals", "notEquals", "greaterThan", "lessThan",
                  "greaterThanOrEqual", "lessThanOrEqual", "contains")
    shownOps <- c("wasShown", "wasNotShown")
    for (op in valueOps) {
        expect_silent(buildQCEshowIfCondition("x", op, "y", "z"))
    }
    for (op in shownOps) {
        expect_silent(buildQCEshowIfCondition("x", op))
    }
})

test_that("wasShown with extraneous value/field warns and drops them", {
    expect_warning(
        c <- buildQCEshowIfCondition("x", "wasShown", value = "Yes", field = "Response"),
        "ignores 'value' and 'field'"
    )
    expect_null(c$value)
    expect_null(c$field)
})

test_that("JSON round-trip preserves value-op condition shape", {
    c <- buildQCEshowIfCondition("smoker", "equals", "Yes", "Response")
    json <- jsonlite::toJSON(c, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    # jsonlite wraps scalars as length-1 arrays per QCEP convention
    expect_equal(parsed$stimRef[[1]], "smoker")
    expect_equal(parsed$operator[[1]], "equals")
    expect_equal(parsed$value[[1]], "Yes")
    expect_equal(parsed$field[[1]], "Response")
})

test_that("JSON round-trip on wasShown omits value/field", {
    c <- buildQCEshowIfCondition("instructions", "wasShown")
    json <- jsonlite::toJSON(c, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_equal(parsed$stimRef[[1]], "instructions")
    expect_equal(parsed$operator[[1]], "wasShown")
    expect_null(parsed$value)
    expect_null(parsed$field)
})
