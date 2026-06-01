# Tests for buildQCEstateCondition -- stateRef leaf for Phase 5
# hook->state->showIf gating.

test_that("value operator produces stateRef+operator+value leaf", {
    c <- buildQCEstateCondition("blockScore", "greaterThan", 0.8)
    expect_equal(c$stateRef, "blockScore")
    expect_equal(c$operator, "greaterThan")
    expect_equal(c$value, 0.8)
    expect_null(c$field)  # stateRef leaves never carry a field
})

test_that("all seven value operators are accepted", {
    for (op in c("equals", "notEquals", "greaterThan", "lessThan",
                 "greaterThanOrEqual", "lessThanOrEqual", "contains")) {
        c <- buildQCEstateCondition("k", op, 1)
        expect_equal(c$operator, op)
        expect_equal(c$value, 1)
    }
})

test_that("presence operators produce stateRef+operator leaf (no value)", {
    c1 <- buildQCEstateCondition("reachedCriterion", "isSet")
    expect_equal(c1$stateRef, "reachedCriterion")
    expect_equal(c1$operator, "isSet")
    expect_null(c1$value)

    c2 <- buildQCEstateCondition("reachedCriterion", "isNotSet")
    expect_equal(c2$operator, "isNotSet")
    expect_null(c2$value)
})

test_that("value operator without value throws and points to isSet", {
    expect_error(buildQCEstateCondition("k", "equals"),
                 "isSet")
})

test_that("presence operator with value warns and drops the value", {
    expect_warning(c <- buildQCEstateCondition("k", "isSet", 5),
                   "ignores 'value'")
    expect_null(c$value)
})

test_that("empty stateRef throws", {
    expect_error(buildQCEstateCondition("", "isSet"),
                 "non-empty single string")
})

test_that("non-string stateRef throws", {
    expect_error(buildQCEstateCondition(c("a", "b"), "isSet"),
                 "non-empty single string")
})

test_that("invalid operator throws and lists valid ops", {
    expect_error(buildQCEstateCondition("k", "switchFired", 1),
                 "operator option must take on one of the following values")
})

test_that("validateShowIfShape accepts stateRef leaf as valid showIf", {
    c <- buildQCEstateCondition("blockScore", "isSet")
    expect_silent(validateShowIfShape(c))
})

test_that("buildQCEshowIfCompound accepts mixed stimRef + blockRef + stateRef", {
    compound <- buildQCEshowIfCompound("all", list(
        buildQCEshowIfCondition("consent", "equals", "Yes", "Response"),
        buildQCEblockSwitchedCondition("Block_T2", "switchFired"),
        buildQCEstateCondition("blockScore", "greaterThan", 0.8)
    ))
    expect_silent(validateShowIfShape(compound))
    expect_equal(length(compound$all), 3)
})

test_that("JSON round-trip preserves stateRef leaf shape", {
    c <- buildQCEstateCondition("blockScore", "greaterThan", 0.8)
    json <- jsonlite::toJSON(c, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_equal(parsed$stateRef[[1]], "blockScore")
    expect_equal(parsed$operator[[1]], "greaterThan")
    expect_equal(parsed$value[[1]], 0.8)
})
