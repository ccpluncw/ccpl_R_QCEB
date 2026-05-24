# Tests for buildQCEblockSwitchedCondition -- blockRef leaf for Phase 3.5
# Decision G showIf gating.

test_that("switchFired operator produces blockRef+operator leaf", {
    c <- buildQCEblockSwitchedCondition("Block_T2", "switchFired")
    expect_equal(c$blockRef, "Block_T2")
    expect_equal(c$operator, "switchFired")
})

test_that("switchNotFired operator produces blockRef+operator leaf", {
    c <- buildQCEblockSwitchedCondition("Practice", "switchNotFired")
    expect_equal(c$blockRef, "Practice")
    expect_equal(c$operator, "switchNotFired")
})

test_that("empty blockRef throws", {
    expect_error(buildQCEblockSwitchedCondition("", "switchFired"),
                 "non-empty single string")
})

test_that("non-string blockRef throws", {
    expect_error(buildQCEblockSwitchedCondition(c("a", "b"), "switchFired"),
                 "non-empty single string")
})

test_that("invalid operator throws", {
    expect_error(buildQCEblockSwitchedCondition("B1", "fired"),
                 "operator option must take on one of the following values")
})

test_that("invalid operator names common stimRef operator", {
    # researcher might confuse blockRef vs stimRef operators -- error
    # should list valid blockRef operators so they spot the mismatch
    expect_error(buildQCEblockSwitchedCondition("B1", "equals"),
                 "switchFired switchNotFired")
})

test_that("validateShowIfShape accepts blockRef leaf as valid showIf", {
    c <- buildQCEblockSwitchedCondition("Block_T2", "switchFired")
    expect_silent(validateShowIfShape(c))
})

test_that("buildQCEshowIfCompound accepts mixed stimRef + blockRef leaves", {
    compound <- buildQCEshowIfCompound("all", list(
        buildQCEshowIfCondition("consent", "equals", "Yes", "Response"),
        buildQCEblockSwitchedCondition("Block_T2", "switchFired")
    ))
    expect_silent(validateShowIfShape(compound))
    expect_equal(length(compound$all), 2)
})

test_that("JSON round-trip preserves blockRef leaf shape", {
    c <- buildQCEblockSwitchedCondition("Block_T2", "switchFired")
    json <- jsonlite::toJSON(c, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_equal(parsed$blockRef[[1]], "Block_T2")
    expect_equal(parsed$operator[[1]], "switchFired")
})
