# Tests for buildQCEblockSwitchRule (Phase 4 Step 2) -- required watchBlock,
# XOR between countResponse / countWhen, threshold shape delegation, optional
# switchToBlock, branching shape, JSON-shape expectations.

makeFixedThreshold <- function(n = 5) {
    buildQCEswitchThreshold(values = n, rule = "fixed")
}

# ---- happy paths ---------------------------------------------------------

test_that("countResponse sugar produces watchBlock + threshold + countResponse + switchToBlock", {
    r <- buildQCEblockSwitchRule(
        watchBlock    = "T2_yesno",
        countResponse = "Yes",
        threshold     = makeFixedThreshold(5),
        switchToBlock = "T3_spacebar"
    )
    expect_equal(r$watchBlock, "T2_yesno")
    expect_equal(r$countResponse, "Yes")
    expect_equal(r$threshold$values, 5)
    expect_equal(r$threshold$rule, "fixed")
    expect_equal(r$switchToBlock, "T3_spacebar")
    expect_null(r$countWhen)
    # block rules must NOT carry the set-level fields
    expect_null(r$switchToSet)
    expect_null(r$switchInstruction)
})

test_that("countWhen full form produces watchBlock + threshold + countWhen", {
    r <- buildQCEblockSwitchRule(
        watchBlock = "Practice",
        countWhen  = list(field = "RT", operator = "lessThan", value = 500),
        threshold  = makeFixedThreshold(10)
    )
    expect_equal(r$watchBlock, "Practice")
    expect_equal(r$countWhen$field, "RT")
    expect_equal(r$countWhen$operator, "lessThan")
    expect_equal(r$countWhen$value, 500)
    expect_null(r$countResponse)
    expect_null(r$switchToBlock)
})

test_that("switchToBlock=NULL → end-session-early (no switchToBlock key)", {
    r <- buildQCEblockSwitchRule(
        watchBlock    = "Practice",
        countResponse = "Yes",
        threshold     = makeFixedThreshold(5)
    )
    expect_null(r$switchToBlock)
    expect_false("switchToBlock" %in% names(r))
})

test_that("randomIntBetween threshold survives", {
    th <- buildQCEswitchThreshold(values = c(3, 7), rule = "randomIntBetween")
    r  <- buildQCEblockSwitchRule(
        watchBlock = "T1", countResponse = "Yes", threshold = th, switchToBlock = "T2"
    )
    expect_equal(r$threshold$values, c(3, 7))
    expect_equal(r$threshold$rule, "randomIntBetween")
})

test_that("argument order allows threshold-first positional call", {
    # threshold is the first formal (mirrors buildQCEswitchRule); make sure a
    # named-arg call in any order works.
    r <- buildQCEblockSwitchRule(
        threshold     = makeFixedThreshold(4),
        watchBlock    = "B1",
        countResponse = "Left",
        switchToBlock = "B2"
    )
    expect_equal(r$watchBlock, "B1")
    expect_equal(r$threshold$values, 4)
})

# ---- watchBlock validation ----------------------------------------------

test_that("missing watchBlock throws", {
    expect_error(
        buildQCEblockSwitchRule(countResponse = "Yes", threshold = makeFixedThreshold()),
        "watchBlock option is required",
        fixed = TRUE
    )
})

test_that("empty / non-string watchBlock throws", {
    expect_error(
        buildQCEblockSwitchRule(watchBlock = "", countResponse = "Yes",
                                threshold = makeFixedThreshold()),
        "non-empty single string"
    )
    expect_error(
        buildQCEblockSwitchRule(watchBlock = c("a", "b"), countResponse = "Yes",
                                threshold = makeFixedThreshold()),
        "non-empty single string"
    )
})

# ---- count XOR validation ------------------------------------------------

test_that("missing both countResponse AND countWhen throws", {
    expect_error(
        buildQCEblockSwitchRule(watchBlock = "T1", threshold = makeFixedThreshold()),
        "either countResponse"
    )
})

test_that("both countResponse AND countWhen throws", {
    expect_error(
        buildQCEblockSwitchRule(
            watchBlock    = "T1",
            countResponse = "Yes",
            countWhen     = list(field = "Key", operator = "equals", value = "Yes"),
            threshold     = makeFixedThreshold()
        ),
        "not both"
    )
})

test_that("empty countResponse throws", {
    expect_error(
        buildQCEblockSwitchRule(watchBlock = "T1", countResponse = "",
                                threshold = makeFixedThreshold()),
        "non-empty single string"
    )
})

test_that("malformed countWhen throws via shared validator", {
    expect_error(
        buildQCEblockSwitchRule(
            watchBlock = "T1",
            countWhen  = list(field = "RT", operator = "lessThan"),  # missing value
            threshold  = makeFixedThreshold()
        ),
        "value is required"
    )
    expect_error(
        buildQCEblockSwitchRule(
            watchBlock = "T1",
            countWhen  = list(field = "RT", operator = "bogusOp", value = 1),
            threshold  = makeFixedThreshold()
        ),
        "operator must be one of"
    )
})

# ---- threshold validation ------------------------------------------------

test_that("missing threshold throws", {
    expect_error(
        buildQCEblockSwitchRule(watchBlock = "T1", countResponse = "Yes"),
        "threshold option is required",
        fixed = TRUE
    )
})

test_that("malformed threshold throws via shared validator", {
    expect_error(
        buildQCEblockSwitchRule(watchBlock = "T1", countResponse = "Yes",
                                threshold = list(rule = "fixed")),  # missing values
        "values is required"
    )
})

# ---- switchToBlock validation -------------------------------------------

test_that("empty switchToBlock throws", {
    expect_error(
        buildQCEblockSwitchRule(watchBlock = "T1", countResponse = "Yes",
                                threshold = makeFixedThreshold(), switchToBlock = ""),
        "non-empty single string"
    )
})

# ---- branching shape (two rules, same watchBlock) ------------------------

test_that("two rules may share a watchBlock (branching) -- builder does not block it", {
    rA <- buildQCEblockSwitchRule("T1", countResponse = "Left",  switchToBlock = "LeftPath",
                                  threshold = makeFixedThreshold(5))
    rB <- buildQCEblockSwitchRule("T1", countResponse = "Right", switchToBlock = "RightPath",
                                  threshold = makeFixedThreshold(5))
    expect_equal(rA$watchBlock, "T1")
    expect_equal(rB$watchBlock, "T1")
    expect_equal(rA$switchToBlock, "LeftPath")
    expect_equal(rB$switchToBlock, "RightPath")
})
