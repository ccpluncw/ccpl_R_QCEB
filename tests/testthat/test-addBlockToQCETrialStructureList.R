# Tests for addBlockToQCETrialStructureList -- showIf addition,
# validation, regression, and JSON round-trip.

# Helper: build a minimal scenario + setInfo + blockIterator chain
.makeBlockInputs <- function() {
    fr <- addFrameToQCEframeList(
        trialType = "key", frameName = "fix", stimulus = "+",
        stimulus_duration = 500, post_trial_gap = 0
    )
    scenarios <- addScenarioToQCEscenarioList(NULL, fr, NULL, list(out = "x"), "setA")
    setInfo   <- addSetToQCEsetInfoList(NULL, scenarios, "setA", 5)
    blockIter <- createBlockIteratorList(1)
    list(setInfo = setInfo, blockIter = blockIter)
}

test_that("regression: block without showIf is byte-identical to legacy output", {
    inp <- .makeBlockInputs()
    tsl <- addBlockToQCETrialStructureList(NULL, inp$setInfo, inp$blockIter)
    entry <- tsl[[1]]
    expect_null(entry$showIf)
    # The four legacy keys still present in the right order
    expect_equal(names(entry)[1:4], c("blockNumber", "setInfo", "blockIterator", "blockName"))
})

test_that("block with showIf emits showIf key", {
    inp <- .makeBlockInputs()
    tsl <- addBlockToQCETrialStructureList(
        NULL, inp$setInfo, inp$blockIter,
        blockName = "smokerBlock",
        showIf    = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response")
    )
    expect_equal(tsl[[1]]$showIf$stimRef, "smoker")
    expect_equal(tsl[[1]]$showIf$operator, "equals")
})

test_that("block with both trigger and showIf emits both", {
    inp <- .makeBlockInputs()
    tsl <- addBlockToQCETrialStructureList(
        NULL, inp$setInfo, inp$blockIter,
        trigger = buildQCETriggerList(onset = 10, offset = 11),
        showIf  = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response")
    )
    expect_equal(tsl[[1]]$trigger$onset, 10L)
    expect_equal(tsl[[1]]$showIf$stimRef, "smoker")
})

test_that("malformed showIf throws", {
    inp <- .makeBlockInputs()
    expect_error(
        addBlockToQCETrialStructureList(
            NULL, inp$setInfo, inp$blockIter,
            showIf = list(stimRef = "smoker")  # missing operator
        ),
        "is not a valid showIf condition",
        fixed = TRUE
    )
})

test_that("non-list showIf throws", {
    inp <- .makeBlockInputs()
    expect_error(
        addBlockToQCETrialStructureList(
            NULL, inp$setInfo, inp$blockIter,
            showIf = "smoker == Yes"
        ),
        "must be a list"
    )
})

test_that("compound showIf works on a block", {
    inp <- .makeBlockInputs()
    tsl <- addBlockToQCETrialStructureList(
        NULL, inp$setInfo, inp$blockIter,
        showIf = buildQCEshowIfCompound("any", list(
            buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"),
            buildQCEshowIfCondition("vapes",  "equals", "Yes", "Response")
        ))
    )
    expect_false(is.null(tsl[[1]]$showIf$any))
    expect_equal(length(tsl[[1]]$showIf$any), 2L)
})

test_that("multiple blocks via repeated calls", {
    inp <- .makeBlockInputs()
    tsl <- addBlockToQCETrialStructureList(NULL, inp$setInfo, inp$blockIter, blockName = "b1")
    tsl <- addBlockToQCETrialStructureList(tsl,  inp$setInfo, inp$blockIter, blockName = "b2",
                                           showIf = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"))
    expect_equal(length(tsl), 2L)
    expect_null(tsl[[1]]$showIf)
    expect_equal(tsl[[2]]$showIf$stimRef, "smoker")
})

test_that("JSON round-trip preserves showIf on a block", {
    inp <- .makeBlockInputs()
    tsl <- addBlockToQCETrialStructureList(
        NULL, inp$setInfo, inp$blockIter,
        showIf = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response")
    )
    json <- jsonlite::toJSON(tsl, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    entry <- parsed[[1]]
    # jsonlite array-wraps scalars per QCEP convention
    expect_equal(entry$showIf$stimRef[[1]], "smoker")
    expect_equal(entry$showIf$operator[[1]], "equals")
})

# --- Phase 3 Chunk G: switchRules ---

# Helper: a well-formed single rule (countResponse sugar form)
.makeSwitchRule <- function(toSet = "setA") {
    buildQCEswitchRule(
        countResponse = "Yes",
        threshold     = buildQCEswitchThreshold(values = 5, rule = "fixed"),
        switchToSet   = toSet
    )
}

test_that("regression: block without switchRules omits the field", {
    inp <- .makeBlockInputs()
    tsl <- addBlockToQCETrialStructureList(NULL, inp$setInfo, inp$blockIter)
    expect_null(tsl[[1]]$switchRules)
})

test_that("block with single switchRule emits switchRules array", {
    inp <- .makeBlockInputs()
    tsl <- addBlockToQCETrialStructureList(
        NULL, inp$setInfo, inp$blockIter,
        blockName   = "PrefBlock",
        switchRules = list(.makeSwitchRule())
    )
    expect_length(tsl[[1]]$switchRules, 1)
    expect_equal(tsl[[1]]$switchRules[[1]]$countResponse, "Yes")
})

test_that("block with two sequential switchRules preserves order", {
    inp <- .makeBlockInputs()
    r1 <- .makeSwitchRule("setA")
    r2 <- buildQCEswitchRule(
        countResponse = "No",
        threshold     = buildQCEswitchThreshold(values = 3, rule = "fixed")
    )  # early-stop rule (no switchToSet)
    tsl <- addBlockToQCETrialStructureList(
        NULL, inp$setInfo, inp$blockIter,
        switchRules = list(r1, r2)
    )
    expect_length(tsl[[1]]$switchRules, 2)
    expect_equal(tsl[[1]]$switchRules[[1]]$countResponse, "Yes")
    expect_equal(tsl[[1]]$switchRules[[2]]$countResponse, "No")
    expect_null(tsl[[1]]$switchRules[[2]]$switchToSet)
})

test_that("non-list switchRules throws", {
    inp <- .makeBlockInputs()
    expect_error(
        addBlockToQCETrialStructureList(
            NULL, inp$setInfo, inp$blockIter,
            switchRules = "not a list"
        ),
        "switchRules must be a list"
    )
})

test_that("empty list switchRules throws", {
    inp <- .makeBlockInputs()
    expect_error(
        addBlockToQCETrialStructureList(
            NULL, inp$setInfo, inp$blockIter,
            switchRules = list()
        ),
        "switchRules must contain at least one rule"
    )
})

test_that("hand-rolled rule missing both countResponse and countWhen throws", {
    inp <- .makeBlockInputs()
    badRule <- list(threshold = buildQCEswitchThreshold(5))  # no count condition
    expect_error(
        addBlockToQCETrialStructureList(
            NULL, inp$setInfo, inp$blockIter,
            switchRules = list(badRule)
        ),
        "missing both countResponse and countWhen"
    )
})

test_that("hand-rolled rule missing threshold throws", {
    inp <- .makeBlockInputs()
    badRule <- list(countResponse = "Yes")  # no threshold
    expect_error(
        addBlockToQCETrialStructureList(
            NULL, inp$setInfo, inp$blockIter,
            switchRules = list(badRule)
        ),
        "missing threshold"
    )
})

test_that("hand-rolled rule with both countResponse and countWhen throws", {
    inp <- .makeBlockInputs()
    badRule <- list(
        countResponse = "Yes",
        countWhen     = list(field = "Key", operator = "equals", value = "Yes"),
        threshold     = buildQCEswitchThreshold(5)
    )
    expect_error(
        addBlockToQCETrialStructureList(
            NULL, inp$setInfo, inp$blockIter,
            switchRules = list(badRule)
        ),
        "has both countResponse and countWhen"
    )
})

test_that("JSON round-trip preserves switchRules shape", {
    inp <- .makeBlockInputs()
    tsl <- addBlockToQCETrialStructureList(
        NULL, inp$setInfo, inp$blockIter,
        switchRules = list(.makeSwitchRule("setA"))
    )
    json <- jsonlite::toJSON(tsl, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    rules <- parsed[[1]]$switchRules
    expect_length(rules, 1)
    expect_equal(rules[[1]]$countResponse[[1]], "Yes")
    expect_equal(rules[[1]]$switchToSet[[1]], "setA")
    expect_equal(unlist(rules[[1]]$threshold$values), 5)
    expect_equal(rules[[1]]$threshold$rule[[1]], "fixed")
})
