# Tests for addBlockSwitchRulesToQCETrialStructureList (Phase 4 Step 2) --
# top-level switchRules attachment, shape validation, block-key coexistence,
# and ordering / empty-list guards.

# Helper: build a minimal 2-block trial structure (T2_yesno, T3_spacebar)
.makeTwoBlockTS <- function() {
    fr <- addFrameToQCEframeList(
        trialType = "key", frameName = "fix", stimulus = "+",
        stimulus_duration = 500, post_trial_gap = 0
    )
    scen     <- addScenarioToQCEscenarioList(NULL, fr, NULL, list(out = "x"), "setA")
    setInfo  <- addSetToQCEsetInfoList(NULL, scen, "setA", 5)
    blockIt  <- createBlockIteratorList(1)
    ts <- addBlockToQCETrialStructureList(NULL, setInfo, blockIt, blockName = "T2_yesno")
    ts <- addBlockToQCETrialStructureList(ts,  setInfo, blockIt, blockName = "T3_spacebar")
    ts
}

.makeRule <- function(watch = "T2_yesno", to = "T3_spacebar", n = 5) {
    buildQCEblockSwitchRule(
        watchBlock    = watch,
        countResponse = "Yes",
        threshold     = buildQCEswitchThreshold(values = n, rule = "fixed"),
        switchToBlock = to
    )
}

# ---- happy paths ---------------------------------------------------------

test_that("attaches a top-level switchRules key alongside numbered blocks", {
    ts <- .makeTwoBlockTS()
    ts <- addBlockSwitchRulesToQCETrialStructureList(ts, list(.makeRule()))

    expect_true("switchRules" %in% names(ts))
    # the two block entries are still present and intact
    expect_true(all(c("1", "2") %in% names(ts)))
    expect_equal(ts[["1"]]$blockName, "T2_yesno")
    expect_equal(ts[["2"]]$blockName, "T3_spacebar")
    # the rule round-trips
    expect_equal(ts$switchRules[[1]]$watchBlock, "T2_yesno")
    expect_equal(ts$switchRules[[1]]$switchToBlock, "T3_spacebar")
    expect_equal(ts$switchRules[[1]]$countResponse, "Yes")
})

test_that("end-session rule (no switchToBlock) attaches", {
    ts <- .makeTwoBlockTS()
    endRule <- buildQCEblockSwitchRule(
        watchBlock = "T2_yesno", countResponse = "Yes",
        threshold  = buildQCEswitchThreshold(values = 3, rule = "fixed")
    )
    ts <- addBlockSwitchRulesToQCETrialStructureList(ts, list(endRule))
    expect_null(ts$switchRules[[1]]$switchToBlock)
})

test_that("branching: two rules on the same watchBlock both attach", {
    ts <- .makeTwoBlockTS()
    rA <- .makeRule(to = "T3_spacebar")
    rB <- buildQCEblockSwitchRule("T2_yesno", countResponse = "No",
                                  switchToBlock = "T3_spacebar",
                                  threshold = buildQCEswitchThreshold(4, "fixed"))
    ts <- addBlockSwitchRulesToQCETrialStructureList(ts, list(rA, rB))
    expect_length(ts$switchRules, 2)
    expect_equal(ts$switchRules[[2]]$countResponse, "No")
})

test_that("block keys are unaffected; switchRules is the only non-numeric key", {
    ts <- .makeTwoBlockTS()
    ts <- addBlockSwitchRulesToQCETrialStructureList(ts, list(.makeRule()))
    nonBlock <- setdiff(names(ts), c("1", "2"))
    expect_equal(nonBlock, "switchRules")
})

# ---- guards --------------------------------------------------------------

test_that("NULL / missing trial structure throws", {
    expect_error(
        addBlockSwitchRulesToQCETrialStructureList(NULL, list(.makeRule())),
        "QCETrialStructureList option is required"
    )
})

test_that("trial structure with no blocks throws", {
    # A bare list carrying only a switchRules key (no real blocks)
    fake <- list(switchRules = list(.makeRule()))
    expect_error(
        addBlockSwitchRulesToQCETrialStructureList(fake, list(.makeRule())),
        "no blocks to watch"
    )
})

test_that("empty switchRules list throws", {
    ts <- .makeTwoBlockTS()
    expect_error(
        addBlockSwitchRulesToQCETrialStructureList(ts, list()),
        "at least one rule"
    )
})

test_that("rule missing watchBlock throws (shape validation)", {
    ts <- .makeTwoBlockTS()
    # hand-rolled rule WITHOUT watchBlock (bypassing the builder)
    badRule <- list(
        countResponse = "Yes",
        threshold     = buildQCEswitchThreshold(5, "fixed"),
        switchToBlock = "T3_spacebar"
    )
    expect_error(
        addBlockSwitchRulesToQCETrialStructureList(ts, list(badRule)),
        "missing watchBlock"
    )
})

test_that("rule with both countResponse and countWhen throws (shape validation)", {
    ts <- .makeTwoBlockTS()
    badRule <- list(
        watchBlock    = "T2_yesno",
        countResponse = "Yes",
        countWhen     = list(field = "Key", operator = "equals", value = "Yes"),
        threshold     = buildQCEswitchThreshold(5, "fixed")
    )
    expect_error(
        addBlockSwitchRulesToQCETrialStructureList(ts, list(badRule)),
        "pick one"
    )
})

# ---- ordering caveat (documented requirement) ----------------------------

test_that("adding a block AFTER attaching switchRules mis-numbers (documents the caveat)", {
    # This is the failure mode the docs warn about: addBlockToQCETrialStructureList
    # derives the next key from length(), which counts the switchRules element.
    # We assert the observable consequence so the contract is pinned by a test.
    inp_fr <- addFrameToQCEframeList(trialType = "key", frameName = "fix",
                                     stimulus = "+", stimulus_duration = 500, post_trial_gap = 0)
    scen   <- addScenarioToQCEscenarioList(NULL, inp_fr, NULL, list(out = "x"), "setA")
    setInfo<- addSetToQCEsetInfoList(NULL, scen, "setA", 5)
    blockIt<- createBlockIteratorList(1)

    ts <- addBlockToQCETrialStructureList(NULL, setInfo, blockIt, blockName = "B1")
    ts <- addBlockSwitchRulesToQCETrialStructureList(ts, list(
        buildQCEblockSwitchRule("B1", countResponse = "Yes",
                                threshold = buildQCEswitchThreshold(5, "fixed"))))
    # length is now 2 (block "1" + switchRules); a further block lands at key "3"
    ts <- addBlockToQCETrialStructureList(ts, setInfo, blockIt, blockName = "B2")
    expect_true("3" %in% names(ts))   # gap at "2" -- exactly why docs say attach LAST
})
