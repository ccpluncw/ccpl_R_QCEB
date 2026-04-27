# Tests for addSetToQCEsetInfoList -- new single-set canonical form
# and the deprecation wrapper addSetToQCEsetInfoListOldDep.

# Helper: build a minimal scenario list with two setNames declared
.makeScenariosTwoSets <- function() {
    fr <- addFrameToQCEframeList(
        trialType = "key", frameName = "fix", stimulus = "+",
        stimulus_duration = 500, post_trial_gap = 0
    )
    sl <- addScenarioToQCEscenarioList(NULL, fr, NULL, list(out = "x"), "setA")
    sl <- addScenarioToQCEscenarioList(sl,  fr, NULL, list(out = "y"), "setB")
    return(sl)
}

# --- New canonical form: addSetToQCEsetInfoList ---

test_that("regression: single-set without trigger/showIf produces N + selection only", {
    sil <- addSetToQCEsetInfoList(NULL, .makeScenariosTwoSets(), "setA", 10)
    expect_equal(names(sil), "setA")
    expect_equal(sil$setA$N, 10)
    expect_equal(sil$setA$selection, "randomWithoutReplacement")
    expect_null(sil$setA$trigger)
    expect_null(sil$setA$showIf)
})

test_that("single-set with trigger emits trigger", {
    sil <- addSetToQCEsetInfoList(
        NULL, .makeScenariosTwoSets(), "setA", 10,
        trigger = buildQCETriggerList(onset = 100, offset = 101)
    )
    expect_equal(sil$setA$trigger$onset, 100L)
    expect_equal(sil$setA$trigger$offset, 101L)
})

test_that("single-set with showIf emits showIf", {
    sil <- addSetToQCEsetInfoList(
        NULL, .makeScenariosTwoSets(), "setA", 10,
        showIf = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response")
    )
    expect_equal(sil$setA$showIf$stimRef, "smoker")
    expect_equal(sil$setA$showIf$operator, "equals")
})

test_that("single-set with both trigger and showIf emits both", {
    sil <- addSetToQCEsetInfoList(
        NULL, .makeScenariosTwoSets(), "setA", 10,
        trigger = buildQCETriggerList(onset = 100, offset = 101),
        showIf  = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response")
    )
    expect_equal(sil$setA$trigger$onset, 100L)
    expect_equal(sil$setA$showIf$stimRef, "smoker")
})

test_that("multiple sets via repeated calls", {
    sil <- addSetToQCEsetInfoList(NULL, .makeScenariosTwoSets(), "setA", 10)
    sil <- addSetToQCEsetInfoList(sil,  .makeScenariosTwoSets(), "setB", 20)
    expect_equal(names(sil), c("setA", "setB"))
    expect_equal(sil$setA$N, 10)
    expect_equal(sil$setB$N, 20)
})

test_that("vector setName throws with migration hint", {
    expect_error(
        addSetToQCEsetInfoList(NULL, .makeScenariosTwoSets(), c("setA", "setB"), 10),
        "addSetToQCEsetInfoListOldDep",
        fixed = TRUE
    )
})

test_that("NULL setName throws with migration hint", {
    expect_error(
        addSetToQCEsetInfoList(NULL, .makeScenariosTwoSets(), NULL, 10),
        "single string"
    )
})

test_that("invalid selectionType throws", {
    expect_error(
        addSetToQCEsetInfoList(NULL, .makeScenariosTwoSets(), "setA", 10, selectionType = "bogus"),
        "selectionType option must take on one of the following values"
    )
})

test_that("setName not declared on any scenario throws", {
    expect_error(
        addSetToQCEsetInfoList(NULL, .makeScenariosTwoSets(), "ghostSet", 10),
        "is not declared on any scenario in QCEScenarioList",
        fixed = TRUE
    )
})

test_that("malformed showIf throws", {
    expect_error(
        addSetToQCEsetInfoList(
            NULL, .makeScenariosTwoSets(), "setA", 10,
            showIf = list(stimRef = "smoker")  # missing operator
        ),
        "is not a valid showIf condition",
        fixed = TRUE
    )
})

test_that("non-numeric numberOfTrialsPerSet throws", {
    expect_error(
        addSetToQCEsetInfoList(NULL, .makeScenariosTwoSets(), "setA", "ten"),
        "numberOfTrialsPerSet option must be a single integer"
    )
})

# --- Deprecation wrapper: addSetToQCEsetInfoListOldDep ---

test_that("OldDep emits .Deprecated() warning", {
    expect_warning(
        addSetToQCEsetInfoListOldDep(NULL, .makeScenariosTwoSets(), c("setA", "setB"), 10),
        "deprecated",
        ignore.case = TRUE
    )
})

test_that("OldDep with vector setName produces multi-key list", {
    suppressWarnings({
        sil <- addSetToQCEsetInfoListOldDep(NULL, .makeScenariosTwoSets(), c("setA", "setB"), 10)
    })
    expect_equal(names(sil), c("setA", "setB"))
    expect_equal(sil$setA$N, 10)
    expect_equal(sil$setB$N, 10)
})

test_that("OldDep with NULL setName uses validSetNames", {
    suppressWarnings({
        sil <- addSetToQCEsetInfoListOldDep(NULL, .makeScenariosTwoSets(), NULL, 5)
    })
    expect_equal(sort(names(sil)), c("setA", "setB"))
    expect_equal(sil$setA$N, 5)
    expect_equal(sil$setB$N, 5)
})

test_that("OldDep with named-list trigger keyed by setName attaches per-set", {
    suppressWarnings({
        sil <- addSetToQCEsetInfoListOldDep(
            NULL, .makeScenariosTwoSets(),
            setName              = c("setA", "setB"),
            numberOfTrialsPerSet = c(10, 20),
            trigger = list(
                setA = buildQCETriggerList(onset = 100, offset = 101)
                # setB intentionally omitted -- should get no trigger
            )
        )
    })
    expect_equal(sil$setA$trigger$onset, 100L)
    expect_null(sil$setB$trigger)
})

test_that("OldDep mismatched lengths throws", {
    expect_error(
        suppressWarnings(
            addSetToQCEsetInfoListOldDep(
                NULL, .makeScenariosTwoSets(),
                setName              = c("setA", "setB"),
                numberOfTrialsPerSet = c(10, 20, 30)
            )
        ),
        "must be equal"
    )
})

# --- JSON round-trip ---

test_that("JSON round-trip preserves single-set with showIf", {
    sil <- addSetToQCEsetInfoList(
        NULL, .makeScenariosTwoSets(), "setA", 10,
        showIf = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response")
    )
    json <- jsonlite::toJSON(sil, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    # jsonlite array-wraps scalars per QCEP convention
    expect_equal(parsed$setA$N[[1]], 10)
    expect_equal(parsed$setA$showIf$stimRef[[1]], "smoker")
    expect_equal(parsed$setA$showIf$operator[[1]], "equals")
})
