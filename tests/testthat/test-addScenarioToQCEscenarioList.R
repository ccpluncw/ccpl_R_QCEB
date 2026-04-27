# Tests for addScenarioToQCEscenarioList -- stimRef + showIf additions,
# duplicate-stimRef warning, validation, and JSON round-trip.

# Helper: a minimal frame list (just enough to satisfy the function)
.makeFrames <- function() {
    addFrameToQCEframeList(
        trialType = "key", frameName = "fix", stimulus = "+",
        stimulus_duration = 500, post_trial_gap = 0
    )
}

test_that("regression: scenario with no stimRef/showIf is byte-identical to legacy output", {
    sl <- addScenarioToQCEscenarioList(
        NULL, .makeFrames(), NULL, list(out = "x"), "setA"
    )
    entry <- sl[[1]]
    expect_null(entry$stimRef)
    expect_null(entry$showIf)
    # The four legacy keys still present in the right order
    expect_equal(names(entry)[1:4], c("frame", "feedback", "outputVariables", "set"))
})

test_that("scenario with stimRef emits stimRef key", {
    sl <- addScenarioToQCEscenarioList(
        NULL, .makeFrames(), NULL, list(out = "x"), "setA",
        stimRef = "smoker"
    )
    expect_equal(sl[[1]]$stimRef, "smoker")
})

test_that("scenario with showIf emits showIf key", {
    sl <- addScenarioToQCEscenarioList(
        NULL, .makeFrames(), NULL, list(out = "x"), "setA",
        showIf = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response")
    )
    expect_equal(sl[[1]]$showIf$stimRef, "smoker")
    expect_equal(sl[[1]]$showIf$operator, "equals")
})

test_that("duplicate stimRef in same list emits warning", {
    sl <- addScenarioToQCEscenarioList(
        NULL, .makeFrames(), NULL, list(out = "x"), "setA",
        stimRef = "smoker"
    )
    expect_warning(
        sl <- addScenarioToQCEscenarioList(
            sl, .makeFrames(), NULL, list(out = "y"), "setA",
            stimRef = "smoker"
        ),
        "already used by another scenario"
    )
    expect_equal(length(sl), 2L)
})

test_that("distinct stimRefs in same list emit no warning", {
    sl <- addScenarioToQCEscenarioList(
        NULL, .makeFrames(), NULL, list(out = "x"), "setA",
        stimRef = "smoker"
    )
    expect_silent(
        addScenarioToQCEscenarioList(
            sl, .makeFrames(), NULL, list(out = "y"), "setA",
            stimRef = "drinker"
        )
    )
})

test_that("non-string stimRef throws", {
    expect_error(
        addScenarioToQCEscenarioList(
            NULL, .makeFrames(), NULL, list(out = "x"), "setA",
            stimRef = c("a", "b")
        ),
        "stimRef option must be a single string"
    )
})

test_that("malformed showIf (hand-rolled missing operator) throws", {
    expect_error(
        addScenarioToQCEscenarioList(
            NULL, .makeFrames(), NULL, list(out = "x"), "setA",
            showIf = list(stimRef = "smoker")  # no operator, not a compound
        ),
        "is not a valid showIf condition",
        fixed = TRUE
    )
})

test_that("non-list showIf throws", {
    expect_error(
        addScenarioToQCEscenarioList(
            NULL, .makeFrames(), NULL, list(out = "x"), "setA",
            showIf = "smoker == Yes"
        ),
        "must be a list"
    )
})

test_that("scenario with both stimRef and showIf works", {
    sl <- addScenarioToQCEscenarioList(
        NULL, .makeFrames(), NULL, list(out = "x"), "setA",
        stimRef = "myTag",
        showIf  = buildQCEshowIfCompound("any", list(
            buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"),
            buildQCEshowIfCondition("over18", "equals", "Yes", "Response")
        ))
    )
    expect_equal(sl[[1]]$stimRef, "myTag")
    expect_false(is.null(sl[[1]]$showIf$any))
})

test_that("JSON round-trip preserves stimRef + showIf on a scenario", {
    sl <- addScenarioToQCEscenarioList(
        NULL, .makeFrames(), NULL, list(out = "x"), "setA",
        stimRef = "smoker",
        showIf  = buildQCEshowIfCondition("over18", "equals", "Yes", "Response")
    )
    json <- jsonlite::toJSON(sl, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    entry <- parsed[[1]]
    # jsonlite array-wraps scalars per QCEP convention
    expect_equal(entry$stimRef[[1]], "smoker")
    expect_equal(entry$showIf$stimRef[[1]], "over18")
})
