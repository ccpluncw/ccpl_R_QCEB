# Tests for addSurveyFrameToQCEframeList + the plugins passthrough on
# addSessionToSessionList.

mkModel <- function() {
    surveyModel(surveyPage("p1",
        surveyQuestion("rating", "mvs1", "Q1", isRequired = TRUE, rateMin = 1, rateMax = 7)))
}

test_that("survey frame has the engine-facing shape", {
    fl <- addSurveyFrameToQCEframeList(NULL, mkModel(), frameName = "MVS")
    fr <- fl[[1]]
    expect_equal(fr$trialType, "survey")
    expect_equal(fr$frameName, "MVS")
    expect_true(is.character(fr$stimulus) && length(fr$stimulus) == 1)
    expect_equal(fr$background, "#FFFFFF")
    expect_true(fr$cursorVisible)
    expect_true(fr$output)
})

test_that("stimulus is a JSON STRING that parses to the model", {
    fl <- addSurveyFrameToQCEframeList(NULL, mkModel(), frameName = "MVS")
    model <- jsonlite::fromJSON(fl[[1]]$stimulus, simplifyVector = FALSE)
    expect_equal(model$pages[[1]]$elements[[1]]$name, "mvs1")
    expect_equal(model$pages[[1]]$elements[[1]]$rateMax, 7)
})

test_that("empty stimulus_duration and choices serialize as JSON arrays", {
    fl <- addSurveyFrameToQCEframeList(NULL, mkModel(), frameName = "MVS")
    json <- jsonlite::toJSON(fl, auto_unbox = FALSE)
    expect_match(as.character(json), '"stimulus_duration":[]', fixed = TRUE)
    expect_match(as.character(json), '"choices":[]', fixed = TRUE)
})

test_that("frameName defaults by position when NULL", {
    fl <- addSurveyFrameToQCEframeList(NULL, mkModel())
    expect_equal(fl[[1]]$frameName, "frame1")
})

test_that("survey frame appends to an existing frame list", {
    fl <- addFrameToQCEframeList(trialType = "key", frameName = "fix",
            stimulus = "+", stimulus_duration = 200, post_trial_gap = 0)
    fl <- addSurveyFrameToQCEframeList(fl, mkModel(), frameName = "MVS")
    expect_equal(length(fl), 2L)
    expect_equal(fl[[2]]$trialType, "survey")
})

test_that("non-model input is rejected", {
    expect_error(addSurveyFrameToQCEframeList(NULL, list(foo = 1)),
                 "must be a SurveyJS model")
})

test_that("scenario round-trip: survey frame works with addScenarioToQCEscenarioList", {
    fl <- addSurveyFrameToQCEframeList(NULL, mkModel(), frameName = "MVS")
    sc <- addScenarioToQCEscenarioList(NULL, fl, NULL, NULL, "mvs_pre")
    expect_equal(sc[[1]]$set, "mvs_pre")
    expect_equal(sc[[1]]$frame[[1]]$trialType, "survey")
    # NULL feedback/outputVariables are dropped (engine reads them defensively).
    expect_null(sc[[1]]$outputVariables)
})

# --- plugins passthrough on addSessionToSessionList -------------------------

test_that("addSessionToSessionList emits plugins only when provided", {
    s0 <- addSessionToSessionList(NULL, sessionName = "noplug")
    expect_null(s0[[1]]$plugins)

    s1 <- addSessionToSessionList(NULL, sessionName = "withplug", plugins = c("survey"))
    expect_equal(s1[[1]]$plugins, "survey")
})

test_that("plugins serializes as a JSON array even for a single plugin", {
    s1 <- addSessionToSessionList(NULL, sessionName = "x", plugins = c("survey"))
    json <- jsonlite::toJSON(s1, auto_unbox = FALSE)
    expect_match(as.character(json), '"plugins":["survey"]', fixed = TRUE)
})

test_that("invalid plugins value is rejected", {
    expect_error(addSessionToSessionList(NULL, plugins = c("survey", "")),
                 "non-empty plugin names")
    expect_error(addSessionToSessionList(NULL, plugins = 5),
                 "character vector")
})
