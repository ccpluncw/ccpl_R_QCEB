# Tests for addFrameToQCEframeList — pluginParams mechanism, kind validation,
# and JSON round-trip of the engine-facing structure.

test_that("textbox frame with kind='number' puts kind in pluginParams", {
    fl <- addFrameToQCEframeList(
        trialType = "textbox",
        frameName = "input",
        stimulus = "<input id='Text_In' />",
        stimulus_duration = 1000,
        post_trial_gap = 0,
        kind = "number"
    )
    frame <- fl[[1]]
    expect_null(frame$kind)
    expect_false(is.null(frame$pluginParams))
    expect_equal(frame$pluginParams$kind, "number")
})

test_that("key frame (non-textbox) does not carry pluginParams", {
    fl <- addFrameToQCEframeList(
        trialType = "key",
        frameName = "fixation",
        stimulus = "+",
        stimulus_duration = 500,
        post_trial_gap = 0
    )
    frame <- fl[[1]]
    expect_null(frame$kind)
    expect_null(frame$pluginParams)
})

test_that("textbox frame with default kind='string' emits it in pluginParams", {
    fl <- addFrameToQCEframeList(
        trialType = "textbox",
        frameName = "input",
        stimulus = "<input id='Text_In' />",
        stimulus_duration = 1000,
        post_trial_gap = 0
    )
    frame <- fl[[1]]
    expect_equal(frame$pluginParams$kind, "string")
})

test_that("invalid kind throws at R-time with descriptive error", {
    expect_error(
        addFrameToQCEframeList(
            trialType = "textbox",
            stimulus = "<input id='Text_In' />",
            stimulus_duration = 1000,
            post_trial_gap = 0,
            kind = "numbr"
        ),
        "kind option must take on one of the following values"
    )
})

test_that("duplicate kind (named arg AND in pluginParams) throws", {
    expect_error(
        addFrameToQCEframeList(
            trialType = "textbox",
            stimulus = "<input id='Text_In' />",
            stimulus_duration = 1000,
            post_trial_gap = 0,
            kind = "number",
            pluginParams = list(kind = "other")
        ),
        "Do not pass 'kind' both as a named argument AND inside pluginParams",
        fixed = TRUE
    )
})

test_that("pluginParams without kind merges in default kind for textbox", {
    fl <- addFrameToQCEframeList(
        trialType = "textbox",
        stimulus = "<input id='Text_In' />",
        stimulus_duration = 1000,
        post_trial_gap = 0,
        pluginParams = list(customField = "value")
    )
    frame <- fl[[1]]
    expect_equal(frame$pluginParams$customField, "value")
    expect_equal(frame$pluginParams$kind, "string")
})

test_that("invalid trialType still rejected (existing validation unchanged)", {
    expect_error(
        addFrameToQCEframeList(
            trialType = "bogus",
            stimulus = "x",
            stimulus_duration = 100,
            post_trial_gap = 0
        ),
        "trialType option must take on one of the following values"
    )
})

test_that("chained frames — second frame appends correctly", {
    fl <- addFrameToQCEframeList(
        trialType = "key", frameName = "fixation", stimulus = "+",
        stimulus_duration = 500, post_trial_gap = 0
    )
    fl <- addFrameToQCEframeList(fl,
        trialType = "textbox", frameName = "input",
        stimulus = "<input id='Text_In' />",
        stimulus_duration = 1000, post_trial_gap = 0,
        kind = "number"
    )
    expect_equal(length(fl), 2L)
    expect_null(fl[[1]]$pluginParams)
    expect_equal(fl[[2]]$pluginParams$kind, "number")
})

test_that("JSON round-trip: pluginParams.kind survives serialization", {
    fl <- addFrameToQCEframeList(
        trialType = "textbox", frameName = "input",
        stimulus = "<input id='Text_In' />",
        stimulus_duration = 1000, post_trial_gap = 0,
        kind = "number"
    )
    json <- jsonlite::toJSON(fl, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    frame <- parsed[[1]]
    expect_null(frame$kind)
    # jsonlite wraps scalars as length-1 arrays (standard QCEP convention);
    # the engine's unwrap() strips one level of wrapping.
    expect_equal(frame$pluginParams$kind[[1]], "number")
})
