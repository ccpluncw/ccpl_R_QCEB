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

# --- choices serialization (jsPsych v8 compatibility) ----------------------

test_that("default choices='ALL_KEYS' serializes as JSON scalar string", {
    fl <- addFrameToQCEframeList(
        trialType = "key", frameName = "f",
        stimulus = "x", stimulus_duration = 0, post_trial_gap = 0
    )
    json <- jsonlite::toJSON(fl, auto_unbox = FALSE)
    # Scalar emits as "ALL_KEYS"; broken old behavior would emit ["ALL_KEYS"].
    expect_match(as.character(json), '"choices":"ALL_KEYS"', fixed = TRUE)
    expect_no_match(as.character(json), '"choices":\\["ALL_KEYS"\\]')
})

test_that("explicit choices='NO_KEYS' serializes as JSON scalar string", {
    fl <- addFrameToQCEframeList(
        trialType = "key", frameName = "f",
        stimulus = "x", stimulus_duration = 1000, post_trial_gap = 0,
        choices = "NO_KEYS"
    )
    json <- jsonlite::toJSON(fl, auto_unbox = FALSE)
    expect_match(as.character(json), '"choices":"NO_KEYS"', fixed = TRUE)
})

test_that("multi-key vector serializes as JSON array (unchanged)", {
    fl <- addFrameToQCEframeList(
        trialType = "key", frameName = "f",
        stimulus = "x", stimulus_duration = 0, post_trial_gap = 0,
        choices = c("d", "D", "k", "K")
    )
    json <- jsonlite::toJSON(fl, auto_unbox = FALSE)
    expect_match(as.character(json), '"choices":\\["d","D","k","K"\\]')
})

test_that("single non-magic key serializes as 1-element array", {
    # jsPsych v8 accepts a 1-element array of literal keys; only the magic
    # ALL_KEYS / NO_KEYS strings need scalar form.
    fl <- addFrameToQCEframeList(
        trialType = "key", frameName = "f",
        stimulus = "x", stimulus_duration = 0, post_trial_gap = 0,
        choices = "d"
    )
    json <- jsonlite::toJSON(fl, auto_unbox = FALSE)
    expect_match(as.character(json), '"choices":\\["d"\\]')
})

test_that("choices=NULL serializes as empty array", {
    fl <- addFrameToQCEframeList(
        trialType = "key", frameName = "f",
        stimulus = "x", stimulus_duration = 1000, post_trial_gap = 0,
        choices = NULL
    )
    json <- jsonlite::toJSON(fl, auto_unbox = FALSE)
    expect_match(as.character(json), '"choices":\\[\\]')
})
