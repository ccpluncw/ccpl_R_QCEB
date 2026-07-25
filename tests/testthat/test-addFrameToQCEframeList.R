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

test_that("unregistered trialType rejected via registry (new validation)", {
    expect_error(
        addFrameToQCEframeList(
            trialType = "bogus",
            stimulus = "x",
            stimulus_duration = 100,
            post_trial_gap = 0
        ),
        "is not registered"
    )
})

test_that("registered 'survey' trialType is accepted by addFrameToQCEframeList", {
    fl <- addFrameToQCEframeList(
        trialType = "survey", frameName = "s",
        stimulus = "{\"pages\":[]}", stimulus_duration = NULL, post_trial_gap = 0,
        choices = NULL
    )
    expect_equal(fl[[1]]$trialType, "survey")
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

# --- trial_duration: three-state resolution + the no-exit guard --------------

test_that("a frame that never mentions trial_duration omits the key entirely", {
    fl <- addFrameToQCEframeList(
        trialType = "key", frameName = "f", stimulus = "x",
        stimulus_duration = 1000, post_trial_gap = 0
    )
    expect_false("trial_duration" %in% names(fl[[1]]))
})

test_that("omitting trial_duration leaves the emitted frame byte-identical", {
    args <- list(trialType = "key", frameName = "f", stimulus = "x",
                 stimulus_duration = 1000, post_trial_gap = 0,
                 choices = c("a", "b"), background = "#FFFFFF")
    withNew <- do.call(addFrameToQCEframeList, args)
    # The reference shape: every key the builder has always emitted, in order.
    expect_equal(names(withNew[[1]]),
                 c("trialType", "frameName", "stimulus", "stimulus_duration",
                   "post_trial_gap", "response_ends_trial", "choices",
                   "background", "cursorVisible", "output"))
})

test_that("a numeric trial_duration is emitted as given", {
    fl <- addFrameToQCEframeList(
        trialType = "key", frameName = "f", stimulus = "x",
        stimulus_duration = 2000, trial_duration = 10000, post_trial_gap = 0,
        choices = c("f", "j")
    )
    expect_equal(fl[[1]]$trial_duration, 10000)
})

test_that("NO_LIMIT is emitted as an unboxed JSON string, not an array", {
    fl <- addFrameToQCEframeList(
        trialType = "key", frameName = "f", stimulus = "x",
        stimulus_duration = 2000, trial_duration = "NO_LIMIT", post_trial_gap = 0,
        choices = c("f", "j")
    )
    js <- jsonlite::toJSON(fl[[1]]$trial_duration)
    expect_equal(as.character(js), "\"NO_LIMIT\"")
})

test_that("NO_LIMIT is accepted case-insensitively and normalized to upper case", {
    fl <- addFrameToQCEframeList(
        trialType = "key", frameName = "f", stimulus = "x",
        stimulus_duration = 2000, trial_duration = "no_limit", post_trial_gap = 0,
        choices = c("f", "j")
    )
    expect_equal(as.character(fl[[1]]$trial_duration), "NO_LIMIT")
})

test_that("trial_duration rejects non-positive, non-finite, and unknown strings", {
    base <- function(td) addFrameToQCEframeList(
        trialType = "key", frameName = "f", stimulus = "x",
        stimulus_duration = 1000, trial_duration = td, post_trial_gap = 0,
        choices = c("f", "j")
    )
    expect_error(base(0), "positive number")
    expect_error(base(-100), "positive number")
    expect_error(base(Inf), "positive number")
    expect_error(base("forever"), "NO_LIMIT")
    expect_error(base(c(100, 200)), "positive number")
})

test_that("no-exit guard: NO_LIMIT with NO_KEYS is refused", {
    expect_error(
        addFrameToQCEframeList(
            trialType = "key", frameName = "f", stimulus = "x",
            trial_duration = "NO_LIMIT", post_trial_gap = 0, choices = "NO_KEYS"
        ),
        "can never end")
})

test_that("no-exit guard: NO_LIMIT with response_ends_trial FALSE is refused", {
    expect_error(
        addFrameToQCEframeList(
            trialType = "key", frameName = "f", stimulus = "x",
            trial_duration = "NO_LIMIT", post_trial_gap = 0,
            response_ends_trial = FALSE, choices = "ALL_KEYS"
        ),
        "can never end")
})

test_that("no-exit guard: no duration at all plus no choices is refused", {
    expect_error(
        addFrameToQCEframeList(
            trialType = "key", frameName = "f", stimulus = "x",
            post_trial_gap = 0, choices = NULL
        ),
        "can never end")
})

test_that("no-exit guard still catches the original response_ends_trial case", {
    expect_error(
        addFrameToQCEframeList(
            trialType = "key", frameName = "f", stimulus = "x",
            post_trial_gap = 0, response_ends_trial = FALSE
        ),
        "can never end")
})

test_that("no-exit guard allows the legitimate combinations", {
    # NO_LIMIT with a real key
    expect_silent(addFrameToQCEframeList(
        trialType = "key", frameName = "f", stimulus = "x",
        stimulus_duration = 2000, trial_duration = "NO_LIMIT",
        post_trial_gap = 0, choices = c("f", "j")))
    # the min-dwell pattern: NO_KEYS, but a duration ends it
    expect_silent(addFrameToQCEframeList(
        trialType = "key", frameName = "f", stimulus = "x",
        stimulus_duration = 1500, post_trial_gap = 0, choices = "NO_KEYS"))
    # a timed frame that refuses responses
    expect_silent(addFrameToQCEframeList(
        trialType = "key", frameName = "f", stimulus = "x",
        stimulus_duration = 3000, post_trial_gap = 0,
        response_ends_trial = FALSE, choices = NULL))
})

test_that("forceResp plugins are exempt from the no-exit guard", {
    # A survey ends on its own submit button, so no keyboard exit is required.
    expect_silent(addFrameToQCEframeList(
        trialType = "survey", frameName = "f", stimulus = "{}",
        trial_duration = "NO_LIMIT", post_trial_gap = 0, choices = "NO_KEYS"))
    # mcKeys supplies its own digit keys.
    expect_silent(addFrameToQCEframeList(
        trialType = "mcKeys", frameName = "f", stimulus = "{}",
        trial_duration = "NO_LIMIT", post_trial_gap = 0, choices = "NO_KEYS"))
})

test_that("mcKeys per-question deadline round-trips as trial_duration", {
    fl <- addFrameToQCEframeList(
        trialType = "mcKeys", frameName = "q", stimulus = "{}",
        trial_duration = 25000, post_trial_gap = 0
    )
    expect_equal(fl[[1]]$trial_duration, 25000)
})
