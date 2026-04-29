# Tests for buildQCEswitchRule -- XOR validation between countResponse /
# countWhen, threshold shape delegation, optional switchToSet /
# switchInstruction, JSON round-trip against engine's expected shape.

# Helpers used across tests
makeFixedThreshold <- function(n = 5) {
    buildQCEswitchThreshold(values = n, rule = "fixed")
}

# ---- happy paths ---------------------------------------------------------

test_that("countResponse sugar produces threshold + countResponse + switchToSet", {
    r <- buildQCEswitchRule(
        countResponse = "Yes",
        threshold     = makeFixedThreshold(5),
        switchToSet   = "SetB"
    )
    expect_equal(r$countResponse, "Yes")
    expect_equal(r$threshold$values, 5)
    expect_equal(r$threshold$rule, "fixed")
    expect_equal(r$switchToSet, "SetB")
    expect_null(r$countWhen)
    expect_null(r$switchInstruction)
})

test_that("countWhen full form produces threshold + countWhen", {
    r <- buildQCEswitchRule(
        countWhen = list(field = "RT", operator = "lessThan", value = 500),
        threshold = makeFixedThreshold(10)
    )
    expect_equal(r$countWhen$field, "RT")
    expect_equal(r$countWhen$operator, "lessThan")
    expect_equal(r$countWhen$value, 500)
    expect_null(r$countResponse)
    expect_null(r$switchToSet)
})

test_that("switchInstruction is included when provided", {
    r <- buildQCEswitchRule(
        countResponse     = "Prefer Left",
        threshold         = makeFixedThreshold(3),
        switchToSet       = "SetB",
        switchInstruction = "switch_instruct.html"
    )
    expect_equal(r$switchInstruction, "switch_instruct.html")
})

test_that("switchToSet=NULL → early-stop without redirect", {
    r <- buildQCEswitchRule(
        countResponse = "Yes",
        threshold     = makeFixedThreshold(5)
    )
    expect_null(r$switchToSet)
})

test_that("randomIntBetween threshold survives", {
    th <- buildQCEswitchThreshold(values = c(3, 7), rule = "randomIntBetween")
    r  <- buildQCEswitchRule(countResponse = "Yes", threshold = th, switchToSet = "B")
    expect_equal(r$threshold$values, c(3, 7))
    expect_equal(r$threshold$rule, "randomIntBetween")
})

# ---- XOR validation ------------------------------------------------------

test_that("missing both countResponse AND countWhen throws", {
    expect_error(
        buildQCEswitchRule(threshold = makeFixedThreshold()),
        "Must provide either countResponse"
    )
})

test_that("both countResponse AND countWhen throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse = "Yes",
            countWhen     = list(field = "Key", operator = "equals", value = "Yes"),
            threshold     = makeFixedThreshold()
        ),
        "Pass either countResponse or countWhen, not both"
    )
})

# ---- countResponse shape -------------------------------------------------

test_that("non-string countResponse throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse = c("Yes", "No"),
            threshold     = makeFixedThreshold()
        ),
        "countResponse option must be a non-empty single string"
    )
})

test_that("empty-string countResponse throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse = "",
            threshold     = makeFixedThreshold()
        ),
        "countResponse option must be a non-empty single string"
    )
})

# ---- countWhen shape -----------------------------------------------------

test_that("countWhen as non-list throws", {
    expect_error(
        buildQCEswitchRule(
            countWhen = "not a list",
            threshold = makeFixedThreshold()
        ),
        "countWhen must be a list"
    )
})

test_that("countWhen missing field throws", {
    expect_error(
        buildQCEswitchRule(
            countWhen = list(operator = "equals", value = "Yes"),
            threshold = makeFixedThreshold()
        ),
        "countWhen.field must be a non-empty single string"
    )
})

test_that("countWhen invalid operator throws", {
    expect_error(
        buildQCEswitchRule(
            countWhen = list(field = "Key", operator = "wasShown", value = "Yes"),
            threshold = makeFixedThreshold()
        ),
        "countWhen.operator must be one of"
    )
})

test_that("countWhen missing value throws", {
    expect_error(
        buildQCEswitchRule(
            countWhen = list(field = "Key", operator = "equals"),
            threshold = makeFixedThreshold()
        ),
        "countWhen.value is required"
    )
})

# ---- threshold shape -----------------------------------------------------

test_that("missing threshold throws", {
    expect_error(
        buildQCEswitchRule(countResponse = "Yes"),
        "threshold option is required"
    )
})

test_that("threshold non-list throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse = "Yes",
            threshold     = 5
        ),
        "threshold must be a list"
    )
})

test_that("threshold missing rule throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse = "Yes",
            threshold     = list(values = 5)
        ),
        "threshold.rule must be one of"
    )
})

test_that("threshold invalid rule throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse = "Yes",
            threshold     = list(values = 5, rule = "monteCarlo")
        ),
        "threshold.rule must be one of"
    )
})

test_that("threshold randomIntBetween wrong arity throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse = "Yes",
            threshold     = list(values = 5, rule = "randomIntBetween")
        ),
        "requires exactly 2 values"
    )
})

# ---- switchToSet / switchInstruction shape -------------------------------

test_that("non-string switchToSet throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse = "Yes",
            threshold     = makeFixedThreshold(),
            switchToSet   = c("A", "B")
        ),
        "switchToSet option must be a non-empty single string"
    )
})

test_that("empty switchToSet throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse = "Yes",
            threshold     = makeFixedThreshold(),
            switchToSet   = ""
        ),
        "switchToSet option must be a non-empty single string"
    )
})

test_that("non-string switchInstruction throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse     = "Yes",
            threshold         = makeFixedThreshold(),
            switchInstruction = c("a.html", "b.html")
        ),
        "switchInstruction option must be a non-empty single string"
    )
})

test_that("empty switchInstruction throws", {
    expect_error(
        buildQCEswitchRule(
            countResponse     = "Yes",
            threshold         = makeFixedThreshold(),
            switchInstruction = ""
        ),
        "switchInstruction option must be a non-empty single string"
    )
})

# ---- JSON round-trip -----------------------------------------------------

test_that("JSON round-trip preserves countResponse-form rule shape", {
    r <- buildQCEswitchRule(
        countResponse = "Yes",
        threshold     = makeFixedThreshold(5),
        switchToSet   = "SetB"
    )
    json <- jsonlite::toJSON(r, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    # Scalars wrapped as 1-element arrays per QCEP convention; engine's
    # _u() unwraps at read sites.
    expect_equal(parsed$countResponse[[1]], "Yes")
    expect_equal(parsed$switchToSet[[1]], "SetB")
    expect_equal(unlist(parsed$threshold$values), 5)
    expect_equal(parsed$threshold$rule[[1]], "fixed")
    expect_null(parsed$countWhen)
})

test_that("JSON round-trip preserves countWhen-form rule shape", {
    r <- buildQCEswitchRule(
        countWhen = list(field = "RT", operator = "lessThan", value = 500),
        threshold = makeFixedThreshold(10)
    )
    json <- jsonlite::toJSON(r, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_equal(parsed$countWhen$field[[1]], "RT")
    expect_equal(parsed$countWhen$operator[[1]], "lessThan")
    expect_equal(unlist(parsed$countWhen$value), 500)
    expect_null(parsed$countResponse)
    expect_null(parsed$switchToSet)
})
