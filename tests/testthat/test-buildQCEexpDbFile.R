# Tests for the experiment-wide completionGate + maxExperimentMinutes params on
# buildQCEexpDbFile (relocated here from buildQCEgroupDbFile: completion and the
# whole-run clock are experiment-wide, beside completionRedirect).

test_that("legacy call emits neither completionGate nor maxExperimentMinutes", {
  base <- buildQCEexpDbFile(expName = "e1")
  expect_null(base$completionGate)
  expect_null(base$maxExperimentMinutes)
  # byte-identical legacy surface: the fields are simply absent
  expect_false("completionGate" %in% names(base))
  expect_false("maxExperimentMinutes" %in% names(base))
})

test_that("formula completionGate emits when supplied", {
  g <- buildQCEexpDbFile(
    expName = "e1",
    completionGate = list(
      combinator = "all",
      formula = list(
        list(fn = "proportion", column = "correct", op = ">=", value = 0.75,
             where = list(trialType = "mcKeys")),
        list(fn = "median", column = "rt", op = ">=", value = 400)
      ),
      noCreditMsg = "No credit."),
    maxExperimentMinutes = 60)
  expect_equal(g$completionGate$combinator, "all")
  expect_equal(length(g$completionGate$formula), 2)
  expect_equal(g$completionGate$formula[[1]]$fn, "proportion")
  expect_equal(g$completionGate$formula[[1]]$column, "correct")
  expect_equal(g$completionGate$formula[[1]]$op, ">=")
  expect_equal(g$completionGate$formula[[1]]$where$trialType, "mcKeys")
  expect_equal(g$maxExperimentMinutes, 60)
})

test_that("gateFn escape hatch emits when supplied", {
  g <- buildQCEexpDbFile(expName = "e1",
                         completionGate = list(gateFn = "myGate", noCreditMsg = "No."))
  expect_equal(g$completionGate$gateFn, "myGate")
})

test_that("completionGate validation rejects malformed configs", {
  # both forms at once
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", formula = list(list(fn = "mean", column = "rt", op = ">=", value = 1)))),
    "EITHER")
  # neither form
  expect_error(buildQCEexpDbFile(expName = "e1", completionGate = list(noCreditMsg = "x")),
               "non-empty 'formula'")
  # invalid fn
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "avg", column = "rt", op = ">=", value = 1)))),
    "invalid fn")
  # invalid op
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "mean", column = "rt", op = "~=", value = 1)))),
    "invalid op")
  # formula missing a value/op/column
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "mean", column = "rt", op = ">=")))),
    "'fn', 'column', 'op', and 'value'")
  # non-numeric value
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "mean", column = "rt", op = ">=", value = "x")))),
    "single finite number")
  # proportion value out of [0,1]
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "proportion", column = "correct", op = ">=", value = 1.5)))),
    "\\[0,1\\]")
  # bad combinator
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(combinator = "most", formula = list(list(fn = "mean", column = "rt", op = ">=", value = 1)))),
    "'all' or 'any'")
  # bad where op
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "count", column = "rt", op = ">=", value = 1,
                                                where = list(rt = list(op = "~", value = 200)))))),
    "invalid op")
  # gateFn not a single string
  expect_error(
    buildQCEexpDbFile(expName = "e1", completionGate = list(gateFn = c("a", "b"))),
    "single string")
})

test_that("where ordering ops require a finite numeric bound", {
  whereVal <- function(v) {
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "count", column = "rt", op = ">=", value = 1,
                                                where = list(rt = list(op = "<", value = v))))))
  }
  # A non-numeric bound can never match a row: it would silently empty the sample
  # at run time, so it is rejected at build time instead.
  expect_error(whereVal("abc"), "must be a single finite number")
  expect_error(whereVal(TRUE),  "must be a single finite number")
  expect_error(whereVal(NULL),  "missing a 'value'")
  # Numbers and numeric strings are accepted.
  expect_silent(whereVal(200))
  expect_silent(whereVal("200"))

  # Equality ops still accept non-numeric values -- that is their purpose.
  expect_silent(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "count", column = "rt", op = ">=", value = 1,
                                                where = list(trialType = list(op = "==", value = "mcKeys")))))))
  # A bare scalar filter is untouched by the rule.
  expect_silent(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "count", column = "rt", op = ">=", value = 1,
                                                where = list(trialType = "mcKeys"))))))
})

test_that("maxExperimentMinutes validation rejects non-positive / non-scalar", {
  expect_error(buildQCEexpDbFile(expName = "e1", maxExperimentMinutes = -5), "positive number")
  expect_error(buildQCEexpDbFile(expName = "e1", maxExperimentMinutes = 0), "positive number")
  expect_error(buildQCEexpDbFile(expName = "e1", maxExperimentMinutes = c(1, 2)), "single positive")
})

test_that("run-integrity flags are absent unless supplied", {
  base <- buildQCEexpDbFile(expName = "e1")
  expect_false("warnOnLeave" %in% names(base))
  expect_false("strictGroupAssignment" %in% names(base))
})

test_that("run-integrity flags emit both Boolean values", {
  # FALSE must emit, not be treated as "unset": the whole point of warnOnLeave is
  # to turn OFF a guard that is on by default, so a builder that dropped FALSE
  # would make the opt-out impossible to express.
  off <- buildQCEexpDbFile(expName = "e1", warnOnLeave = FALSE)
  expect_true("warnOnLeave" %in% names(off))
  expect_false(off$warnOnLeave)

  on <- buildQCEexpDbFile(expName = "e1", warnOnLeave = TRUE, strictGroupAssignment = TRUE)
  expect_true(on$warnOnLeave)
  expect_true(on$strictGroupAssignment)

  lax <- buildQCEexpDbFile(expName = "e1", strictGroupAssignment = FALSE)
  expect_true("strictGroupAssignment" %in% names(lax))
  expect_false(lax$strictGroupAssignment)
})

test_that("run-integrity flags reject non-Boolean and non-scalar", {
  # Strings are rejected even though the engine's flag reader accepts them from a
  # hand-authored dbfile: in R code a string is a mistake, not a spelling.
  expect_error(buildQCEexpDbFile(expName = "e1", warnOnLeave = "TRUE"), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", warnOnLeave = 1), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", warnOnLeave = NA), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", warnOnLeave = c(TRUE, FALSE)), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", strictGroupAssignment = "yes"), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", strictGroupAssignment = 0), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", strictGroupAssignment = NA), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", strictGroupAssignment = c(TRUE, TRUE)), "single Boolean")
})

test_that("run-integrity flags survive the JSON round trip as scalars", {
  # The engine reads these through a flag helper that unwraps jsonlite's
  # one-element array wrapping. Confirm the wrapping is what that helper expects
  # and that FALSE does not come back as an empty value.
  db <- buildQCEexpDbFile(expName = "e1", warnOnLeave = FALSE, strictGroupAssignment = TRUE)
  back <- jsonlite::fromJSON(jsonlite::toJSON(db), simplifyVector = FALSE)
  expect_equal(length(back$warnOnLeave), 1)
  expect_false(back$warnOnLeave[[1]])
  expect_true(back$strictGroupAssignment[[1]])
})

# --- attempts policy: attemptsAllowed + retryPrompt --------------------------
# Mirrors the engine's qcepValidateAttemptsPolicy. These ran only at experiment
# start before, so a mistyped attempts policy reached a participant's browser
# before anyone heard about it.

.promptOK <- list(text = "Try again?", yesLabel = "Yes", noLabel = "No")

test_that("a gate that says nothing about attempts is unchanged", {
  g <- buildQCEexpDbFile(expName = "e1",
        completionGate = list(gateFn = "f"))
  expect_false("attemptsAllowed" %in% names(g$completionGate))
  expect_false("retryPrompt" %in% names(g$completionGate))
})

test_that("a valid attempts policy is emitted verbatim", {
  g <- buildQCEexpDbFile(expName = "e1",
        completionGate = list(gateFn = "f", attemptsAllowed = 2,
                              retryPrompt = .promptOK))
  expect_equal(g$completionGate$attemptsAllowed, 2)
  expect_equal(g$completionGate$retryPrompt$yesLabel, "Yes")
})

test_that("attemptsAllowed must be a whole number of 1 or more", {
  mk <- function(n) buildQCEexpDbFile(expName = "e1",
          completionGate = list(gateFn = "f", attemptsAllowed = n,
                                retryPrompt = .promptOK))
  expect_error(mk(0), "whole number")
  expect_error(mk(-1), "whole number")
  expect_error(mk(1.5), "whole number")
  expect_error(mk("2"), "whole number")
  expect_error(mk(c(2, 3)), "whole number")
  expect_error(mk(Inf), "whole number")
})

test_that("attempts and prompt are cross-checked in both directions", {
  # More than one attempt with no way to ask for it: the extra attempts are
  # unreachable.
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", attemptsAllowed = 2)),
    "unreachable")
  # A prompt with an explicit 1 -- the researcher meant one attempt.
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", attemptsAllowed = 1,
                            retryPrompt = .promptOK)),
    "Raise attemptsAllowed")
  # A prompt with no attemptsAllowed at all -- they probably forgot the key, and
  # the advice must differ from the case above.
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", retryPrompt = .promptOK)),
    "Add attemptsAllowed")
})

test_that("retryPrompt needs all three non-empty strings", {
  mk <- function(p) buildQCEexpDbFile(expName = "e1",
          completionGate = list(gateFn = "f", attemptsAllowed = 2, retryPrompt = p))
  expect_error(mk(list(yesLabel = "Y", noLabel = "N")), "retryPrompt\\$text")
  expect_error(mk(list(text = "T", noLabel = "N")), "retryPrompt\\$yesLabel")
  expect_error(mk(list(text = "T", yesLabel = "Y")), "retryPrompt\\$noLabel")
  expect_error(mk(list(text = "", yesLabel = "Y", noLabel = "N")), "retryPrompt\\$text")
  expect_error(mk("Try again?"), "named list")
  expect_warning(mk(c(.promptOK, list(yesLabl = "Y"))), "unrecognized")
})

test_that("the attempts policy is checked on a formula gate too, not just gateFn", {
  # ⚠ The engine validates attempts after the shape check, whichever form the
  # gate took. A check that lived in the formula branch would leave the escape
  # hatch unguarded, and one in the gateFn branch would leave formulas unguarded.
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(
        formula = list(list(fn = "proportion", column = "correct", op = ">=", value = 0.8)),
        attemptsAllowed = 2)),
    "unreachable")
})

test_that("a stated gate message may not be empty", {
  for (m in c("noCreditMsg", "supersededMsg")) {
    cfg <- list(gateFn = "f"); cfg[[m]] <- ""
    expect_error(buildQCEexpDbFile(expName = "e1", completionGate = cfg), m)
  }
  # Absent is fine -- the engine supplies its own default.
  expect_silent(buildQCEexpDbFile(expName = "e1", completionGate = list(gateFn = "f")))
})

test_that("a misspelled gate key warns instead of vanishing", {
  # ⚠⚠ The mistake nothing else catches: misspell BOTH attempts keys and the
  # cross-checks stay quiet, leaving a one-attempt gate that never offers a
  # retry -- the exact failure the feature exists to prevent.
  expect_warning(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", attemptsAllowd = 2,
                            retryPromt = .promptOK)),
    "unrecognized")
  expect_warning(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", attemptsAllowd = 2, retryPromt = .promptOK)),
    "attemptsAllowd")
  # Every documented key is silent.
  expect_silent(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "mean", column = "rt", op = ">=", value = 1)),
                            combinator = "any", noCreditMsg = "no",
                            supersededMsg = "already", attemptsAllowed = 2,
                            retryPrompt = .promptOK)))
})

# --- creditClaimTimeoutMs ---------------------------------------------------

test_that("creditClaimTimeoutMs is absent unless stated", {
  expect_false("creditClaimTimeoutMs" %in% names(buildQCEexpDbFile(expName = "e1")))
})

test_that("creditClaimTimeoutMs accepts a usable value and refuses an unusable one", {
  expect_equal(buildQCEexpDbFile(expName = "e1",
                 creditClaimTimeoutMs = 30000)$creditClaimTimeoutMs, 30000)
  expect_equal(buildQCEexpDbFile(expName = "e1",
                 creditClaimTimeoutMs = 1000)$creditClaimTimeoutMs, 1000)
  # ⚠ 0 does not mean "no timeout", it REMOVES the bound on the one request that
  # can hang the end of a run. A string does the same thing by way of NaN.
  expect_error(buildQCEexpDbFile(expName = "e1", creditClaimTimeoutMs = 0), "at least 1000")
  expect_error(buildQCEexpDbFile(expName = "e1", creditClaimTimeoutMs = "30s"), "at least 1000")
  # A few milliseconds fails every claim, and the claim fails open.
  expect_error(buildQCEexpDbFile(expName = "e1", creditClaimTimeoutMs = 5), "at least 1000")
  expect_error(buildQCEexpDbFile(expName = "e1", creditClaimTimeoutMs = Inf), "at least 1000")
})
