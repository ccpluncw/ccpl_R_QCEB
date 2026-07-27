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
