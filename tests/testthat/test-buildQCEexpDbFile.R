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
