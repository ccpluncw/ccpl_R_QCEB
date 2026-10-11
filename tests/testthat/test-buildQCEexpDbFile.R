#tests for completionGate and maxExperimentMinutes on buildQCEexpDbFile

test_that("legacy call emits neither completionGate nor maxExperimentMinutes", {
  base <- buildQCEexpDbFile(expName = "e1")
  expect_null(base$completionGate)
  expect_null(base$maxExperimentMinutes)
  #the fields are simply absent
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
  #both forms at once
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", formula = list(list(fn = "mean", column = "rt", op = ">=", value = 1)))),
    "EITHER")
  #neither form
  expect_error(buildQCEexpDbFile(expName = "e1", completionGate = list(noCreditMsg = "x")),
               "non-empty 'formula'")
  #invalid fn
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "avg", column = "rt", op = ">=", value = 1)))),
    "invalid fn")
  #invalid op
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "mean", column = "rt", op = "~=", value = 1)))),
    "invalid op")
  #formula missing a value, op or column
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "mean", column = "rt", op = ">=")))),
    "'fn', 'column', 'op', and 'value'")
  #non-numeric value
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "mean", column = "rt", op = ">=", value = "x")))),
    "single finite number")
  #proportion value out of 0 to 1
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "proportion", column = "correct", op = ">=", value = 1.5)))),
    "\\[0,1\\]")
  #bad combinator
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(combinator = "most", formula = list(list(fn = "mean", column = "rt", op = ">=", value = 1)))),
    "'all' or 'any'")
  #bad where op
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "count", column = "rt", op = ">=", value = 1,
                                                where = list(rt = list(op = "~", value = 200)))))),
    "invalid op")
  #gateFn not a single string
  expect_error(
    buildQCEexpDbFile(expName = "e1", completionGate = list(gateFn = c("a", "b"))),
    "takes one value")
})

test_that("where ordering ops require a finite numeric bound", {
  whereVal <- function(v) {
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "count", column = "rt", op = ">=", value = 1,
                                                where = list(rt = list(op = "<", value = v))))))
  }
  #a non-numeric bound would empty the sample at run time, so it is rejected
  expect_error(whereVal("abc"), "must be a single finite number")
  expect_error(whereVal(TRUE),  "must be a single finite number")
  expect_error(whereVal(NULL),  "missing a 'value'")
  #numbers and numeric strings are accepted
  expect_silent(whereVal(200))
  expect_silent(whereVal("200"))

  #equality ops still accept non-numeric values
  expect_silent(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "count", column = "rt", op = ">=", value = 1,
                                                where = list(trialType = list(op = "==", value = "mcKeys")))))))
  #a bare scalar filter is untouched
  expect_silent(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "count", column = "rt", op = ">=", value = 1,
                                                where = list(trialType = "mcKeys"))))))
})

test_that("maxExperimentMinutes validation rejects non-positive / non-scalar", {
  expect_error(buildQCEexpDbFile(expName = "e1", maxExperimentMinutes = -5), "positive number")
  expect_error(buildQCEexpDbFile(expName = "e1", maxExperimentMinutes = 0), "positive number")
  expect_error(buildQCEexpDbFile(expName = "e1", maxExperimentMinutes = c(1, 2)), "takes one value")
})

test_that("run-integrity flags are absent unless supplied", {
  base <- buildQCEexpDbFile(expName = "e1")
  expect_false("warnOnLeave" %in% names(base))
  expect_false("strictGroupAssignment" %in% names(base))
})

test_that("run-integrity flags emit both Boolean values", {
  #a FALSE must emit, it is the opt-out of a guard on by default
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
  #a string flag is a mistake in R code, though the engine reads one
  expect_error(buildQCEexpDbFile(expName = "e1", warnOnLeave = "TRUE"), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", warnOnLeave = 1), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", warnOnLeave = NA), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", warnOnLeave = c(TRUE, FALSE)), "takes one value")
  expect_error(buildQCEexpDbFile(expName = "e1", strictGroupAssignment = "yes"), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", strictGroupAssignment = 0), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", strictGroupAssignment = NA), "single Boolean")
  expect_error(buildQCEexpDbFile(expName = "e1", strictGroupAssignment = c(TRUE, TRUE)), "takes one value")
})

test_that("run-integrity flags survive the JSON round trip as scalars", {
  #the engine's flag helper unwraps one-element arrays, FALSE must survive
  db <- buildQCEexpDbFile(expName = "e1", warnOnLeave = FALSE, strictGroupAssignment = TRUE)
  back <- jsonlite::fromJSON(jsonlite::toJSON(db), simplifyVector = FALSE)
  expect_equal(length(back$warnOnLeave), 1)
  expect_false(back$warnOnLeave[[1]])
  expect_true(back$strictGroupAssignment[[1]])
})

#attempts policy, attemptsAllowed and retryPrompt

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
  expect_error(mk(c(2, 3)), "takes one value")
  expect_error(mk(Inf), "whole number")
})

test_that("attempts and prompt are cross-checked in both directions", {
  #more than one attempt with no way to ask for it
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", attemptsAllowed = 2)),
    "unreachable")
  #a prompt with an explicit 1 means one attempt
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", attemptsAllowed = 1,
                            retryPrompt = .promptOK)),
    "Raise attemptsAllowed")
  #a prompt with no attemptsAllowed, the advice differs from the case above
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
  #attempts are validated after the shape check for both gate forms
  expect_error(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(
        formula = list(list(fn = "proportion", column = "correct", op = ">=", value = 0.8)),
        attemptsAllowed = 2)),
    "unreachable")
})

test_that("a stated gate message may not be empty", {
  #every message the engine can show is checked the same way
  for (m in c("creditMsg", "noCreditMsg", "unscoredMsg",
              "supersededMsg", "duplicateMsg")) {
    cfg <- list(gateFn = "f"); cfg[[m]] <- ""
    expect_error(buildQCEexpDbFile(expName = "e1", completionGate = cfg), m)
  }
  #absent is fine, the engine supplies its own default
  expect_silent(buildQCEexpDbFile(expName = "e1", completionGate = list(gateFn = "f")))
})

test_that("creditMsg, unscoredMsg and scoredBy are KNOWN gate keys", {
  #the key list must track the engine's reads, creditMsg and unscoredMsg too
  cfg <- list(gateFn = "f", creditMsg = "you passed",
              unscoredMsg = "left for review", scoredBy = "client")
  expect_silent(buildQCEexpDbFile(expName = "e1", completionGate = cfg))
  built <- buildQCEexpDbFile(expName = "e1", completionGate = cfg)$completionGate
  expect_equal(built$creditMsg,   "you passed")
  expect_equal(built$unscoredMsg, "left for review")
  expect_equal(built$scoredBy,    "client")
})

test_that("scoredBy accepts only the two values the engine accepts", {
  for (ok in c("client", "server")) {
    expect_silent(buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", scoredBy = ok)))
  }
  for (bad in list("Server", "auto", 1, c("client", "server"))) {
    cfg <- list(gateFn = "f"); cfg$scoredBy <- bad
    expect_error(buildQCEexpDbFile(expName = "e1", completionGate = cfg),
                 "scoredBy")
  }
})

test_that("a misspelled gate key warns instead of vanishing", {
  #misspelling both attempts keys leaves a gate that never offers a retry
  expect_warning(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", attemptsAllowd = 2,
                            retryPromt = .promptOK)),
    "unrecognized")
  expect_warning(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", attemptsAllowd = 2, retryPromt = .promptOK)),
    "attemptsAllowd")
  #every documented key is silent
  expect_silent(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(formula = list(list(fn = "mean", column = "rt", op = ">=", value = 1)),
                            combinator = "any", scoredBy = "client",
                            creditMsg = "yes", noCreditMsg = "no",
                            unscoredMsg = "pending",
                            supersededMsg = "already", attemptsAllowed = 2,
                            retryPrompt = .promptOK)))
})

test_that("duplicateMsg is a KNOWN gate key", {
  #duplicateMsg is an engine-read key, so it must not draw a warning
  expect_silent(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f", duplicateMsg = "already credited")))
  expect_equal(
    buildQCEexpDbFile(expName = "e1",
      completionGate = list(gateFn = "f",
                            duplicateMsg = "already credited"))$completionGate$duplicateMsg,
    "already credited")
})

test_that("attemptsAllowed has a floor and no ceiling", {
  #the budget is any whole number of 1 or more, not capped at 2
  for (n in c(1, 2, 3, 7, 40)) {
    cfg <- list(gateFn = "f", attemptsAllowed = n)
    if (n > 1) cfg$retryPrompt <- .promptOK
    expect_equal(buildQCEexpDbFile(expName = "e1",
                                   completionGate = cfg)$completionGate$attemptsAllowed, n)
  }
  expect_error(buildQCEexpDbFile(expName = "e1",
    completionGate = list(gateFn = "f", attemptsAllowed = 0)), "attemptsAllowed")
  expect_error(buildQCEexpDbFile(expName = "e1",
    completionGate = list(gateFn = "f", attemptsAllowed = 2.5, retryPrompt = .promptOK)),
    "attemptsAllowed")
})

#creditClaimTimeoutMs

test_that("creditClaimTimeoutMs is absent unless stated", {
  expect_false("creditClaimTimeoutMs" %in% names(buildQCEexpDbFile(expName = "e1")))
})

test_that("creditClaimTimeoutMs accepts a usable value and refuses an unusable one", {
  expect_equal(buildQCEexpDbFile(expName = "e1",
                 creditClaimTimeoutMs = 30000)$creditClaimTimeoutMs, 30000)
  expect_equal(buildQCEexpDbFile(expName = "e1",
                 creditClaimTimeoutMs = 1000)$creditClaimTimeoutMs, 1000)
  #0 removes the bound on the credit claim, and a string does the same
  expect_error(buildQCEexpDbFile(expName = "e1", creditClaimTimeoutMs = 0), "at least 1000")
  expect_error(buildQCEexpDbFile(expName = "e1", creditClaimTimeoutMs = "30s"), "at least 1000")
  #a few milliseconds fails every claim, and the claim fails open
  expect_error(buildQCEexpDbFile(expName = "e1", creditClaimTimeoutMs = 5), "at least 1000")
  expect_error(buildQCEexpDbFile(expName = "e1", creditClaimTimeoutMs = Inf), "at least 1000")
})

#reservationMinutes

test_that("reservationMinutes is absent unless stated", {
  expect_false("reservationMinutes" %in% names(buildQCEexpDbFile(expName = "e1")))
  expect_null(buildQCEexpDbFile(expName = "e1")$reservationMinutes)
})

test_that("a stated reservation window is carried through verbatim", {
  expect_equal(buildQCEexpDbFile(expName = "e1",
                 reservationMinutes = 90)$reservationMinutes, 90)
  #fractions are allowed: the window is a duration, not a count
  expect_equal(buildQCEexpDbFile(expName = "e1",
                 reservationMinutes = 45.5)$reservationMinutes, 45.5)
})

test_that("reservationMinutes rejects non-positive / non-scalar / unusable", {
  mk <- function(v) buildQCEexpDbFile(expName = "e1", reservationMinutes = v)
  expect_error(mk(0), "positive number")
  expect_error(mk(-5), "positive number")
  expect_error(mk("90"), "positive number")
  expect_error(mk(c(30, 60)), "takes one value")
  expect_error(mk(NA), "positive number")
  expect_error(mk(NA_real_), "positive number")
  expect_error(mk(Inf), "positive number")
  #a Boolean passes is.finite and is.finite alone would let it through
  expect_error(mk(TRUE), "positive number")
  expect_error(mk(FALSE), "positive number")
})

test_that("reservationMinutes survives the JSON round trip as a one-element array", {
  db <- buildQCEexpDbFile(expName = "e1", reservationMinutes = 90)
  back <- jsonlite::fromJSON(jsonlite::toJSON(db), simplifyVector = FALSE)
  expect_equal(length(back$reservationMinutes), 1)
  expect_equal(back$reservationMinutes[[1]], 90)
})

#researcherContact

test_that("researcherContact is absent unless stated", {
  expect_false("researcherContact" %in% names(buildQCEexpDbFile(expName = "e1")))
})

test_that("a valid email address is stored as given, trimmed", {
  db <- buildQCEexpDbFile(expName = "e1", researcherContact = " lab.person@uni.edu ")
  expect_equal(db$researcherContact, "lab.person@uni.edu")
})

test_that("any single string is stored, trimmed; the platform and the engine check the address", {
  for (a in c("a@b", "first.o'brien@uni.edu", "name@uni.ac.uk", "x@localhost", "jos\u00e9@uni.edu")) {
    expect_equal(buildQCEexpDbFile(expName = "e1", researcherContact = paste0(" ", a, " "))$researcherContact, a)
  }
})

test_that("a contact that is not a single string is refused", {
  for (b in list(c("a@b.edu", "c@d.edu"), 3, NA_character_, character(0))) {
    expect_error(buildQCEexpDbFile(expName = "e1", researcherContact = b), "researcherContact")
  }
})
