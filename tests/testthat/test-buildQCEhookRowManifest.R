# Build a stimfile-shaped list. `frames` is a vector of output flags, one per
# frame, in frame order.
mkScenario <- function(frames) {
  fr <- list()
  for (i in seq_along(frames)) {
    fr[[as.character(i)]] <- list(trialType = list("key"), output = list(frames[i]))
  }
  list(frame = fr, feedback = structure(list(), names = character(0)),
       outputVariables = list(v = list("x")), set = list("s1"))
}

writeStim <- function(dir, scenarios, file = "test_stimfile.json") {
  jsonlite::write_json(scenarios, file.path(dir, file), auto_unbox = FALSE)
}

writeDb <- function(dir, hooksFile = "customHooks.js", file = "dbfile.json") {
  db <- if (is.null(hooksFile)) list(condName = list("c")) else
        list(condName = list("c"), customHooksFile = list(hooksFile))
  jsonlite::write_json(db, file.path(dir, file), auto_unbox = FALSE)
}

newDir <- function() {
  d <- file.path(tempdir(), paste0("hookrow_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

test_that("a scenario ending on a discarded frame is reported", {
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_bad = mkScenario(c(TRUE, FALSE))))

  expect_equal(unsavedQCEhookRows(d), "s_bad")

  rpt <- buildQCEhookRowManifest(d, outFile = NULL, quiet = TRUE)
  expect_true(any(grepl("CHECK THIS!", rpt, fixed = TRUE)))
  expect_true(any(grepl("s_bad", rpt, fixed = TRUE)))
})

test_that("a scenario ending on a kept frame is not reported", {
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_ok = mkScenario(c(FALSE, TRUE))))

  expect_equal(unsavedQCEhookRows(d), character(0))
  rpt <- buildQCEhookRowManifest(d, outFile = NULL, quiet = TRUE)
  expect_true(any(grepl("No scenario ends on a discarded frame", rpt, fixed = TRUE)))
})

test_that("a scenario with NO kept frame is excluded", {
  # Nothing of its own is saved either way, so there is no row to lose. This is
  # the clause that keeps the check quiet on filler/interstitial scenarios.
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_filler = mkScenario(c(FALSE, FALSE, FALSE))))

  expect_equal(unsavedQCEhookRows(d), character(0))
})

test_that("a single-frame scenario is never reported", {
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_one = mkScenario(c(FALSE))))

  expect_equal(unsavedQCEhookRows(d), character(0))
})

test_that("no declared hooks file means nothing is reported by default", {
  d <- newDir()
  writeDb(d, hooksFile = NULL)
  writeStim(d, list(s_bad = mkScenario(c(TRUE, FALSE))))

  expect_equal(unsavedQCEhookRows(d), character(0))
  # ...but the shape is still findable when asked for explicitly.
  expect_equal(unsavedQCEhookRows(d, requireHooksFile = FALSE), "s_bad")
})

test_that("an empty customHooksFile counts as undeclared", {
  # An unset config field serializes as an empty object, not as absent.
  d <- newDir()
  jsonlite::write_json(list(condName = list("c"),
                            customHooksFile = structure(list(), names = character(0))),
                       file.path(d, "dbfile.json"), auto_unbox = FALSE)
  writeStim(d, list(s_bad = mkScenario(c(TRUE, FALSE))))

  expect_equal(unsavedQCEhookRows(d), character(0))
})

test_that("frame keys are ordered numerically, not as text", {
  # With text ordering "10" sorts before "2", so the last frame would be read as
  # frame 9 and a scenario that genuinely ends on a kept frame would be reported.
  d <- newDir()
  writeDb(d)
  flags <- c(rep(FALSE, 9), TRUE)   # frames 1..9 discarded, frame 10 kept
  writeStim(d, list(s_ten = mkScenario(flags)))

  expect_equal(unsavedQCEhookRows(d), character(0))

  # And the mirror case: frame 10 discarded, an earlier frame kept.
  d2 <- newDir()
  writeDb(d2)
  flags2 <- c(TRUE, rep(FALSE, 9))
  writeStim(d2, list(s_ten2 = mkScenario(flags2)))
  expect_equal(unsavedQCEhookRows(d2), "s_ten2")
})

test_that("string flags are accepted as well as JSON booleans", {
  d <- newDir()
  writeDb(d)
  fr <- list("1" = list(output = list("TRUE")), "2" = list(output = list("FALSE")))
  writeStim(d, list(s_str = list(frame = fr)))

  expect_equal(unsavedQCEhookRows(d), "s_str")
})

test_that("the report is written to disk and names the hooks file", {
  d <- newDir()
  writeDb(d, hooksFile = "myHooks.js")
  writeStim(d, list(s_bad = mkScenario(c(TRUE, FALSE))))

  buildQCEhookRowManifest(d, quiet = TRUE)
  p <- file.path(d, "hook_row_manifest.txt")
  expect_true(file.exists(p))
  txt <- readLines(p)
  expect_true(any(grepl("myHooks.js", txt, fixed = TRUE)))
  expect_true(any(grepl("s_bad", txt, fixed = TRUE)))
})

test_that("quiet = FALSE emits a message", {
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_bad = mkScenario(c(TRUE, FALSE))))

  expect_message(buildQCEhookRowManifest(d, outFile = NULL, quiet = FALSE),
                 "hook-row check")
})

test_that("several scenarios across several stimfiles are all reported", {
  d <- newDir()
  writeDb(d)
  writeStim(d, list(a_bad = mkScenario(c(TRUE, FALSE)),
                    a_ok  = mkScenario(c(FALSE, TRUE))), file = "a_stimfile.json")
  writeStim(d, list(b_bad = mkScenario(c(TRUE, TRUE, FALSE))), file = "b_stimfile.json")

  expect_setequal(unsavedQCEhookRows(d), c("a_bad", "b_bad"))
})

test_that("bad arguments are rejected", {
  d <- newDir()
  expect_error(buildQCEhookRowManifest("no/such/dir"), "existing experiment directory")
  expect_error(buildQCEhookRowManifest(d, outFile = ""), "non-empty filename")
  expect_error(buildQCEhookRowManifest(d, quiet = "yes"), "single logical")
  expect_error(unsavedQCEhookRows(d, requireHooksFile = NA), "single logical")
})
