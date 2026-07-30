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

test_that("a scenario keeping no row is reported", {
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_filler = mkScenario(c(FALSE, FALSE, FALSE))))

  expect_equal(promotedQCEhookRows(d), "s_filler")

  rpt <- buildQCEhookRowManifest(d, outFile = NULL, quiet = TRUE)
  expect_true(any(grepl("keep no row of their own", rpt, fixed = TRUE)))
  expect_true(any(grepl("s_filler", rpt, fixed = TRUE)))
})

test_that("a scenario ending on a discarded frame is NOT reported", {
  # The engine routes annotations to the last kept row, so this shape costs
  # nothing and reporting it would be noise.
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_trailing = mkScenario(c(TRUE, FALSE))))

  expect_equal(promotedQCEhookRows(d), character(0))
  rpt <- buildQCEhookRowManifest(d, outFile = NULL, quiet = TRUE)
  expect_true(any(grepl("keeps at least one row of its own", rpt, fixed = TRUE)))
})

test_that("a scenario ending on a kept frame is not reported", {
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_ok = mkScenario(c(FALSE, TRUE))))

  expect_equal(promotedQCEhookRows(d), character(0))
})

test_that("a single-frame scenario is never reported", {
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_one = mkScenario(c(FALSE))))

  expect_equal(promotedQCEhookRows(d), character(0))
})

test_that("no declared hooks file means nothing is reported by default", {
  d <- newDir()
  writeDb(d, hooksFile = NULL)
  writeStim(d, list(s_filler = mkScenario(c(FALSE, FALSE))))

  expect_equal(promotedQCEhookRows(d), character(0))
  # ...but the shape is still findable when asked for explicitly.
  expect_equal(promotedQCEhookRows(d, requireHooksFile = FALSE), "s_filler")
})

test_that("an empty customHooksFile counts as undeclared", {
  # An unset config field serializes as an empty object, not as absent.
  d <- newDir()
  jsonlite::write_json(list(condName = list("c"),
                            customHooksFile = structure(list(), names = character(0))),
                       file.path(d, "dbfile.json"), auto_unbox = FALSE)
  writeStim(d, list(s_filler = mkScenario(c(FALSE, FALSE))))

  expect_equal(promotedQCEhookRows(d), character(0))
})

test_that("frame keys are ordered numerically, not as text", {
  # With text ordering "10" sorts before "2". The reported set does not depend on
  # order, but nFrames/last in the report do, so the ordering is still asserted.
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_ten = mkScenario(rep(FALSE, 10))))

  rpt <- buildQCEhookRowManifest(d, outFile = NULL, quiet = TRUE)
  hit <- grep("s_ten", rpt, value = TRUE)
  expect_length(hit, 1)
  expect_match(hit, "10\\s+10$")   # ten frames, last frame keyed "10"
})

test_that("string flags are accepted as well as JSON booleans", {
  d <- newDir()
  writeDb(d)
  fr <- list("1" = list(output = list("FALSE")), "2" = list(output = list("FALSE")))
  writeStim(d, list(s_str = list(frame = fr)))
  expect_equal(promotedQCEhookRows(d), "s_str")

  # A string TRUE must count as kept, or the scenario would be reported wrongly.
  d2 <- newDir()
  writeDb(d2)
  fr2 <- list("1" = list(output = list("TRUE")), "2" = list(output = list("FALSE")))
  writeStim(d2, list(s_str2 = list(frame = fr2)))
  expect_equal(promotedQCEhookRows(d2), character(0))
})

test_that("the report is written to disk and names the hooks file", {
  d <- newDir()
  writeDb(d, hooksFile = "myHooks.js")
  writeStim(d, list(s_filler = mkScenario(c(FALSE, FALSE))))

  buildQCEhookRowManifest(d, quiet = TRUE)
  p <- file.path(d, "hook_row_manifest.txt")
  expect_true(file.exists(p))
  txt <- readLines(p)
  expect_true(any(grepl("myHooks.js", txt, fixed = TRUE)))
  expect_true(any(grepl("s_filler", txt, fixed = TRUE)))
})

test_that("quiet = FALSE emits a message", {
  d <- newDir()
  writeDb(d)
  writeStim(d, list(s_filler = mkScenario(c(FALSE, FALSE))))

  expect_message(buildQCEhookRowManifest(d, outFile = NULL, quiet = FALSE),
                 "hook-row check")
})

test_that("several scenarios across several stimfiles are all reported", {
  d <- newDir()
  writeDb(d)
  writeStim(d, list(a_filler = mkScenario(c(FALSE, FALSE)),
                    a_ok     = mkScenario(c(FALSE, TRUE))), file = "a_stimfile.json")
  writeStim(d, list(b_filler = mkScenario(c(FALSE, FALSE, FALSE))), file = "b_stimfile.json")

  expect_setequal(promotedQCEhookRows(d), c("a_filler", "b_filler"))
})

test_that("bad arguments are rejected", {
  d <- newDir()
  expect_error(buildQCEhookRowManifest("no/such/dir"), "existing experiment directory")
  expect_error(buildQCEhookRowManifest(d, outFile = ""), "non-empty filename")
  expect_error(buildQCEhookRowManifest(d, quiet = "yes"), "single logical")
  expect_error(promotedQCEhookRows(d, requireHooksFile = NA), "single logical")
})
