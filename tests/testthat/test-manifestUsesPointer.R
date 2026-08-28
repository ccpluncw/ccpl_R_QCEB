newDir <- function() {
  d <- file.path(tempdir(), paste0("manptr_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

writeStim <- function(d, stim) {
  jsonlite::write_json(stim, file.path(d, "stimfile.json"), auto_unbox = FALSE)
}

test_that("a mouse-driven trialType is flagged in the manifest's type section", {
  d <- newDir()
  writeStim(d, list(
    scen1 = list(frame = list(f1 = list(trialType = "numberline"))),
    scen2 = list(frame = list(f1 = list(trialType = "key")))
  ))

  rpt <- buildQCEoutputFieldManifest(d, outFile = NULL)
  expect_true(any(grepl("# [numberline] is mouse-driven (usesPointer)", rpt,
                        fixed = TRUE)))
  # A keyboard type carries no pointer note -- the flag means something only
  # because it is absent where it does not apply.
  expect_false(any(grepl("# [key] is mouse-driven", rpt, fixed = TRUE)))
})

test_that("an unregistered trialType gets no pointer note, only the undeclared line", {
  d <- newDir()
  writeStim(d, list(
    scen1 = list(frame = list(f1 = list(trialType = "cyberball")))
  ))

  rpt <- buildQCEoutputFieldManifest(d, outFile = NULL)
  expect_false(any(grepl("mouse-driven", rpt, fixed = TRUE)))
  expect_true(any(grepl("# [cyberball] declares no outputColumns", rpt,
                        fixed = TRUE)))
})

test_that("the pointer note is advisory only -- it adds no expected column", {
  d <- newDir()
  writeStim(d, list(
    scen1 = list(frame = list(f1 = list(trialType = "numberline")))
  ))

  fields <- file.path(d, "fields.txt")
  writeLines(c("Exp_Name", "Group", "sn", "Cond_Name", "Sess_Name",
               "BlockName", "BlockNum", "BlockIt", "Trial", "TrialInSession",
               "TrialinBlock", "trial_index", "trial_type", "StimNum",
               "FrameNum", "FrameName", "respType", "posttgap", "stim_dur",
               "rt", "Set", "stimRef", "ShowFeedBack",
               "Key", "FeedBack", "Response", "Stimulus"), fields)

  expect_length(missingQCEoutputFields(d, fieldsFile = fields), 0)
})
