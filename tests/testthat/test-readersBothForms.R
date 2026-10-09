#a study written with every single value bare, and the same study boxed
formsDirs <- function() {
  base <- file.path(tempdir(), paste0("forms_", as.integer(runif(1, 1, 1e9))))
  bare <- file.path(base, "bare")
  boxed <- file.path(base, "boxed")
  dir.create(bare, recursive = TRUE, showWarnings = FALSE)
  dir.create(boxed, recursive = TRUE, showWarnings = FALSE)
  km <- buildKeyMap(data.frame(correct = "d", wrong = "k"))
  fr <- addFixationToQCEframeList(NULL)
  fr <- addFrameToQCEframeList(fr, stimulus = "<p>x</p>", post_trial_gap = 0,
                               choices = c("d", "k"), output = TRUE)
  fb <- createFeedbackList(addKeyToFeedbackKeyList(NULL, km["correct"], "Right!"),
                           showFeedback = TRUE)
  sc <- addScenarioToQCEscenarioList(NULL, fr, fb,
                                     createQCEoutputVariableList(data.frame(cond = "a", rtEst = NA_real_)),
                                     "A")
  db <- buildQCEgroupDbFile(condName = "c", keyMap = km, customHooksFile = "h.js",
                            customHooksColumns = "score")
  sidecar <- buildQCEpageSidecar(fields = list(buildQCEpageField("age", as = "Age")))
  saveJsonFile(sc, file.path(bare, "s_Stimfile.json"))
  saveJsonFile(db, file.path(bare, "g_Dbfile.json"))
  saveJsonFile(sidecar, file.path(bare, "age.page.json"))
  writeLines("ok", file.path(bare, "h.js"))
  #the same files with every single value inside a list of one
  for (f in c("s_Stimfile.json", "g_Dbfile.json", "age.page.json")) {
    j <- jsonlite::fromJSON(file.path(bare, f), simplifyVector = FALSE)
    box <- function(x) {
      if (!is.list(x)) return(list(x))
      if (is.null(names(x))) return(lapply(x, function(e) if (is.list(e)) box(e) else e))
      lapply(x, box)
    }
    writeLines(jsonlite::toJSON(box(j), auto_unbox = TRUE, null = "null", pretty = TRUE),
               file.path(boxed, f))
  }
  writeLines("ok", file.path(boxed, "h.js"))
  list(bare = bare, boxed = boxed)
}

test_that("the field manifest does not report a feedback label as a data column", {
  d <- formsDirs()
  fields <- file.path(d$bare, "fields.txt")
  writeLines(c("Exp_Name", "Group", "sn", "Cond_Name", "Sess_Name", "BlockName", "BlockNum",
               "BlockIt", "Trial", "TrialInSession", "TrialinBlock", "trial_index", "trial_type",
               "StimNum", "FrameNum", "FrameName", "respType", "posttgap", "stim_dur", "rt", "Set",
               "stimRef", "ShowFeedBack", "Key", "FeedBack", "Response", "cond", "rtEst", "Age",
               "score"), fields)
  rep <- buildQCEoutputFieldManifest(d$bare, outFile = NULL, fieldsFile = fields)
  expect_false(any(grepl("list(", rep, fixed = TRUE)))
  expect_length(missingQCEoutputFields(d$bare, fields), 0)
})

test_that("the field manifest knows the identity columns a study may keep", {
  d <- formsDirs()
  fields <- file.path(d$bare, "fields.txt")
  writeLines(c("Exp_Name", "SessionKey", "BlockKey"), fields)
  rep <- buildQCEoutputFieldManifest(d$bare, outFile = NULL, fieldsFile = fields)
  extra <- which(grepl("not expected here", rep, fixed = TRUE))
  listed <- if (length(extra)) rep[seq(extra[1] + 1, length(rep))] else character(0)
  expect_false(any(grepl("SessionKey|BlockKey", listed)))
  expect_false("SessionKey" %in% missingQCEoutputFields(d$bare, fields))
})

test_that("the field manifest reads a study written either way alike", {
  d <- formsDirs()
  a <- buildQCEoutputFieldManifest(d$bare, outFile = NULL)
  b <- buildQCEoutputFieldManifest(d$boxed, outFile = NULL)
  expect_identical(a, b)
  expect_true(all(c("cond", "rtEst", "Age") %in% a))
})

test_that("the hook row report reads a study written either way alike", {
  d <- formsDirs()
  a <- promotedQCEhookRows(d$bare)
  b <- promotedQCEhookRows(d$boxed)
  expect_identical(a, b)
  ra <- buildQCEhookRowManifest(d$bare, outFile = NULL, quiet = TRUE)
  rb <- buildQCEhookRowManifest(d$boxed, outFile = NULL, quiet = TRUE)
  expect_identical(sub("^# directory: .*", "", ra), sub("^# directory: .*", "", rb))
})

test_that("readQCEjsonFile gives the same list for a study written either way", {
  d <- formsDirs()
  a <- readQCEjsonFile(file.path(d$bare, "g_Dbfile.json"))
  b <- readQCEjsonFile(file.path(d$boxed, "g_Dbfile.json"), unboxLists = TRUE)
  f1 <- tempfile(fileext = ".json")
  f2 <- tempfile(fileext = ".json")
  saveJsonFile(a, f1, fileKind = "groupSettings")
  saveJsonFile(b, f2, fileKind = "groupSettings")
  expect_identical(readLines(f1), readLines(f2))
  expect_identical(readLines(f1), readLines(file.path(d$bare, "g_Dbfile.json")))
})
