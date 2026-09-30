#tests for stimulus-set references and their expansion into scenarios

.writeSet <- function(root, name, kind = "files") {
  dir.create(file.path(root, name, "files"), recursive = TRUE, showWarnings = FALSE)
  if (kind == "files") {
    items <- lapply(1:5, function(i) {
      list(id = sprintf("d%03d", i), file = sprintf("files/d%03d.png", i),
           mediaType = "image/png", bytes = 10, sha256 = strrep("a", 64),
           width = 10, height = 10,
           attrs = list(rating = i + 0.5, gender = if (i %% 2) "f" else "m"))
    })
    attrs <- list(list(name = "rating", type = "number"),
                  list(name = "gender", type = "category", levels = c("f", "m")))
  } else {
    items <- lapply(1:4, function(i) {
      list(id = sprintf("w%03d", i), text = c("a & b", "<tag>", "plain", "quote \"x\"")[i],
           attrs = list(frequency = i * 10))
    })
    items[[4]]$attrs <- list()
    attrs <- list(list(name = "frequency", type = "number"))
  }
  doc <- list(manifestVersion = 1,
              set = list(name = name, version = 3, kind = kind,
                         lockedAt = "2026-01-01T00:00:00Z", contentHash = "sha256:x",
                         source = list(description = "test", basis = "own",
                                       identifiablePeople = FALSE)),
              attributes = attrs, items = items)
  writeLines(jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE),
             file.path(root, name, "manifest.json"))
  invisible(root)
}

.stimRoot <- function() {
  root <- file.path(tempfile("stim"), "stimuli")
  .writeSet(root, "faces")
  .writeSet(root, "words", kind = "text")
  root
}

.frames <- function(stim = "<div>{{stimulus}}</div>") {
  addFrameToQCEframeList(trialType = "key", frameName = "show", stimulus = stim,
                         post_trial_gap = 0, choices = c("d", "k"))
}

test_that("a reference with no filter keeps every item and n defaults to the count", {
  ref <- buildQCEstimSetRef("faces", stimuliDir = .stimRoot())
  expect_equal(ref$stimSet, "faces")
  expect_equal(ref$version, 3)
  expect_equal(length(ref$items), 5)
  expect_equal(ref$n, 5)
})

test_that("equality and min/max filters select on attributes", {
  root <- .stimRoot()
  expect_equal(length(buildQCEstimSetRef("faces", where = list(gender = "f"),
                                          stimuliDir = root)$items), 3)
  ref <- buildQCEstimSetRef("faces", where = list(rating = list(min = 2, max = 4)),
                            stimuliDir = root)
  expect_equal(vapply(ref$items, function(it) it$id, ""), c("d002", "d003"))
  expect_equal(length(buildQCEstimSetRef("faces", where = list(rating = 3.5),
                                          stimuliDir = root)$items), 1)
})

test_that("n draws that many of the matching items", {
  ref <- buildQCEstimSetRef("faces", where = list(gender = "f"), n = 2,
                            stimuliDir = .stimRoot())
  expect_equal(ref$n, 2)
  expect_equal(length(ref$items), 3)
})

test_that("a missing set, an undeclared attribute, a wrong level and a large n are refused", {
  root <- .stimRoot()
  expect_error(buildQCEstimSetRef("nope", stimuliDir = root), "no stimulus set \"nope\"")
  expect_error(buildQCEstimSetRef("faces", where = list(age = 3), stimuliDir = root),
               "has no attribute \"age\"")
  expect_error(buildQCEstimSetRef("faces", where = list(gender = "x"), stimuliDir = root),
               "not one of its levels")
  expect_error(buildQCEstimSetRef("faces", where = list(gender = list(min = 1)),
                                  stimuliDir = root), "only a number attribute")
  expect_error(buildQCEstimSetRef("faces", where = list(gender = "f"), n = 4,
                                  stimuliDir = root), "only 3 items match")
  expect_error(buildQCEstimSetRef("faces", where = list(gender = "m", rating = list(min = 9)),
                                  stimuliDir = root), "no item")
})

test_that("an engine before 10 is refused with a plain message", {
  expect_error(buildQCEstimSetRef("faces", stimuliDir = .stimRoot(), engineVersion = "9.1"),
               "engine 10.0 or later")
})

test_that("a files set expands into one scenario per item with its columns and the endpoint path", {
  ref <- buildQCEstimSetRef("faces", where = list(gender = "f"), stimuliDir = .stimRoot())
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames(), createFeedbackList(), "faceSet",
                                    list(Task = "rate"))
  expect_equal(length(sc), 3)
  ov <- sc[[1]]$outputVariables
  expect_equal(ov$stimSet, "faces")
  expect_equal(ov$stimSetVersion, "3")
  expect_equal(ov$stimId, "d001")
  expect_equal(ov$stim_rating, "1.5")
  expect_equal(ov$stim_gender, "f")
  expect_equal(ov$Task, "rate")
  expect_equal(sc[[1]]$set, "faceSet")
  expect_equal(sc[[2]]$frame[[1]]$stimulus,
               "<div><img src=\"stimFile.php?set=faces&amp;id=d003&amp;v=3\" alt=\"\"></div>")
})

test_that("a text set expands into escaped text, and a missing attribute is an empty column", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames("<p>{{stimulus}}</p>"),
                                    createFeedbackList(), "wordSet")
  expect_equal(sc[[1]]$frame[[1]]$stimulus, "<p>a &amp; b</p>")
  expect_equal(sc[[2]]$frame[[1]]$stimulus, "<p>&lt;tag&gt;</p>")
  expect_equal(sc[[4]]$frame[[1]]$stimulus, "<p>quote &quot;x&quot;</p>")
  expect_equal(sc[[4]]$outputVariables$stim_frequency, "")
})

test_that("the stimulusUrl placeholder gives the path alone, for a files set only", {
  root <- .stimRoot()
  ref <- buildQCEstimSetRef("faces", stimuliDir = root)
  sc <- addStimSetToQCEscenarioList(NULL, ref,
                                    .frames("<img class='x' src='{{stimulusUrl}}'>"),
                                    createFeedbackList(), "s")
  expect_equal(sc[[1]]$frame[[1]]$stimulus,
               "<img class='x' src='stimFile.php?set=faces&amp;id=d001&amp;v=3'>")
  words <- buildQCEstimSetRef("words", stimuliDir = root)
  expect_error(addStimSetToQCEscenarioList(NULL, words, .frames("{{stimulusUrl}}"),
                                           createFeedbackList(), "s"), "text set")
})

test_that("frames without a placeholder, a clashing column and a bad reference are refused", {
  ref <- buildQCEstimSetRef("faces", stimuliDir = .stimRoot())
  expect_error(addStimSetToQCEscenarioList(NULL, ref, .frames("<p>none</p>"),
                                           createFeedbackList(), "s"), "placeholder")
  expect_error(addStimSetToQCEscenarioList(NULL, ref, .frames(), createFeedbackList(), "s",
                                           list(stimId = "x")), "stimId")
  expect_error(addStimSetToQCEscenarioList(NULL, list(stimSet = "faces"), .frames(),
                                           createFeedbackList(), "s"), "buildQCEstimSetRef")
})

test_that("scenarios append to an existing list", {
  ref <- buildQCEstimSetRef("faces", stimuliDir = .stimRoot())
  first <- addScenarioToQCEscenarioList(NULL, .frames("<p>x</p>"), createFeedbackList(),
                                        list(A = "1"), "other")
  sc <- addStimSetToQCEscenarioList(first, ref, .frames(), createFeedbackList(), "s")
  expect_equal(names(sc), as.character(1:6))
  expect_equal(sc[[6]]$outputVariables$stimId, "d005")
})

test_that("savePreloadFiles adds a set's files by media type and leaves text sets out", {
  root <- .stimRoot()
  old <- setwd(tempdir())
  on.exit(setwd(old))
  faces <- buildQCEstimSetRef("faces", where = list(gender = "m"), stimuliDir = root)
  words <- buildQCEstimSetRef("words", stimuliDir = root)
  savePreloadFiles(imageFileArray = c("img/a.png"), stimSets = list(faces, words))
  pr <- jsonlite::fromJSON("preloadFile.json")
  expect_equal(pr$images, c("img/a.png", "stimFile.php?set=faces&id=d002&v=3",
                            "stimFile.php?set=faces&id=d004&v=3"))
  savePreloadFiles(stimSets = faces)
  expect_equal(length(jsonlite::fromJSON("preloadFile.json")$images), 2)
})

test_that("an empty preload list is written as an empty array, which the engine can count", {
  old <- setwd(tempdir())
  on.exit(setwd(old))
  savePreloadFiles(imageFileArray = c("img/a.png"))
  txt <- paste(readLines("preloadFile.json"), collapse = "")
  expect_match(txt, "\"video\": \\[\\]")
  expect_match(txt, "\"audio\": \\[\\]")
  savePreloadFiles()
  pr <- jsonlite::fromJSON("preloadFile.json", simplifyVector = FALSE)
  expect_equal(lengths(pr), c(images = 0L, video = 0L, audio = 0L))
})

test_that("an attribute value holding a tab or line break is refused before it reaches the data", {
  root <- .stimRoot()
  ref <- buildQCEstimSetRef("faces", stimuliDir = root)
  ref$items[[2]]$attrs$gender <- "f\tm"
  expect_error(addStimSetToQCEscenarioList(NULL, ref, .frames(), createFeedbackList(), "s"),
               "item d002: \"gender\" holds a tab, line break or other control character")
  ref <- buildQCEstimSetRef("words", stimuliDir = root)
  ref$items[[1]]$text <- "two\nlines"
  expect_error(addStimSetToQCEscenarioList(NULL, ref, .frames(), createFeedbackList(), "s"),
               "item w001: the text holds a tab, line break or other control character")
})

test_that("number attributes are written in full without scientific notation and filter numerically", {
  root <- file.path(tempfile("stim"), "stimuli")
  dir.create(file.path(root, "nums"), recursive = TRUE)
  vals <- c(0.0001, 100000, 3e9, 1e-7, 4.2)
  txt <- paste0('{"manifestVersion":1,"set":{"name":"nums","version":1,"kind":"text",',
                '"lockedAt":"2026-01-01T00:00:00Z","contentHash":"sha256:x",',
                '"source":{"description":"t","basis":"own","identifiablePeople":false}},',
                '"attributes":[{"name":"frequency","type":"number"}],"items":[',
                '{"id":"a","text":"a","attrs":{"frequency":0.0001}},',
                '{"id":"b","text":"b","attrs":{"frequency":100000}},',
                '{"id":"c","text":"c","attrs":{"frequency":3000000000}},',
                '{"id":"d","text":"d","attrs":{"frequency":1e-7}},',
                '{"id":"e","text":"e","attrs":{"frequency":4.2}}]}')
  writeLines(txt, file.path(root, "nums", "manifest.json"))
  ref <- buildQCEstimSetRef("nums", stimuliDir = root)
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames(), createFeedbackList(), "s")
  got <- vapply(sc, function(s) s$outputVariables$stim_frequency, "")
  expect_equal(unname(got), c("0.0001", "100000", "3000000000", "0.0000001", "4.2"))
  expect_false(any(grepl("e", got, fixed = TRUE)))
  for (i in seq_along(vals)) {
    hit <- buildQCEstimSetRef("nums", where = list(frequency = vals[i]), stimuliDir = root)
    expect_equal(hit$items[[1]]$id, letters[i])
  }
  expect_equal(buildQCEstimSetRef("nums", where = list(frequency = 100000L),
                                  stimuliDir = root)$items[[1]]$id, "b")
})

test_that("a missing bound is refused with the function's own message", {
  root <- .stimRoot()
  expect_error(buildQCEstimSetRef("faces", where = list(rating = list(min = NA_real_)),
                                  stimuliDir = root), "must be list\\(min = <number>")
  expect_error(buildQCEstimSetRef("faces", where = list(rating = list(max = NA)),
                                  stimuliDir = root), "must be list\\(min = <number>")
})
