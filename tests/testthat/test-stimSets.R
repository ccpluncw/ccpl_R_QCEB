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

#the opening of a marked element as the package writes it
.mk <- function(key) "<span dir='auto' style='white-space:pre-wrap'>"

test_that("a text set expands into marked, escaped text, and a missing attribute is an empty column", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames("<p>{{stimulus}}</p>"),
                                    createFeedbackList(), "wordSet")
  expect_equal(sc[[1]]$frame[[1]]$stimulus, paste0("<p>", .mk("words:w001"), "a &amp; b</span></p>"))
  expect_equal(sc[[2]]$frame[[1]]$stimulus, paste0("<p>", .mk("words:w002"), "&lt;tag&gt;</span></p>"))
  expect_equal(sc[[4]]$frame[[1]]$stimulus,
               paste0("<p>", .mk("words:w004"), "quote &quot;x&quot;</span></p>"))
  expect_equal(sc[[4]]$outputVariables$stim_frequency, "")
})

test_that("an item's own opening bracket is written so it never reads as a placeholder", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  ref$items[[1]]$text <- "the pair \u27e6ab:cd\u27e7"
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames("{{stimulus}}"), createFeedbackList(), "s")
  expect_equal(sc[[1]]$frame[[1]]$stimulus, paste0(.mk("words:w001"), "the pair &#10214;ab:cd\u27e7</span>"))
})

test_that("a missing set's error names only the directories that are sets", {
  root <- .stimRoot()
  dir.create(file.path(root, "notes"))
  dir.create(file.path(root, "qcep-private-copy"))
  err <- tryCatch(buildQCEstimSetRef("absent", stimuliDir = root), error = function(e) conditionMessage(e))
  expect_match(err, "The study carries: faces, words.", fixed = TRUE)
  expect_false(grepl("notes|qcep-private", err))
})

test_that("a text item's length is its chars, from the manifest when it gives one", {
  root <- .stimRoot()
  ref <- buildQCEstimSetRef("words", stimuliDir = root)
  expect_equal(vapply(ref$items, function(it) it$chars, 1L), c(5L, 5L, 5L, 9L))
  f <- file.path(root, "words", "manifest.json")
  doc <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  doc$items[[1]]$text <- "\u27e6words:w001\u27e7"
  doc$items[[1]]$chars <- 42
  writeLines(jsonlite::toJSON(doc, auto_unbox = TRUE), f)
  expect_equal(buildQCEstimSetRef("words", stimuliDir = root)$items[[1]]$chars, 42L)
})

test_that("a token read from a stripped manifest is written into the marked element as it is", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  ref$items[[1]]$text <- "\u27e6words:w001\u27e7"
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames("<p>{{stimulus}}</p>"),
                                    createFeedbackList(), "wordSet")
  expect_equal(sc[[1]]$frame[[1]]$stimulus,
               paste0("<p>", "\u27e6words:w001\u27e7", "</p>"))
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
  ref$items[[1]]$text <- "bell\u0007"
  expect_error(addStimSetToQCEscenarioList(NULL, ref, .frames(), createFeedbackList(), "s"),
               "item w001: the text holds a control character other than a line break or a tab")
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

test_that("a set with no attributes expands with only the set columns", {
  root <- file.path(tempfile("stim"), "stimuli")
  dir.create(file.path(root, "plain"), recursive = TRUE)
  writeLines(paste0('{"manifestVersion":1,"set":{"name":"plain","version":2,"kind":"text",',
                    '"lockedAt":"2026-01-01T00:00:00Z","contentHash":"sha256:x",',
                    '"source":{"description":"t","basis":"own","identifiablePeople":false}},',
                    '"attributes":[],"items":[{"id":"a","text":"one"},{"id":"b","text":"two"}]}'),
             file.path(root, "plain", "manifest.json"))
  ref <- buildQCEstimSetRef("plain", stimuliDir = root)
  expect_equal(length(ref$attributes), 0)
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames(), createFeedbackList(), "s")
  expect_equal(length(sc), 2)
  expect_equal(names(sc[[1]]$outputVariables), c("stimSet", "stimSetVersion", "stimId"))
  expect_equal(sc[[2]]$outputVariables$stimId, "b")
  expect_error(buildQCEstimSetRef("plain", where = list(x = 1), stimuliDir = root),
               "its attributes are none")
})

test_that("a Unicode line or paragraph separator or a C1 control is refused like a tab", {
  root <- .stimRoot()
  for (bad in c("a b\u0085c", "x y", "x y", "c1\u009bz")) {
    ref <- buildQCEstimSetRef("faces", stimuliDir = root)
    ref$items[[1]]$attrs$gender <- bad
    expect_error(addStimSetToQCEscenarioList(NULL, ref, .frames(), createFeedbackList(), "s"),
                 "holds a tab, line break or other control character")
    ref <- buildQCEstimSetRef("words", stimuliDir = root)
    ref$items[[3]]$text <- bad
    expect_error(addStimSetToQCEscenarioList(NULL, ref, .frames(), createFeedbackList(), "s"),
                 "the text holds a control character other than a line break or a tab")
  }
})

test_that("an item inside a JSON-string stimulus keeps the JSON valid, quotes, backslashes and line breaks included", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  ref$items[[1]]$text <- "say \"hi\" to C:\\temp\nthen 'go'"
  stem <- "{\"stem\":\"<p>{{stimulus}}</p>\",\"options\":[\"yes\",\"no\"]}"
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames(stem), createFeedbackList(), "s")
  out <- sc[[1]]$frame[[1]]$stimulus
  expect_true(jsonlite::validate(out))
  expect_equal(jsonlite::fromJSON(out)$stem,
               paste0("<p>", .mk("words:w001"), "say &quot;hi&quot; to C:&#92;temp&#10;then &#39;go&#39;</span></p>"))
})

test_that("{{stimulus:<attribute>}} shows a value: a text value marked, a number as written", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  ref$attributes <- c(ref$attributes, list(list(name = "prime", type = "text")))
  ref$items[[1]]$attrs$prime <- "doctor's \"note\""
  ref$items[[2]]$attrs$prime <- "\u27e6words:w002:prime\u27e7"
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames("<p>{{stimulus:prime}}</p>{{stimulus}} ({{stimulus:frequency}})"),
                                    createFeedbackList(), "s")
  expect_equal(sc[[1]]$frame[[1]]$stimulus,
               paste0("<p>", .mk("words:w001:prime"), "doctor&#39;s &quot;note&quot;</span></p>",
                      .mk("words:w001"), "a &amp; b</span> (10)"))
  expect_equal(sc[[2]]$frame[[1]]$stimulus,
               paste0("<p>", "\u27e6words:w002:prime\u27e7", "</p>",
                      .mk("words:w002"), "&lt;tag&gt;</span> (20)"))
  expect_equal(sc[[4]]$frame[[1]]$stimulus,
               paste0("<p>", .mk("words:w004:prime"), "</span></p>", .mk("words:w004"), "quote &quot;x&quot;</span> ()"))
})

test_that("{{stimulus:<attribute>}} alone is a placeholder, and an unknown attribute is refused", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames("<p>{{stimulus:frequency}}</p>"), createFeedbackList(), "s")
  expect_equal(sc[[3]]$frame[[1]]$stimulus, "<p>30</p>")
  expect_error(addStimSetToQCEscenarioList(NULL, ref, .frames("{{stimulus}} {{stimulus:prime}}"), createFeedbackList(), "s"),
               "has no attribute \"prime\"; its attributes are frequency")
})

test_that("a text item keeps its indentation and runs of spaces", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  ref$items[[1]]$text <- "for (i in x) {\n    total <- total + i\n}"
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames("{{stimulus}}"), createFeedbackList(), "s")
  expect_equal(sc[[1]]$frame[[1]]$stimulus,
               paste0(.mk("words:w001"), "for (i in x) {&#10;    total &lt;- total + i&#10;}</span>"))
})

test_that("a text item's line breaks and tabs are written as markup", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  ref$items[[1]]$text <- "Alex found an error.\r\n\r\nAlex said nothing.\nThe end\there."
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames("<p>{{stimulus}}</p>"), createFeedbackList(), "s")
  expect_equal(sc[[1]]$frame[[1]]$stimulus,
               paste0("<p>", .mk("words:w001"), "Alex found an error.&#10;&#10;",
                      "Alex said nothing.&#10;The end&#9;here.</span></p>"))
})

test_that("a survey frame shows a text item and a text value as plain words inside its JSON model", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  ref$attributes <- c(ref$attributes, list(list(name = "gloss", type = "text")))
  ref$items[[1]]$text <- "I \"enjoy\" parties\\ <b>\nand ⟦x:y⟧"
  ref$items[[1]]$attrs$gloss <- "a 'gloss'"
  ref$items[[2]]$text <- "⟦words:w002⟧"
  model <- surveyModel(surveyPage("p1", surveyQuestion("radiogroup", "q", "{{stimulus}} ({{stimulus:gloss}})",
                                                       choices = c("{{stimulus}}", "No"))))
  fr <- addSurveyFrameToQCEframeList(NULL, model, frameName = "item")
  fr <- addFrameToQCEframeList(fr, trialType = "key", frameName = "shown", stimulus = "<p>{{stimulus}}</p>",
                               post_trial_gap = 0, choices = c("d"))
  sc <- addStimSetToQCEscenarioList(NULL, ref, fr, createFeedbackList(), "s")
  out <- sc[[1]]$frame[[1]]$stimulus
  expect_true(jsonlite::validate(out))
  q <- jsonlite::fromJSON(out, simplifyVector = FALSE)$pages[[1]]$elements[[1]]
  expect_equal(q$title, "I \"enjoy\" parties\\ <b>\nand ⟦x:y⟧ (a 'gloss')")
  expect_equal(q$choices[[1]], "I \"enjoy\" parties\\ <b>\nand ⟦x:y⟧")
  #the model's text never takes a placeholder's form
  expect_false(grepl("⟦", out, fixed = TRUE))
  expect_false(grepl("<span", out, fixed = TRUE))
  #a placeholder is written as it is, for the platform to fill
  expect_true(grepl("⟦words:w002⟧", sc[[2]]$frame[[1]]$stimulus, fixed = TRUE))
  #the other frame is marked as before
  expect_true(grepl("white-space:pre-wrap", sc[[1]]$frame[[2]]$stimulus, fixed = TRUE))
})

test_that("saveQCEstimSetList writes every item a hook needs: text, html, length and attributes", {
  root <- .stimRoot()
  ref <- buildQCEstimSetRef("words", stimuliDir = root, n = 2)
  ref$attributes <- c(ref$attributes, list(list(name = "gloss", type = "text")))
  ref$items[[1]]$attrs$gloss <- "a 'gloss'"
  ref$items[[2]]$text <- "⟦words:w002⟧"
  ref$items[[2]]$chars <- 5L
  dir <- tempfile("list")
  dir.create(dir)
  path <- saveQCEstimSetList(ref, "wordList.json", dir = dir)
  expect_equal(path, file.path(dir, "wordList.json"))
  doc <- jsonlite::read_json(path)
  expect_equal(doc$stimSetList, list(set = "words", version = "3", kind = "text"))
  expect_length(doc$items, 4)
  expect_equal(doc$items[[1]]$text, "a & b")
  expect_equal(doc$items[[1]]$chars, 5)
  expect_equal(doc$items[[1]]$html, paste0(.mk("words:w001"), "a &amp; b</span>"))
  expect_equal(doc$items[[1]]$attrs, list(frequency = 10, gloss = "a 'gloss'"))
  #a placeholder is written as it is, for the platform to fill
  expect_equal(doc$items[[2]]$text, "⟦words:w002⟧")
  expect_equal(doc$items[[2]]$chars, 5)
  expect_equal(doc$items[[2]]$html, "⟦words:w002⟧")
  expect_equal(doc$items[[4]]$attrs, setNames(list(), character(0)))
  saveQCEstimSetList(ref, "freq.json", attributes = "frequency", dir = dir)
  doc <- jsonlite::read_json(file.path(dir, "freq.json"))
  expect_equal(doc$items[[1]]$attrs, list(frequency = 10))
})

test_that("saveQCEstimSetList lists a files set by id and attributes, and refuses a bad name or attribute", {
  root <- .stimRoot()
  faces <- buildQCEstimSetRef("faces", stimuliDir = root)
  dir <- tempfile("list")
  dir.create(dir)
  doc <- jsonlite::read_json(saveQCEstimSetList(faces, "faces.json", attributes = "gender", dir = dir))
  expect_equal(doc$items[[1]], list(id = "d001", attrs = list(gender = "f")))
  expect_error(saveQCEstimSetList(faces, "faces.txt", dir = dir), "ending in .json")
  expect_error(saveQCEstimSetList(faces, "sub/faces.json", dir = dir), "with no directory")
  expect_error(saveQCEstimSetList(faces, "f.json", attributes = "age", dir = dir), "has no attribute \"age\"; its attributes are rating, gender")
  expect_error(saveQCEstimSetList(list(), "f.json"), "must be a reference")
})

test_that("a text item is escaped for where it stands in the frame, and refused in a script, a style or a comment", {
  ref <- buildQCEstimSetRef("words", stimuliDir = .stimRoot())
  ref$attributes <- c(ref$attributes, list(list(name = "gloss", type = "text")))
  ref$items[[1]]$text <- "it's <b>\"x\"</b>\nnext"
  ref$items[[1]]$attrs$gloss <- "two words"
  stim <- paste0("<p>{{stimulus}}</p><button data-word='{{stimulus:gloss}}' value={{stimulus:gloss}}>Pick</button>",
                 "<textarea>{{stimulus}}</textarea><svg><text>{{stimulus}}</text></svg>",
                 "<select><option>{{stimulus}}<option>other</select><title>{{stimulus}}</title>")
  sc <- addStimSetToQCEscenarioList(NULL, ref, .frames(stim), createFeedbackList(), "s")
  esc <- "it&#39;s &lt;b&gt;&quot;x&quot;&lt;/b&gt;&#10;next"
  expect_equal(sc[[1]]$frame[[1]]$stimulus,
               paste0("<p>", .mk(), esc, "</span></p><button data-word='two&#32;words' value=two&#32;words>Pick</button>",
                      "<textarea>", esc, "</textarea><svg><text>", esc, "</text></svg>",
                      "<select><option>", esc, "<option>other</select><title>", esc, "</title>"))
  for (bad in c("<script>var w = '{{stimulus}}';</script>", "<style>p::after{content:'{{stimulus}}'}</style>",
                "<!-- {{stimulus}} -->", "<p>ok</p><script type='text/template'>{{stimulus:gloss}}</script>",
                "<button onclick=\"pick('{{stimulus}}')\">x</button>", "<p style='content:{{stimulus}}'>x</p>",
                "<a href='javascript:go(\"{{stimulus}}\")'>x</a>", "<div {{stimulus}}>x</div>")) {
    expect_error(addStimSetToQCEscenarioList(NULL, ref, .frames(bad), createFeedbackList(), "s"),
                 "where a set's words are never written")
  }
})
