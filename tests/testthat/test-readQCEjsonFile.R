test_that("readQCEjsonFile round-trips a saved config without changing its shape", {
  lst <- addScenarioToQCEscenarioList(NULL, NULL, NULL, NULL, "setA")
  f <- tempfile(fileext = ".json")
  saveJsonFile(lst, f)
  before <- readLines(f, warn = FALSE)

  saveJsonFile(readQCEjsonFile(f), f)
  expect_identical(readLines(f, warn = FALSE), before)
})

test_that("a scalar is not re-boxed on the way back out", {
  #a boxed scalar must not be boxed twice, set "setA" stays one level deep
  lst <- addScenarioToQCEscenarioList(NULL, NULL, NULL, NULL, "setA")
  f <- tempfile(fileext = ".json")
  saveJsonFile(lst, f)
  saveJsonFile(readQCEjsonFile(f), f)

  txt <- paste(readLines(f, warn = FALSE), collapse = "")
  expect_false(grepl('\\[\\s*\\[\\s*"setA"', txt))
  expect_identical(jsonlite::fromJSON(f)[[1]]$set, "setA")
})

test_that("reloading, appending and saving keeps every set a one-element array", {
  a <- addScenarioToQCEscenarioList(NULL, NULL, NULL, NULL, "setA")
  f <- tempfile(fileext = ".json")
  saveJsonFile(a, f)

  back <- readQCEjsonFile(f)
  back <- addScenarioToQCEscenarioList(back, NULL, NULL, NULL, "setB")
  saveJsonFile(back, f)

  parsed <- jsonlite::fromJSON(f)
  sets <- vapply(parsed, function(s) s$set, character(1))
  expect_setequal(sets, c("setA", "setB"))
})

test_that("a deliberately unboxed value stays unboxed", {
  #choices is unboxed on purpose, so the reader keeps the distinction
  f <- tempfile(fileext = ".json")
  writeLines('{\n  "a": {\n    "choices": "NO_KEYS",\n    "set": ["setA"]\n  }\n}', f)
  saveJsonFile(readQCEjsonFile(f), f)

  txt <- paste(readLines(f, warn = FALSE), collapse = "")
  expect_true(grepl('"choices":\\s*"NO_KEYS"', txt))
  expect_false(grepl('"choices":\\s*\\["NO_KEYS"\\]', txt))
  #one value, bare
  expect_true(grepl('"set":\\s*"setA"', txt))
})

test_that("multi-element arrays, nested objects and empties survive", {
  f <- tempfile(fileext = ".json")
  writeLines(paste0('{"keys": ["y", "Y", "n"], "nested": {"deep": ["x"]},',
                    ' "emptyArr": [], "emptyObj": {}}'), f)
  got <- readQCEjsonFile(f)

  expect_identical(as.character(got$keys), c("y", "Y", "n"))
  expect_identical(as.character(got$nested$deep), "x")
  expect_length(got$emptyArr, 0L)
  expect_length(got$emptyObj, 0L)

  out <- tempfile(fileext = ".json")
  expect_warning(saveJsonFile(got, out), "not a settings file")
  txt <- paste(readLines(out, warn = FALSE), collapse = "")
  expect_true(grepl('"keys":\\s*\\[\\s*"y",\\s*"Y",\\s*"n"\\s*\\]', txt))
  expect_true(grepl('"emptyArr":\\s*\\[\\]', txt))
  expect_true(grepl('"emptyObj":\\s*\\{\\}', txt))
})

test_that("an entry the study named with nothing set in it survives the round trip", {
  f <- tempfile(fileext = ".json")
  writeLines(c('{', '  "1": {', '    "set": "A",', '    "feedback": {', '      "feedback_key": {',
               '        "Same": {},', '        "Different": {', '          "outputText": "no"',
               '        }', '      },', '      "showFeedback": false', '    }', '  }', '}'), f)
  before <- readLines(f, warn = FALSE)
  saveJsonFile(readQCEjsonFile(f), f, fileKind = "stimulus")
  expect_identical(readLines(f, warn = FALSE), before)
  expect_identical(jsonlite::fromJSON(f, simplifyVector = FALSE)[["1"]]$feedback$feedback_key$Same,
                   structure(list(), names = character(0)))
})

test_that("readQCEjsonFile validates its argument", {
  expect_error(readQCEjsonFile(c("a", "b")), "single string")
  expect_error(readQCEjsonFile(file.path(tempdir(), "definitely-not-here.json")), "cannot find")
})
