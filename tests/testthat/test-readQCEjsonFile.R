test_that("readQCEjsonFile round-trips a saved config without changing its shape", {
  lst <- addScenarioToQCEscenarioList(NULL, NULL, NULL, NULL, "setA")
  f <- tempfile(fileext = ".json")
  saveJsonFile(lst, f)
  before <- readLines(f, warn = FALSE)

  saveJsonFile(readQCEjsonFile(f), f)
  expect_identical(readLines(f, warn = FALSE), before)
})

test_that("a scalar is not re-boxed on the way back out", {
  # The defect this closes: fromJSON() hands a boxed scalar back as a
  # one-element LIST, which toJSON() then boxes a second time, so "set":
  # ["setA"] becomes [["setA"]]. A set name nested one level too deep matches
  # no setInfo entry, and the first symptom is a session that runs no trials.
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
  # addFrameToQCEframeList unboxes `choices` on purpose, because the engine
  # expects a bare scalar there. Re-boxing it would silently change what the
  # engine is handed, so the reader has to preserve the distinction.
  f <- tempfile(fileext = ".json")
  writeLines('{\n  "a": {\n    "choices": "NO_KEYS",\n    "set": ["setA"]\n  }\n}', f)
  saveJsonFile(readQCEjsonFile(f), f)

  txt <- paste(readLines(f, warn = FALSE), collapse = "")
  expect_true(grepl('"choices":\\s*"NO_KEYS"', txt))     # still bare
  expect_false(grepl('"choices":\\s*\\["NO_KEYS"\\]', txt))
  expect_true(grepl('"set":\\s*\\["setA"\\]', txt))      # still boxed
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
  saveJsonFile(got, out)
  txt <- paste(readLines(out, warn = FALSE), collapse = "")
  expect_true(grepl('"keys":\\s*\\[\\s*"y",\\s*"Y",\\s*"n"\\s*\\]', txt))
  expect_true(grepl('"emptyArr":\\s*\\[\\]', txt))
  expect_true(grepl('"emptyObj":\\s*\\{\\}', txt))
})

test_that("readQCEjsonFile validates its argument", {
  expect_error(readQCEjsonFile(c("a", "b")), "single string")
  expect_error(readQCEjsonFile(file.path(tempdir(), "definitely-not-here.json")), "cannot find")
})
