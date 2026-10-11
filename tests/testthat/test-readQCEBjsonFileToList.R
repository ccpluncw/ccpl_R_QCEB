test_that("readQCEBjsonFileToList reads a plain JSON file", {
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f))
  writeLines(c("{", '  "1": {', '    "set": "practice",', '    "stimRef": "a1"', "  }", "}"), f)
  got <- readQCEBjsonFileToList(f)
  expect_identical(got[["1"]]$set, "practice")
  expect_identical(got[["1"]]$stimRef, "a1")
})

test_that("readQCEBjsonFileToList reads a file that declares a variable on its first line", {
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f))
  writeLines(c("var scenarios =", "{", '  "1": {', '    "set": ["practice"]', "  }", "}"), f)
  got <- readQCEBjsonFileToList(f)
  expect_identical(got[["1"]]$set, "practice")
})

test_that("readQCEBjsonFileToList reads a file that declares a variable and its JSON on one line", {
  f <- tempfile(fileext = ".js")
  on.exit(unlink(f))
  writeLines('var scenarios = {"1": {"set": "practice", "stimRef": "a1"}};', f)
  got <- readQCEBjsonFileToList(f)
  expect_identical(got[["1"]]$set, "practice")
  expect_identical(got[["1"]]$stimRef, "a1")
})

test_that("readQCEBjsonFileToList reads a file QCEB writes", {
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f))
  saveJsonFile(list(expName = "demo", saveDataEveryNTrials = 20L), f)
  got <- readQCEBjsonFileToList(f)
  expect_identical(got$expName, "demo")
  expect_equal(got$saveDataEveryNTrials, 20)
})
