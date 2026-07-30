newDir <- function() {
  d <- file.path(tempdir(), paste0("hookcols_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

km <- function() buildKeyMap(data.frame(Yes = "y", No = "n"))

test_that("customHooksColumns is emitted only when supplied", {
  plain <- buildQCEgroupDbFile(condName = "c", keyMap = km(),
                               customHooksFile = "h.js")
  expect_null(plain$customHooksColumns)

  withCols <- buildQCEgroupDbFile(condName = "c", keyMap = km(),
                                  customHooksFile = "h.js",
                                  customHooksColumns = c("accept", "offer"))
  expect_equal(withCols$customHooksColumns, c("accept", "offer"))
})

test_that("customHooksColumns is validated like the state keys", {
  db <- buildQCEgroupDbFile(condName = "c", keyMap = km())
  expect_error(addHooksToQCEgroupDbFile(db, "h.js", customHooksColumns = 7),
               "character vector")
  expect_error(addHooksToQCEgroupDbFile(db, "h.js", customHooksColumns = c("ok", "")),
               "non-empty column names")
  expect_error(addHooksToQCEgroupDbFile(db, "h.js", customHooksColumns = c("ok", NA)),
               "non-empty column names")
  expect_warning(addHooksToQCEgroupDbFile(db, "h.js", customHooksColumns = c("a", "a")),
                 "duplicate")
})

test_that("declared hook columns are checked against fields.txt", {
  # The gap this closes: a hook column exists only inside JavaScript, so without
  # a declaration nothing at build time can tell it is missing from the
  # whitelist -- and a column missing from the whitelist is dropped at save.
  d <- newDir()
  db <- buildQCEgroupDbFile(condName = "c", keyMap = km(),
                            customHooksFile = "h.js",
                            customHooksColumns = c("accept", "offer"))
  jsonlite::write_json(db, file.path(d, "dbfile.json"), auto_unbox = FALSE)

  fields <- file.path(d, "fields.txt")
  writeLines(c("Exp_Name", "sn", "accept"), fields)   # 'offer' deliberately absent

  gaps <- missingQCEoutputFields(d, fieldsFile = fields)
  expect_true("offer" %in% gaps)
  expect_false("accept" %in% gaps)
})

test_that("undeclared hook columns still produce the CHECK THIS warning", {
  d <- newDir()
  db <- buildQCEgroupDbFile(condName = "c", keyMap = km(), customHooksFile = "h.js")
  jsonlite::write_json(db, file.path(d, "dbfile.json"), auto_unbox = FALSE)

  rpt <- buildQCEoutputFieldManifest(d, outFile = NULL)
  expect_true(any(grepl("CHECK THIS! A hook can write any column", rpt, fixed = TRUE)))
  expect_true(any(grepl("customHooksColumns", rpt, fixed = TRUE)))
})

test_that("declared hook columns replace that warning with the list", {
  d <- newDir()
  db <- buildQCEgroupDbFile(condName = "c", keyMap = km(),
                            customHooksFile = "h.js",
                            customHooksColumns = c("accept", "offer"))
  jsonlite::write_json(db, file.path(d, "dbfile.json"), auto_unbox = FALSE)

  rpt <- buildQCEoutputFieldManifest(d, outFile = NULL)
  expect_false(any(grepl("CHECK THIS! A hook can write any column", rpt, fixed = TRUE)))
  expect_true(any(grepl("checked against fields.txt like the rest", rpt, fixed = TRUE)))
  expect_true(any(grepl("offer", rpt, fixed = TRUE)))
})
