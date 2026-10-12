test_that("splitListCell splits a cell on ; into its items", {
  expect_identical(splitListCell("p;q"), c("p", "q"))
  expect_identical(splitListCell("1;2.5;TRUE;FALSE;;t"), c("1", "2.5", "TRUE", "FALSE", "", "t"))
})

test_that("splitListCell keeps a \\; inside an item as ;", {
  expect_identical(splitListCell("a;b\\;c"), c("a", "b;c"))
  expect_identical(splitListCell("x;y\\;z"), c("x", "y;z"))
  expect_identical(splitListCell("a\\\\;b"), c("a\\", "b"))
})

test_that("splitListCell returns a cell with no ; as itself", {
  expect_identical(splitListCell("solo"), "solo")
  expect_identical(splitListCell("a\\;b"), "a;b")
  expect_identical(splitListCell('["x","y"]'), '["x","y"]')
})

test_that("splitListCell returns character(0) for an empty cell and NA for NA", {
  expect_identical(splitListCell(""), character(0))
  expect_identical(splitListCell(NA), NA_character_)
  expect_identical(splitListCell(NA_character_), NA_character_)
})

test_that("splitListCell keeps empty items at either end", {
  expect_identical(splitListCell(";"), c("", ""))
  expect_identical(splitListCell("a;"), c("a", ""))
  expect_identical(splitListCell(";a"), c("", "a"))
})

test_that("splitListCell splits a list of objects into JSON texts that read back whole", {
  items <- splitListCell('{"a":1,"b":"x\\;y"};{"a":2,"b":"w"};{}')
  expect_length(items, 3)
  expect_identical(jsonlite::fromJSON(items[1]), list(a = 1L, b = "x;y"))
  expect_identical(jsonlite::fromJSON(items[2]), list(a = 2L, b = "w"))
  expect_identical(items[3], "{}")
  nested <- splitListCell('["p","q\\;r"];s')
  expect_identical(jsonlite::fromJSON(nested[1]), c("p", "q;r"))
  expect_identical(nested[2], "s")
})

test_that("splitListCell takes a value read.delim has converted", {
  expect_identical(splitListCell(7L), "7")
  expect_identical(splitListCell(TRUE), "TRUE")
})

test_that("splitListCell stops on more than one cell", {
  expect_error(splitListCell(c("a", "b")), "one cell, not 2")
  expect_error(splitListCell(character(0)), "one cell, not 0")
})

test_that("splitListCell reads the cells a data file holds", {
  f <- tempfile(fileext = ".dat")
  on.exit(unlink(f))
  writeLines(c("sn\tlist\tone\tnone\tflag\tnum",
               "1\ta;b\\;c\tsolo\t\tTRUE\t1234.5678901234567",
               "2\tp;q\tx\t\tFALSE\t0.30000000000000004"), f)
  dat <- read.delim(f, stringsAsFactors = FALSE)
  expect_identical(lapply(dat$list, splitListCell), list(c("a", "b;c"), c("p", "q")))
  expect_identical(splitListCell(dat$one[1]), "solo")
  expect_identical(dat$flag, c(TRUE, FALSE))
  expect_identical(dat$num[1], 1234.5678901234567)
})

test_that("splitListCell reads back an item that holds or ends in a backslash", {
  #each cell as the data writer saves the list, each item as it was sent
  expect_identical(splitListCell("a\\\\;b"), c("a\\", "b"))
  expect_identical(splitListCell("C:\\\\dir\\\\;x"), c("C:\\dir\\", "x"))
  expect_identical(splitListCell("\\\\\\;;y"), c("\\;", "y"))
  expect_identical(splitListCell("\\\\\\\\;z"), c("\\\\", "z"))
  expect_identical(splitListCell("\\\\;w"), c("\\", "w"))
  expect_identical(splitListCell("w;\\\\"), c("w", "\\"))
  expect_identical(splitListCell("x\\ty\\\\;z"), c("x\\ty\\", "z"))
  expect_identical(splitListCell("x\\\\t;z\\n"), c("x\\t", "z\\n"))
  expect_identical(splitListCell("a\\\\\\;b\\\\\\\\\\;c;d\\;\\\\"), c("a\\;b\\\\;c", "d;\\"))
  expect_identical(splitListCell("{\"a\":1};{\"b\":\"x\\;y\\\\\\\\\"}"), c("{\"a\":1}", "{\"b\":\"x;y\\\\\"}"))
  expect_identical(splitListCell(";;\\\\"), c("", "", "\\"))
  expect_identical(splitListCell("1;2.5;TRUE;FALSE;;t\\\\"), c("1", "2.5", "TRUE", "FALSE", "", "t\\"))
  expect_identical(splitListCell("é\\;x\\\\;漢"), c("é;x\\", "漢"))
})

test_that("splitListCell reads a JSON item holding a backslash as JSON that decodes", {
  items <- splitListCell("{\"p\":\"C:\\\\\\\\\"};q")
  expect_identical(jsonlite::fromJSON(items[1]), list(p = "C:\\"))
  expect_identical(items[2], "q")
  nested <- splitListCell("[\"a\\\\tb\",\"c\"];2")
  expect_identical(jsonlite::fromJSON(nested[1]), c("a\tb", "c"))
  expect_identical(nested[2], "2")
})

test_that("splitListCell returns a bare or one-item cell holding a backslash as it is", {
  expect_identical(splitListCell("C:\\dir\\"), "C:\\dir\\")
  expect_identical(splitListCell("\\"), "\\")
})

test_that("splitListCell reads a list column read as text, and what read.delim makes of one it converted", {
  f <- tempfile(fileext = ".dat")
  on.exit(unlink(f))
  writeLines(c("sn\tnone\tnum", "1\t\t1234.5678901234567"), f)
  txt <- read.delim(f, colClasses = "character")
  expect_identical(splitListCell(txt$none[1]), character(0))
  expect_identical(splitListCell(txt$num[1]), "1234.5678901234567")
  conv <- read.delim(f)
  expect_identical(splitListCell(conv$none[1]), NA_character_)
  expect_identical(splitListCell(conv$num[1]), "1234.56789012346")
})
