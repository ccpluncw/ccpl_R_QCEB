test_that("saveQCEunplacedPages writes the pages as names without their extension", {
  d <- tempfile("unplaced")
  dir.create(d)
  p <- saveQCEunplacedPages(c("debrief_full.html", "debrief_short", "debrief_full"), dir = d)
  expect_equal(p, file.path(d, "unplacedPages.json"))
  doc <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  expect_equal(unlist(doc$pages), c("debrief_full", "debrief_short"))
})

test_that("a page in a subfolder is declared by its path, written with forward slashes", {
  d <- tempfile("unplaced")
  dir.create(d)
  p <- saveQCEunplacedPages(c("debriefs/full.html", "debriefs\\short"), dir = d)
  expect_equal(unlist(jsonlite::fromJSON(p, simplifyVector = FALSE)$pages), c("debriefs/full", "debriefs/short"))
})

test_that("saveQCEunplacedPages refuses an empty list and a bad directory", {
  expect_error(saveQCEunplacedPages(character(0)), "character vector of page names")
  expect_error(saveQCEunplacedPages(c("a", "")), "character vector of page names")
  expect_error(saveQCEunplacedPages("debrief", dir = c("a", "b")), "single string")
})
