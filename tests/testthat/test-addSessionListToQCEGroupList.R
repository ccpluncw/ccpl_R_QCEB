#tests for nPerBlock, the per-group share of one server-side assignment block

test_that("a group that states no share emits no nPerBlock key", {
  g <- addSessionListToQCEGroupList(NULL, list(a = 1), "grp")
  expect_false("nPerBlock" %in% names(g[["1"]]))
  expect_null(g[["1"]]$nPerBlock)
  expect_equal(names(g[["1"]]), c("sessions", "groupName"))
})

test_that("a stated share is carried through verbatim", {
  g <- addSessionListToQCEGroupList(NULL, list(a = 1), "grp", nPerBlock = 60)
  expect_equal(g[["1"]]$nPerBlock, 60)
})

test_that("a share sits alongside pages and cards", {
  g <- addSessionListToQCEGroupList(NULL, list(a = 1), "grp",
                                    pages = "pagesA.json", cards = "cards1.json",
                                    nPerBlock = 2)
  expect_equal(g[["1"]]$pages, "pagesA.json")
  expect_equal(g[["1"]]$cards, "cards1.json")
  expect_equal(g[["1"]]$nPerBlock, 2)
})

test_that("each group keeps its own share", {
  g <- addSessionListToQCEGroupList(NULL, list(a = 1), "low", nPerBlock = 2)
  g <- addSessionListToQCEGroupList(g, list(a = 2), "high", nPerBlock = 1)
  expect_equal(g[["1"]]$nPerBlock, 2)
  expect_equal(g[["2"]]$nPerBlock, 1)
})

test_that("nPerBlock must be a single positive whole number", {
  mk <- function(n) addSessionListToQCEGroupList(NULL, list(a = 1), "grp", nPerBlock = n)
  #a zero or negative share cannot be a denominator in the fill fraction
  expect_error(mk(0), "nPerBlock option")
  expect_error(mk(-1), "nPerBlock option")
  expect_error(mk(1.5), "nPerBlock option")
  expect_error(mk("60"), "nPerBlock option")
  expect_error(mk(c(1, 2)), "nPerBlock option")
  expect_error(mk(NA), "nPerBlock option")
  expect_error(mk(NA_real_), "nPerBlock option")
  expect_error(mk(Inf), "nPerBlock option")
  expect_error(mk(TRUE), "nPerBlock option")
})

test_that("a whole number stored as a double is accepted", {
  expect_equal(addSessionListToQCEGroupList(NULL, list(a = 1), "grp",
                                            nPerBlock = 3)$`1`$nPerBlock, 3)
  expect_equal(addSessionListToQCEGroupList(NULL, list(a = 1), "grp",
                                            nPerBlock = 3L)$`1`$nPerBlock, 3L)
})

test_that("nPerBlock survives the write as a one-element array", {
  #the server reads the wrapped form saveJsonFile produces
  d <- file.path(tempdir(), "npbtest"); dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "expInfo.json")
  g <- addSessionListToQCEGroupList(NULL, list(a = 1), "grp", nPerBlock = 60)
  saveJsonFile(g, f)
  back <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_equal(length(back[["1"]]$nPerBlock), 1)
  expect_equal(back[["1"]]$nPerBlock[[1]], 60)
  expect_equal(back[["1"]]$groupName[[1]], "grp")
})
