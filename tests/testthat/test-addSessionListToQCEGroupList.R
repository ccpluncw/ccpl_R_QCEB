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
  expect_error(mk(-Inf), "nPerBlock option")
  expect_error(mk(NaN), "nPerBlock option")
  expect_error(mk(TRUE), "nPerBlock option")
})

test_that("a share past the integer range is refused without a coercion warning", {
  #wholeness is tested by rounding, so nothing is coerced and nothing warns
  mk <- function(n) addSessionListToQCEGroupList(NULL, list(a = 1), "grp", nPerBlock = n)
  expect_no_warning(expect_error(mk(3e9), "nPerBlock option"))
  expect_no_warning(expect_error(mk(.Machine$integer.max + 1), "nPerBlock option"))
  expect_equal(mk(.Machine$integer.max)$`1`$nPerBlock, .Machine$integer.max)
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


test_that("a group that disagrees with the list it joins is refused", {
  withShare <- addSessionListToQCEGroupList(NULL, list(a = 1), "low", nPerBlock = 2)
  without   <- addSessionListToQCEGroupList(NULL, list(a = 1), "low")
  #a share added to groups that have none
  expect_error(addSessionListToQCEGroupList(without, list(a = 2), "high", nPerBlock = 1),
               "every group of an experiment or on none")
  #a group with no share added to groups that have one
  expect_error(addSessionListToQCEGroupList(withShare, list(a = 2), "high"),
               "every group of an experiment or on none")
})

test_that("the refusal names the groups on each side", {
  g <- addSessionListToQCEGroupList(NULL, list(a = 1), "low", nPerBlock = 2)
  expect_error(addSessionListToQCEGroupList(g, list(a = 2), "high"),
               "Declared: low\\. Not declared: high\\.")
  h <- addSessionListToQCEGroupList(NULL, list(a = 1), "low")
  expect_error(addSessionListToQCEGroupList(h, list(a = 2), "high", nPerBlock = 3),
               "Declared: high\\. Not declared: low\\.")
})

test_that("a consistent list is never refused, either way", {
  none <- addSessionListToQCEGroupList(NULL, list(a = 1), "low")
  none <- addSessionListToQCEGroupList(none, list(a = 2), "high")
  expect_equal(length(none), 2)
  expect_false("nPerBlock" %in% names(none[["2"]]))

  all <- addSessionListToQCEGroupList(NULL, list(a = 1), "low", nPerBlock = 1)
  all <- addSessionListToQCEGroupList(all, list(a = 2), "high", nPerBlock = 1)
  expect_equal(all[["2"]]$nPerBlock, 1)
})

test_that("a three-group ratio round-trips through the written file", {
  d <- file.path(tempdir(), "npb3test"); dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "expInfo.json")
  g <- NULL
  for (i in seq_along(c("low", "mid", "high"))) {
    g <- addSessionListToQCEGroupList(g, list(a = i),
                                      c("low", "mid", "high")[i],
                                      nPerBlock = c(2, 1, 1)[i])
  }
  saveJsonFile(g, f)
  back <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_equal(length(back), 3)
  expect_equal(vapply(back, function(x) x$nPerBlock[[1]], numeric(1),
                      USE.NAMES = FALSE), c(2, 1, 1))
  expect_equal(vapply(back, function(x) x$groupName[[1]], character(1),
                      USE.NAMES = FALSE), c("low", "mid", "high"))
})
