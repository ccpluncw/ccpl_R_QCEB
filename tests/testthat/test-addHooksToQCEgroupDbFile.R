# Tests for addHooksToQCEgroupDbFile + the customHooksFile / customHooksStateKeys
# path through buildQCEgroupDbFile (Phase 5).

mkDb <- function() buildQCEgroupDbFile(condName = "hooksTest")

test_that("declares customHooksFile on the dbfile", {
    db <- addHooksToQCEgroupDbFile(mkDb(), "customHooks.js")
    expect_equal(db$customHooksFile, "customHooks.js")
    expect_null(db$customHooksStateKeys)
})

test_that("records customHooksStateKeys when supplied", {
    db <- addHooksToQCEgroupDbFile(mkDb(), "customHooks.js",
                                   customHooksStateKeys = c("a", "b"))
    expect_equal(db$customHooksStateKeys, c("a", "b"))
})

test_that("rejects a non-.js filename", {
    expect_error(addHooksToQCEgroupDbFile(mkDb(), "customHooks.txt"),
                 "ends ")
})

test_that("rejects empty filename", {
    expect_error(addHooksToQCEgroupDbFile(mkDb(), ""),
                 "non-empty filename")
})

test_that("rejects non-list QCEdbfile", {
    expect_error(addHooksToQCEgroupDbFile("notadbfile", "customHooks.js"),
                 "QCEdbfile")
})

test_that("rejects stateKeys with an empty-string entry", {
    expect_error(addHooksToQCEgroupDbFile(mkDb(), "customHooks.js",
                                          customHooksStateKeys = c("a", "")),
                 "non-empty state-key names")
})

test_that("de-duplicates stateKeys with a warning", {
    expect_warning(db <- addHooksToQCEgroupDbFile(mkDb(), "customHooks.js",
                                                  customHooksStateKeys = c("a", "a", "b")),
                   "duplicate")
    expect_equal(db$customHooksStateKeys, c("a", "b"))
})

test_that("re-declaring the hooks file warns and overwrites", {
    db <- addHooksToQCEgroupDbFile(mkDb(), "first.js")
    expect_warning(db <- addHooksToQCEgroupDbFile(db, "second.js"),
                   "already declared")
    expect_equal(db$customHooksFile, "second.js")
})

# --- one-shot path through buildQCEgroupDbFile -------------------------------

test_that("buildQCEgroupDbFile one-shot sets the hooks fields", {
    db <- buildQCEgroupDbFile(condName = "x", customHooksFile = "customHooks.js",
                              customHooksStateKeys = c("score"))
    expect_equal(db$customHooksFile, "customHooks.js")
    expect_equal(db$customHooksStateKeys, "score")
})

test_that("no hooks file -> no hooks keys in the dbfile (legacy byte-identical)", {
    db <- buildQCEgroupDbFile(condName = "x")
    expect_null(db$customHooksFile)
    expect_null(db$customHooksStateKeys)
    expect_false("customHooksFile" %in% names(db))
    expect_false("customHooksStateKeys" %in% names(db))
})

test_that("stateKeys without a hooks file warns and is ignored", {
    expect_warning(db <- buildQCEgroupDbFile(condName = "x",
                                             customHooksStateKeys = c("score")),
                   "without customHooksFile")
    expect_null(db$customHooksStateKeys)
})

test_that("JSON round-trip preserves the hooks fields", {
    db <- buildQCEgroupDbFile(condName = "x", customHooksFile = "customHooks.js",
                              customHooksStateKeys = c("a", "b"))
    json <- jsonlite::toJSON(db, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_equal(parsed$customHooksFile[[1]], "customHooks.js")
    expect_equal(vapply(parsed$customHooksStateKeys, function(x) x[[1]], character(1)),
                 c("a", "b"))
})
