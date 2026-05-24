# Tests for addKeyMapToDbfile -- incremental add, NULL-init, duplicate warning.

make_dbfile <- function() {
    buildQCEgroupDbFile(condName = "testCond",
                         keyMap = buildKeyMap(data.frame(Yes = "y", No = "n")))
}
make_entry <- function() {
    buildQCEkeyMapEntry(map = buildKeyMap(data.frame(Left = "j", Right = "k")))
}

test_that("first add initializes $keyMaps and registers entry", {
    db <- make_dbfile()
    expect_null(db$keyMaps)
    db <- addKeyMapToDbfile(db, "directional", make_entry())
    expect_false(is.null(db$keyMaps))
    expect_equal(names(db$keyMaps), "directional")
    expect_equal(db$keyMaps$directional$map$Left, "j")
})

test_that("subsequent adds append without clobbering existing entries", {
    db <- make_dbfile()
    db <- addKeyMapToDbfile(db, "yesNo", buildQCEkeyMapEntry(map = buildKeyMap(data.frame(Yes = "y", No = "n"))))
    db <- addKeyMapToDbfile(db, "directional", make_entry())
    expect_equal(names(db$keyMaps), c("yesNo", "directional"))
})

test_that("duplicate name warns and overwrites", {
    db <- make_dbfile()
    db <- addKeyMapToDbfile(db, "x", make_entry())
    expect_warning(
        db <- addKeyMapToDbfile(db, "x", buildQCEkeyMapEntry(map = buildKeyMap(data.frame(Top = "q")))),
        "already defined on dbfile; overwriting"
    )
    expect_equal(names(db$keyMaps$x$map), "Top")
})

test_that("NULL dbfile throws", {
    expect_error(addKeyMapToDbfile(NULL, "x", make_entry()),
                 "must be a QCEdbfile")
})

test_that("empty name throws", {
    db <- make_dbfile()
    expect_error(addKeyMapToDbfile(db, "", make_entry()),
                 "non-empty single string")
})

test_that("non-string name throws", {
    db <- make_dbfile()
    expect_error(addKeyMapToDbfile(db, c("a", "b"), make_entry()),
                 "non-empty single string")
})

test_that("entry without map throws", {
    db <- make_dbfile()
    expect_error(addKeyMapToDbfile(db, "x", list(randomize = TRUE)),
                 "keyMap entry")
})

test_that("legacy dbfile (no addKeyMapToDbfile calls) has no keyMaps field in JSON", {
    db <- make_dbfile()
    json <- jsonlite::toJSON(db, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_null(parsed$keyMaps)
})
