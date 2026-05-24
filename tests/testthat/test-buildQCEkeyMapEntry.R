# Tests for buildQCEkeyMapEntry -- map validation, optional-fields, JSON shape.

test_that("minimal entry: map only -- defaults randomize=FALSE, no optional fields", {
    km <- buildKeyMap(data.frame(Yes = "y", No = "n"))
    e <- buildQCEkeyMapEntry(map = km)
    expect_equal(e$map, km)
    expect_equal(e$randomize, FALSE)
    expect_null(e$presentAfterTrials)
    expect_null(e$instructionFile)
})

test_that("full entry: map + randomize + presentAfterTrials + instructionFile", {
    km <- buildKeyMap(data.frame(Yes = "y", No = "n"))
    e <- buildQCEkeyMapEntry(
        map = km, randomize = TRUE,
        presentAfterTrials = c(20, 50),
        instructionFile = "yesNo_keymap.html"
    )
    expect_equal(e$randomize, TRUE)
    expect_equal(e$presentAfterTrials, c(20, 50))
    expect_equal(e$instructionFile, "yesNo_keymap.html")
})

test_that("missing map throws", {
    expect_error(buildQCEkeyMapEntry(), "map option is required")
})

test_that("empty map throws", {
    expect_error(buildQCEkeyMapEntry(map = list()), "non-empty QCEkeyMap")
})

test_that("non-list map throws", {
    expect_error(buildQCEkeyMapEntry(map = "not-a-list"), "non-empty QCEkeyMap")
})

test_that("non-boolean randomize throws", {
    km <- buildKeyMap(data.frame(Yes = "y"))
    expect_error(buildQCEkeyMapEntry(map = km, randomize = "yes"),
                 "single boolean")
})

test_that("non-numeric presentAfterTrials throws", {
    km <- buildKeyMap(data.frame(Yes = "y"))
    expect_error(buildQCEkeyMapEntry(map = km, presentAfterTrials = "twenty"),
                 "numeric vector")
})

test_that("non-.html instructionFile throws", {
    km <- buildKeyMap(data.frame(Yes = "y"))
    expect_error(buildQCEkeyMapEntry(map = km, instructionFile = "yesNo.txt"),
                 "\\.html filename")
})

test_that("JSON round-trip preserves entry shape", {
    km <- buildKeyMap(data.frame(Yes = "y", No = "n"))
    e <- buildQCEkeyMapEntry(map = km, randomize = TRUE,
                              presentAfterTrials = c(10, 30),
                              instructionFile = "km.html")
    json <- jsonlite::toJSON(e, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_equal(parsed$map$Yes[[1]], "y")
    expect_equal(parsed$randomize[[1]], TRUE)
    expect_equal(parsed$presentAfterTrials, list(10, 30))
    expect_equal(parsed$instructionFile[[1]], "km.html")
})
