# Tests for buildQCEgroupDbFile -- restEveryNMinutes + restMaxTrial
# additions, validation, and JSON round-trip.

test_that("regression: dbfile without new params has no rest extension keys", {
    db <- buildQCEgroupDbFile(condName = "TestCond")
    expect_null(db$restEveryNMinutes)
    expect_null(db$restMaxTrial)
})

test_that("regression: legacy keys still present in pre-Phase-2 dbfile output", {
    db <- buildQCEgroupDbFile(condName = "TestCond")
    # Verify the core legacy keys still appear
    legacyKeys <- c("condName", "keyMap", "randomizeKeyMap",
                    "presentKeyMapAfterTrialNumbers", "defaultBackgroundColor",
                    "restTrials", "speedFeedbackParams", "instructionFile",
                    "keyMapInstructionFile", "restMsg", "friendlyReminderMsg",
                    "remindMsg", "proceedMsg")
    expect_true(all(legacyKeys %in% names(db)))
})

test_that("restEveryNMinutes only emits the key when set", {
    db <- buildQCEgroupDbFile(condName = "TestCond", restEveryNMinutes = 10)
    expect_equal(db$restEveryNMinutes, 10)
    expect_null(db$restMaxTrial)
})

test_that("restMaxTrial only emits the key when set, coerced to integer", {
    db <- buildQCEgroupDbFile(condName = "TestCond", restMaxTrial = 200)
    expect_equal(db$restMaxTrial, 200L)
    expect_true(is.integer(db$restMaxTrial))
    expect_null(db$restEveryNMinutes)
})

test_that("both rest extension params can be set together", {
    db <- buildQCEgroupDbFile(condName = "TestCond",
                              restEveryNMinutes = 5, restMaxTrial = 100)
    expect_equal(db$restEveryNMinutes, 5)
    expect_equal(db$restMaxTrial, 100L)
})

test_that("zero restEveryNMinutes throws", {
    expect_error(
        buildQCEgroupDbFile(condName = "TestCond", restEveryNMinutes = 0),
        "single positive number"
    )
})

test_that("negative restEveryNMinutes throws", {
    expect_error(
        buildQCEgroupDbFile(condName = "TestCond", restEveryNMinutes = -5),
        "single positive number"
    )
})

test_that("non-numeric restEveryNMinutes throws", {
    expect_error(
        buildQCEgroupDbFile(condName = "TestCond", restEveryNMinutes = "ten"),
        "single positive number"
    )
})

test_that("zero restMaxTrial throws", {
    expect_error(
        buildQCEgroupDbFile(condName = "TestCond", restMaxTrial = 0),
        "single positive integer"
    )
})

test_that("negative restMaxTrial throws", {
    expect_error(
        buildQCEgroupDbFile(condName = "TestCond", restMaxTrial = -10),
        "single positive integer"
    )
})

test_that("triggers block still emitted alongside rest extension params", {
    db <- buildQCEgroupDbFile(condName = "fNIRS_session",
                              enableTriggers = TRUE, triggerRelayPort = 5678,
                              restEveryNMinutes = 10, restMaxTrial = 100)
    expect_equal(db$triggers$enabled, TRUE)
    expect_equal(db$triggers$relayPort, 5678L)
    expect_equal(db$restEveryNMinutes, 10)
    expect_equal(db$restMaxTrial, 100L)
})

test_that("JSON round-trip preserves rest extension params", {
    db <- buildQCEgroupDbFile(condName = "LongSession",
                              restEveryNMinutes = 10, restMaxTrial = 200)
    json <- jsonlite::toJSON(db, auto_unbox = FALSE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    # jsonlite array-wraps scalars per QCEP convention
    expect_equal(parsed$restEveryNMinutes[[1]], 10)
    expect_equal(parsed$restMaxTrial[[1]], 200)
})


# --- Phase 3.5 Chunk G: keyMaps arg (one-shot construction) ---

test_that("regression: dbfile without keyMaps arg has no $keyMaps field", {
    db <- buildQCEgroupDbFile(condName = "x",
                               keyMap = buildKeyMap(data.frame(Yes = "y")))
    expect_null(db$keyMaps)
})

test_that("keyMaps arg: named list registers each entry", {
    km1 <- buildQCEkeyMapEntry(map = buildKeyMap(data.frame(Yes = "y", No = "n")))
    km2 <- buildQCEkeyMapEntry(map = buildKeyMap(data.frame(Left = "j", Right = "k")),
                                randomize = TRUE)
    db <- buildQCEgroupDbFile(condName = "x",
                               keyMap = buildKeyMap(data.frame(Yes = "y")),
                               keyMaps = list(yesNo = km1, directional = km2))
    expect_equal(names(db$keyMaps), c("yesNo", "directional"))
    expect_equal(db$keyMaps$yesNo$map$Yes, "y")
    expect_equal(db$keyMaps$directional$randomize, TRUE)
})

test_that("keyMaps arg: unnamed list throws", {
    km1 <- buildQCEkeyMapEntry(map = buildKeyMap(data.frame(Yes = "y")))
    expect_error(buildQCEgroupDbFile(condName = "x",
                                      keyMap = buildKeyMap(data.frame(Yes = "y")),
                                      keyMaps = list(km1)),
                 "named list")
})

test_that("incremental + one-shot interop: keyMaps from arg + addKeyMapToDbfile coexist", {
    km1 <- buildQCEkeyMapEntry(map = buildKeyMap(data.frame(Yes = "y")))
    km2 <- buildQCEkeyMapEntry(map = buildKeyMap(data.frame(Left = "j")))
    db <- buildQCEgroupDbFile(condName = "x",
                               keyMap = buildKeyMap(data.frame(Yes = "y")),
                               keyMaps = list(km1 = km1))
    db <- addKeyMapToDbfile(db, "km2", km2)
    expect_equal(names(db$keyMaps), c("km1", "km2"))
})
