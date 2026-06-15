# Tests for the QCEB trialType registry (mirrors engine trialTypeRegistry.js).

test_that("core types and survey are pre-registered", {
    expect_true(isRegisteredQCEBtrialType("key"))
    expect_true(isRegisteredQCEBtrialType("textbox"))
    expect_true(isRegisteredQCEBtrialType("numberline"))
    expect_true(isRegisteredQCEBtrialType("angleline"))
    expect_true(isRegisteredQCEBtrialType("survey"))
})

test_that("getRegisteredQCEBtrialTypes returns the seeded set sorted", {
    types <- getRegisteredQCEBtrialTypes()
    expect_true(all(c("angleline", "key", "numberline", "survey", "textbox") %in% types))
})

test_that("unknown type is not registered", {
    expect_false(isRegisteredQCEBtrialType("cyberball_unreg"))
    expect_false(isRegisteredQCEBtrialType("bogus"))
})

test_that("registering a custom type makes it valid", {
    nm <- "customPluginTest1"
    # guard against re-run pollution within a single session
    if (!isRegisteredQCEBtrialType(nm)) registerQCEBtrialType(nm, requiresKeymap = FALSE)
    expect_true(isRegisteredQCEBtrialType(nm))
    expect_true(nm %in% getRegisteredQCEBtrialTypes())
})

test_that("double-registration of the same name throws", {
    nm <- "customPluginTest2"
    if (!isRegisteredQCEBtrialType(nm)) registerQCEBtrialType(nm)
    expect_error(registerQCEBtrialType(nm), "already registered")
})

test_that("registering with an invalid name throws", {
    expect_error(registerQCEBtrialType(""), "non-empty single string")
    expect_error(registerQCEBtrialType(c("a", "b")), "non-empty single string")
})

test_that("isRegisteredQCEBtrialType is robust to non-string input", {
    expect_false(isRegisteredQCEBtrialType(NULL))
    expect_false(isRegisteredQCEBtrialType(42))
})
