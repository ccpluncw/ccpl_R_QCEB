#tests for saveCustomHooksTemplate, written into a temp dir

test_that("writes a .js file that defines QCEPHooks with all five hooks", {
    tmp <- withr::local_tempdir()
    old <- setwd(tmp); on.exit(setwd(old), add = TRUE)

    fn <- saveCustomHooksTemplate("customHooks.js")
    expect_equal(fn, "customHooks.js")
    expect_true(file.exists("customHooks.js"))

    txt <- paste(readLines("customHooks.js"), collapse = "\n")
    expect_true(grepl("var QCEPHooks", txt, fixed = TRUE))
    for (h in c("onTrialStart", "onTrialFinish", "onSetEnd", "onBlockEnd", "onSessionEnd")) {
        expect_true(grepl(h, txt, fixed = TRUE), info = h)
    }
    #documents the hook to state to showIf channel and ctx fields
    expect_true(grepl("qceState.custom", txt, fixed = TRUE))
    expect_true(grepl("ctx.scenarios", txt, fixed = TRUE))
})

test_that("rejects a non-.js filename", {
    expect_error(saveCustomHooksTemplate("hooks.txt"), "ends in '.js'")
})

test_that("the written template is syntactically valid JavaScript", {
    skip_if(Sys.which("node") == "", "node not available")
    tmp <- withr::local_tempdir()
    old <- setwd(tmp); on.exit(setwd(old), add = TRUE)
    saveCustomHooksTemplate("customHooks.js")
    status <- system2("node", c("--check", "customHooks.js"),
                      stdout = FALSE, stderr = FALSE)
    expect_equal(status, 0L)
})
