#tests for the local plugin manifest writer, written into a temp dir

test_that("buildQCElocalPluginEntry assembles a full entry in engine order", {
    e <- buildQCElocalPluginEntry("myRegister.js",
                                  scriptAssets = c("bundleA.js", "bundleB.js"),
                                  cssAssets = "myPlugin.css",
                                  description = "a test plugin")
    expect_equal(e$register, "myRegister.js")
    expect_equal(e$description, "a test plugin")
    expect_length(e$assets, 3)
    #scripts first in order then css, the register script needs its assets
    expect_equal(e$assets[[1]], list(type = "script", src = "bundleA.js"))
    expect_equal(e$assets[[2]], list(type = "script", src = "bundleB.js"))
    expect_equal(e$assets[[3]], list(type = "css", href = "myPlugin.css"))
})

test_that("buildQCElocalPluginEntry omits absent optional parts", {
    e <- buildQCElocalPluginEntry("myRegister.js")
    expect_equal(names(e), "register")
})

test_that("buildQCElocalPluginEntry rejects every path the engine would refuse", {
    expect_error(buildQCElocalPluginEntry("https://cdn.example/x.js"), "URL")
    expect_error(buildQCElocalPluginEntry("//cdn.example/x.js"), "URL")
    expect_error(buildQCElocalPluginEntry("/abs/x.js"), "relative")
    expect_error(buildQCElocalPluginEntry("C:/x.js"), "relative")
    #a single backslash is refused here though the engine resolves it
    expect_error(buildQCElocalPluginEntry("sub\\x.js"), "relative")
    expect_error(buildQCElocalPluginEntry("../x.js"), "\\.\\.")
    expect_error(buildQCElocalPluginEntry("sub/../x.js"), "\\.\\.")
    expect_error(buildQCElocalPluginEntry(""), "non-empty")
    expect_error(buildQCElocalPluginEntry("ok.js", scriptAssets = "../bad.js"), "\\.\\.")
    expect_error(buildQCElocalPluginEntry("ok.js", cssAssets = "https://x/y.css"), "URL")
})

test_that("savePluginManifestLocal writes engine-readable, unboxed JSON", {
    tmp <- withr::local_tempdir()
    old <- setwd(tmp); on.exit(setwd(old), add = TRUE)

    writeLines("// bundle", "bundle.js")
    writeLines("// register", "reg.js")

    savePluginManifestLocal(list(
        myType = buildQCElocalPluginEntry("reg.js", scriptAssets = "bundle.js")
    ))
    expect_true(file.exists("pluginManifest.local.json"))

    d <- jsonlite::fromJSON("pluginManifest.local.json", simplifyVector = FALSE)
    entry <- d$plugins$myType
    #the engine parses this file without unwrapping, so scalars stay scalars
    expect_true(is.character(entry$register) || is.character(unlist(entry$register)))
    expect_equal(entry$register, "reg.js")
    expect_equal(entry$assets[[1]]$type, "script")
    expect_equal(entry$assets[[1]]$src, "bundle.js")
    #a single asset still serializes as an array of objects
    txt <- paste(readLines("pluginManifest.local.json"), collapse = "\n")
    expect_true(grepl('"assets": \\[', txt))
})

test_that("savePluginManifestLocal warns on files missing from the working directory", {
    tmp <- withr::local_tempdir()
    old <- setwd(tmp); on.exit(setwd(old), add = TRUE)
    expect_warning(
        savePluginManifestLocal(list(t = buildQCElocalPluginEntry("nowhere.js"))),
        "not found")
})

test_that("savePluginManifestLocal rejects structural mistakes", {
    expect_error(savePluginManifestLocal(list()), "non-empty")
    expect_error(savePluginManifestLocal(list(buildQCElocalPluginEntry("r.js"))), "named")
    expect_error(savePluginManifestLocal(list(t = list(description = "no register"))), "register")
    #an asset with neither src nor href fails naming the plugin and asset
    expect_error(
        savePluginManifestLocal(list(t = list(register = "r.js",
                                              assets = list(list(type = "script"))))),
        "neither 'src' nor 'href'")
    p <- list(t = buildQCElocalPluginEntry("r.js"), t = buildQCElocalPluginEntry("r.js"))
    expect_error(suppressWarnings(savePluginManifestLocal(p)), "duplicate")
})
