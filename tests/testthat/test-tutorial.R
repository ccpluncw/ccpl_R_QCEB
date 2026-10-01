#the r blocks of TUTORIAL.md run in order as one script and build its study

TUTORIAL_RUN_BLOCKS <- 32
TUTORIAL_NORUN_BLOCKS <- 0

docPath <- function(name) {
    normalizePath(test_path("..", "..", name), mustWork = FALSE)
}

#each fenced r block of a markdown file, as a character vector of its lines
rBlocks <- function(lines) {
    open <- grep("^```r[[:space:]]*$", lines)
    fence <- grep("^```[[:space:]]*$", lines)
    lapply(open, function(o) {
        e <- fence[fence > o][1]
        if (is.na(e)) stop("an r block opened at line ", o, " never closes")
        lines[seq_len(e - o - 1) + o]
    })
}

isNoRun <- function(block) {
    length(block) > 0 && grepl("^#not run", block[1])
}

#runs blocks in env, returning the first error as text or NULL
runBlocks <- function(blocks, env) {
    for (i in seq_along(blocks)) {
        err <- tryCatch({
            utils::capture.output(eval(parse(text = blocks[[i]]), envir = env))
            NULL
        }, error = function(e) paste0("block ", i, ": ", conditionMessage(e)))
        if (!is.null(err)) return(err)
    }
    NULL
}

#a fresh environment whose library() leaves the attached package alone
blockEnv <- function() {
    env <- new.env(parent = globalenv())
    env$library <- function(...) invisible(NULL)
    env
}

test_that("every r block of the tutorial runs in order and builds its study", {
    skip_on_cran()
    path <- docPath("TUTORIAL.md")
    skip_if_not(file.exists(path), "the tutorial is not part of the built package")
    skip_if_not(capabilities("png"), "no png device to draw the pictures")

    blocks <- rBlocks(readLines(path, warn = FALSE, encoding = "UTF-8"))
    norun <- vapply(blocks, isNoRun, logical(1))
    expect_equal(sum(!norun), TUTORIAL_RUN_BLOCKS)
    expect_equal(sum(norun), TUTORIAL_NORUN_BLOCKS)
    for (b in blocks[norun]) expect_no_error(parse(text = b))

    work <- tempfile("tutorial")
    dir.create(work)
    old <- setwd(work)
    on.exit(setwd(old), add = TRUE)
    expect_null(runBlocks(blocks[!norun], blockEnv()))

    study <- file.path(work, "shapeMatch")
    built <- c("shapeMatch.php", "preloadFile.json", "shapeMatch_Stimfile.json",
               "shapeMatch_Tsfile.json", "shape_Dbfile.json",
               "colour_Dbfile.json", "expDBfile.json", "expInfo.json",
               "customHooks.js", "pages.json", "aboutYou.page.json",
               "age.page.json", "debrief.page.json", "consent.txt",
               "fields.txt", "output_fields_manifest.txt")
    expect_true(all(file.exists(file.path(study, built))))
    expect_length(list.files(file.path(study, "pictures"), pattern = "png$"), 12)
    expect_length(missingQCEoutputFields(study), 0)
    sc <- readQCEjsonFile(file.path(study, "shapeMatch_Stimfile.json"))
    expect_length(sc, 145)
    ts <- readQCEjsonFile(file.path(study, "shapeMatch_Tsfile.json"))
    expect_setequal(names(ts), c(as.character(1:5), "switchRules"))
})
