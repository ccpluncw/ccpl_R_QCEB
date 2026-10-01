#the r blocks of TUTORIAL.md run in order as one script and build its study

TUTORIAL_RUN_BLOCKS <- 30
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
})
