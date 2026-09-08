# Tests for the reference generator's optional PDF render step.
#
# The generator is a standalone script rather than a package function, so these
# tests run it the way a user does -- through Rscript, against a throwaway
# package root built in a temp dir. They are skipped where the script is not
# on disk (it is excluded from the built package) or where Rscript cannot be
# found.

generator_script <- function() {
    normalizePath(test_path("..", "..", "tools", "generate_api_reference.R"),
                  mustWork = FALSE)
}

# A minimal package root: the generator needs man/, NAMESPACE, and a
# BUILDER_REFERENCE.md carrying exactly one BEGIN/END marker pair.
make_fixture_root <- function() {
    root <- file.path(tempfile("qcebRef"))
    dir.create(file.path(root, "man"), recursive = TRUE)
    writeLines(c("\\name{fixtureFn}", "\\title{A fixture function}",
                 "\\usage{fixtureFn(x)}"),
               file.path(root, "man", "fixtureFn.Rd"))
    writeLines("export(fixtureFn)", file.path(root, "NAMESPACE"))
    writeLines(c(
        "# Fixture",
        "",
        paste0("<!-- BEGIN GENERATED API — do not edit by hand; ",
               "run tools/generate_api_reference.R -->"),
        "<!-- END GENERATED API -->"),
        file.path(root, "BUILDER_REFERENCE.md"))
    root
}

# A renderer that exits 0 and writes nothing at all -- the failure mode a bare
# existence check cannot see.
make_lying_renderer <- function(root) {
    path <- file.path(root, "lyingRenderer.sh")
    writeLines(c("#!/bin/sh", "exit 0"), path)
    Sys.chmod(path, "0755")
    path
}

run_generator <- function(root, renderer = NULL) {
    args <- c(generator_script())
    if (!is.null(renderer)) args <- c(args, paste0("--pdf-renderer=", renderer))
    args <- c(args, root)
    suppressWarnings(system2("Rscript", shQuote(args),
                             stdout = TRUE, stderr = TRUE))
}

skip_unless_runnable <- function() {
    skip_on_cran()
    skip_if_not(file.exists(generator_script()),
                "generator script is not part of the built package")
    skip_if_not(nzchar(Sys.which("Rscript")), "Rscript not found")
    skip_on_os("windows")  # the fixture renderer is a shell script
}

test_that("a renderer that writes nothing fails, and takes no stale PDF with it", {
    skip_unless_runnable()

    root <- make_fixture_root()
    pdf <- file.path(root, "BUILDER_REFERENCE.pdf")
    # A PDF left over from an earlier, successful run.
    writeLines("stale output from an earlier run", pdf)
    expect_true(file.exists(pdf))

    out <- run_generator(root, make_lying_renderer(root))

    # The run fails ...
    expect_true(any(grepl("wrote no file", out)))
    expect_equal(attr(out, "status"), 1L)
    # ... and the stale PDF is gone rather than passing as this run's output.
    expect_false(file.exists(pdf))
    # The markdown is still regenerated: rendering is the last step.
    expect_true(any(grepl("fixtureFn",
                          readLines(file.path(root, "BUILDER_REFERENCE.md")))))
})

test_that("a renderer whose output has no readable page count fails", {
    skip_unless_runnable()

    root <- make_fixture_root()
    renderer <- file.path(root, "garbageRenderer.sh")
    # Exits 0 and writes a file, but not one with a readable page tree.
    writeLines(c("#!/bin/sh", "printf 'not a pdf' > \"$2\""), renderer)
    Sys.chmod(renderer, "0755")

    out <- run_generator(root, renderer)

    expect_true(any(grepl("no readable page count", out)))
    expect_equal(attr(out, "status"), 1L)
})

test_that("a page count is only ever reported as a whole number", {
    skip_unless_runnable()

    root <- make_fixture_root()
    renderer <- file.path(root, "countingRenderer.sh")
    # The smallest thing the page-count scan will accept: a /Count entry.
    writeLines(c("#!/bin/sh", "printf '%%PDF-1.4 /Count 7' > \"$2\""), renderer)
    Sys.chmod(renderer, "0755")

    out <- run_generator(root, renderer)

    expect_true(any(grepl("BUILDER_REFERENCE\\.pdf: 7 pages\\.", out)))
    expect_false(any(grepl("unavailable", out)))
})

test_that("no renderer named renders nothing and says so", {
    skip_unless_runnable()

    root <- make_fixture_root()
    # The environment variable is the other way to name a renderer, so it has
    # to be absent for this to be the "named nothing" case.
    had <- Sys.getenv("QCEB_PDF_RENDERER", unset = NA)
    Sys.unsetenv("QCEB_PDF_RENDERER")
    on.exit(if (!is.na(had)) Sys.setenv(QCEB_PDF_RENDERER = had), add = TRUE)

    out <- run_generator(root)

    expect_true(any(grepl("PDF not rendered", out)))
    expect_false(file.exists(file.path(root, "BUILDER_REFERENCE.pdf")))
})

test_that("an empty --pdf-renderer= is refused rather than silently obeyed", {
    skip_unless_runnable()

    root <- make_fixture_root()
    out <- run_generator(root, "")

    expect_true(any(grepl("empty value", out)))
    expect_equal(attr(out, "status"), 1L)
})
