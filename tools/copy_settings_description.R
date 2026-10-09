#!/usr/bin/env Rscript

#copy the settings description of record into inst/, byte for byte
#usage: Rscript tools/copy_settings_description.R <settingsDescription.json> [--check] [package_root]
#with --check nothing is written and the exit status is 1 when the copies differ

args <- commandArgs(trailingOnly = TRUE)
check <- "--check" %in% args
args <- args[args != "--check"]
if (length(args) < 1) {
  stop("name the settings description of record: Rscript tools/copy_settings_description.R <settingsDescription.json> [--check] [package_root]")
}
src <- args[1]
root <- if (length(args) >= 2) args[2] else "."
dest <- file.path(root, "inst", "settingsDescription.json")
if (!file.exists(src)) stop("no file at ", src)

#read raw bytes so line endings and encoding are kept
readBytes <- function(p) readBin(p, "raw", n = file.info(p)$size)
srcBytes <- readBytes(src)
#the copy must at least parse as json
invisible(jsonlite::fromJSON(rawToChar(srcBytes), simplifyVector = FALSE))

same <- file.exists(dest) && identical(srcBytes, readBytes(dest))
if (check) {
  if (same) {
    cat("settingsDescription.json: the copy in inst/ is the description of record\n")
    quit(status = 0)
  }
  cat("settingsDescription.json: the copy in inst/ differs from", src, "\n")
  quit(status = 1)
}
dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
writeBin(srcBytes, dest)
cat(if (same) "unchanged:" else "written:", dest, "\n")
