# Shared scanner behind buildQCEhookRowManifest() and unsavedQCEhookRows().
#
# Both entry points need the same answer, so the scan lives here once and each
# returns a different view of it. Keeping the machine-readable half a view of a
# structure -- rather than a re-parse of the report text -- means a wording
# change to the report cannot quietly change what a build script fails on.
#
# Returns a list:
#   hooksFiles  character vector of customHooksFile values found (may be empty)
#   atRisk      list of records: file, scenario, nFrames, lastFrame
#   scanned     number of scenarios examined
.qcebScanHookRows <- function(dir) {

  u <- function(x) if (is.list(x) && length(x) >= 1) x[[1]] else x

  # A flag arrives as a JSON boolean wrapped in a one-element array, but a
  # hand-edited config may carry the string form instead. Accept both rather
  # than silently reading "TRUE" as not-true.
  isTrueFlag <- function(v) {
    v <- u(v)
    if (is.null(v) || length(v) == 0) return(FALSE)
    if (is.logical(v)) return(isTRUE(v))
    toupper(as.character(v)[1]) %in% c("TRUE", "T", "1", "YES")
  }

  # An unset config field serializes as an empty object/array rather than being
  # absent, so "declared" means non-empty, not merely non-NULL.
  isSet <- function(v) !is.null(v) && length(unlist(v)) > 0 &&
                       any(nzchar(as.character(unlist(v))))

  readJson <- function(p) {
    tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE),
             error = function(e) NULL)
  }

  # Frames are keyed "1", "2", ... Sorting those as text puts "10" before "2",
  # which would pick the wrong last frame on any scenario with ten or more
  # frames. Order numerically when the keys are numeric, and fall back to the
  # file's own order when they are not.
  frameOrder <- function(fr) {
    nms <- names(fr)
    if (is.null(nms)) return(seq_along(fr))
    suppressWarnings(nn <- as.integer(nms))
    if (any(is.na(nn))) nms else nms[order(nn)]
  }

  hooksFiles <- character(0)
  atRisk     <- list()
  scanned    <- 0L

  for (p in list.files(dir, pattern = "\\.json$", full.names = TRUE)) {
    j <- readJson(p)
    if (is.null(j) || !is.list(j)) next

    # Hooks are declared on a group dbfile, scenarios live in a stimfile, and
    # both sit in the same directory. Any group declaring a hooks file makes the
    # check apply: pairing each group to the scenarios it actually plays would
    # need the trial structure too, and the reachable extra precision does not
    # change the advice.
    if (isSet(j$customHooksFile)) {
      hooksFiles <- c(hooksFiles, as.character(u(j$customHooksFile)))
    }

    for (nm in names(j)) {
      sc <- j[[nm]]
      if (!is.list(sc) || is.null(sc$frame) || !is.list(sc$frame)) next
      fr <- sc$frame
      if (length(fr) < 2) next          # single-frame scenarios cannot hit this
      scanned <- scanned + 1L

      ord  <- frameOrder(fr)
      outs <- vapply(ord, function(k) isTrueFlag(fr[[k]]$output),
                     logical(1), USE.NAMES = FALSE)

      # The shape that loses data: some frame is kept, but the LAST one is not.
      #
      # A scenario with NO kept frame is excluded deliberately. Nothing of its
      # own is saved either way, so there is no row an annotation could have
      # landed on and nothing is lost -- reporting it would be noise, and noise
      # is what stops a check like this from being read.
      if (any(outs) && !outs[length(outs)]) {
        atRisk[[length(atRisk) + 1L]] <- list(
          file      = basename(p),
          scenario  = nm,
          nFrames   = length(ord),
          lastFrame = ord[length(ord)]
        )
      }
    }
  }

  list(hooksFiles = sort(unique(hooksFiles)),
       atRisk     = atRisk,
       scanned    = scanned)
}


#' Report scenarios whose trial-hook data would not be saved
#'
#' Scans the JSON config files in an experiment directory for scenarios that end
#' on a frame the save path discards, and writes a plain-text report naming
#' them. It is ADVISORY: it changes no config, it tells you where data a hook
#' returns would go missing.
#'
#' Why it matters: \code{onTrialFinish} fires once per scenario, on its LAST
#' frame, and what it returns is written onto that frame's row. Rows from frames
#' marked \code{output = FALSE} are dropped when the data is saved. So a scenario
#' that ends on a discarded frame -- a trailing fixation or inter-trial blank
#' after the frame that mattered -- loses every \code{dataAnnotations} value the
#' hook computed for it. Nothing errors and no column goes missing; the affected
#' rows simply hold no value, in a file that otherwise looks complete.
#'
#' Only scenarios with more than one frame can be affected, and only those with
#' at least one kept frame are reported: a scenario with nothing saved at all has
#' no row to lose.
#'
#' What it CANNOT see: whether your hook returns \code{dataAnnotations} at all.
#' That is decided in JavaScript at run time. A hook that only returns
#' \code{feedback} is unaffected, so treat a report as "check this", not as proof
#' of a defect.
#'
#' @param dir The experiment directory holding the JSON config files.
#' @param outFile Filename to write the report to, inside \code{dir}. Pass NULL
#'   to return the report without writing it. DEFAULT = "hook_row_manifest.txt".
#' @param quiet When FALSE, also prints a one-line summary via \code{message()},
#'   so a build that scrolls past still shows the finding. DEFAULT = FALSE.
#'
#' @return Invisibly, a character vector: the lines of the report.
#' @keywords QCE hooks onTrialFinish output frames manifest
#' @export
#' @examples
#' # After writing an experiment's config files:
#' # buildQCEhookRowManifest("myExperiment/")
buildQCEhookRowManifest <- function(dir, outFile = "hook_row_manifest.txt",
                                    quiet = FALSE) {

  if (missing(dir) || !isSingleString(dir) || !dir.exists(dir)) {
    stop("dir option must be a single string naming an existing experiment directory.")
  }
  if (!is.null(outFile) && (!isSingleString(outFile) || nchar(outFile) == 0)) {
    stop("outFile option must be a single non-empty filename, or NULL to return without writing.")
  }
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    stop("quiet option must be a single logical value (TRUE or FALSE).")
  }

  scan <- .qcebScanHookRows(dir)
  hasHooks <- length(scan$hooksFiles) > 0
  n <- length(scan$atRisk)

  out <- c(
    "# Trial-hook data that would not be saved",
    sprintf("# directory: %s", dir),
    "#",
    "# onTrialFinish writes what it returns onto a scenario's LAST frame. Rows",
    "# from frames with output = FALSE are dropped when the data is saved, so a",
    "# scenario ending on one loses the dataAnnotations its hook computed.",
    "#",
    sprintf("# multi-frame scenarios scanned: %d", scan$scanned)
  )

  out <- c(out, if (hasHooks) {
    c(sprintf("# hooks file declared: %s", paste(scan$hooksFiles, collapse = ", ")))
  } else {
    c("# No hooks file is declared in this directory, so no trial hook runs and",
      "# nothing below can be lost. Listed for reference only.")
  })

  out <- c(out, "")
  if (n == 0) {
    out <- c(out, "# No scenario ends on a discarded frame.")
  } else {
    out <- c(out,
             sprintf("# CHECK THIS! %d scenario(s) end on a frame that is not saved.", n),
             "# If your onTrialFinish returns dataAnnotations for these, those values",
             "# are being dropped. Fix by marking the final frame output = TRUE, or by",
             "# moving the trailing frame ahead of the one you keep.",
             "#",
             "#   file                      scenario        frames  last")
    for (r in scan$atRisk) {
      out <- c(out, sprintf("#   %-24s  %-14s  %6d  %s",
                            r$file, r$scenario, r$nFrames, r$lastFrame))
    }
  }

  if (!quiet) {
    if (n == 0) {
      message("QCEB hook-row check: no scenario ends on a discarded frame.")
    } else {
      message(sprintf(
        "QCEB hook-row check: %d scenario(s) end on a frame that is not saved%s. See %s",
        n,
        if (hasHooks) "" else " (no hooks file declared, so nothing is lost today)",
        if (is.null(outFile)) "the returned report" else file.path(dir, outFile)))
    }
  }

  if (!is.null(outFile)) {
    writeLines(out, file.path(dir, outFile))
  }

  invisible(out)
}


#' Which scenarios would lose their trial-hook data
#'
#' The machine-readable half of \code{\link{buildQCEhookRowManifest}}: the same
#' scan, returning scenario names instead of a report to read. Use it to make a
#' build script FAIL when a scenario would silently drop what its hook returns,
#' rather than leaving the gap to be noticed in the data later.
#'
#' Returns nothing when the directory declares no hooks file, since no trial hook
#' runs and there is nothing to lose.
#'
#' @param dir The experiment directory holding the JSON config files.
#' @param requireHooksFile When TRUE, only report scenarios if a hooks file is
#'   declared. Set FALSE to check the scenario shape on its own, for a config
#'   whose hooks are added later. DEFAULT = TRUE.
#'
#' @return A character vector of scenario names that end on a discarded frame;
#'   empty when there are none.
#' @keywords QCE hooks onTrialFinish output frames
#' @export
#' @examples
#' # In a build script, right after writing the config files:
#' # lost <- unsavedQCEhookRows("myExperiment/")
#' # if (length(lost)) stop("hook data dropped for: ", paste(lost, collapse = ", "))
unsavedQCEhookRows <- function(dir, requireHooksFile = TRUE) {

  if (missing(dir) || !isSingleString(dir) || !dir.exists(dir)) {
    stop("dir option must be a single string naming an existing experiment directory.")
  }
  if (!is.logical(requireHooksFile) || length(requireHooksFile) != 1 || is.na(requireHooksFile)) {
    stop("requireHooksFile option must be a single logical value (TRUE or FALSE).")
  }

  scan <- .qcebScanHookRows(dir)
  if (requireHooksFile && length(scan$hooksFiles) == 0) return(character(0))
  vapply(scan$atRisk, function(r) as.character(r$scenario), character(1))
}
