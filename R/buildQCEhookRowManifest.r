# Shared scanner behind buildQCEhookRowManifest() and promotedQCEhookRows().
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

      # Scenarios that keep NO frame. A hook annotating one of these has no row
      # to write to, so the engine keeps the last frame's row to carry the
      # values -- that scenario records a row the config did not ask for.
      #
      # Scenarios that keep SOME frame are not reported at any position of the
      # kept frames: the engine routes annotations to the last kept row, so
      # ending on a discarded fixation costs nothing.
      if (!any(outs)) {
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


#' Report scenarios that would gain a row to hold trial-hook data
#'
#' Scans the JSON config files in an experiment directory for scenarios that keep
#' no row of their own, and writes a plain-text report naming them. It is
#' ADVISORY: it changes no config, and nothing it lists is a defect.
#'
#' Why it matters: \code{onTrialFinish} fires once per scenario and its return
#' value is written onto that scenario's last SAVED row. A scenario whose frames
#' are all \code{output = FALSE} has no such row, so the engine keeps its last
#' frame to carry the values rather than discard them. Those scenarios therefore
#' record one row each that the config does not otherwise ask for -- which
#' changes the row count of the saved data, and is worth knowing before it turns
#' up in an analysis.
#'
#' A scenario that ends on a discarded frame but keeps an earlier one is NOT
#' reported: the engine routes annotations to the last kept row, so a trailing
#' fixation costs nothing.
#'
#' What it CANNOT see: whether your hook returns \code{dataAnnotations} at all.
#' That is decided in JavaScript at run time, and no row is added unless it does.
#' A hook that returns only \code{feedback}, or that skips these scenarios, leaves
#' the data exactly as the config describes it.
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
    "# Scenarios that would gain a row to hold trial-hook data",
    sprintf("# directory: %s", dir),
    "#",
    "# onTrialFinish writes what it returns onto the scenario's last SAVED row.",
    "# A scenario with no frame marked output = TRUE has no such row, so the",
    "# engine keeps its last frame to carry the values rather than discard them.",
    "# Those scenarios record one row each that this config does not otherwise",
    "# ask for -- only if a hook actually annotates them.",
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
    out <- c(out, "# Every multi-frame scenario keeps at least one row of its own.")
  } else {
    out <- c(out,
             sprintf("# NOTE: %d scenario(s) keep no row of their own.", n),
             "# If your onTrialFinish annotates these, each records one extra row whose",
             "# only content is those values. That is intended -- it is how the data",
             "# survives. Mark a frame output = TRUE if you want the row to carry that",
             "# frame's own data as well.",
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


#' Which scenarios would gain a row for their trial-hook data
#'
#' The machine-readable half of \code{\link{buildQCEhookRowManifest}}: the same
#' scan, returning scenario names instead of a report to read. Use it when a
#' build needs to assert its own row count -- for instance to confirm that a
#' change to which frames are kept has not quietly altered how many rows a
#' participant produces.
#'
#' This is informational, not a defect check. Nothing is lost either way: the
#' engine keeps the last frame of such a scenario precisely so the hook's values
#' survive. Failing a build on it is usually the wrong response.
#'
#' Returns nothing when the directory declares no hooks file, since no trial hook
#' runs and no row can be added.
#'
#' @param dir The experiment directory holding the JSON config files.
#' @param requireHooksFile When TRUE, only report scenarios if a hooks file is
#'   declared. Set FALSE to check the scenario shape on its own, for a config
#'   whose hooks are added later. DEFAULT = TRUE.
#'
#' @return A character vector of scenario names that keep no row of their own;
#'   empty when there are none.
#' @keywords QCE hooks onTrialFinish output frames
#' @export
#' @examples
#' # After writing the config files:
#' # promotedQCEhookRows("myExperiment/")
promotedQCEhookRows <- function(dir, requireHooksFile = TRUE) {

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
