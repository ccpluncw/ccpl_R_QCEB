#' Register a custom-hooks file on a QCEdbfile (Phase 5)
#'
#' Phase 5 (Custom Hooks): declares the researcher-authored JavaScript hooks
#' file for a group (session), plus the optional list of state keys those
#' hooks will write to `qceState.custom`. Mirrors the incremental pattern of
#' addKeyMapToDbfile -- pass a partially-built dbfile (output of
#' buildQCEgroupDbFile) and chain the call, or declare the same values at
#' dbfile-build time via the `customHooksFile` / `customHooksStateKeys`
#' arguments to buildQCEgroupDbFile (that path delegates here).
#'
#' The hooks file is a `.js` file that, when loaded by the engine, defines a
#' global `QCEPHooks` object with any of the recognized hook functions
#' (onTrialStart, onTrialFinish, onSetEnd, onBlockEnd, onSessionEnd). Use
#' saveCustomHooksTemplate() to scaffold a starter file. The file must be
#' copied into the experiment directory alongside the other preload assets.
#'
#' `customHooksStateKeys` is the set of keys the hooks promise to write into
#' `qceState.custom`. The engine uses this list for static validation: any
#' `stateRef` in a showIf condition (built with buildQCEstateCondition) that
#' is NOT in this list is flagged as a likely typo at session start. Omit it
#' (NULL) to skip that check -- the engine then only warns rather than errors
#' on unknown stateRefs.
#'
#' Backwards compatibility: a dbfile with no `customHooksFile` declared
#' produces byte-identical JSON to a pre-Phase-5 dbfile (no `customHooksFile`
#' / `customHooksStateKeys` keys in the output), and the engine takes the
#' legacy no-hooks code path.
#'
#' @param QCEdbfile A QCEdbfile (output of buildQCEgroupDbFile). Required.
#' @param customHooksFile Single non-empty string ending in `.js` -- the
#'   filename of the hooks file (relative to the experiment directory).
#' @param customHooksStateKeys Optional character vector of state-key names
#'   the hooks will write to `qceState.custom`, used for static validation of
#'   `stateRef` showIf conditions. NULL means no declared keys (the engine
#'   warns rather than errors on unknown stateRefs). DEFAULT = NULL.
#''
#' @return The updated QCEdbfile, with `$customHooksFile` set (and
#'   `$customHooksStateKeys` set when supplied).
#' @keywords QCE hooks dbfile dynamic
#' @export
#' @examples
#' dbfile <- buildQCEgroupDbFile(condName = "rubyT4",
#'   keyMap = buildKeyMap(data.frame(Yes = "y", No = "n")))
#'
#' # Declare the hooks file and the state keys the hooks will set
#' dbfile <- addHooksToQCEgroupDbFile(dbfile, "customHooks.js",
#'             customHooksStateKeys = c("reachedCriterion", "blockScore"))
#'
#' # Hooks file with no state keys (e.g., feedback-only hooks)
#' dbfile <- addHooksToQCEgroupDbFile(dbfile, "summaryHooks.js")

addHooksToQCEgroupDbFile <- function(QCEdbfile, customHooksFile, customHooksStateKeys = NULL) {

  if (is.null(QCEdbfile) || !is.list(QCEdbfile)) {
    stop("QCEdbfile option must be a QCEdbfile (output of buildQCEgroupDbFile).")
  }

  if (missing(customHooksFile) || !isSingleString(customHooksFile) ||
      nchar(customHooksFile) == 0 || !isValidFilename(customHooksFile, "js")) {
    stop("customHooksFile option must be a single non-empty filename that ends ",
         "in '.js' (the researcher-authored hooks file, relative to the ",
         "experiment directory).")
  }

  # customHooksStateKeys, when supplied, must be a non-empty character vector
  # of non-empty names. These are the keys the hooks promise to write to
  # qceState.custom; the engine validates stateRef showIf conditions against
  # them. Reject NA / empty-string entries -- they would defeat the typo check.
  if (!is.null(customHooksStateKeys)) {
    if (!is.character(customHooksStateKeys) || length(customHooksStateKeys) < 1 ||
        any(is.na(customHooksStateKeys)) || any(nchar(customHooksStateKeys) == 0)) {
      stop("customHooksStateKeys option must be a character vector of one or ",
           "more non-empty state-key names, or NULL.")
    }
    if (any(duplicated(customHooksStateKeys))) {
      warning("customHooksStateKeys contains duplicate names; de-duplicating.")
      customHooksStateKeys <- unique(customHooksStateKeys)
    }
  }

  # Duplicate-declaration warning. Catches a copy-pasted dbfile that wasn't
  # fully edited, mirroring addKeyMapToDbfile's footgun guard.
  if (!is.null(QCEdbfile$customHooksFile)) {
    warning(sprintf(
      "customHooksFile already declared on dbfile ('%s'); overwriting with '%s'.",
      QCEdbfile$customHooksFile, customHooksFile))
  }

  QCEdbfile$customHooksFile <- customHooksFile

  # Emit customHooksStateKeys only when supplied, so a hooks-but-no-stateKeys
  # dbfile omits the key entirely (engine treats absence as "no declared keys").
  if (!is.null(customHooksStateKeys)) {
    QCEdbfile$customHooksStateKeys <- customHooksStateKeys
  }

  return(QCEdbfile)
}
