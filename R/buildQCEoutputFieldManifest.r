#' Report the data columns an experiment's config is expected to produce
#'
#' Scans the JSON config files in an experiment directory and writes a plain-text
#' manifest of the columns those files imply, grouped by where each one comes
#' from. It is ADVISORY: it does not write \code{fields.txt}, it tells you what
#' could go in one.
#'
#' Why it matters: \code{fields.txt} is a WHITELIST. The save path keeps only the
#' columns listed there, so a column you forget to add is silently dropped -- the
#' run looks fine and the data simply has a hole in it. Diffing this manifest
#' against your \code{fields.txt} is the cheapest way to catch that.
#'
#' It reads the files rather than watching the builders, so it reports what would
#' actually ship. That matters when a config is hand-edited after generation, and
#' it means the manifest cannot quietly disagree with the files on disk.
#'
#' What it CANNOT see: any column a custom hook writes at run time. When the
#' config declares a hooks file, the manifest says so explicitly rather than
#' implying the list is complete.
#'
#' @param dir The experiment directory holding the JSON config files.
#' @param outFile Filename to write the manifest to, inside \code{dir}. Pass NULL
#'   to return the report without writing it. DEFAULT = "output_fields_manifest.txt".
#' @param fieldsFile Optional path to an existing \code{fields.txt} to compare
#'   against. When given, the manifest flags every expected column missing from
#'   it -- the check that catches a silently dropped column. DEFAULT = NULL.
#' @param engineVersion A string recording which engine's row-stamp list this was
#'   built against. Stamped into the header so a stale manifest is visible rather
#'   than silent. DEFAULT = "9.1".
#'
#' @return Invisibly, a character vector: the lines of the manifest.
#' @keywords QCE fields manifest columns output fields.txt
#' @export
#' @examples
#' # After writing an experiment's config files:
#' # buildQCEoutputFieldManifest("myExperiment/")
#' # buildQCEoutputFieldManifest("myExperiment/", fieldsFile = "myExperiment/fields.txt")
buildQCEoutputFieldManifest <- function(dir, outFile = "output_fields_manifest.txt",
                                        fieldsFile = NULL, engineVersion = "9.1") {

  if (missing(dir) || !isSingleString(dir) || !dir.exists(dir)) {
    stop("dir option must be a single string naming an existing experiment directory.")
  }
  if (!is.null(outFile) && (!isSingleString(outFile) || nchar(outFile) == 0)) {
    stop("outFile option must be a single non-empty filename, or NULL to return without writing.")
  }
  if (!is.null(fieldsFile) && !isSingleString(fieldsFile)) {
    stop("fieldsFile option must be a single string naming an existing fields.txt, or NULL.")
  }

  readJson <- function(p) {
    tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE),
             error = function(e) NULL)
  }
  # Config scalars arrive wrapped as one-element arrays; take the first element.
  u <- function(x) if (is.list(x) && length(x) >= 1) x[[1]] else x

  jsonFiles <- list.files(dir, pattern = "\\.json$", full.names = TRUE)

  # --- columns written server-side, once per session ------------------------
  serverCols <- c("Exp_Name", "Group", "sn", "Cond_Name", "Sess_Name")

  # --- columns the engine stamps on every row -------------------------------
  engineCols <- c("BlockName", "BlockNum", "BlockIt", "Trial", "TrialInSession",
                  "TrialinBlock", "trial_index", "trial_type", "StimNum", "FrameNum",
                  "FrameName", "respType", "posttgap", "stim_dur", "rt", "Set",
                  "stimRef", "ShowFeedBack")

  # --- scan the stimfiles: trial types used, outputVariables, feedback keys --
  typesUsed <- character(0)
  outputVars <- character(0)
  feedbackKeys <- character(0)
  # --- scan for hooks + demographics ----------------------------------------
  hooksFiles <- character(0)
  demographicCols <- character(0)

  for (p in jsonFiles) {
    j <- readJson(p)
    if (is.null(j) || !is.list(j)) next

    # A stimfile is a map of scenarios, each with a `frame` map.
    for (nm in names(j)) {
      sc <- j[[nm]]
      if (!is.list(sc)) next
      if (!is.null(sc$frame) && is.list(sc$frame)) {
        for (fr in sc$frame) {
          if (is.list(fr) && !is.null(fr$trialType)) {
            typesUsed <- c(typesUsed, as.character(u(fr$trialType)))
          }
        }
      }
      if (!is.null(sc$outputVariables) && is.list(sc$outputVariables)) {
        outputVars <- c(outputVars, names(sc$outputVariables))
      }
      if (!is.null(sc$feedback) && is.list(sc$feedback) &&
          !is.null(sc$feedback$feedback_key)) {
        feedbackKeys <- c(feedbackKeys, as.character(u(sc$feedback$feedback_key)))
      }
    }

    # An unset config field serializes as an empty object/array rather than
    # being absent, so "declared" means non-empty, not merely non-NULL. Testing
    # only for NULL reports screens the engine will in fact skip.
    isSet <- function(v) !is.null(v) && length(unlist(v)) > 0 &&
                         any(nzchar(as.character(unlist(v))))

    # Hooks are declared on a group dbfile.
    if (isSet(j$customHooksFile)) {
      hooksFiles <- c(hooksFiles, as.character(u(j$customHooksFile)))
    }
    # The legacy intake screens the engine renders itself.
    if (isSet(j$getDemographicsFile)) demographicCols <- c(demographicCols, "Birth", "Ethnicity")
    if (isSet(j$getGenderFile))       demographicCols <- c(demographicCols, "Gender", "Sex")
  }

  typesUsed    <- sort(unique(typesUsed))
  outputVars   <- sort(unique(outputVars))
  feedbackKeys <- sort(unique(feedbackKeys))
  hooksFiles   <- sort(unique(hooksFiles))
  demographicCols <- sort(unique(demographicCols))

  # --- scan the page sidecars for their output columns ----------------------
  pageCols <- character(0)
  for (p in list.files(dir, pattern = "\\.page\\.json$", full.names = TRUE)) {
    j <- readJson(p)
    if (is.null(j) || is.null(j$fields)) next
    for (f in j$fields) {
      if (!is.list(f)) next
      col <- if (!is.null(f$as)) u(f$as) else u(f$input)
      if (!is.null(col)) pageCols <- c(pageCols, as.character(col))
    }
  }
  pageCols <- sort(unique(pageCols))

  # --- per-trialType columns, from the registry -----------------------------
  typeLines <- character(0)
  typeCols <- character(0)
  undeclared <- character(0)
  for (tt in typesUsed) {
    cols <- .qcebTrialTypeOutputColumns(tt)
    if (is.null(cols)) {
      undeclared <- c(undeclared, tt)
      typeLines <- c(typeLines,
                     sprintf("# [%s] declares no outputColumns -- check this plugin's own columns", tt))
    } else {
      typeCols <- c(typeCols, cols)
      typeLines <- c(typeLines, cols)
    }
  }
  typeCols <- unique(typeCols)

  # --- assemble --------------------------------------------------------------
  out <- c(
    "# Output field manifest -- ADVISORY, generated by QCEB.",
    "#",
    "# These are the data columns this experiment's config is expected to produce.",
    "# Copy the ones you want into fields.txt.",
    "#",
    "# fields.txt is a WHITELIST: a column that is not listed there is silently",
    "# dropped from the saved data. A missing line costs you data with no error.",
    sprintf("# Engine row-stamp list mirrors engine %s -- regenerate after an engine upgrade.", engineVersion),
    "",
    "# --- written server-side, once per session ---",
    serverCols,
    if (length(demographicCols)) c("", "# --- collected by the engine's intake screens ---", demographicCols) else NULL,
    "",
    "# --- stamped by the engine on every row ---",
    engineCols,
    "",
    sprintf("# --- from the trial types this experiment uses (%s) ---",
            if (length(typesUsed)) paste(typesUsed, collapse = ", ") else "none found"),
    if (length(typeLines)) typeLines else "# (none)",
    "",
    "# --- declared in your config ---",
    if (length(outputVars)) c("# outputVariables:", outputVars) else "# (no outputVariables declared)",
    if (length(feedbackKeys)) c("# feedback keys:", feedbackKeys) else NULL,
    if (length(pageCols)) c("# page fields:", pageCols) else NULL,
    ""
  )

  if (length(hooksFiles) > 0) {
    out <- c(out,
             "# ============================================================",
             sprintf("# CHECK THIS! Custom hooks are in use (%s).", paste(hooksFiles, collapse = ", ")),
             "# A hook can write any column it likes at run time, and none of them can",
             "# be seen from the config files. Read your hooks file and add whatever",
             "# columns it writes -- they will be dropped silently otherwise.",
             "# ============================================================",
             "")
  }

  out <- c(out,
           "# Auxiliary streams (showIf audit, triggers, switch events) are written to",
           "# separate files with fixed schemas. They do NOT come from fields.txt, so",
           "# nothing needs adding here for them.")

  # --- optional diff against an existing fields.txt --------------------------
  if (!is.null(fieldsFile)) {
    if (!file.exists(fieldsFile)) {
      stop("fieldsFile '", fieldsFile, "' does not exist.")
    }
    have <- trimws(readLines(fieldsFile, warn = FALSE))
    have <- have[nchar(have) > 0]
    expected <- unique(c(serverCols, demographicCols, engineCols, typeCols,
                         outputVars, feedbackKeys, pageCols))
    missingCols <- setdiff(expected, have)
    extraCols <- setdiff(have, expected)

    out <- c(out, "",
             sprintf("# === compared against %s ===", fieldsFile))
    if (length(missingCols)) {
      out <- c(out,
               "# CHECK THIS! Expected columns MISSING from fields.txt -- these are being",
               "# dropped from your saved data:",
               paste0("#   ", missingCols))
    } else {
      out <- c(out, "# No expected column is missing from fields.txt.")
    }
    if (length(extraCols)) {
      out <- c(out,
               "# In fields.txt but not expected here (a hook-written column, or stale):",
               paste0("#   ", extraCols))
    }
  }

  if (!is.null(outFile)) {
    writeLines(out, file.path(dir, outFile))
  }

  invisible(out)
}
