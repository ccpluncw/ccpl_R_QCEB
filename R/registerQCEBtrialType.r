#' Register a trialType so QCEB frame builders will accept it
#'
#' QCEB mirrors, on the R side, the engine's trialType registry
#' (trialTypeRegistry.js). \code{addFrameToQCEframeList} validates its
#' \code{trialType} argument against this registry instead of a hard-coded list,
#' so adding support for a new jsPsych plugin in your experiment is a matter of
#' registering its trialType name here -- no edit to the QCEB core is needed.
#'
#' The CORE types (\code{"key"}, \code{"textbox"}, \code{"numberline"},
#' \code{"angleline"}) and the bundled \code{"survey"} plugin are pre-registered
#' automatically, so you only call this for additional third-party / custom
#' plugins (e.g. a Cyberball plugin).
#'
#' This is a friendly, R-side typo guard only. The authoritative gate is the
#' engine's \code{validateTrialTypes} at session start, which checks that the
#' plugin is actually loaded (listed in a session's \code{plugins} array and
#' present in pluginManifest.json). Registering a type in QCEB does NOT load the
#' plugin -- you still pass \code{plugins = c("yourPlugin")} to
#' \code{\link{addSessionToSessionList}}.
#'
#' @param name A non-empty single string -- the trialType name, matching the
#'   name the plugin uses in its \code{registerTrialType()} call on the engine
#'   side (e.g. "survey").
#' @param ... Optional named metadata describing the type (e.g.
#'   \code{stimulusParam}, \code{requiresKeymap}). Stored verbatim for
#'   introspection; QCEB does not act on it. The registry entry is an OPEN
#'   object, exactly like the engine's, so plugins can carry extra metadata
#'   without a QCEB change.
#'
#' @return Invisibly, the registry entry list that was stored.
#' @keywords QCE trialType register plugin
#' @export
#' @examples
#' # Register a hypothetical custom plugin, then build a frame that uses it:
#' registerQCEBtrialType("cyberball", requiresKeymap = FALSE)
#' # addFrameToQCEframeList(trialType = "cyberball", stimulus = myStim, ...)
registerQCEBtrialType <- function(name, ...) {
  .seedCoreQCEBtrialTypes()
  if (!isSingleString(name) || nchar(name) == 0) {
    stop("registerQCEBtrialType: name must be a non-empty single string, got ",
         deparse(name), ".")
  }
  if (exists(name, envir = .qcebTrialTypeRegistry, inherits = FALSE)) {
    stop("registerQCEBtrialType('", name, "'): trialType is already registered. ",
         "Each name can be registered only once (last-wins would mask mistakes).")
  }
  entry <- list(name = name, ...)
  assign(name, entry, envir = .qcebTrialTypeRegistry)
  invisible(entry)
}

#' Test whether a trialType is registered with QCEB
#'
#' @param name A single string trialType name.
#' @return TRUE if the trialType is registered (core, survey, or custom), else
#'   FALSE.
#' @keywords QCE trialType register plugin
#' @export
#' @examples
#' isRegisteredQCEBtrialType("key")     # TRUE
#' isRegisteredQCEBtrialType("survey")  # TRUE
#' isRegisteredQCEBtrialType("nope")    # FALSE
isRegisteredQCEBtrialType <- function(name) {
  .seedCoreQCEBtrialTypes()
  isSingleString(name) && exists(name, envir = .qcebTrialTypeRegistry, inherits = FALSE)
}

#' List all trialTypes registered with QCEB
#'
#' @return A character vector of registered trialType names (core + survey +
#'   any custom types registered via \code{\link{registerQCEBtrialType}}).
#' @keywords QCE trialType register plugin
#' @export
#' @examples
#' getRegisteredQCEBtrialTypes()
getRegisteredQCEBtrialTypes <- function() {
  .seedCoreQCEBtrialTypes()
  sort(ls(envir = .qcebTrialTypeRegistry))
}

# --- internal registry store ------------------------------------------------
# Package-level mutable environment. Created once when the namespace loads.
# Seeded lazily (see .seedCoreQCEBtrialTypes) so the accessors are
# order-independent and work under both devtools::load_all and an installed
# package without needing an .onLoad hook.
.qcebTrialTypeRegistry <- new.env(parent = emptyenv())

# Seed the built-in types if they are not already present. Idempotent.
#   key / textbox / numberline / angleline -- engine CORE types.
#   survey                                 -- the bundled Phase 6 plugin
#                                             (surveyTrialType.js). Pre-seeded so
#                                             addSurveyFrameToQCEframeList works
#                                             out of the box; the plugin is still
#                                             only LOADED if the session lists it
#                                             in plugins=.
.seedCoreQCEBtrialTypes <- function() {
  core <- list(
    key        = list(name = "key",        requiresKeymap = TRUE),
    textbox    = list(name = "textbox",    requiresKeymap = FALSE),
    numberline = list(name = "numberline", requiresKeymap = FALSE),
    angleline  = list(name = "angleline",  requiresKeymap = FALSE),
    survey     = list(name = "survey",     requiresKeymap = FALSE,
                      stimulusParam = "survey_json")
  )
  for (nm in names(core)) {
    if (!exists(nm, envir = .qcebTrialTypeRegistry, inherits = FALSE)) {
      assign(nm, core[[nm]], envir = .qcebTrialTypeRegistry)
    }
  }
  invisible(NULL)
}
