#' Register a trialType so QCEB frame builders will accept it
#'
#' QCEB mirrors, on the R side, the engine's trialType registry
#' (trialTypeRegistry.js). \code{addFrameToQCEframeList} validates its
#' \code{trialType} argument against this registry instead of a hard-coded list,
#' so adding support for a new jsPsych plugin in your experiment is a matter of
#' registering its trialType name here -- no edit to the QCEB core is needed.
#'
#' The CORE types (\code{"key"}, \code{"textbox"}, \code{"numberline"},
#' \code{"angleline"}) and the bundled \code{"survey"} and \code{"mcKeys"} plugins
#' are pre-registered automatically, so you only call this for additional
#' third-party / custom plugins (e.g. a Cyberball plugin).
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
#'   \code{stimulusParam}, \code{requiresKeymap}, \code{forceResp}). Stored
#'   verbatim for introspection. The registry entry is an OPEN object, exactly
#'   like the engine's, so plugins can carry extra metadata without a QCEB
#'   change. Two fields are acted on. \code{usesPointer = TRUE} declares that the
#'   participant answers this type with the mouse, so a frame that does not state
#'   \code{cursorVisible} gets the pointer shown, and one that sets it to FALSE is
#'   warned about. Omit it for a keyboard- or text-driven plugin, whose pointer is
#'   only a distractor. \code{forceResp = TRUE} declares that the
#'   plugin always gives the participant a way to respond -- a mouse, a text
#'   field, a submit button -- independently of the frame's \code{choices}. Such
#'   a frame is exempt from the check that a frame with no time limit still has
#'   some way to end. Omit it (the default) for keyboard-driven types, whose
#'   only exit is a key listed in \code{choices}.
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

#true when the type's plugin always lets the participant respond
.qcebTrialTypeForcesResponse <- function(name) {
  .seedCoreQCEBtrialTypes()
  if (!isSingleString(name) ||
      !exists(name, envir = .qcebTrialTypeRegistry, inherits = FALSE)) {
    return(FALSE)
  }
  isTRUE(get(name, envir = .qcebTrialTypeRegistry, inherits = FALSE)$forceResp)
}

#true when the type needs the pointer on screen, unregistered means FALSE
.qcebTrialTypeUsesPointer <- function(name) {
  .seedCoreQCEBtrialTypes()
  if (!isSingleString(name) ||
      !exists(name, envir = .qcebTrialTypeRegistry, inherits = FALSE)) {
    return(FALSE)
  }
  isTRUE(get(name, envir = .qcebTrialTypeRegistry, inherits = FALSE)$usesPointer)
}

#output columns the type declares, NULL when it never said
.qcebTrialTypeOutputColumns <- function(name) {
  .seedCoreQCEBtrialTypes()
  if (!isSingleString(name) ||
      !exists(name, envir = .qcebTrialTypeRegistry, inherits = FALSE)) {
    return(NULL)
  }
  cols <- get(name, envir = .qcebTrialTypeRegistry, inherits = FALSE)$outputColumns
  if (is.null(cols)) return(NULL)
  as.character(cols)
}

#registry store, created when the namespace loads and seeded lazily
.qcebTrialTypeRegistry <- new.env(parent = emptyenv())

#seed the built-in types if absent
.seedCoreQCEBtrialTypes <- function() {
  #forceResp and usesPointer mirror the engine, survey columns come from models
  core <- list(
    key        = list(name = "key",        requiresKeymap = TRUE,
                      outputColumns = c("Key", "FeedBack", "Response")),
    textbox    = list(name = "textbox",    requiresKeymap = FALSE, forceResp = TRUE,
                      outputColumns = c("Key", "FeedBack", "Response")),
    numberline = list(name = "numberline", requiresKeymap = FALSE, forceResp = TRUE,
                      usesPointer = TRUE,
                      outputColumns = c("Key", "FeedBack", "Response", "Stimulus")),
    angleline  = list(name = "angleline",  requiresKeymap = FALSE, forceResp = TRUE,
                      usesPointer = TRUE,
                      outputColumns = c("Key", "FeedBack", "Response", "Stimulus")),
    survey     = list(name = "survey",     requiresKeymap = FALSE, forceResp = TRUE,
                      usesPointer = TRUE,
                      stimulusParam = "survey_json"),
    mcKeys     = list(name = "mcKeys",     requiresKeymap = FALSE, forceResp = TRUE,
                      stimulusParam = "mc_spec",
                      outputColumns = c("qid", "shownOrder", "pressedKey", "selectedLabel",
                                        "selectedValue", "correctValue", "correct", "timedOut"))
  )
  for (nm in names(core)) {
    if (!exists(nm, envir = .qcebTrialTypeRegistry, inherits = FALSE)) {
      assign(nm, core[[nm]], envir = .qcebTrialTypeRegistry)
    }
  }
  invisible(NULL)
}
