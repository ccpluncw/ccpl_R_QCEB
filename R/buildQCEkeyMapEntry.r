#' Build a single named-keyMap entry for use on a QCEdbfile
#'
#' Phase 3.5: assembles one entry in `mydB.keyMaps` -- the dictionary the
#' engine reads at session init to populate the named-keyMap registry. Each
#' entry pairs a label-to-keys mapping with optional shuffle / reminder /
#' instruction-file settings. Blocks reference the entry by name via the
#' new `keyMapName` argument on addBlockToQCETrialStructureList.
#'
#' Why named keyMaps exist: the locked Phase 3.5 design (Decision A) keeps
#' the shuffle of randomized keyMaps at the SESSION scope so multiple blocks
#' pointing at the same name share the same shuffled instance (within-subject
#' consistency). Declaring keyMaps on individual blocks would mean each block
#' shuffles independently -- two blocks with the same "yesNo" map randomized
#' independently would show Yes=y in one and Yes=n in the other. Named
#' keyMaps on the dbfile solve that.
#'
#' Pair this with addKeyMapToDbfile to register the entry, or pass entries
#' as the `keyMaps` argument to buildQCEgroupDbFile for one-shot construction.
#'
#' @param map Required QCEkeyMap (output of buildKeyMap or
#'   addKeyToKeyMap). The label-to-keys dictionary, e.g.,
#'   list(Yes = c('y','Y'), No = c('n','N')).
#' @param randomize Single boolean. When TRUE, the engine shuffles the
#'   label-to-keys mapping ONCE at session init -- every block that
#'   references this name sees the same shuffled instance. DEFAULT = FALSE.
#' @param presentAfterTrials Optional numeric vector. Block-relative
#'   scenario counts at which a keymap reminder fires (e.g., c(20, 50)
#'   fires after scenario 20 and scenario 50 of the block). Counter is
#'   per-keyMap, resets at block boundary (not iteration boundary), and
#'   increments only on scenarios containing a key-response frame.
#'   NULL means no reminders. DEFAULT = NULL.
#' @param instructionFile Optional single string -- filename of an HTML
#'   page describing this keyMap to the participant. The engine pushes
#'   this as a jsPsychExternalHtml trial at the first block that uses
#'   this keyMap (and at any subsequent block that switches TO this
#'   keyMap from a different one). The HTML file must contain a button
#'   with id="continue" (legacy convention; Phase 5.5 forms refactor
#'   will harmonize). When NULL, the engine generates a basic table from
#'   the map -- usable but limited. DEFAULT = NULL.
#''
#' @return A named-keyMap entry list of the shape consumed by
#'   addKeyMapToDbfile / buildQCEgroupDbFile's keyMaps argument.
#' @keywords QCE keyMap dbfile dynamic
#' @export
#' @examples
#' # Minimal -- map only, no randomization, no reminders
#' km <- buildKeyMap(data.frame(Yes = "y", No = "n"))
#' entry <- buildQCEkeyMapEntry(map = km)
#'
#' # With shuffle + reminders + custom HTML
#' entry <- buildQCEkeyMapEntry(
#'   map                = km,
#'   randomize          = TRUE,
#'   presentAfterTrials = c(20, 50),
#'   instructionFile    = "yesNo_keymap.html"
#' )

buildQCEkeyMapEntry <- function(map, randomize = FALSE,
                                 presentAfterTrials = NULL,
                                 instructionFile = NULL) {

  # map: required, non-empty named list (the QCEkeyMap shape)
  if (missing(map) || is.null(map) || !is.list(map) || length(map) == 0) {
    stop("map option is required and must be a non-empty QCEkeyMap ",
         "(use buildKeyMap or addKeyToKeyMap to construct one).")
  }

  # randomize: single boolean
  if (!is.logical(randomize) || length(randomize) != 1 || is.na(randomize)) {
    stop("randomize option must be a single boolean (TRUE or FALSE).")
  }

  # presentAfterTrials: optional numeric vector
  if (!is.null(presentAfterTrials)) {
    if (!is.numeric(presentAfterTrials) || any(is.na(presentAfterTrials))) {
      stop("presentAfterTrials option must be a numeric vector when present ",
           "(e.g., c(20, 50) fires a keymap reminder at scenario 20 and ",
           "scenario 50 of any block using this keyMap).")
    }
  }

  # instructionFile: optional non-empty .html filename
  if (!is.null(instructionFile)) {
    if (!isValidFilename(instructionFile, "html")) {
      stop("instructionFile option must be a single .html filename when present.")
    }
  }

  # Build the entry. Only include optional fields when set so the JSON shape
  # matches what the engine expects (NULL defaults omitted, byte-identical
  # to a hand-rolled minimal entry).
  entry <- list(map = map, randomize = randomize)
  if (!is.null(presentAfterTrials)) entry$presentAfterTrials <- presentAfterTrials
  if (!is.null(instructionFile))    entry$instructionFile    <- instructionFile

  return(entry)
}
