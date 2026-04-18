#' Build a trigger-object list for fNIRS event markers
#'
#' Creates a list in the unified shape used by QCEP engine v9 for fNIRS trigger
#' codes at any level (block, set, trial, or frame). The returned list is passed
#' to the `trigger` parameter of builder functions (addBlockToQCETrialStructureList,
#' addSetToQCEsetInfoList, addScenarioToQCEscenarioList, addFrameToQCEframeList).
#'
#' Returns NULL when all arguments are NULL, which signals "no trigger at this
#' level" to builder functions and results in no `trigger` key being emitted to
#' the JSON.
#'
#' Code range convention (by digit count, for LSL analysis readability):
#'   Block: 1-99         (1-2 digits)
#'   Set:   100-999      (3 digits)
#'   Trial: 1000-9999    (4 digits)
#'   Frame: 10000-99999  (5 digits)
#'
#' See tools/FNIRS_SETUP.md in the QCEP repo for full documentation.
#'
#' @param onset Integer — the trigger code sent to LSL when this event begins. NULL for no onset marker.
#' @param offset Integer — the trigger code sent to LSL when this event ends. NULL for no offset marker.
#' @param ... Additional named trigger types for extensibility (e.g., `response = 1234`). Any named integer is passed through.
#'
#' @return A list of the form `list(onset=X, offset=Y, ...)`, or NULL if all args are NULL.
#' @keywords QCE trigger fNIRS
#' @export
#' @examples
#' # Simple onset + offset pair
#' blockTrig <- buildQCETriggerList(onset = 10, offset = 11)
#'
#' # Onset only (no offset marker — valid for instantaneous events)
#' onsetOnly <- buildQCETriggerList(onset = 1000)
#'
#' # Extended with a custom marker type (extensibility — engine forwards any named integer)
#' custom <- buildQCETriggerList(onset = 10000, offset = 10001, response = 10002)

buildQCETriggerList <- function (onset = NULL, offset = NULL, ...) {
  extras <- list(...)
  tmpList <- list()
  if (!is.null(onset))  tmpList$onset  <- as.integer(onset)
  if (!is.null(offset)) tmpList$offset <- as.integer(offset)
  for (nm in names(extras)) {
    val <- extras[[nm]]
    if (!is.null(val)) tmpList[[nm]] <- as.integer(val)
  }
  if (length(tmpList) == 0) return(NULL)
  return(tmpList)
}
