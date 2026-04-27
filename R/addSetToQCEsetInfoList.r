#' Add a single set's configuration to a QCEsetInfoList
#'
#' Adds one set entry to a QCEsetInfoList. Call this function once per set
#' you want in the trial structure.
#'
#' Note: prior to Phase 2 this function accepted a vector of setNames in
#' a single call. That multi-set form is now deprecated -- see
#' addSetToQCEsetInfoListOldDep for the legacy behavior.
#'
#' @param QCEsetInfoList Existing QCEsetInfoList to append to. Pass NULL to start a new one. DEFAULT = NULL.
#' @param QCEScenarioList The QCEScenarioList that declares which setNames are valid.
#' @param setName A single string -- the name of the set being added. Must match a setName declared on at least one scenario in QCEScenarioList.
#' @param numberOfTrialsPerSet Single integer -- number of stimuli to select for this set. DEFAULT = 1.
#' @param selectionType String -- "randomWithoutReplacement" (default), "randomWithReplacement", or "fixed". See documentation for semantics.
#' @param trigger Optional list produced by buildQCETriggerList() specifying fNIRS trigger codes for this set's onset/offset. NULL means no triggers on this set. DEFAULT = NULL.
#' @param showIf Optional condition (output of buildQCEshowIfCondition or buildQCEshowIfCompound) that gates whether this set runs. Evaluated at set entry -- if FALSE, the entire set is skipped. NULL means always run. DEFAULT = NULL.
#''
#' @return The updated QCEsetInfoList, with the new set appended.
#' @keywords QCE QCEsetInfoList update add set
#' @export
#' @examples
#' # Build a setInfo with two sets, one trigger-gated and one showIf-gated.
#' sl <- addSetToQCEsetInfoList(NULL, scenarios, "baseline", 10)
#' sl <- addSetToQCEsetInfoList(sl, scenarios, "task", 20,
#'   trigger = buildQCETriggerList(onset = 200, offset = 201))
#' sl <- addSetToQCEsetInfoList(sl, scenarios, "smokingFollowUp", 15,
#'   showIf = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"))


addSetToQCEsetInfoList <- function(QCEsetInfoList = NULL, QCEScenarioList = NULL, setName, numberOfTrialsPerSet = 1, selectionType = "randomWithoutReplacement", trigger = NULL, showIf = NULL) {

  # Hard error with migration guidance if researcher passes the old vector form
  if (missing(setName) || is.null(setName) || length(setName) != 1 || !is.character(setName)) {
    stop("setName option must be a single string. ",
         "If you previously called this function with a vector of setNames, ",
         "the multi-set form has been deprecated -- call this function once per set instead. ",
         "If you need the old behavior temporarily, use addSetToQCEsetInfoListOldDep.")
  }

  # Validate selectionType
  validSelectionTypes <- c("randomWithoutReplacement", "randomWithReplacement", "fixed")
  if (!(selectionType %in% validSelectionTypes)) {
    stop(paste("selectionType option must take on one of the following values: ",
               paste(validSelectionTypes, collapse = " ")))
  }

  # Validate setName against QCEScenarioList
  validSetNames <- getSetnamesFromScenarioList(QCEScenarioList)
  if (!(setName %in% validSetNames)) {
    stop(paste0("setName '", setName, "' is not declared on any scenario in QCEScenarioList. ",
                "Valid setNames are: ", paste(validSetNames, collapse = ", ")))
  }

  # Validate numberOfTrialsPerSet
  if (!isSingleNumeric(numberOfTrialsPerSet)) {
    stop("numberOfTrialsPerSet option must be a single integer.")
  }

  # Validate showIf shape
  if (!is.null(showIf)) {
    validateShowIfShape(showIf, "showIf")
  }

  # Build the entry
  entry <- list(N = numberOfTrialsPerSet, selection = selectionType)
  if (!is.null(trigger)) {
    entry$trigger <- trigger
  }
  if (!is.null(showIf)) {
    entry$showIf <- showIf
  }

  # Append to QCEsetInfoList
  if (is.null(QCEsetInfoList)) {
    QCEsetInfoList <- list()
  }
  QCEsetInfoList[[setName]] <- entry

  return(QCEsetInfoList)
}
