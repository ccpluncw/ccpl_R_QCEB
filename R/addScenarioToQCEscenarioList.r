#' This  function is used to create or modify a QCEScenarioList
#'
#' Function that creates or modifys an QCEScenarioList by adding scenarios to the list one at a time.
#' @param QCEScenarioList A list that specifies all the possible scenarios that participants might see. A scenario is, essentially, a trial.  It is composed of a series of frames, some potential response, and maybe feedback.  Included in each scenario are an output variable list to code in the datafile and a setName that is used for presentation rules (see trialStructure.json).  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEScenarioList that you are adding an effect to. DEFAULT = NULL
#' @param QCEframeList A list that specifies the frames to show a participant in a single scenario.  These frames are presented in succession: 1, 2, ... N.
#' @param QCEoutvariableList A list of variable names and their contents that will be output in the datafile for this trial.  DEFAULT = NULL.
#' @param setName A string that specifies the name of the set that this scenario belongs to.  Set names are used for selecting scenarios to show participants.  The rules are set in trialStructure.json.
#' @param trigger Optional list produced by buildQCETriggerList() specifying the fNIRS trigger codes that fire at this trial's boundaries -- onset fires on the scenario's first frame; offset fires on the response frame. NULL means no trial-level triggers. Recommended code range: 1000-9999 (4 digits). DEFAULT = NULL.
#' @param stimRef Optional single string -- a data-lookup tag for this scenario. When this scenario completes at runtime, its trial data is recorded under this tag in qceState.conditions.dataIndex. Other scenarios/sets/blocks with a `showIf` referencing this stimRef will read that data. Most scenarios don't need a stimRef; only tag the ones whose data drives downstream conditional logic. DEFAULT = NULL.
#' @param showIf Optional condition (output of buildQCEshowIfCondition or buildQCEshowIfCompound) that gates whether this scenario runs at all. Evaluated at trial entry -- if FALSE, the scenario is skipped entirely. NULL means always show. DEFAULT = NULL.
#''
#' @return the updated QCEScenarioList
#' @keywords QCE QCEScenarioList update add scenario
#' @export
#' @examples
#' # Basic
#' addScenarioToQCEscenarioList (QCEScenarioList, myFrameList, myFeebackList, MyOutputVariables, "ponys")
#'
#' # With fNIRS trial-level trigger
#' addScenarioToQCEscenarioList (QCEScenarioList, myFrameList, myFeebackList, MyOutputVariables,
#'   setName = "ponys",
#'   trigger = buildQCETriggerList(onset = 1000, offset = 1001))
#'
#' # With stimRef for downstream conditional gating
#' addScenarioToQCEscenarioList (QCEScenarioList, myFrameList, myFeebackList, MyOutputVariables,
#'   setName = "questions",
#'   stimRef = "smoker")
#'
#' # With showIf -- scenario only runs if a previous tagged scenario answered "Yes"
#' addScenarioToQCEscenarioList (QCEScenarioList, myFrameList, myFeebackList, MyOutputVariables,
#'   setName = "followUp",
#'   showIf  = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"))


addScenarioToQCEscenarioList <- function(QCEScenarioList, QCEframeList, QCEfeebackList, QCEoutvariableList, setName, trigger = NULL, stimRef = NULL, showIf = NULL) {

  if (!is.null(stimRef)) {
    if (!isSingleString(stimRef)) {
      stop("stimRef option must be a single string or NULL.")
    }
    # Warn if this stimRef is already used by another scenario in this list.
    # Engine semantic for duplicate stimRef is last-writer-wins in dataIndex,
    # which is almost always a bug rather than a feature.
    if (!is.null(QCEScenarioList)) {
      existingRefs <- vapply(QCEScenarioList, function(sc) {
        if (is.null(sc$stimRef)) NA_character_ else as.character(sc$stimRef)
      }, character(1))
      if (stimRef %in% existingRefs) {
        warning("stimRef '", stimRef, "' is already used by another scenario in this QCEScenarioList. ",
                "At runtime, dataIndex['", stimRef, "'] will reflect whichever scenario completed most recently. ",
                "If this is intentional (e.g., a pool of scenarios where exactly one is sampled), you can ignore this warning. ",
                "Otherwise, give each scenario a unique stimRef.")
      }
    }
  }

  if (!is.null(showIf)) {
    validateShowIfShape(showIf, "showIf")
  }

  tmpList <- list(frame = QCEframeList, feedback = QCEfeebackList, outputVariables = QCEoutvariableList, set = setName)
  if (!is.null(trigger)) {
    tmpList$trigger <- trigger
  }
  if (!is.null(stimRef)) {
    tmpList$stimRef <- stimRef
  }
  if (!is.null(showIf)) {
    tmpList$showIf <- showIf
  }

  if(is.null(QCEScenarioList)) {
    numList <- 0
  } else {
    numList <- length(QCEScenarioList)
  }

  QCEScenarioList[[as.name(numList + 1)]] <- tmpList

  return(QCEScenarioList)

}
