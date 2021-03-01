#' This  function is used to create or modify a QCEScenarioList
#'
#' Function that creates or modifys an QCEScenarioList by adding frames to the list one at a time.
#' @param QCEScenarioList A list that specifies all the possible scenarios that participants might see. A scenario is, essentially, a trial.  It is composed of a series of frames, some potential response, and maybe feedback.  Included in each scenario are an output variable list to code in the datafile and a setName that is used for presentation rules (see trialStructure.json).  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEScenarioList that you are adding an effect to. DEFAULT = NULL
#' @param QCEframeList A list that specifies the frames to show a participant in a single scenario.  These frames are presented in succession: 1, 2, ... N.
#' @param QCEoutvariableList A list of variable names and their contents that will be output in the datafile for this trial.  DEFAULT = NULL.
#' @param setName A string that specifies the name of the set that this scenario belongs to.  Set names are used for selecting scenarios to show participants.  The rules are set in trialStructure.json.
#''
#' @return the updated QCEScenarioList
#' @keywords QCE QCEScenarioList update add scenario
#' @export
#' @examples addScenarioToQCEscenarioList (QCEScenarioList, myFrameList, myFeebackList, MyOutputVariables, "ponys")


addScenarioToQCEscenarioList <- function(QCEScenarioList, QCEframeList, QCEfeebackList, QCEoutvariableList, setName) {

  tmpList <- list(frame = QCEframeList, feedback = QCEfeebackList, outputVariables = QCEoutvariableList, set = setName)

  if(is.null(QCEScenarioList)) {
    numList <- 0
  } else {
    numList <- length(QCEScenarioList)
  }

  QCEScenarioList[[as.name(numList + 1)]] <- tmpList

  return(QCEScenarioList)

}
