#' This  function is used to create or modify a QCEframeList
#'
#' Function that creates or modifys an QCEframeList by adding frames to the list one at a time.
#' @param QCEsetInfoList A list that specifies the the setInfo information.  This includes, for each setName, : N (the number of trials per set) and selection (the method of selecting the scenarios from the QCEScenarioList).  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEsetInfoList that you are adding an effect to. DEFAULT = NULL
#' @param QCEScenarioList A list that specifies all the possible scenarios that participants might see. A scenario is, essentially, a trial.  It is composed of a series of frames, some potential response, and maybe feedback.  Included in each scenario are an output variable list to code in the datafile and a setName that is used for presentation rules.
#' @param setName A vector of strings that specify the name of the sets that you will be adding to the setInfo. The order of the strings is the order that they will be entered into the QCEsetInfoList.  It is that order that the sets will be presented in if randomizeSetOrder is set to "fixed" in the trialStructure file. DEFAULT = NULL.  If setName is NULL, then the function will create a setInfo list that is equivelent for all sets in order of validSetNames.
#' @param numberOfTrialsPerSet A vector of integers (one for each setName) to indicate the number of stimuli to select for each set. DEFAULT = 1.
#' @param selectionType  A string to indicate the selection method: "randomWithReplacement" indicates the stimuli  will be selected from all the stimuli with that setName in the stimFile randomly with replacement. "randomWithoutReplacement" indicates the stimuli  will be selected from all the stimuli with that setName in the stimFile randomly without replacement. "fixed indicates the stimuli  will be selected from all the stimuli with that setName in the stimFile starting with the first stimulus of that type and moving successively until the total needed is selected. The nect block will start where the previous block left off.  DEFAULT = "randomWithoutReplacement"
#' @param trigger Optional named list of trigger objects (each produced by buildQCETriggerList()), keyed by setName. Sets not named in this list get no trigger. Sets named with a NULL value also get no trigger. Recommended code range: 100-999 (3 digits). DEFAULT = NULL (no triggers on any set).
#''
#' @return the updated QCEsetInfoList
#' @keywords QCE QCEsetInfoList update add effect
#' @export
#' @examples
#' # Basic: no triggers
#' addSetToQCEsetInfoList (QCEsetInfoList, QCEScenarioList, setName = c("type1", "type2"),
#'   numberOfTrialsPerSet = c(10, 20), selectionType = "randomWithReplacement")
#'
#' # With fNIRS triggers on select sets (named list keyed by setName)
#' addSetToQCEsetInfoList (QCEsetInfoList, QCEScenarioList,
#'   setName = c("baseline", "task"),
#'   numberOfTrialsPerSet = c(10, 20),
#'   trigger = list(
#'     baseline = buildQCETriggerList(onset = 100, offset = 101),
#'     task     = buildQCETriggerList(onset = 200, offset = 201)
#'   ))

addSetToQCEsetInfoList <- function (QCEsetInfoList = NULL, QCEScenarioList = NULL, setName = NULL, numberOfTrialsPerSet = 1, selectionType = "randomWithoutReplacement", trigger = NULL) {

  tmpList <- NULL

  #check for valid types
  validSelectionTypes <- c("randomWithoutReplacement", "randomWithReplacement", "fixed")
  if(!(selectionType %in% validSelectionTypes)) {
    stop(paste("selectionType option must take on one of the following values: ", paste(validSelectionTypes, sep="", collapse=" ")))
  }

  # Helper: attach a trigger object to a set entry if one is declared for that setName
  attachTrigger <- function(entry, nm) {
    if (!is.null(trigger) && !is.null(trigger[[nm]])) {
      entry$trigger <- trigger[[nm]]
    }
    return(entry)
  }

  #check for valid types
  validSetNames <- getSetnamesFromScenarioList(QCEScenarioList)
  if(!is.null(setName)) {
    #create in order of setName
    if(any((setNames %in% validSetNames) == FALSE)) {
      stop(paste("setName option must take on one of the following values: ", paste(validSetNames, sep="", collapse=" ")))
    }
    if( length(numberOfTrialsPerSet) == 1 ) {
      for (i in 1:length(setName)) {
        tmpList[[setName[i]]] <- attachTrigger(list(N = numberOfTrialsPerSet, selection = selectionType), setName[i])
      }
    } else {
      if(length(setName) != length(numberOfTrialsPerSet)) {
          stop("Either the number of setNames must be equal to the number of numberOfTrialsPerSet or the numberOfTrialsPerSet must be a single integer that is applied to all sets")
        }
        #if setName is not NULL and equal in number to numberOfTrialsPerSet, then create a setInfo list that is equivelent for all sets
        for (i in 1:length(setName)) {
          tmpList[[setName[i]]] <- attachTrigger(list(N = numberOfTrialsPerSet[i], selection = selectionType), setName[i])
      }
    }
  } else {
    #if setName is NULL, then create a setInfo list that is equivelent for all sets in order of validSetNames
    for (i in 1:length(validSetNames)) {
      tmpList[[validSetNames[i]]] <- attachTrigger(list(N = numberOfTrialsPerSet, selection = selectionType), validSetNames[i])
    }
  }

  if(is.null(QCEsetInfoList)) {
    QCEsetInfoList <- tmpList
  } else {
    QCEsetInfoList <- c(QCEsetInfoList,tmpList)
  }

  return(QCEsetInfoList)

}
