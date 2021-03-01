#' This  function is used to get all the set names from a QCEScenarioList
#'
#' Function that gets all the set names from a QCEScenarioList.
#'
#' @param QCEScenarioList A list that specifies all the possible scenarios that participants might see. A scenario is, essentially, a trial.  It is composed of a series of frames, some potential response, and maybe feedback.  Included in each scenario are an output variable list to code in the datafile and a setName that is used for presentation rules (see trialStructure.json).
#''
#' @return  a vector containing setnames
#' @keywords QCE QCEScenarioList setnames get
#' @export
#' @examples getSetnamesFromScenarioList (myQCEScenarioList)

getSetnamesFromScenarioList <- function (QCEScenarioList = NULL) {

  if(is.null(QCEScenarioList)) {
    stop("QCEScenarioList must not be NULL")
  }
  setNameVector <- sapply(QCEScenarioList, `[[`, "set", simplify = "array", USE.NAMES = T)
  namesOfSets <- unique(setNameVector)

  return(namesOfSets)
}
