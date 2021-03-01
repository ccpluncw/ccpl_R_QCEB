#' This  function is used to save the QCEScenarioList to stimFile.json
#'
#' Function that save the QCEScenarioList to stimFile.json.
#' @param QCEScenarioList A list that specifies all the possible scenarios that participants might see. A scenario is, essentially, a trial.  It is composed of a series of frames, some potential response, and maybe feedback.  Included in each scenario are an output variable list to code in the datafile and a setName that is used for presentation rules (see trialStructure.json).
#''
#' @return the json data
#' @keywords QCE QCEScenarioList update add scenario
#' @export
#' @examples saveStimFile (myQCEScenarioList)

saveStimFile <- function (QCEScenarioList) {

  #convert the list to a json file and write it out.
  jsonData <- jsonlite::toJSON(QCEScenarioList, pretty=T)
  write("var scenarios =", "stimFile.json")
  write(jsonData, "stimFile.json", append = T)

  return(jsonData)
}
