#' This  function is used to save the QCETrialStructureList to trialStructure.json
#'
#' Function that save the QCETrialStructureList to trialStructure.json.
#' @param QCETrialStructureList A list that specifies how the trials will be presented in the experiment.  This list specifies the selection of stimuli from stimFile.json, the ordering of stimuli, the blocking structure, etc.
#''
#' @return the json data
#' @keywords QCE QCETrialStructureList
#' @export
#' @examples saveTSFile (myQCETrialStructureList)

saveTSFile <- function (QCETrialStructureList) {

  filename <- "trialStructure.json"

  #convert the list to a json file and write it out.
  jsonData <- jsonlite::toJSON(QCETrialStructureList, pretty=T)

  write("var trialStructure =", filename)
  write(jsonData, filename, append = T)

  return(jsonData)
}
