#' This  function is used to save the QCETrialStructureList to trialStructure.json
#'
#' Function that save the QCETrialStructureList to trialStructure.json. LEGACY FORMAT WRITER: the output is wrapped in a JavaScript assignment (`var trialStructure =`) rather than being plain JSON, and the output filename is fixed, so an engine that requires plain JSON cannot load what this writes. Use saveJsonFile(data, filename) for current engines.
#' @param QCETrialStructureList A list that specifies how the trials will be presented in the experiment.  This list specifies the selection of stimuli from stimFile.json, the ordering of stimuli, the blocking structure, etc.
#'
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
