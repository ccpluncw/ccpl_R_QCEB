#' This  function is used to save the QCEdbFileList to dbfile.json
#'
#' Function that save the QCEdbFileList to dbfile.json.
#' @param myQCEScenarioList A list that specifies the experiment level options of QCEB.
#''
#' @return the json data
#' @keywords QCE QCEdbFileList save dbFile
#' @export
#' @examples saveDbFile (myQCEdbFileList)

saveJsonFile <- function (data, filename) {

  #convert the list to a json file and write it out.
  jsonData <- jsonlite::toJSON(data, pretty=T)
  write(jsonData, filename, append = F)

  return(jsonData)
}
