#' This  function is used to save the QCEdbFileList to dbfile.json
#'
#' Function that save the QCEdbFileList to dbfile.json. LEGACY FORMAT WRITER: the output is wrapped in a JavaScript assignment (`var dbInfo =`) rather than being plain JSON, and the output filename is fixed, so an engine that requires plain JSON cannot load what this writes. Use saveJsonFile(data, filename) for current engines.
#' @param QCEdbFileList A list that specifies the experiment- or group-level options of QCEB.
#'
#' @return the json data
#' @keywords QCE QCEdbFileList save dbFile
#' @export
#' @examples saveDbFile (myQCEdbFileList)

saveDbFile <- function (QCEdbFileList) {

  #convert the list to a json file and write it out.
  jsonData <- jsonlite::toJSON(QCEdbFileList, pretty=T)
  write("var dbInfo =", "dbFile.json")
  write(jsonData, "dbFile.json", append = T)

  return(jsonData)
}
