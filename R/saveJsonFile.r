#' This function writes any QCEB configuration list to a JSON file
#'
#' Function that serializes a QCEB configuration list to plain JSON and writes it to a named file. This is the writer for every configuration file a build produces -- Stimfile, Tsfile, group dbfile, experiment dbfile and expInfo alike.
#' @param data The list to serialize. Any QCEB configuration list.
#' @param filename A string giving the path of the file to write. The file is overwritten.
#'
#' @return the json data
#' @keywords QCE save json
#' @export
#' @examples saveJsonFile (myQCEdbFileList, "expDBfile.json")

saveJsonFile <- function (data, filename) {

  #convert the list to a json file and write it out.
  jsonData <- jsonlite::toJSON(data, pretty=T)
  write(jsonData, filename, append = F)

  return(jsonData)
}
