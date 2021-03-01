#' This  function is used to read a QCEB json file and returns it in the form of a list
#'
#' Function that reads a QCEB json file and returns it in the form of a list
#' @param filename A string specifying the name of the QCEB json file.
#''
#' @return the json data in a list structure
#' @keywords json file list
#' @export
#' @examples readQCEBjsonFileToList ("stimFile.json")

readQCEBjsonFileToList <- function (filename) {


  out.df <- jsonlite::fromJSON(paste(readLines(filename)[-1], collapse=""))

  return(out.df)
}
