#' Read a QCEB JSON file into a list
#'
#' Reads a JSON file and returns its contents as \code{jsonlite::fromJSON}
#' returns them. The file may be plain JSON, as every file QCEB writes is, or a
#' file whose first line declares a JavaScript variable (\code{var scenarios =})
#' with the JSON on the lines after it; that first line is left out of the read.
#' To read a settings file back and save it again, use \code{readQCEjsonFile},
#' which keeps a list of one a list.
#' @param filename A string, the path of the JSON file.
#'
#' @return The file's contents: a named list for a JSON object, with arrays
#'   simplified to vectors, matrices or data frames as \code{jsonlite::fromJSON}
#'   simplifies them by default.
#' @keywords json file list
#' @export
#' @examples
#' f <- tempfile(fileext = ".json")
#' writeLines('{"expName": "demo", "saveDataEveryNTrials": 20}', f)
#' readQCEBjsonFileToList(f)

readQCEBjsonFileToList <- function (filename) {
  lines <- readLines(filename, warn = FALSE)
  #a var x = file declares its variable alone on the first line
  if (length(lines) && grepl("^\\s*var\\s", lines[1])) lines <- lines[-1]
  jsonlite::fromJSON(paste(lines, collapse = ""))
}
