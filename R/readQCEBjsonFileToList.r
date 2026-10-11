#' Read a QCEB JSON file into a list
#'
#' Reads a JSON file and returns its contents as \code{jsonlite::fromJSON}
#' returns them. The file may be plain JSON, as every file QCEB writes is, or a
#' file that declares a JavaScript variable (\code{var scenarios =}) before the
#' JSON, on the same line or the line before it; the declaration and a trailing
#' semicolon are left out of the read.
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
  txt <- paste(readLines(filename, warn = FALSE), collapse = "")
  #a var x = file declares its variable before the json
  if (grepl("^\\s*var\\s", txt)) {
    txt <- sub("^\\s*var\\s+[A-Za-z_$][A-Za-z0-9_$]*\\s*=\\s*", "", txt)
    txt <- sub(";\\s*$", "", txt)
  }
  jsonlite::fromJSON(txt)
}
