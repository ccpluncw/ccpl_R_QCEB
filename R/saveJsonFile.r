#' This function writes any QCEB configuration list to a JSON file
#'
#' Function that serializes a QCEB configuration list to plain JSON and writes it to a named file. This is the writer for every configuration file a build produces -- Stimfile, Tsfile, group dbfile, experiment dbfile and expInfo alike. Each value is written in the form the QCEP settings description gives its key: a setting that takes one value is written bare (\code{"x"}), a setting that takes a list is written as a list also when it holds one item (\code{["d"]}), a setting that is not set (\code{NULL}, \code{NA}, \code{""} or an empty vector) is left out of the file, except that empty text in a setting whose value the engine copies into a column of the data file (a name or a label) is written \code{""} and saved as an empty cell, and numbers are written at full precision. A frame whose \code{choices} is not set is written \code{"NO_KEYS"}. A value the description does not list -- a study's own setting, an output variable, a plugin parameter -- is written whole: one value bare, several values as a list, a value wrapped in \code{I()} as a list also when it holds one item, and \code{NA} as \code{null}. The experiment settings file carries \code{settingsForm: 2}. A setting that takes one value but is given several stops the build with a message naming it.
#' @param data The list to serialize. Any QCEB configuration list.
#' @param filename A string giving the path of the file to write. The file is overwritten.
#' @param fileKind A single string naming which settings file \code{data} is: one of \code{"expInfo"}, \code{"expDBfile"}, \code{"groupSettings"}, \code{"trialStructure"}, \code{"stimulus"}, \code{"preload"}, \code{"pages"}, \code{"pageSidecar"}, \code{"cards"}, \code{"cardSidecar"}, \code{"localPluginManifest"}, \code{"unplacedPages"} or \code{"stimSetList"}. \code{NULL} tells it from the file name (\code{expDBfile.json}, \code{expInfo.json}, \code{*.page.json} and the like) or else from the list's own shape, and warns when it cannot. Default \code{NULL}.
#'
#' @return the json data
#' @keywords QCE save json
#' @export
#' @examples saveJsonFile (myQCEdbFileList, "expDBfile.json")

saveJsonFile <- function (data, filename, fileKind = NULL) {

  if (!is.null(fileKind) && !(isSingleString(fileKind) && fileKind %in% names(.qcebFileLabels))) {
    stop("saveJsonFile: fileKind must be NULL or one of ", paste(names(.qcebFileLabels), collapse = ", "), ".")
  }
  .qcebWriteSettings(data, filename, fileKind = fileKind, fn = "saveJsonFile")
}
