#' This function is used to write the preload manifest to preloadFile.json
#'
#' Function that writes the list of image, video and audio files the experiment should preload to preloadFile.json in the working directory. Each list is written as a list, also when it holds one file, and a kind with nothing to preload is left out of the file. A path is written as given, so it is the address the page fetches: a stimulus set's \code{path} column, or a file of the study's own.
#' @param imageFileArray An array of the image filenames (plus paths) that need to be preloaded. Default \code{NULL}.
#' @param videoFileArray An array of the video filenames (plus paths) that need to be preloaded. Default \code{NULL}.
#' @param audioFileArray An array of the audio filenames (plus paths) that need to be preloaded. Default \code{NULL}.
#'
#' @return the json data
#' @keywords QCE preload images save
#' @export
#' @examples savePreloadFiles (imageFileArray, videoFileArray, audioFileArray)

savePreloadFiles <- function (imageFileArray = NULL, videoFileArray = NULL, audioFileArray = NULL) {

  prFiles <- list(images = as.character(imageFileArray), video = as.character(videoFileArray),
                  audio = as.character(audioFileArray))
  .qcebWriteSettings(prFiles, "preloadFile.json", fileKind = "preload", fn = "savePreloadFiles")
}
