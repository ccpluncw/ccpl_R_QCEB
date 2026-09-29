#' This function is used to write the preload manifest to preloadFile.json
#'
#' Function that writes the list of image, video and audio files the experiment should preload to preloadFile.json in the working directory. The files of any stimulus sets passed in \code{stimSets} are added by media type, as their stimulus-endpoint addresses; a set drawn at random per participant preloads every item that could be drawn. Text sets add nothing.
#' @param imageFileArray An array of the image filenames (plus paths) that need to be preloaded. Default \code{NULL}.
#' @param videoFileArray An array of the video filenames (plus paths) that need to be preloaded. Default \code{NULL}.
#' @param audioFileArray An array of the audio filenames (plus paths) that need to be preloaded. Default \code{NULL}.
#' @param stimSets A stimulus-set reference from \code{buildQCEstimSetRef}, or a list of them, whose files are preloaded too. Default \code{NULL}.
#'
#' @return the json data
#' @keywords QCE preload images save
#' @export
#' @examples savePreloadFiles (imageFileArray, videoFileArray, audioFileArray)

savePreloadFiles <- function (imageFileArray = NULL, videoFileArray = NULL, audioFileArray = NULL, stimSets = NULL) {

  #a single reference is a list with an items element
  if (!is.null(stimSets) && !is.null(stimSets$items)) {
    stimSets <- list(stimSets)
  }
  for (ref in stimSets) {
    if (!is.list(ref) || is.null(ref$items) || !isSingleString(ref$stimSet)) {
      stop("stimSets must be a reference from buildQCEstimSetRef(), or a list of them.")
    }
    if (identical(ref$kind, "text")) next
    for (it in ref$items) {
      url <- paste0("stimFile.php?set=", ref$stimSet, "&id=", it$id)
      family <- sub("/.*$", "", it$mediaType)
      if (family == "image") imageFileArray <- c(imageFileArray, url)
      if (family == "video") videoFileArray <- c(videoFileArray, url)
      if (family == "audio") audioFileArray <- c(audioFileArray, url)
    }
  }

  #convert the list to a json file and write it out.
  prFiles <- list(images = imageFileArray, video = videoFileArray, audio = audioFileArray)
  jsonData <- jsonlite::toJSON(prFiles, pretty=T)
#  write("var preloadFiles =", "preloadFile.json")
  write(jsonData, "preloadFile.json", append = F)

  return(jsonData)
}
