#' This  function is used to save the QCEdbFileList to dbfile.json
#'
#' Function that save the QCEdbFileList to dbfile.json.
#' @param imageFileArray An array of the image filenames (plus paths) that need to be preloaded.
#' @param videoFileArray An array of the video filenames (plus paths) that need to be preloaded.
#' @param audioFileArray An array of the audio filenames (plus paths) that need to be preloaded.
#''
#' @return the json data
#' @keywords QCE preload images save
#' @export
#' @examples savePreloadImages (imageFileArray)

savePreloadFiles <- function (imageFileArray = NULL, videoFileArray = NULL, audioFileArray = NULL) {

  #convert the list to a json file and write it out.
  prFiles <- list(images = imageFileArray, video = videoFileArray, audio = audioFileArray)
  jsonData <- jsonlite::toJSON(prFiles, pretty=T)
  write("var preloadFiles =", "preloadFile.json")
  write(jsonData, "preloadFile.json", append = T)

  return(jsonData)
}
