#' This  function is used to save the QCEdbFileList to dbfile.json
#'
#' Function that save the QCEdbFileList to dbfile.json.
#' @param imageFileArray An array of the image filenames (plus paths) that need to be preloaded.
#''
#' @return the json data
#' @keywords QCE preload images save
#' @export
#' @examples savePreloadImages (imageFileArray)

savePreloadImages <- function (imageFileArray) {

  #convert the list to a json file and write it out.
  prImages <- list(images = imageFileArray)
  jsonData <- jsonlite::toJSON(prImages, pretty=T)
  write("var preloadImages =", "preloadFile.json")
  write(jsonData, "preloadFile.json", append = T)

  return(jsonData)
}
