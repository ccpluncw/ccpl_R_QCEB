#' This function is used to write an image-only preload manifest to preloadFile.json
#'
#' Function that writes the list of image files the experiment should preload to preloadFile.json in the working directory. LEGACY FORMAT WRITER: the output is wrapped in a JavaScript assignment (`var preloadImages =`) rather than being plain JSON, so an engine that requires plain JSON cannot load it. Use savePreloadFiles() for current engines.
#' @param imageFileArray An array of the image filenames (plus paths) that need to be preloaded.
#'
#' @return the json data
#' @keywords QCE preload images save
#' @export
#' @examples savePreloadImages (imageFileArray)

savePreloadImages <- function (imageFileArray) {

  prImages <- list(images = imageFileArray)
  .qcebWriteSettings(prImages, "preloadFile.json", fileKind = "preload",
                     fn = "savePreloadImages", prefix = "var preloadImages =")
}
