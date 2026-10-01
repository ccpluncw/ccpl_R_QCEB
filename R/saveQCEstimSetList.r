#' Write a stimulus set's items to a file a hook can load
#'
#' Writes a JSON file listing the items of a stimulus-set reference from \code{buildQCEstimSetRef}, for a custom hook that needs the whole list rather than its own trial's item: to score free recall against it, or to draw a foil for a recognition test. The file holds \code{stimSetList}, with the set's \code{set} name, \code{version} and \code{kind}, and \code{items}, one per item of the reference, each with its \code{id} and \code{attrs} (every attribute, or those named in \code{attributes}) and, for a text item, \code{text}, its words, \code{chars}, their length in characters, and \code{html}, the item as a frame's text shows it, escaped inside \code{<span dir='auto' style='white-space:pre-wrap'>}. A platform that gives the build a copy of the set whose words are placeholders puts the words into \code{text}, \code{html} and each text value after the build. A hook loads the file with \code{fetch()} in the trial that uses it, compares with \code{text} and shows an item with \code{html}.
#' @param stimSetRef A reference from \code{buildQCEstimSetRef}. Every item it selects is listed, whatever its \code{n}.
#' @param fileName A single string naming the file: a file name ending in \code{.json}, with no directory.
#' @param attributes A character vector naming the attributes each item carries in the file, or \code{NULL} for all of them. Default \code{NULL}.
#' @param dir A single string naming the directory to write into: the one the configuration files go to, \code{OUT_DIR} in a builder script. Default \code{"."}.
#'
#' @return Invisibly, the path written.
#' @keywords QCE stimulus set list hook recall recognition foil
#' @export
#' @examples
#' \dontrun{
#' words <- buildQCEstimSetRef("words")
#' saveQCEstimSetList(words, "wordList.json", dir = "output")
#' }

saveQCEstimSetList <- function(stimSetRef, fileName, attributes = NULL, dir = ".") {

  if (!is.list(stimSetRef) || is.null(stimSetRef$items) || is.null(stimSetRef$kind) ||
      is.null(stimSetRef$version) || !isSingleString(stimSetRef$stimSet)) {
    stop("stimSetRef must be a reference from buildQCEstimSetRef().")
  }
  if (!isSingleString(fileName) || !grepl("^[^/\\\\]+\\.json$", fileName)) {
    stop("fileName must be a single file name ending in .json, with no directory, such as \"wordList.json\".")
  }
  if (!isSingleString(dir)) {
    stop("dir must be a single string naming a directory.")
  }
  attrNames <- vapply(stimSetRef$attributes, function(a) a$name, "")
  if (is.null(attributes)) {
    attributes <- attrNames
  } else if (!is.character(attributes) || any(is.na(attributes))) {
    stop("attributes must be a character vector of attribute names, or NULL for all of them.")
  }
  unknown <- setdiff(attributes, attrNames)
  if (length(unknown)) {
    stop("stimulus set \"", stimSetRef$stimSet, "\" has no attribute \"", unknown[1], "\"; its attributes are ",
         if (length(attrNames)) paste(attrNames, collapse = ", ") else "none", ".")
  }
  isText <- identical(stimSetRef$kind, "text")

  items <- lapply(stimSetRef$items, function(it) {
    out <- list(id = it$id)
    if (isText) {
      out$text <- it$text
      out$chars <- if (is.null(it$chars)) nchar(it$text) else as.integer(it$chars)
      out$html <- .stimWordsIn(list(kind = "text", name = ""), it$text)
    }
    keep <- intersect(attributes, names(it$attrs))
    #an empty named list is written as {}
    out$attrs <- if (length(keep)) it$attrs[keep] else stats::setNames(list(), character(0))
    out
  })
  doc <- list(stimSetList = list(set = stimSetRef$stimSet, version = as.character(stimSetRef$version),
                                 kind = stimSetRef$kind),
              items = items)
  path <- file.path(dir, fileName)
  writeLines(enc2utf8(as.character(jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE, digits = NA))), path, useBytes = TRUE)
  invisible(path)
}
