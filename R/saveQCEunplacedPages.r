#' Declare the pages a study shows without a placement
#'
#' Writes \code{unplacedPages.json}, the list of a study's pages that no page placement, card placement or configuration file names -- a page a hook opens (a debrief chosen by condition) or a page another page links to. A platform that sets aside pages nothing uses keeps every page this list names. The engine does not read the file.
#' @param pages A character vector of page names, each a file name in the study's directory, with or without its \code{.html} extension, for example \code{c("debrief_full", "debrief_short")}.
#' @param dir A single string naming the directory to write into: the one the configuration files go to. Default \code{"."}.
#'
#' @return Invisibly, the path written.
#' @keywords QCE pages page unplaced hook declare
#' @export
#' @examples
#' saveQCEunplacedPages(c("debrief_full", "debrief_short"), dir = tempdir())

saveQCEunplacedPages <- function(pages, dir = ".") {

  if (!is.character(pages) || length(pages) == 0 || any(is.na(pages)) || any(pages == "")) {
    stop("pages must be a character vector of page names, such as c(\"debrief_full\", \"debrief_short\").")
  }
  if (any(grepl("[/\\\\]", pages))) {
    stop("pages names a path (", paste(pages[grepl("[/\\\\]", pages)], collapse = ", "),
         "); give each page's file name only, as it stands in the study's directory.")
  }
  if (!isSingleString(dir)) {
    stop("dir must be a single string naming a directory.")
  }
  #the list is kept as names without the extension
  names <- unique(sub("\\.html?$", "", pages, ignore.case = TRUE))
  path <- file.path(dir, "unplacedPages.json")
  writeLines(jsonlite::toJSON(list(pages = names), auto_unbox = FALSE, pretty = TRUE), path)
  invisible(path)
}
