#' Split a list cell of a data file into its items
#'
#' A study on engine 10 or later saves a value that is a list of several items
#' in one cell of its data file, the items joined by \code{;}. A \code{;} inside
#' an item is saved as \code{\\;}, and an item that is itself a list or an object
#' is saved as its JSON text. This function splits such a cell on every
#' \code{;} that has no backslash before it and turns each \code{\\;} back into
#' \code{;}. A list of one item is saved as that item alone, so a cell with no
#' \code{;} is returned as it is. To split a whole column, apply the function to
#' each cell, for example \code{lapply(dat$myList, splitListCell)}.
#' @param x One cell of a data file read with \code{read.delim}: a string, or a
#'   number or logical value \code{read.delim} has already converted.
#'
#' @return A character vector of the cell's items, in the order they were
#'   saved: \code{character(0)} for an empty cell (a list of none), the cell
#'   itself as a string when it holds no unescaped \code{;}, and
#'   \code{NA_character_} when \code{x} is \code{NA}. An item saved empty (a
#'   missing value in the list) is \code{""}.
#' @keywords data file list cell
#' @export
#' @examples
#' splitListCell("p;q")
#' splitListCell("a;b\\;c")
#' splitListCell("solo")
#' splitListCell("")
#' items <- splitListCell('{"a":1};{"b":"x\\;y"}')
#' lapply(items, jsonlite::fromJSON)

splitListCell <- function (x) {
  if (length(x) != 1) {
    stop("splitListCell: x must be one cell, not ", length(x),
         "; to split a column use lapply(column, splitListCell)", call. = FALSE)
  }
  if (is.na(x)) return(NA_character_)
  x <- as.character(x)
  if (x == "") return(character(0))
  items <- strsplit(x, "(?<!\\\\);", perl = TRUE)[[1]]
  #strsplit drops an empty last item
  if (grepl("(?<!\\\\);$", x, perl = TRUE)) items <- c(items, "")
  gsub("\\;", ";", items, fixed = TRUE)
}
