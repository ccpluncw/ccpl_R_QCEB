#' Split a list cell of a data file into its items
#'
#' A study on engine 10 or later saves a value that is a list of several items
#' in one cell of its data file, the items joined by \code{;}. Inside an item a
#' \code{\\} is saved as \code{\\\\} and then a \code{;} as \code{\\;}, and an
#' item that is itself a list or an object is saved as its JSON text. This
#' function splits such a cell on every \code{;} that follows an even number of
#' backslashes (none included) and reads each \code{\\\\} back as \code{\\} and
#' each \code{\\;} as \code{;}; any other backslash, such as the \code{\\t} a
#' tab is saved as, is kept. A list of one item is saved as that item alone,
#' never escaped, so a cell with no \code{;} is returned as it is. To split a
#' whole column, apply the function to each cell, for example
#' \code{lapply(dat$myList, splitListCell)}. A column \code{read.delim} has
#' already converted gives a number to 15 significant digits, and a column
#' whose every cell is empty (lists of none) reads back as \code{NA}, so read a
#' list column as text, for example with
#' \code{colClasses = c(myList = "character")}.
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
#' splitListCell("C:\\\\dir\\\\;x")
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
  #an escape pair matches as two characters, so a lone semicolon splits
  m <- gregexpr("\\\\[\\s\\S]|;", x, perl = TRUE)[[1]]
  cuts <- m[attr(m, "match.length") == 1L]
  items <- substring(x, c(1L, cuts + 1L), c(cuts - 1L, nchar(x)))
  gsub("\\\\([\\\\;])", "\\1", items, perl = TRUE)
}
