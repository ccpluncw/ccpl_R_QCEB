#' This function tests whether a variable contains a single string
#'
#' Function tests whether a variable contains a single string.
#' @param input A variable to be tested.
#''
#' @return a boolean (TRUE or FALSE) identifying whether the input contains a single string (TRUE) or not (FALSE)
#' @keywords is string single
#' @export
#' @examples isSingleString ("hello")

isSingleString <- function(input) {
    is.character(input) & length(input) == 1
}


#' This function tests whether a variable contains a single numeric
#'
#' Function tests whether a variable contains a single numeric.
#' @param input A variable to be tested.
#''
#' @return a boolean (TRUE or FALSE) identifying whether the input contains a single numeric (TRUE) or not (FALSE)
#' @keywords is numeric single
#' @export
#' @examples isSingleNumeric (100)

isSingleNumeric <- function(input) {
    is.numeric(input) & length(input) == 1
}

#' This function tests whether a variable contains a valid color
#'
#' Function tests whether a variable contains a valid color.
#' @param input A variable to be tested.
#''
#' @return a boolean (TRUE or FALSE) identifying whether the input contains a valid color (TRUE) or not (FALSE)
#' @keywords is color valid
#' @export
#' @examples isColor ('#000000')

isColor <- function(input)
{
  res <- try(col2rgb(input),silent=TRUE)
  return(!"try-error"%in%class(res))
}

#' This function tests whether a variable is a valid filename
#'
#' Function tests whether a variable is a valid filename.
#' @param filename A variable to be tested.
#' @param extension A string that represents the file extension you are testing against (e.g. "html").
#''
#' @return a boolean (TRUE or FALSE) identifying whether the input is a valid filename (TRUE) or not (FALSE)
#' @keywords is filename valid
#' @export
#' @examples isValidFilename ('myfile.html', "html")

isValidFilename <- function (filename, extension) {

  out <- FALSE

  filename <- tolower(filename)
  extension <- tolower(extension)

  if(stringi::stri_sub(extension,1,1) != ".") {
    extension <- paste(".", extension, sep="")
  }

  strN <- stringi::stri_length(extension)

  if(isSingleString(filename)) {
    if(stringr::str_sub(filename,-1*strN) == extension) {
      out <- TRUE
    }
  }

  return (out)

}


# Internal helper (not exported): shape-validate a showIf condition.
# Used by addScenarioToQCEscenarioList, addSetToQCEsetInfoList, and
# addBlockToQCETrialStructureList to reject malformed hand-rolled lists.
# Same shape rule buildQCEshowIfCompound applies to its children.
validateShowIfShape <- function(x, paramName = "showIf") {
  if (!is.list(x)) {
    stop(paramName, " must be a list (output of buildQCEshowIfCondition or buildQCEshowIfCompound).")
  }
  isSingle   <- !is.null(x$stimRef) && !is.null(x$operator)
  isCompound <- !is.null(x$all) || !is.null(x$any)
  if (!isSingle && !isCompound) {
    stop(paramName, " is not a valid showIf condition: ",
         "expected stimRef+operator (single) or all/any (compound). ",
         "Did you forget to wrap with buildQCEshowIfCondition or buildQCEshowIfCompound?")
  }
  invisible(TRUE)
}
