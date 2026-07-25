#' Describe one form field on a positionable HTML page
#'
#' A page is a researcher-authored HTML file played at an event anchor. Its
#' sidecar (\code{<file>.page.json}) declares which form controls the engine
#' should read when the participant clicks continue. This function builds one
#' such declaration; pass a list of them to \code{\link{buildQCEpageSidecar}}.
#'
#' The engine reads the control by its HTML \code{name} attribute, so
#' \code{input} must match the name in your HTML exactly. Whatever it captures is
#' written to a data column named by \code{as} (or by \code{input} when \code{as}
#' is omitted). Remember that a column is saved only if it also appears in the
#' experiment's \code{fields.txt} whitelist --
#' \code{\link{buildQCEoutputFieldManifest}} lists the columns a config is
#' expected to produce, so you can copy the ones you want.
#'
#' @param input A single string: the HTML \code{name} attribute of the control to
#'   read. Required.
#' @param type The kind of control. One of \code{"text"}, \code{"number"},
#'   \code{"hidden"}, \code{"radio"}, \code{"checkbox"}, \code{"select"}. A
#'   checkbox group captures every checked value, joined with underscores.
#'   DEFAULT = "text".
#' @param as A single string naming the output data column. NULL uses
#'   \code{input}. DEFAULT = NULL.
#' @param required A boolean. TRUE blocks the continue button while the field is
#'   empty. DEFAULT = FALSE.
#' @param emptyValue A single value that should count as "still blank" alongside
#'   NULL and "". Use it when your HTML carries a placeholder default -- an age
#'   box pre-set to 0, a select whose first option is "Choose..." -- so the
#'   required check treats the placeholder as unanswered without you editing the
#'   HTML. DEFAULT = NULL.
#'
#' @return A list describing one page field.
#' @keywords QCE pages page field form
#' @export
#' @examples
#' # A required text box whose value lands in a column called "Birth"
#' buildQCEpageField("birth_year", type = "number", as = "Birth", required = TRUE)
#'
#' # A select whose placeholder first option should not count as an answer
#' buildQCEpageField("ethnicity", type = "select", required = TRUE,
#'                   emptyValue = "Choose...")
buildQCEpageField <- function(input, type = "text", as = NULL, required = FALSE,
                              emptyValue = NULL) {

  if (missing(input) || !isSingleString(input) || nchar(input) == 0) {
    stop("input option must be a single non-empty string naming the HTML control's 'name' attribute.")
  }

  validTypes <- c("text", "number", "hidden", "radio", "checkbox", "select")
  if (!isSingleString(type) || !(type %in% validTypes)) {
    stop(paste("type option must be one of:", paste(validTypes, collapse = ", ")))
  }

  if (!is.null(as) && (!isSingleString(as) || nchar(as) == 0)) {
    stop("as option must be a single non-empty string naming the output column, or NULL.")
  }

  if (!is.logical(required) || length(required) != 1 || is.na(required)) {
    stop("required option must be TRUE or FALSE.")
  }

  if (!is.null(emptyValue) && length(emptyValue) != 1) {
    stop("emptyValue option must be a single value, or NULL.")
  }

  tmpList <- list(input = input, type = type, required = required)
  if (!is.null(as)) {
    tmpList$as <- as
  }
  if (!is.null(emptyValue)) {
    tmpList$emptyValue <- emptyValue
  }

  return(tmpList)
}
