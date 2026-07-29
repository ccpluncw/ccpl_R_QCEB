#' Build the sidecar that describes one positionable HTML page
#'
#' A page's sidecar (\code{<file>.page.json}) sits beside the HTML file and says
#' what the page IS -- which button ends it and which form fields to read --
#' while the placement file built by \code{\link{addPageToQCEpagePlacement}} says
#' WHERE and WHEN it plays. Keeping the two apart is what lets a page be copied
#' between experiments without carrying one study's timeline with it.
#'
#' Every part is optional. A page with no \code{fields} is a display-only screen
#' (instructions, a debrief); the sidecar may then be omitted entirely, in which
#' case the engine looks for its default button ID.
#'
#' @param fields A list of field declarations from
#'   \code{\link{buildQCEpageField}}, in the order you want them checked. NULL
#'   for a display-only page. DEFAULT = NULL.
#' @param contBtn A single string: the HTML \code{id} attribute of the button
#'   that ends the page -- NOT the words printed on it. The engine binds its
#'   click handler by looking this id up in the loaded page, so a page whose
#'   button carries a different id never advances and errors instead. The
#'   visible label lives in your HTML and nothing here changes it. NULL uses the
#'   engine default id. DEFAULT = NULL.
#' @param dataScope Where captured values are written. \code{"global"} stamps
#'   them onto every row of the dataset, which is how intake pages behave --
#'   demographics belong to the whole session. \code{"row"} writes them only onto
#'   this page's own data row, which suits a page asked repeatedly whose answers
#'   differ each time. DEFAULT = "global".
#'
#' @return A list: the page sidecar, ready for \code{\link{saveQCEpageFiles}}.
#' @keywords QCE pages page sidecar
#' @export
#' @examples
#' # Display-only. The HTML holds <button id="agreeBtn">I agree</button>, so the
#' # sidecar names the ID -- "agreeBtn" -- and the wording stays in the HTML.
#' consent <- buildQCEpageSidecar(contBtn = "agreeBtn")
#'
#' intake <- buildQCEpageSidecar(
#'   fields = list(
#'     buildQCEpageField("birth_year", type = "number", as = "Birth", required = TRUE),
#'     buildQCEpageField("gender", type = "radio", as = "Gender", required = TRUE)
#'   ),
#'   contBtn = "startBtn")
buildQCEpageSidecar <- function(fields = NULL, contBtn = NULL, dataScope = "global") {

  if (!is.null(contBtn) && (!isSingleString(contBtn) || nchar(contBtn) == 0)) {
    stop("contBtn option must be a single non-empty string, or NULL.")
  }

  validScopes <- c("global", "row")
  if (!isSingleString(dataScope) || !(dataScope %in% validScopes)) {
    stop(paste("dataScope option must be one of:", paste(validScopes, collapse = ", ")))
  }

  if (!is.null(fields)) {
    if (!is.list(fields) || length(fields) == 0) {
      stop("fields option must be a non-empty list of buildQCEpageField() results, or NULL.")
    }
    # An unnamed list, so it serializes as a JSON ARRAY -- the engine tests
    # `sidecar.fields.length` and would not see a named object. This is the one
    # place pages and cards differ structurally: card fields are a named object.
    if (!is.null(names(fields))) {
      stop("fields option must be an UNNAMED list (the engine expects a JSON array). ",
           "Use list(field1, field2), not list(a = field1, b = field2).")
    }
    seen <- character(0)
    for (i in seq_along(fields)) {
      f <- fields[[i]]
      if (!is.list(f) || is.null(f$input) || is.null(f$type)) {
        stop(sprintf(paste0("fields[[%d]] does not look like a buildQCEpageField() result ",
                            "(no 'input'/'type') -- did you forget to wrap it?"), i))
      }
      col <- if (!is.null(f$as)) f$as else f$input
      if (col %in% seen) {
        stop(sprintf(paste0("fields[[%d]] writes to column '%s', which an earlier field ",
                            "already writes to. Two fields sharing an output column means ",
                            "one silently overwrites the other."), i, col))
      }
      seen <- c(seen, col)
    }
  }

  tmpList <- list(dataScope = dataScope)
  if (!is.null(contBtn)) {
    tmpList$contBtn <- contBtn
  }
  if (!is.null(fields)) {
    tmpList$fields <- fields
  }

  return(tmpList)
}
