#' Place a positionable HTML page at an event anchor
#'
#' Builds the per-group placement map (\code{pagesX.json}) one page at a time.
#' The map answers WHERE and WHEN each page plays; what a page IS lives in its
#' sidecar (\code{\link{buildQCEpageSidecar}}). Point a group at the finished map
#' with the \code{pages} argument of
#' \code{\link{addSessionListToQCEGroupList}}.
#'
#' Pages at the same anchor play in the order you add them.
#'
#' Anchors are \code{"experimentStart"}, \code{"sessionStart"},
#' \code{"sessionEnd"}, or entry/exit of a named block or set:
#' \code{"entry(block:practice)"}, \code{"exit(set:trialSet1)"}. The block or set
#' name must match the one used when building the trial structure. Cards use the
#' same vocabulary.
#'
#' \code{"experimentStart"} and \code{"sessionStart"} are not interchangeable.
#' \code{"experimentStart"} plays ONCE for the whole run, before the experiment
#' instructions -- the position the engine's built-in intake screens occupy, and
#' the right home for consent, demographics, or anything that must precede the
#' framing of the study. \code{"sessionStart"} plays at the top of EVERY session,
#' after that session's instructions.
#'
#' @param QCEpagePlacement An existing placement map to add to, or NULL to start
#'   a new one. DEFAULT = NULL.
#' @param anchor A single string naming the event at which the page plays. See
#'   above for the vocabulary.
#' @param file A single string: the page's base filename, with no extension. The
#'   engine loads \code{<file>.html} and \code{<file>.page.json}, so a value of
#'   "consent" means consent.html plus consent.page.json.
#' @param playOnce A boolean. TRUE shows the page only the first time its anchor
#'   fires, which matters at anchors that recur -- a block entered repeatedly, or
#'   a set that iterates. Use it for a one-time instruction that should not
#'   reappear on every pass. DEFAULT = FALSE.
#'
#' @return The updated placement map.
#' @keywords QCE pages page placement anchor
#' @export
#' @examples
#' pages <- NULL
#' pages <- addPageToQCEpagePlacement(pages, "sessionStart", "consent")
#' pages <- addPageToQCEpagePlacement(pages, "sessionStart", "demographics")
#' pages <- addPageToQCEpagePlacement(pages, "entry(block:practice)", "howToPractice",
#'                                    playOnce = TRUE)
#' pages <- addPageToQCEpagePlacement(pages, "sessionEnd", "debrief")
addPageToQCEpagePlacement <- function(QCEpagePlacement = NULL, anchor, file,
                                      playOnce = FALSE) {

  if (missing(anchor) || !isValidQCEanchor(anchor)) {
    stop("anchor option must be a single string: 'sessionStart', 'sessionEnd', ",
         "or 'entry(block:<name>)' / 'exit(block:<name>)' / 'entry(set:<name>)' / ",
         "'exit(set:<name>)'. Got: ", deparse(anchor))
  }

  if (missing(file) || !isSingleString(file) || nchar(file) == 0) {
    stop("file option must be a single non-empty string: the page's base filename ",
         "with NO extension (the engine appends .html and .page.json).")
  }

  if (grepl("\\.(html?|json)$", file, ignore.case = TRUE)) {
    stop("file option must NOT carry an extension -- the engine appends .html and ",
         ".page.json itself. Use 'consent', not 'consent.html'.")
  }

  if (!is.logical(playOnce) || length(playOnce) != 1 || is.na(playOnce)) {
    stop("playOnce option must be TRUE or FALSE.")
  }

  if (is.null(QCEpagePlacement)) {
    QCEpagePlacement <- list()
  }
  if (!is.list(QCEpagePlacement)) {
    stop("QCEpagePlacement option must be a placement map (output of this function) or NULL.")
  }

  entry <- list(file = file, playOnce = playOnce)

  # Each anchor holds an UNNAMED list, so it serializes as a JSON array of page
  # entries even when only one page sits there.
  if (is.null(QCEpagePlacement[[anchor]])) {
    QCEpagePlacement[[anchor]] <- list(entry)
  } else {
    QCEpagePlacement[[anchor]] <- c(QCEpagePlacement[[anchor]], list(entry))
  }

  return(QCEpagePlacement)
}
