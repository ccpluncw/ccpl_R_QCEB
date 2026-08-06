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
#' Build the anchor with \code{\link{QCEanchor}}, which documents the five events
#' and which qualifiers each one takes. Cards use the same vocabulary.
#'
#' The SAME page file may be placed more than once -- once per session, or in
#' several blocks -- by adding it with a different anchor each time. Each
#' placement carries its own \code{playOnce}, so a page may repeat in one
#' position and appear only once in another.
#'
#' @param QCEpagePlacement An existing placement map to add to, or NULL to start
#'   a new one. DEFAULT = NULL.
#' @param anchor An anchor built by \code{\link{QCEanchor}}, naming the event at
#'   which the page plays and which occurrence of it is meant.
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
#' pages <- addPageToQCEpagePlacement(pages, QCEanchor("sessionStart"), "consent")
#' pages <- addPageToQCEpagePlacement(pages, QCEanchor("sessionStart"), "demographics")
#' pages <- addPageToQCEpagePlacement(pages,
#'            QCEanchor("entry", session = "1", block = "practice"),
#'            "howToPractice", playOnce = TRUE)
#' # The same page in a second block -- a separate placement, not a shared one.
#' pages <- addPageToQCEpagePlacement(pages,
#'            QCEanchor("entry", session = "1", block = "test"), "howToPractice")
#' pages <- addPageToQCEpagePlacement(pages, QCEanchor("sessionEnd"), "debrief")
addPageToQCEpagePlacement <- function(QCEpagePlacement = NULL, anchor, file,
                                      playOnce = FALSE) {

  if (missing(anchor)) {
    stop("anchor option is required. Build it with QCEanchor(), e.g. ",
         "QCEanchor('entry', session = '1', block = 'practice').")
  }
  if (is.character(anchor)) {
    stop("anchor option is a string. Anchors are now built by QCEanchor(), which names ",
         "the session and block as separate fields: ",
         "QCEanchor('entry', session = '1', block = 'practice', set = 'warmup'). ",
         "A string anchor could not say which block a set belonged to, so one set ",
         "anchor fired in every block sharing that set's name. Got: ", deparse(anchor))
  }
  anchorProblem <- qcebAnchorProblem(anchor)
  if (!is.null(anchorProblem)) {
    stop("anchor option ", anchorProblem)
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

  # Anchor fields sit BESIDE the page's own fields in one flat entry rather than
  # nested under a key, so the emitted JSON reads as a list of placements and the
  # engine matches an entry without unpacking it first.
  entry <- c(anchor, list(file = file, playOnce = playOnce))

  # The map is a single named element holding an UNNAMED list, so it serializes
  # as {"placements": [ ... ]} -- an array even when only one page is placed.
  # List order is the order pages were added, which is the order they play at a
  # shared anchor.
  if (is.null(QCEpagePlacement$placements)) {
    QCEpagePlacement$placements <- list(entry)
  } else {
    QCEpagePlacement$placements <- c(QCEpagePlacement$placements, list(entry))
  }

  return(QCEpagePlacement)
}
