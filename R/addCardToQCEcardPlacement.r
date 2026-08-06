#' Place a persistent card between two event anchors
#'
#' Builds the per-group placement list (\code{cardsX.json}) one card at a time.
#' Unlike a page, which plays once and ends, a card MOUNTS at one anchor and
#' stays on screen -- surviving every trial transition -- until it UNMOUNTS at
#' another. Point a group at the finished list with the \code{cards} argument of
#' \code{\link{addSessionListToQCEGroupList}}.
#'
#' Several cards may be on screen at once; each is placed independently.
#'
#' Anchors use the same vocabulary as pages and are built by
#' \code{\link{QCEanchor}}: \code{QCEanchor("sessionStart")},
#' \code{QCEanchor("sessionEnd")}, or entry/exit of a named block or set, e.g.
#' \code{QCEanchor("entry", session = "1", block = "test")}.
#'
#' @param QCEcardPlacement An existing placement list to add to, or NULL to start
#'   a new one. DEFAULT = NULL.
#' @param card A single string: the card's base name, with no extension. The
#'   engine loads \code{<card>.card.json} and, if present, an optional
#'   \code{<card>.html} shell.
#' @param mount A \code{\link{QCEanchor}} naming the moment the card appears.
#'   DEFAULT = \code{QCEanchor("sessionStart")}.
#' @param unmount A \code{\link{QCEanchor}} naming the moment it disappears.
#'   DEFAULT = \code{QCEanchor("sessionEnd")}.
#' @param position A list describing where the card sits, e.g.
#'   \code{list(region = "top-right")}. Overrides any position on the sidecar.
#'   DEFAULT = NULL.
#'
#' @return The updated placement list.
#' @keywords QCE cards card placement anchor mount
#' @export
#' @examples
#' cards <- NULL
#' # A progress card for the whole session
#' cards <- addCardToQCEcardPlacement(cards, "progress",
#'                                    position = list(region = "top-right"))
#' # A card that only exists during the test block
#' cards <- addCardToQCEcardPlacement(cards, "runningScore",
#'                                    mount = QCEanchor("entry", session = "1", block = "test"),
#'                                    unmount = QCEanchor("exit", session = "1", block = "test"),
#'                                    position = list(region = "bottom-left"))
addCardToQCEcardPlacement <- function(QCEcardPlacement = NULL, card,
                                      mount = QCEanchor("sessionStart"),
                                      unmount = QCEanchor("sessionEnd"),
                                      position = NULL) {

  if (missing(card) || !isSingleString(card) || nchar(card) == 0) {
    stop("card option must be a single non-empty string: the card's base name with ",
         "NO extension (the engine appends .card.json).")
  }

  if (grepl("\\.(html?|json)$", card, ignore.case = TRUE)) {
    stop("card option must NOT carry an extension -- the engine appends .card.json ",
         "itself. Use 'progress', not 'progress.card.json'.")
  }

  for (argName in c("mount", "unmount")) {
    argValue <- get(argName)
    if (is.character(argValue)) {
      stop(argName, " option is a string. Anchors are now built by QCEanchor(), which ",
           "names the session and block as separate fields: ",
           "QCEanchor('entry', session = '1', block = 'test'). Got: ", deparse(argValue))
    }
    argProblem <- qcebAnchorProblem(argValue)
    if (!is.null(argProblem)) stop(argName, " option ", argProblem)
  }

  # A card removed at the moment it is added is never seen. OVERLAP is the test,
  # not equality: an unmount that names no session covers every session, so it
  # meets a session-specific mount without being identical to it. Equality would
  # also be defeated by field order, which the author does not control.
  if (qcebAnchorsOverlap(mount, unmount)) {
    stop("mount and unmount can fire at the same moment, so the card would be ",
         "removed as soon as it appeared and never be seen. Use anchors that ",
         "cannot coincide.")
  }

  if (!is.null(position) && !is.list(position)) {
    stop("position option must be a list, e.g. list(region = 'top-right'), or NULL.")
  }

  if (is.null(QCEcardPlacement)) {
    QCEcardPlacement <- list()
  }
  if (!is.list(QCEcardPlacement)) {
    stop("QCEcardPlacement option must be a placement list (output of this function) or NULL.")
  }
  if (!is.null(names(QCEcardPlacement))) {
    stop("QCEcardPlacement must stay an UNNAMED list -- the engine reads cardsX.json ",
         "as a JSON array of placements.")
  }

  entry <- list(card = card, mount = mount, unmount = unmount)
  if (!is.null(position)) {
    entry$position <- position
  }

  # Appended to an UNNAMED list so the file serializes as a JSON array, which is
  # what the engine's card loader expects (pages, by contrast, use a keyed map).
  QCEcardPlacement <- c(QCEcardPlacement, list(entry))

  return(QCEcardPlacement)
}
