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
#' Anchors use the same vocabulary as pages: \code{"sessionStart"},
#' \code{"sessionEnd"}, or entry/exit of a named block or set, e.g.
#' \code{"entry(block:test)"}.
#'
#' @param QCEcardPlacement An existing placement list to add to, or NULL to start
#'   a new one. DEFAULT = NULL.
#' @param card A single string: the card's base name, with no extension. The
#'   engine loads \code{<card>.card.json} and, if present, an optional
#'   \code{<card>.html} shell.
#' @param mount A single string naming the anchor at which the card appears.
#'   DEFAULT = "sessionStart".
#' @param unmount A single string naming the anchor at which it disappears.
#'   DEFAULT = "sessionEnd".
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
#'                                    mount = "entry(block:test)",
#'                                    unmount = "exit(block:test)",
#'                                    position = list(region = "bottom-left"))
addCardToQCEcardPlacement <- function(QCEcardPlacement = NULL, card,
                                      mount = "sessionStart", unmount = "sessionEnd",
                                      position = NULL) {

  if (missing(card) || !isSingleString(card) || nchar(card) == 0) {
    stop("card option must be a single non-empty string: the card's base name with ",
         "NO extension (the engine appends .card.json).")
  }

  if (grepl("\\.(html?|json)$", card, ignore.case = TRUE)) {
    stop("card option must NOT carry an extension -- the engine appends .card.json ",
         "itself. Use 'progress', not 'progress.card.json'.")
  }

  if (!isValidQCEanchor(mount)) {
    stop("mount option must be a single string: 'sessionStart', 'sessionEnd', or ",
         "'entry(block:<name>)' / 'exit(block:<name>)' / 'entry(set:<name>)' / ",
         "'exit(set:<name>)'. Got: ", deparse(mount))
  }

  if (!isValidQCEanchor(unmount)) {
    stop("unmount option must be a single string: 'sessionStart', 'sessionEnd', or ",
         "'entry(block:<name>)' / 'exit(block:<name>)' / 'entry(set:<name>)' / ",
         "'exit(set:<name>)'. Got: ", deparse(unmount))
  }

  if (identical(mount, unmount)) {
    stop("mount and unmount are both '", mount, "', so the card would be removed at ",
         "the same anchor that adds it and never be seen. Use different anchors.")
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
