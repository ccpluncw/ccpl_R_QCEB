#' Build a placement anchor for a page or a card
#'
#' An anchor says WHERE in the run a positionable page plays, or where a
#' persistent card is mounted or unmounted. Pass the result to
#' \code{\link{addPageToQCEpagePlacement}} or
#' \code{\link{addCardToQCEcardPlacement}}; pages and cards use the same
#' vocabulary.
#'
#' There are five events. Which qualifiers each one accepts differs, because
#' they attach to different things:
#'
#' \describe{
#'   \item{\code{"experimentStart"}}{Plays ONCE for the whole run, before the
#'     experiment instructions -- the position the built-in intake screens
#'     occupy, and the right home for consent or demographics. No session exists
#'     yet, so it takes no qualifiers at all.}
#'   \item{\code{"sessionStart"} / \code{"sessionEnd"}}{The top and tail of a
#'     session, after that session's instructions. \code{session} is OPTIONAL:
#'     omit it and the anchor fires in EVERY session, which is usually what is
#'     wanted.}
#'   \item{\code{"entry"} / \code{"exit"}}{A block boundary, or a set boundary
#'     inside a block. \code{session} and \code{block} are BOTH REQUIRED.
#'     \code{set} is the only optional qualifier: give it to address that set's
#'     boundary, omit it to address the block's own.}
#' }
#'
#' \strong{Why entry/exit demand a session and a block.} Set names are not
#' unique across blocks -- reusing one is a deliberate authoring pattern -- so a
#' set named on its own addresses every block that happens to contain a set of
#' that name, and a page written once plays in all of them. Naming the block is
#' what makes the address an address. Block names, in turn, are unique only
#' WITHIN a session, so the session is part of a block's identity rather than a
#' tie-breaker: an anchor without it is incomplete even when no collision
#' happens to exist yet, and would change meaning the day a session is added
#' that reuses the name.
#'
#' Anchors are compared field by field, so block and set names may contain any
#' characters -- nothing here has to be escaped or avoided.
#'
#' @param at A single string: one of \code{"experimentStart"},
#'   \code{"sessionStart"}, \code{"sessionEnd"}, \code{"entry"}, \code{"exit"}.
#' @param session A single string naming the session, as keyed in
#'   \code{expInfo.json} -- that is, the order in which the session was added to
#'   its session list, "1" for the first. Required for \code{"entry"} and
#'   \code{"exit"}; optional for \code{"sessionStart"}/\code{"sessionEnd"}, where
#'   omitting it means every session; forbidden for \code{"experimentStart"}.
#'   DEFAULT = NULL.
#' @param block A single string naming the block, matching the name used when
#'   building the trial structure. Required for \code{"entry"} and
#'   \code{"exit"}; forbidden otherwise. DEFAULT = NULL.
#' @param set A single string naming a set within that block, matching the name
#'   used when building the trial structure. Optional, and only with
#'   \code{"entry"}/\code{"exit"}: present addresses the set's boundary, absent
#'   addresses the block's. DEFAULT = NULL.
#'
#' @return A validated anchor list, to be passed to a placement function.
#' @keywords QCE pages cards placement anchor
#' @export
#' @examples
#' QCEanchor("experimentStart")
#' QCEanchor("sessionStart")                      # every session
#' QCEanchor("sessionEnd", session = "2")         # session 2 only
#' QCEanchor("entry", session = "1", block = "practice")
#' QCEanchor("exit",  session = "1", block = "practice", set = "warmup")
QCEanchor <- function(at, session = NULL, block = NULL, set = NULL) {

  if (missing(at)) {
    stop("at option must be a single string: ", paste(.qcebAnchorEvents, collapse = ", "), ".")
  }

  # Field order is fixed here rather than left to the caller so that two anchors
  # meaning the same thing are also structurally identical -- comparisons and
  # emitted JSON stay stable regardless of the order arguments were supplied in.
  anchor <- list(at = at)
  if (!is.null(session)) anchor$session <- session
  if (!is.null(block))   anchor$block   <- block
  if (!is.null(set))     anchor$set     <- set

  problem <- qcebAnchorProblem(anchor)
  if (!is.null(problem)) {
    stop("anchor ", problem)
  }

  anchor
}
