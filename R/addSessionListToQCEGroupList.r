#' This  function is used to create or modify a QCEGroupList by adding a QCEsessionList to a QCEGroupList
#'
#' Function that creates or modifys a QCEGroupList by adding QCEsessionList to the list one at a time.
#' @param QCEGroupList A list that specifies all the session that participants will see for a single, betweeen subjects group. A session is, essentially, a group of trials that use the same scenario list and have the same instructions and response types. If you are building a new list, then the QCEGroupList should be NULL. If you are adding a new session to an old list, then QCEGroupList should be the QCEGroupList that you are adding an effect to. DEFAULT = NULL
#' @param QCEsessionList A list that specifies the session name, order number, dbfileName, tsFilename, and scenarioFilename.
#' @param groupName A string that specifies the name of the name of the between subjects group that contains these sessions.  This will be output in the datafile.
#' @param pages A single string naming this group's page placement file (e.g. "pagesA.json", written by \code{\link{saveQCEpageFiles}}). Positionable HTML pages play at event anchors -- consent, demographics, a debrief. Each group may point at a different file, so groups can differ in the pages they show. NULL means this group shows no pages. DEFAULT = NULL.
#' @param cards A single string naming this group's card placement file (e.g. "cards1.json", written by \code{\link{saveQCEcardFiles}}). Cards are persistent panels that stay on screen across trials. NULL means this group shows no cards. DEFAULT = NULL.
#' @param nPerBlock A single positive whole number giving this group's share of one assignment block, read by the server when it assigns groups in balance rather than by a uniform draw. Groups that share a \code{groupName} form one arm, and that arm's total is the sum of \code{nPerBlock} over those groups, so a ratio is stated by the numbers themselves (equal numbers balance the arms; 2 against 1 fills the first twice as fast). The number is a share, not a cap: when a block fills, assignment carries on in the same ratio, and no group ever closes. Every group in the experiment must declare it or none may -- a set declared on some groups only is a configuration error the server refuses at assignment time, and nothing in this package can see the other groups to catch it here. NULL leaves the key out, which is how an unbalanced experiment is written. Default \code{NULL}.
#'
#' @return the updated QCEGroupList
#' @keywords QCE QCEGroupList QCEsessionList update add session pages cards
#' @export
#' @examples
#' addSessionListToQCEGroupList (QCEGroupList, QCEsessionList, "ponys")
#'
#' # A group that shows intake pages and a persistent progress card
#' addSessionListToQCEGroupList (QCEGroupList, QCEsessionList, "ponys",
#'                               pages = "pagesA.json", cards = "cards1.json")
#'
#' # Two groups taking equal shares of every assignment block
#' addSessionListToQCEGroupList (QCEGroupList, QCEsessionList, "ponys",
#'                               nPerBlock = 1)

addSessionListToQCEGroupList <- function (QCEGroupList = NULL, QCEsessionList, groupName = "groupName", pages = NULL, cards = NULL, nPerBlock = NULL) {

  if (!is.null(pages) && (!isSingleString(pages) || nchar(pages) == 0)) {
    stop("pages option must be a single non-empty filename naming this group's page ",
         "placement file (e.g. 'pagesA.json'), or NULL.")
  }
  if (!is.null(cards) && (!isSingleString(cards) || nchar(cards) == 0)) {
    stop("cards option must be a single non-empty filename naming this group's card ",
         "placement file (e.g. 'cards1.json'), or NULL.")
  }
  if (!is.null(nPerBlock) &&
      (!is.numeric(nPerBlock) || length(nPerBlock) != 1 || !is.finite(nPerBlock) ||
       nPerBlock <= 0 || !isTRUE(nPerBlock == as.integer(nPerBlock)))) {
    stop("nPerBlock option must be a single positive whole number giving this ",
         "group's share of one assignment block, or NULL.")
  }

  tmpList <- list(sessions = QCEsessionList, groupName = groupName)

  # Emitted only when set, so a group that declares neither produces exactly the
  # JSON it always has. The engine treats an absent key as "no pages/cards" and
  # degrades to legacy behavior.
  if (!is.null(pages)) {
    tmpList$pages <- pages
  }
  if (!is.null(cards)) {
    tmpList$cards <- cards
  }

  #a share of one assignment block, summed over the groups sharing a groupName
  if (!is.null(nPerBlock)) {
    tmpList$nPerBlock <- nPerBlock
  }

  if(is.null(QCEGroupList)) {
    QCEGroupList[[as.name(1)]] <- tmpList
  } else {
    numList <- length(QCEGroupList)
    QCEGroupList[[as.name(numList + 1)]] <- tmpList
  }

  return(QCEGroupList)

}
