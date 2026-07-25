#' This  function is used to create or modify a QCEGroupList by adding a QCEsessionList to a QCEGroupList
#'
#' Function that creates or modifys a QCEGroupList by adding QCEsessionList to the list one at a time.
#' @param QCEGroupList A list that specifies all the session that participants will see for a single, betweeen subjects group. A session is, essentially, a group of trials that use the same scenario list and have the same instructions and response types. If you are building a new list, then the QCEGroupList should be NULL. If you are adding a new session to an old list, then QCEGroupList should be the QCEGroupList that you are adding an effect to. DEFAULT = NULL
#' @param QCEsessionList A list that specifies the session name, order number, dbfileName, tsFilename, and scenarioFilename.
#' @param groupName A string that specifies the name of the name of the between subjects group that contains these sessions.  This will be output in the datafile.
#' @param pages A single string naming this group's page placement file (e.g. "pagesA.json", written by \code{\link{saveQCEpageFiles}}). Positionable HTML pages play at event anchors -- consent, demographics, a debrief. Each group may point at a different file, so groups can differ in the pages they show. NULL means this group shows no pages. DEFAULT = NULL.
#' @param cards A single string naming this group's card placement file (e.g. "cards1.json", written by \code{\link{saveQCEcardFiles}}). Cards are persistent panels that stay on screen across trials. NULL means this group shows no cards. DEFAULT = NULL.
#''
#' @return the updated QCEGroupList
#' @keywords QCE QCEGroupList QCEsessionList update add session pages cards
#' @export
#' @examples
#' addSessionListToQCEGroupList (QCEGroupList, QCEsessionList, "ponys")
#'
#' # A group that shows intake pages and a persistent progress card
#' addSessionListToQCEGroupList (QCEGroupList, QCEsessionList, "ponys",
#'                               pages = "pagesA.json", cards = "cards1.json")

addSessionListToQCEGroupList <- function (QCEGroupList = NULL, QCEsessionList, groupName = "groupName", pages = NULL, cards = NULL) {

  if (!is.null(pages) && (!isSingleString(pages) || nchar(pages) == 0)) {
    stop("pages option must be a single non-empty filename naming this group's page ",
         "placement file (e.g. 'pagesA.json'), or NULL.")
  }
  if (!is.null(cards) && (!isSingleString(cards) || nchar(cards) == 0)) {
    stop("cards option must be a single non-empty filename naming this group's card ",
         "placement file (e.g. 'cards1.json'), or NULL.")
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

  if(is.null(QCEGroupList)) {
    QCEGroupList[[as.name(1)]] <- tmpList
  } else {
    numList <- length(QCEGroupList)
    QCEGroupList[[as.name(numList + 1)]] <- tmpList
  }

  return(QCEGroupList)

}
