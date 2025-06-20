#' This  function is used to create or modify a QCEGroupList by adding a QCEsessionList to a QCEGroupList
#'
#' Function that creates or modifys a QCEGroupList by adding QCEsessionList to the list one at a time.
#' @param QCEGroupList A list that specifies all the session that participants will see for a single, betweeen subjects group. A session is, essentially, a group of trials that use the same scenario list and have the same instructions and response types. If you are building a new list, then the QCEGroupList should be NULL. If you are adding a new session to an old list, then QCEGroupList should be the QCEGroupList that you are adding an effect to. DEFAULT = NULL
#' @param QCEsessionList A list that specifies the session name, order number, dbfileName, tsFilename, and scenarioFilename.
#' @param groupName A string that specifies the name of the name of the between subjects group that contains these sessions.  This will be output in the datafile.
#''
#' @return the updated QCEGroupList
#' @keywords QCE QCEGroupList QCEsessionList update add session
#' @export
#' @examples addSessionListToQCEGroupList (QCEGroupList, QCEsessionList, "ponys")

addSessionListToQCEGroupList <- function (QCEGroupList = NULL, QCEsessionList, groupName = "groupName") {

  tmpList <- list(sessions = QCEsessionList, groupName = groupName)

  if(is.null(QCEGroupList)) {
    QCEGroupList[[as.name(1)]] <- tmpList
  } else {
    numList <- length(QCEGroupList)
    QCEGroupList[[as.name(numList + 1)]] <- tmpList
  }

  return(QCEGroupList)

}
