#' This  function is used to create or modify a QCEsessionList
#'
#' Function that creates or modifys an QCEsessionList by adding sessions to the list one at a time.
#' @param QCEsessionList A list that specifies all session parameters: see next 5 parameters in this function. If you are building a new list, then QCEsessionList should be NULL. If you are adding a new effect to an old list, then QCEsessionList should be the QCEsessionList that you are adding an effect to. DEFAULT = NULL
#' @param sessionOrder An integer specifying the ordinal position that this session should be presented relative to other sessions in the group.  If sessionOrder = -1, then the session order will be randomized. DEFAULT = -1
#' @param sessionName A string used to label this session.  It is output in the dataFile.  DEFAULT = "unspecified".
#' @param dbFile A string that specifies the name of the dbfile that contains necessary information for this session. DEFAULT = "dbfile.txt".
#' @param tsFile A string that specifies the name of the trial structure file for this session. DEFAULT = "tsFile.txt".
#' @param stimFile A string that specifies the name of the stimFile (that contains the scenarios) for this session. DEFAULT = "stimFile.txt".
#''
#' @return the updated QCEsessionList
#' @keywords QCE QCEsessionList update add
#' @export
#' @examples addSessionToSessionList (QCEsessionList, sessionOrder = -1, sessionName = "unspecified", dbFile = "dbfile.txt", tsFile = "tsFile.txt", stimFile = "stimulus.txt")


addSessionToSessionList <- function (QCEsessionList = NULL, sessionOrder = -1, sessionName = "unspecified", dbFile = "dbfile.txt", tsFile = "tsFile.txt", stimFile = "stimulus.txt") {

  tmpList <- list(sessionOrder = sessionOrder, sessionName = sessionName, dbFile = dbFile, tsFile = tsFile, stimFile = stimFile)

  if(is.null(QCEsessionList)) {
    QCEsessionList[[as.name(1)]] <- tmpList
  } else {
    numList <- length(QCEsessionList)
    QCEsessionList[[as.name(numList + 1)]] <- tmpList
  }

  return(QCEsessionList)

}
