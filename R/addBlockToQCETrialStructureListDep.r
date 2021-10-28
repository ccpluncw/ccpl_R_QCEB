#' This  function is used to create or modify a QCETrialStructureList.  This version is valid for the first incantation of the QCE.
#'
#' Function that creates or modifys an QCETrialStructureList by adding blocks to the list one at a time. This version is valid for the first incantation of the QCE. This version does not add blockNumber to the list.
#' @param QCETrialStructureList A list that specifies how the trials will be presented in the experiment.  This list specifies the selection of stimuli from stimFile.json, the ordering of stimuli, the blocking structure, etc.  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCETrialStructureList that you are adding an effect to. DEFAULT = NULL
#' @param QCEsetInfoList A list that specifies the the setInfo information.  This includes, for each setName, : N (the number of trials per set) and selection (the method of selecting the scenarios from the QCEScenarioList).
#' @param QCEblockIteratorList A list that specifies the the block iteration information.  This includes, for each block, the number of times to repeat each block, and the presentation order of the sets and trials.
#''
#' @return the updated QCETrialStructureList
#' @keywords QCE QCETrialStructureList update add block
#' @export
#' @examples addBlockToQCETrialStructureList (TSlist, mySetInfoList, myBlockIteratorList)

addBlockToQCETrialStructureListDep <- function (QCETrialStructureList = NULL, QCEsetInfoList, QCEblockIteratorList) {

  tmpList <- list(setInfo = QCEsetInfoList, blockIterator = QCEblockIteratorList)

  if(is.null(QCETrialStructureList)) {
    QCETrialStructureList[[as.name(1)]] <- tmpList
  } else {
    numList <- length(QCETrialStructureList)
    QCETrialStructureList[[as.name(numList + 1)]] <- tmpList
  }

  return(QCETrialStructureList)

}
