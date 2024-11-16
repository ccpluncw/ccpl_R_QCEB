#' This  function is used to create or modify a QCETrialStructureList
#'
#' Function that creates or modifys an QCETrialStructureList by adding blocks to the list one at a time.
#' @param QCETrialStructureList A list that specifies how the trials will be presented in the experiment.  This list specifies the selection of stimuli from stimFile.json, the ordering of stimuli, the blocking structure, etc.  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCETrialStructureList that you are adding an effect to. DEFAULT = NULL
#' @param QCEsetInfoList A list that specifies the the setInfo information.  This includes, for each setName, : N (the number of trials per set) and selection (the method of selecting the scenarios from the QCEScenarioList).
#' @param QCEblockIteratorList A list that specifies the the block iteration information.  This includes, for each block, the number of times to repeat each block, and the presentation order of the sets and trials.
#' @param blockNumber An integer specifying the ordinal position of this block relative to all the other blocks.  If this is set to -1, then it will be randomly placed in the one of the non-specified positions. DEFAULT = -1.
#' @param blockName A string specifying the name of this block. It is used for the experimenter to identify block condition names. DEFAULT = "blockName".
#''
#' @return the updated QCETrialStructureList
#' @keywords QCE QCETrialStructureList update add block
#' @export
#' @examples addBlockToQCETrialStructureList (TSlist, mySetInfoList, myBlockIteratorList)

addBlockToQCETrialStructureList <- function (QCETrialStructureList = NULL, QCEsetInfoList, QCEblockIteratorList, blockNumber = -1, blockName = "blockName") {

  tmpList <- list(blockNumber = blockNumber, setInfo = QCEsetInfoList, blockIterator = QCEblockIteratorList, blockName = blockName)

  if(is.null(QCETrialStructureList)) {
    QCETrialStructureList[[as.name(1)]] <- tmpList
  } else {
    numList <- length(QCETrialStructureList)
    QCETrialStructureList[[as.name(numList + 1)]] <- tmpList
  }

  return(QCETrialStructureList)

}
