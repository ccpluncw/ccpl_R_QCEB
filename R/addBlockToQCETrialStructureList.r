#' This  function is used to create or modify a QCETrialStructureList
#'
#' Function that creates or modifys an QCETrialStructureList by adding blocks to the list one at a time.
#' @param QCETrialStructureList A list that specifies how the trials will be presented in the experiment.  This list specifies the selection of stimuli from stimFile.json, the ordering of stimuli, the blocking structure, etc.  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCETrialStructureList that you are adding an effect to. DEFAULT = NULL
#' @param QCEsetInfoList A list that specifies the the setInfo information.  This includes, for each setName, : N (the number of trials per set) and selection (the method of selecting the scenarios from the QCEScenarioList).
#' @param QCEblockIteratorList A list that specifies the the block iteration information.  This includes, for each block, the number of times to repeat each block, and the presentation order of the sets and trials.
#' @param blockNumber An integer specifying the ordinal position of this block relative to all the other blocks.  If this is set to -1, then it will be randomly placed in the one of the non-specified positions. DEFAULT = -1.
#' @param blockName A string specifying the name of this block. It is used for the experimenter to identify block condition names. DEFAULT = "blockName".
#' @param trigger Optional list produced by buildQCETriggerList() specifying the fNIRS trigger codes that fire at this block's boundaries (onset before the first trial, offset after the last). NULL means no block-level triggers. Recommended code range: 1-99 (1-2 digits). DEFAULT = NULL.
#' @param showIf Optional condition (output of buildQCEshowIfCondition or buildQCEshowIfCompound) that gates whether this block runs. Evaluated at block entry -- if FALSE, the entire block is skipped. NULL means always run. DEFAULT = NULL.
#' @param switchRules Optional list of switch-rule lists (each from buildQCEswitchRule). Rules are sequential -- they fire in array order, advancing activeRuleIndex on each fire. Switch rules govern intra-block flow (set switching at threshold); they are scoped per-block (Phase 3 Decision 4). NULL means no switching. DEFAULT = NULL.
#''
#' @return the updated QCETrialStructureList
#' @keywords QCE QCETrialStructureList update add block
#' @export
#' @examples
#' # Basic: no triggers
#' addBlockToQCETrialStructureList (TSlist, mySetInfoList, myBlockIteratorList)
#'
#' # With fNIRS block trigger
#' addBlockToQCETrialStructureList (TSlist, mySetInfoList, myBlockIteratorList,
#'   blockName = "Familiarization",
#'   trigger = buildQCETriggerList(onset = 10, offset = 11))
#'
#' # With showIf -- block only runs if a previous tagged scenario answered "Yes"
#' addBlockToQCETrialStructureList (TSlist, mySetInfoList, myBlockIteratorList,
#'   blockName = "smokerFollowUpBlock",
#'   showIf = buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"))
#'
#' # With switchRules -- after 5 "Yes" responses in current set, jump to SetB
#' rule <- buildQCEswitchRule(
#'   countResponse = "Yes",
#'   threshold     = buildQCEswitchThreshold(values = 5, rule = "fixed"),
#'   switchToSet   = "SetB"
#' )
#' addBlockToQCETrialStructureList (TSlist, mySetInfoList, myBlockIteratorList,
#'   blockName   = "PreferenceSwitching",
#'   switchRules = list(rule))

addBlockToQCETrialStructureList <- function (QCETrialStructureList = NULL, QCEsetInfoList, QCEblockIteratorList, blockNumber = -1, blockName = "blockName", trigger = NULL, showIf = NULL, switchRules = NULL) {

  if (!is.null(showIf)) {
    validateShowIfShape(showIf, "showIf")
  }

  if (!is.null(switchRules)) {
    validateSwitchRulesShape(switchRules, "switchRules")
  }

  tmpList <- list(blockNumber = blockNumber, setInfo = QCEsetInfoList, blockIterator = QCEblockIteratorList, blockName = blockName)
  if (!is.null(trigger)) {
    tmpList$trigger <- trigger
  }
  if (!is.null(showIf)) {
    tmpList$showIf <- showIf
  }
  if (!is.null(switchRules)) {
    tmpList$switchRules <- switchRules
  }

  if(is.null(QCETrialStructureList)) {
    QCETrialStructureList[[as.name(1)]] <- tmpList
  } else {
    numList <- length(QCETrialStructureList)
    QCETrialStructureList[[as.name(numList + 1)]] <- tmpList
  }

  return(QCETrialStructureList)

}
