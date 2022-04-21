#' This  function creates a QCEblockIteratorList
#'
#' Function that creates a QCEblockIteratorList .
#' @param numberOfIterations An integer specifying how many times the current block should be repeated. DEFAULT = 1
#' @param randomizeTrialInSetOrder A boolean that specifies whether the order of the trials in each set will be randomized on every iteration. TRUE = Randomize the order.  FALSE = Present the trials in the order they were selected in the first iteration. DEFAULT = TRUE.
#' @param randomizeSetOrder  A string that specifies how the sets shoud be randomized. "randomFirst" = the order of the sets in first iteration will be randomized, and the remaining iterations will be fixed to this order.  "randomAll" = the order of the sets will be randomized. "fixed" = the order of the sets will be presented in the order they appear in setInfo.  DEFAULT = "randomAll"
#' @param randomizeAllTrials  A boolean that specifies whether all the trials from all the sets are placed in a large vector and are randomized. TRUE = Randomize all the trials.  FALSE = the randomization will follow the rules set out above in “randomizeSetOrder” and “randomPresentation” DEFAULT = FALSE
#''
#' @return the blockIterator list
#' @keywords QCE createBlockIteratorList list
#' @export
#' @examples createBlockIteratorList (1,  randomizeTrialInSetOrder = FALSE, randomizeSetOrder = "fixed", randomizeAllTrials = FALSE)

createBlockIteratorList <- function (numberOfIterations = 1, randomizeTrialInSetOrder = TRUE, randomizeSetOrder = "randomAll", randomizeAllTrials = FALSE) {

  validRandomizeSetOrders <- c("fixed", "randomFirst", "randomAll")

  if(!(randomizeSetOrder %in% validRandomizeSetOrders)) {
    stop(paste("randomizeSetOrder option must take on one of the following values: ", paste(validRandomizeSetOrders, sep="", collapse=" ")))
  }

  if(!isSingleNumeric(numberOfIterations)) {
    stop("numberOfIterations option must be a single integer.")
  }

  if (!is.logical(randomizeTrialInSetOrder)) {
    stop("randomizeTrialInSetOrder option must be a boolean: either TRUE or FALSE.")
  }

  if (!is.logical(randomizeAllTrials)) {
    stop("randomizeAllTrials option must be a boolean: either TRUE or FALSE.")
  }


  blockIterator <- list(N = numberOfIterations, randomizeTrialInSetOrder = randomizeTrialInSetOrder, randomizeSetOrder = randomizeSetOrder, randomizeAllTrials = randomizeAllTrials)


  return(blockIterator)

}
