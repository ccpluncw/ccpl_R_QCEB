#' This function returns a vector of all the allowable keys in a QCEkeyMap
#'
#' Function that returns a vector of all the allowable keys in a QCEkeyMap.
#' @param QCEkeyMap A list that specifies the maping of the keys to their meaning for the experiment.
#''
#' @return the vector of all the allowable keys in the QCEkeyMap
#' @keywords QCE QCEkeyMap get choices
#' @export
#' @examples getKeyChoicesFromKeyMap (QCEkeyMap)

getKeyChoicesFromKeyMap <- function(QCEkeyMap) {
  outVec <- as.vector(unlist(QCEkeyMap))

  return(outVec)
}
