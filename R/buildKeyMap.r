#' This  function builds a QCEkeyMap from a dataframe
#'
#' Function  builds a QCEkeyMap from a dataframe.
#' @param dfKeys A dataframe in which each column specifies a key definition.  The column name is the keyMeaning and the column contents contains caracters representing the keys that map to that meaning.  The number of columns indicates the number of defined keys.
#''
#' @return the  QCEkeyMap
#' @keywords QCE QCEkeyMap key definition
#' @export
#' @examples buildKeyMap ( dataframe(yes = c("d"), no = ("k")) )

buildKeyMap <- function (dfKeys) {
  nKeys <- ncol(dfKeys)

  outList <- NULL
  for(i in 1:nKeys) {
    keyName <- names(dfKeys[i])
    keyVec <- as.vector(dfKeys[[i]])
    outList <- addKeyToKeyMap (QCEkeyMap = outList, keyMeaning = keyName, keys = keyVec)
  }

  return(outList)
}
