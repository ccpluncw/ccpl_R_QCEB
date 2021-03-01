#' This  function adds a key definition to a QCEkeyMap
#'
#' Function that creates or modifys an QCEkeyMap by adding a key Definition.
#' @param QCEkeyMap A list that specifies the maping of the keys to their meaning for the experiment. If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEkeyMap that you are adding an key definition to. DEFAULT = NULL
#' @param keyMeaning A single string that specifies the meaning of the keys. For example, in a target present/target absent experiment, the keyMeaning for one set of keys may be "targetPresent". DEFAULT = NULL
#' @param keys A vector of characters that specifies the keys that map to the keyMeaning.  Often these are a single key and it's capital. For example, c("d", "D"). DEFAULT = NULL
#''
#' @return the updated QCEkeyMap
#' @keywords QCE QCEkeyMap key definition
#' @export
#' @examples addKeyToKeyMap (QCEkeyMap, keyMeaning = "Yes", keys = c('d', 'D'))

addKeyToKeyMap <- function (QCEkeyMap = NULL, keyMeaning, keys) {
  if(!isSingleString(keyMeaning)) {
    stop("keyMeaning option must be a single string.")
  }

  if(!all(stringi::stri_length(keys) == 1)) {
    stop("keys option must be a vector of single characters.")
  }

  QCEkeyMap[[keyMeaning]] <- keys

  return(QCEkeyMap)
}
