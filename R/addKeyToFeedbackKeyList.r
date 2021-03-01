#' This  function adds feedback information for a key in QCEkeyMap to a feedback_key list
#'
#' Function that creates or modifys an feedback_key list by adding feedback information for a key in QCEkeyMap.
#' @param feedback_key A list that specifies the maping of the keys to their feedback text to display to the subject and the feedback text to print in the data file. If you are building a new list, then this should be NULL. If you are adding a new key to an old list, then this should be the feedback_key that you are adding the feedback key information to. DEFAULT = NULL
#' @param QCEkeyMapKey A single key definition from the QCEkeyMap.  This is the key that the feedback information will be defined for.  For example, QCEkeyMap[[1]] will define the feedback information for the first key in the keyMap. This option must be input.
#' @param displayText A string that specifies the text to be displayed to the participant if they press this key in a trial containing this feedback_key list.   The stimulus must be in html format.  You can use any html codes. DEFAULT = NULL
#' @param outputText A single string that specifies the text to be printed in the datafile to code for the pressing of this key and it's meaning in relation to the trial. For example, in a target present/target absent experiment, the output for pressing this key might be, "correct" whereas the other key might be "incorrect" DEFAULT = NULL.  NULL will result in the keyMeaning being output in the datafile.
#''
#' @return the updated feedback_key
#' @keywords QCE feedback_key key feedback
#' @export
#' @examples addKeyToFeedbackKeyList (feedback_key, myKeyMap["yes"], displayText = "<span>You Are Correct!</span>", outputText = "correct")

addKeyToFeedbackKeyList <- function (feedback_key = NULL, QCEkeyMapKey = NULL, displayText = NULL, outputText = NULL) {

  if(is.null(QCEkeyMapKey) | (length(QCEkeyMapKey) > 1)) {
    stop("QCEkeyMapKey option must be a single key entry from a OCEPkeyMap list.")
  }

  if(!isSingleString(displayText) & !is.null(displayText)) {
    stop("displayText option must be a single string or NULL.")
  }

  if(!isSingleString(outputText) & !is.null(outputText)) {
    stop("outputText option must be a single string or NULL.")
  }

  keyMeaning <-  names(QCEkeyMapKey)

  if(is.null(outputText)) {
    outputText <- keyMeaning
  }
  if(is.null(displayText)) {
    displayText <- character()
  }


  feedback_key[[keyMeaning]] <- list(displayText = displayText, outputText = outputText)

  return(feedback_key)
}
