#' This  function reverses keys assocaited with the display and outtext for a feedback_key
#'
#' Function that reverses keys assocaited with the display and outtext for a feedback_key. This is useful to create a feedback_key for trials in which the opposite feedback is needed for the keys.
#' @param feedback_key A list that specifies the maping of the keys to their feedback text to display to the subject and the feedback text to print in the data file. This is the list that will be returned with the keys and output reversed
#''
#' @return the reversed feedback_key
#' @keywords QCE feedback_key key definition
#' @export
#' @examples reverseTwoChoiceFeedbackKey ( myFeedbackKey )

reverseTwoChoiceFeedbackKey <- function (feedback_key) {
  outList <- NULL

  outList[[names(feedback_key[2])]] <- feedback_key[[1]]
  outList[[names(feedback_key[1])]] <- feedback_key[[2]]

  return(outList)
}
