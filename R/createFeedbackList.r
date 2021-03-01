#' This  function creates a QCEfeebackList
#'
#' Function that creates a QCEfeebackList .
#' @param feedback_key A list that specifies the maping of the keys to their feedback text to display to the subject and the feedback text to print in the data file. If feedback_key = NULL, then showFeedback will be set to FALSE. DEFAULT = NULL
#' @param showFeedback A boolean that specifies whether to show feedback to the participant after each trial. DEFAULT = FALSE.
#' @param stimulus_duration  An integer that specifies how long to present the feedback in milliseconds. A NULL will present the stimulus until their is a user input. DEFAULT = 500
#' @param post_trial_gap  An integer that specifies how long to present a blank frame after this frame in milliseconds. DEFAULT = 500
#''
#' @return the QCEfeebackList
#' @keywords QCE feedback list
#' @export
#' @examples createFeedbackList (feedback_key, showFeedback = TRUE, stimulus_duration = 500, post_trial_gap = 200)

createFeedbackList <- function (feedback_key = NULL, showFeedback = FALSE, stimulus_duration = 500, post_trial_gap = 500) {

  if(!isSingleNumeric(stimulus_duration)) {
    stop("stimulus_duration option must be a single integer.")
  }

  if(!isSingleNumeric(post_trial_gap)) {
    stop("post_trial_gap option must be a single integer.")
  }

  if (!is.logical(showFeedback)) {
    stop("showFeedback option must be a boolean: either TRUE or FALSE.")
  }

  if (is.null(feedback_key)) {
    showFeedback <- FALSE
  }

  if(is.null(stimulus_duration)) {
    stimulus_duration <- numeric()
  }
  if(is.null(post_trial_gap)) {
    post_trial_gap <- numeric()
  }

  if(is.null(feedback_key)) {
    feedback <- NULL
  } else {
    feedback <- list(feedback_key = feedback_key, showFeedback = showFeedback, stimulus_duration = stimulus_duration, post_trial_gap = post_trial_gap)
  }

  return(feedback)

}
