#' This  function is used to create or modify a QCEframeList
#'
#' Function that creates or modifys an QCEframeList by adding frames to the list one at a time.
#' @param QCEframeList A list that specifies the frames to show a participant in a single scenario.  These frames are presented in succession: 1, 2, ... N.  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEframeList that you are adding an effect to. DEFAULT = NULL
#' @param trialType A string that specifies the response type that you will be collecting.  It can take on one of the following values: "key" or "textbox".  "key" indicates a key press or no input to move on to next frame. "textbox" indicates that a textbox will be presented for participants to input their text response. DEFAULT = "key"
#' @param frameName A string that specifies the name of the frame that will be output in the datafile, to indicate the data collected for this particular frame. One row is output in the datafile for each frame, so the frameName helps you keep track of the frame.  DEFAULT = NULL.  A NULL will force the frameName to equal "frame#" where # is the frame number.
#' @param stimulis A string that specifies the stimulus to be presented on this frame.  The stimulus must be in html format.  You can use any html codes. IMPORTANT: if the trialType = "key" you cannot have an input box of anykind.  If the trialType is "textbox" you must contain a textbox input field specified in html.  The fields for the html textbox MUST contain the following: <label id = TIN for="Text_In"> and 	<input id ="Text_In" …> DEFAULT = NULL. A NULL will present a blank screen.
#' @param stimulus_duration  An integer that specifies how long to present the frame in milliseconds. A NULL will present the stimulus until their is a user input. DEFAULT = NULL
#' @param post_trial_gap  An integer that specifies how long to present a blank frame after this frame in milliseconds. DEFAULT = NULL (indicating no gap)
#' @param response_ends_trial  A boolean that specifies whether the key response ends the trial.  If set to FALSE, then stimulus_duration must not be NULL. DEFAULT = TRUE
#' @param choices A vector of characters or keyCodes that specifies the allowable keys to be accepted as a response.  If “choices” is NULL, then all keys are allowed.  “choices” should be NULL if trialType = textbox.  choices does not control the keys that can be pressed to input a value into the textbox.  That is controlled by the html input code used to create the textbox.  DEFAULT = NULL.
#' @param background an RGB color, specified in hexadecimal, that controls the background color of the frame page. DEFAULT = "#000000" (black).
#' @param outut a boolean that specifies whether to output the data from the frame into the dataset. Manytimes frames such as fixation and mask frames do not need to be output. DEFAULT = TRUE.
#''
#' @return the updated QCEframeList
#' @keywords QCE QCEframeList update add effect
#' @export
#' @examples addFrameToQCEframeList (frameList, trialType = "key", frameName = "mask2", stimulus = myStimString,	stimulus_duration = 1000, post_trial_gap = 0, choices = NULL, background = "#000000")

addFrameToQCEframeList <- function (QCEframeList = NULL, trialType = "key", frameName = NULL, stimulus = NULL,	stimulus_duration = NULL, post_trial_gap = NULL, response_ends_trial = TRUE, choices = NULL, background = "#000000", output = TRUE) {

  validTrialTypes <- c("key", "textbox")

  if(!(trialType %in% validTrialTypes)) {
    stop(paste("trialType option must take on one of the following values: ", paste(validTrialTypes, sep="", collapse=" ")))
  }

  if(is.null(frameName)) {
    if(is.null(QCEframeList)) {
      frameName <- "frame1"
    } else {
      numList <- length(QCEframeList)
      frameName <- paste("frame", numList, sep="")
    }
  } else {
    frameName <- as.character(frameName)
  }

  if(!isSingleString(stimulus) & !is.null(stimulus)) {
    stop("stimulus option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the stimulus option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
  }

  if(!isSingleNumeric(stimulus_duration) & !is.null(stimulus_duration)) {
    stop("stimulus_duration option must be a single integer or NULL.")
  }

  if(!isSingleNumeric(post_trial_gap)) {
    stop("post_trial_gap option must be a single integer.")
  }

  if(response_ends_trial == FALSE & is.null(stimulus_duration)) {
    stop("response_ends_trial option must be TRUE if stimulus_duration is NULL.  stimulus_duration specifies how long the trial will last when there is no response.")
  }

  if(!is.character(choices) & !is.null(choices)) {
    stop("choices option must be a vector of charactors representing allowable keys or NULL.")
  }

  if(!isColor(background)) {
    stop("background option must be a valid color.")
  }

  if(is.null(choices)) {
    choices <- character()
  }
  if(is.null(stimulus)) {
    stimulus <- character()
  }
  if(is.null(stimulus_duration)) {
    stimulus_duration <- numeric()
  }
  if(is.null(post_trial_gap)) {
    post_trial_gap <- numeric()
  }


  tmpList <- list (trialType = trialType, frameName = frameName, stimulus = stimulus,	stimulus_duration = stimulus_duration, post_trial_gap = post_trial_gap, response_ends_trial = response_ends_trial, choices = choices, background = background, output = output)

  if(is.null(QCEframeList)) {
    QCEframeList[[as.name(1)]] <- tmpList
  } else {
    numList <- length(QCEframeList)
    QCEframeList[[as.name(numList + 1)]] <- tmpList
  }

  return(QCEframeList)

}
