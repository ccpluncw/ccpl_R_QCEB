#' This  function is used to add a fixation frameto a QCEframeList
#'
#' Function that creates or modifys an QCEframeList by adding a fixation frame.
#' @param QCEframeList A list that specifies the frames to show a participant in a single scenario.  These frames are presented in succession: 1, 2, ... N.  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEframeList that you are adding an effect to. DEFAULT = NULL
#' @param frameSymbol A string that specifies the character symbol to use for the fixation. DEFAULT = "+"
#' @param fixationFontSize A string that specifies the font size of the fixation. Font size is specified by an integer followed by px. For example, "50px" DEFAULT = "50px"
#' @param fixationColor an RGB color, specified in hexadecimal, that controls the color of the fixation symbol. DEFAULT = "#FF6464" (reddish).
#' @param stimulus_duration  An integer that specifies how long to present the fixation in milliseconds. A NULL will present the stimulus until their is a user input. DEFAULT = 500
#' @param post_trial_gap  An integer that specifies how long to present a blank frame after this frame in milliseconds. DEFAULT = 500
#' @param stimTableWidth An integer specifying the width of the stimuli in the other frames.  This is generally not necessary. Rather, the fixation will be centered on the screen.  DEFAULT = 100.
#' @param background an RGB color, specified in hexadecimal, that controls the background color of the frame page. DEFAULT = "#000000" (black).
#' @param output a boolean that specifies whether to output the data from the frame into the dataset. Often fixation frames do not need to be output. DEFAULT = FALSE.
#' @param trigger Optional list produced by buildQCETriggerList() specifying fNIRS trigger codes that fire at the fixation frame's boundaries — onset in on_start, offset in on_finish. Rarely used at this level for fNIRS (analysts usually mark the stimulus frame, not fixation), but provided for parity with addFrameToQCEframeList. Recommended code range: 10000-99999 (5 digits). DEFAULT = NULL.
#'
#' @return the updated QCEframeList
#' @keywords QCE QCEframeList fixation add effect
#' @export
#' @examples
#' # Basic
#' addFixationToQCEframeList (frameList, fixationFontSize = "20px", fixationColor = "#FF6464", stimulus_duration = 1000, post_trial_gap = 250, stimTableWidth = 800, background = "#000000")

addFixationToQCEframeList <- function (QCEframeList = NULL, frameSymbol = '+', fixationFontSize = "50px", fixationColor = "#FF6464", stimulus_duration = 500,  post_trial_gap = 500, stimTableWidth = 100, background = "#000000", output = FALSE, trigger = NULL) {

  if(!isSingleString(frameSymbol)) {
    stop("frameSymbol option must be a single string.")
  }

  if(!isSingleNumeric(stimulus_duration)) {
    stop("stimulus_duration option must be a single integer.")
  }

  if(!isSingleNumeric(post_trial_gap)) {
    stop("post_trial_gap option must be a single integer.")
  }

  if(!isColor(background)) {
    stop("background option must be a valid color.")
  }

  if(is.null(stimulus_duration)) {
    stimulus_duration <- numeric()
  }
  if(is.null(post_trial_gap)) {
    post_trial_gap <- numeric()
  }

  stimulus <- paste("<table style = \"font-family:Courier, monospace; width:", stimTableWidth, "; text-align: center\"><tr><td style=\"font-size: ", fixationFontSize, "; color: ", fixationColor, "\">", frameSymbol,"</td></tr></table>", sep="")

  choices <- "ALL_KEYS"
  trialType <- "key"
  frameName <- "fixation"

  tmpList <- list(trialType = trialType, frameName = frameName, stimulus = stimulus, post_trial_gap = post_trial_gap, choices= choices, stimulus_duration = stimulus_duration, background = background, output = output)
  if (!is.null(trigger)) {
    tmpList$trigger <- trigger
  }

  if(is.null(QCEframeList)) {
    QCEframeList[[as.name(1)]] <- tmpList
  } else {
    numList <- length(QCEframeList)
    QCEframeList[[as.name(numList + 1)]] <- tmpList
  }

  return(QCEframeList)

}
