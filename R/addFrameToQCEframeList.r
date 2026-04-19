#' This function is used to create or modify a QCEframeList
#'
#' Function that creates or modifies a QCEframeList by adding frames to the list one at a time.
#' @param QCEframeList A list that specifies the frames to show a participant in a single scenario.  These frames are presented in succession: 1, 2, ... N.  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEframeList that you are adding an effect to. DEFAULT = NULL
#' @param trialType A string that specifies the response type that you will be collecting.  It can take on one of the following values: "key", "textbox", "numberline", or "angleline".  "key" indicates a key press (or no input) to move on to the next frame. "textbox" presents a textbox for participants to input text. "numberline" presents a draggable number-line response plugin. "angleline" presents a draggable angle-line response plugin. DEFAULT = "key".
#' @param frameName A string that specifies the name of the frame that will be output in the datafile, to indicate the data collected for this particular frame. One row is output in the datafile for each frame, so the frameName helps you keep track of the frame.  DEFAULT = NULL.  A NULL will force the frameName to equal "frame#" where # is the frame number.
#' @param stimulus A string that specifies the stimulus to be presented on this frame.  The stimulus must be in html format.  You can use any html codes. IMPORTANT: if the trialType = "key" you cannot have an input box of any kind.  If the trialType is "textbox" you must contain a textbox input field specified in html.  The fields for the html textbox MUST contain the following: <label id = TIN for="Text_In"> and 	<input id ="Text_In" …> DEFAULT = NULL. A NULL will present a blank screen.
#' @param stimulus_duration  An integer that specifies how long to present the frame in milliseconds. A NULL will present the stimulus until there is a user input. DEFAULT = NULL
#' @param post_trial_gap  An integer that specifies how long to present a blank frame after this frame in milliseconds. DEFAULT = NULL (indicating no gap)
#' @param response_ends_trial  A boolean that specifies whether the key response ends the trial.  If set to FALSE, then stimulus_duration must not be NULL. DEFAULT = TRUE
#' @param choices Specifies the keyboard keys accepted as a response. Interpretation depends on trialType.
#'   For trialType = "key": a character vector of allowed key names (e.g., c("a", "b", " ", "Enter")), or the sentinel "ALL_KEYS" to accept any key. NULL or an empty vector disables the keyboard response path entirely — the trial then advances on stimulus_duration only.
#'   For trialType = "textbox" with kind = "string" or "number": the default "ALL_KEYS" is appropriate (the plugin handles character filtering internally via the kind argument).
#'   For trialType = "textbox" with kind = "other": you MUST provide a character vector of specific allowed keys (e.g., c("y", "n")). The sentinel "ALL_KEYS" will NOT work in this case — the plugin tests membership against the vector, so "ALL_KEYS" would match nothing.
#'   For trialType = "numberline" or "angleline": ignored (these plugins handle their own interaction model).
#'   choices does not control what can be typed into an html textbox input field — that is controlled by the html input code plus the kind argument. DEFAULT = "ALL_KEYS".
#' @param kind A string that specifies the type of allowable input in a textbox. Use "string" to allow all input, "number" to allow numbers, and "other" to restrict the textbox to the subset of keys specified in "choices". Only meaningful when trialType = "textbox"; silently ignored (not emitted to JSON) for other trial types. DEFAULT = "string".
#' @param pluginParams A named list of plugin-specific parameters passed through to the jsPsych plugin for this frame. For textbox trials, the 'kind' argument (above) is automatically merged into this list — passing 'kind' both as a named argument AND inside pluginParams is an error. For future custom plugins (Cyberball etc.), pass their specific parameters here. DEFAULT = NULL.
#' @param background an RGB color, specified in hexadecimal, that controls the background color of the frame page. DEFAULT = "#000000" (black).
#' @param cursorVisible  A boolean that specifies whether the cursor is visible during the frame.  If set to FALSE, then the cursor will not be visible. DEFAULT = TRUE
#' @param output A boolean that specifies whether to output the data from the frame into the dataset. Many times frames such as fixation and mask frames do not need to be output. DEFAULT = TRUE.
#' @param trigger Optional list produced by buildQCETriggerList() specifying the fNIRS trigger codes that fire at this frame's boundaries — onset fires in the frame's on_start, offset fires in the frame's on_finish (even for non-response frames like fixation). NULL means no frame-level triggers. Recommended code range: 10000-99999 (5 digits). DEFAULT = NULL.
#'
#' @return the updated QCEframeList
#' @keywords QCE QCEframeList update add effect
#' @export
#' @examples
#' # Basic
#' addFrameToQCEframeList (frameList, trialType = "key", frameName = "mask2", stimulus = myStimString, stimulus_duration = 1000, post_trial_gap = 0, choices = NULL, background = "#000000")
#'
#' # With fNIRS frame-level trigger (typical event-related design: marker on stimulus frame)
#' addFrameToQCEframeList (frameList, trialType = "key", frameName = "stimulus",
#'   stimulus = myStimString, stimulus_duration = 1000, post_trial_gap = 0,
#'   trigger = buildQCETriggerList(onset = 10000, offset = 10001))

addFrameToQCEframeList <- function (QCEframeList = NULL, trialType = "key", frameName = NULL, stimulus = NULL,	stimulus_duration = NULL, post_trial_gap = NULL, response_ends_trial = TRUE, choices = "ALL_KEYS", kind = "string", background = "#000000", cursorVisible = TRUE, output = TRUE, trigger = NULL, pluginParams = NULL) {

  validTrialTypes <- c("key", "textbox", "numberline", "angleline")

  if(!(trialType %in% validTrialTypes)) {
    stop(paste("trialType option must take on one of the following values: ", paste(validTrialTypes, sep="", collapse=" ")))
  }

  validKinds <- c("string", "number", "other")
  if(!(kind %in% validKinds)) {
    stop(paste("kind option must take on one of the following values:", paste(validKinds, collapse = ", ")))
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

  # Plugin-specific parameters. The QCEP engine (post-2026-04-19 refactor)
  # expects plugin-specific fields to live inside pluginParams, NOT at frame
  # top level. For textbox trials, `kind` is automatically merged in. Non-
  # textbox frames don't carry kind metadata at all (it has no meaning).
  # Passing `kind` twice (as a named arg AND inside pluginParams) is an error.
  if (!is.null(pluginParams) && "kind" %in% names(pluginParams)) {
    stop("Do not pass 'kind' both as a named argument AND inside pluginParams. Use only one.")
  }
  finalPluginParams <- if (is.null(pluginParams)) list() else pluginParams
  if (trialType == "textbox") {
    finalPluginParams$kind <- kind
  }

  tmpList <- list (trialType = trialType, frameName = frameName, stimulus = stimulus,	stimulus_duration = stimulus_duration, post_trial_gap = post_trial_gap, response_ends_trial = response_ends_trial, choices = choices, background = background, cursorVisible = cursorVisible, output = output)
  if (!is.null(trigger)) {
    tmpList$trigger <- trigger
  }
  if (length(finalPluginParams) > 0) {
    tmpList$pluginParams <- finalPluginParams
  }

  if(is.null(QCEframeList)) {
    QCEframeList[[as.name(1)]] <- tmpList
  } else {
    numList <- length(QCEframeList)
    QCEframeList[[as.name(numList + 1)]] <- tmpList
  }

  return(QCEframeList)

}
