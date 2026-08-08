#' This function is used to create or modify a QCEframeList
#'
#' Function that creates or modifies a QCEframeList by adding frames to the list one at a time.
#' @param QCEframeList A list that specifies the frames to show a participant in a single scenario.  These frames are presented in succession: 1, 2, ... N.  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEframeList that you are adding an effect to. DEFAULT = NULL
#' @param trialType A string that specifies the response type that you will be collecting.  It can take on one of the following values: "key", "textbox", "numberline", or "angleline".  "key" indicates a key press (or no input) to move on to the next frame. "textbox" presents a textbox for participants to input text. "numberline" presents a draggable number-line response plugin. "angleline" presents a draggable angle-line response plugin. DEFAULT = "key".
#' @param frameName A string that specifies the name of the frame that will be output in the datafile, to indicate the data collected for this particular frame. One row is output in the datafile for each frame, so the frameName helps you keep track of the frame.  DEFAULT = NULL.  A NULL will force the frameName to equal "frame#" where # is the frame number.
#' @param stimulus A string that specifies the stimulus to be presented on this frame.  The stimulus must be in html format.  You can use any html codes. IMPORTANT: if the trialType = "key" you cannot have an input box of any kind.  If the trialType is "textbox" you must contain a textbox input field specified in html.  The fields for the html textbox MUST contain the following: <label id = TIN for="Text_In"> and 	<input id ="Text_In" …> DEFAULT = NULL. A NULL will present a blank screen.
#' @param stimulus_duration  An integer that specifies how long the stimulus is VISIBLE, in milliseconds. A NULL leaves it visible until there is a user input. By default the frame also ENDS when the stimulus disappears; set trial_duration to separate the two. DEFAULT = NULL
#' @param trial_duration  An integer that specifies how long the frame lasts before it ends on its own, in milliseconds. Three states:
#'   NULL means "not specified", and the frame ends when its stimulus does (it inherits stimulus_duration). This is the long-standing behavior and what every frame that does not mention trial_duration still gets.
#'   The sentinel "NO_LIMIT" removes the time limit entirely, so the frame ends only when the participant responds. This is the only way to hide a stimulus partway through (via stimulus_duration) while continuing to accept input, since the inheritance above otherwise ties the two durations together.
#'   A positive number is that many milliseconds, independent of stimulus_duration. A value LARGER than stimulus_duration gives a limited-exposure stimulus with a response window that outlives it.
#'   DEFAULT = NULL
#' @param post_trial_gap  An integer that specifies how long to present a blank frame after this frame in milliseconds. REQUIRED: pass 0 when you want no gap. The NULL in the signature is not a working default -- omitting this argument is an error, so that a frame's inter-trial timing is always something the researcher stated rather than something inherited silently. NO DEFAULT.
#' @param response_ends_trial  A boolean that specifies whether the key response ends the trial. A frame must have SOME way to end: if this is FALSE, the frame needs a stimulus_duration or a trial_duration to end it. The same applies when choices is NULL, empty, or "NO_KEYS", since those leave no key to press. Trial types whose plugin supplies its own response surface (textbox, numberline, angleline, survey, mcKeys) can always be ended by the participant and are exempt. DEFAULT = TRUE
#' @param choices Specifies the keyboard keys accepted as a response. Interpretation depends on trialType.
#'   For trialType = "key": a character vector of allowed key names (e.g., c("a", "b", " ", "Enter")), or the sentinel "ALL_KEYS" to accept any key. NULL or an empty vector disables the keyboard response path entirely — the trial then advances on stimulus_duration only.
#'   For trialType = "textbox" with kind = "string" or "number": the default "ALL_KEYS" is appropriate (the plugin handles character filtering internally via the kind argument).
#'   For trialType = "textbox" with kind = "other": you MUST provide a character vector of specific allowed keys (e.g., c("y", "n")). The sentinel "ALL_KEYS" will NOT work in this case — the plugin tests membership against the vector, so "ALL_KEYS" would match nothing.
#'   For trialType = "numberline" or "angleline": ignored (these plugins handle their own interaction model).
#'   choices does not control what can be typed into an html textbox input field — that is controlled by the html input code plus the kind argument. DEFAULT = "ALL_KEYS".
#' @param kind A string that specifies the type of allowable input in a textbox. Use "string" to allow all input, "number" to allow numbers, and "other" to restrict the textbox to the subset of keys specified in "choices". Only meaningful when trialType = "textbox"; silently ignored (not emitted to JSON) for other trial types. DEFAULT = "string".
#' @param pluginParams A named list of plugin-specific parameters passed through to the jsPsych plugin for this frame. For textbox trials, the 'kind' argument (above) is automatically merged into this list — passing 'kind' both as a named argument AND inside pluginParams is an error. For future custom plugins (Cyberball etc.), pass their specific parameters here. DEFAULT = NULL.
#' @param background an RGB color, specified in hexadecimal, that controls the background color of the frame page. DEFAULT = "#000000" (black).
#' @param cursorVisible Three-state, like trial_duration. NULL (the default) omits the key, which tells the engine to decide from the trialType: hidden for a keyboard-response frame, where a pointer sitting over the stimulus is a distractor, and shown for a type that declares usesPointer because the participant answers it with the mouse. TRUE or FALSE overrides that decision for this frame. ⚠ An explicit FALSE on a pointer-driven type hides the cursor the participant needs in order to respond, so it warns. DEFAULT = NULL (let the trialType decide).
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
#'
#' # Limited exposure: show the stimulus for 2 s, then hide it but keep waiting
#' # for the response for as long as the participant needs.
#' addFrameToQCEframeList (frameList, trialType = "key", frameName = "masked",
#'   stimulus = myStimString, stimulus_duration = 2000, trial_duration = "NO_LIMIT",
#'   post_trial_gap = 0, choices = c("f", "j"))
#'
#' # The same, but cut the response window off at 10 s.
#' addFrameToQCEframeList (frameList, trialType = "key", frameName = "masked",
#'   stimulus = myStimString, stimulus_duration = 2000, trial_duration = 10000,
#'   post_trial_gap = 0, choices = c("f", "j"))

addFrameToQCEframeList <- function (QCEframeList = NULL, trialType = "key", frameName = NULL, stimulus = NULL,	stimulus_duration = NULL, post_trial_gap = NULL, response_ends_trial = TRUE, choices = "ALL_KEYS", kind = "string", background = "#000000", cursorVisible = NULL, output = TRUE, trigger = NULL, pluginParams = NULL, trial_duration = NULL) {

  # trialType is validated against the QCEB trialType registry (mirrors the
  # engine's trialTypeRegistry.js), NOT a hard-coded list, so custom/third-party
  # plugins are accepted once registered with registerQCEBtrialType(). The core
  # types (key/textbox/numberline/angleline) and the bundled "survey" plugin are
  # pre-registered. For surveys, prefer addSurveyFrameToQCEframeList(), which
  # serializes a SurveyJS model into the stimulus for you.
  if(!isRegisteredQCEBtrialType(trialType)) {
    stop("trialType '", trialType, "' is not registered. Registered types: ",
         paste(getRegisteredQCEBtrialTypes(), collapse = ", "),
         ". Register custom plugin types with registerQCEBtrialType().")
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

  # trial_duration is three-state: NULL (inherit stimulus_duration), the
  # sentinel "NO_LIMIT" (no timer at all), or a positive number of milliseconds.
  isNoLimitDuration <- is.character(trial_duration) && length(trial_duration) == 1 &&
                       toupper(trial_duration) == "NO_LIMIT"
  if(!is.null(trial_duration) && !isNoLimitDuration &&
     !(isSingleNumeric(trial_duration) && is.finite(trial_duration) && trial_duration > 0)) {
    stop("trial_duration option must be NULL, a single positive number of milliseconds, or the sentinel \"NO_LIMIT\".")
  }

  if(!is.character(choices) & !is.null(choices)) {
    stop("choices option must be a vector of charactors representing allowable keys or NULL.")
  }

  # Does anything end this frame on a clock? "NO_LIMIT" says no; an explicit
  # number says yes; NULL defers to stimulus_duration.
  frameHasTimer <- if (isNoLimitDuration) FALSE
                   else if (!is.null(trial_duration)) TRUE
                   else !is.null(stimulus_duration)

  # Can the participant end it instead? Plugins that bring their own response
  # surface always can (registry forceResp). A keyboard frame can only when a
  # response ends the trial AND some key is actually accepted -- an empty or
  # absent choices vector disables the keyboard path, and "NO_KEYS" tells
  # jsPsych to accept nothing.
  frameCanEndOnResponse <-
    .qcebTrialTypeForcesResponse(trialType) ||
    (response_ends_trial == TRUE && !is.null(choices) && length(choices) > 0 &&
     !(length(choices) == 1 && toupper(choices) == "NO_KEYS"))

  # No-exit guard. A frame with neither a clock nor a response path strands the
  # participant on a screen that never advances, which in the browser presents
  # as a frozen experiment with nothing in the log. Caught here instead. This
  # supersedes the narrower response_ends_trial/stimulus_duration pairing, and
  # additionally covers the case where choices alone removes the only exit.
  if(!frameHasTimer && !frameCanEndOnResponse) {
    stop("This frame can never end: it has no time limit (trial_duration is \"NO_LIMIT\", ",
         "or neither duration is set) and no way to respond (response_ends_trial = ",
         response_ends_trial, ", choices = ", deparse(choices), "). ",
         "Give it a stimulus_duration or trial_duration, or keys to press.")
  }

  if(!isColor(background)) {
    stop("background option must be a valid color.")
  }

  # cursorVisible is three-state (see the emit site below for why NULL is not
  # simply FALSE). Only the two stated forms are accepted; anything else is a
  # mistake worth naming rather than coercing.
  if(!is.null(cursorVisible) &&
     !(is.logical(cursorVisible) && length(cursorVisible) == 1 && !is.na(cursorVisible))) {
    stop("cursorVisible option must be NULL (let the trialType decide), TRUE, or FALSE.")
  }

  # A pointer-driven type answered with the pointer hidden is a screen the
  # participant cannot complete, and nothing downstream can detect it: the frame
  # is valid, the plugin loads, and the trial simply never ends. Warn rather than
  # stop, because a deliberate FALSE is legitimate for a plugin that draws its
  # own cursor.
  if(isTRUE(!is.null(cursorVisible) && cursorVisible == FALSE) &&
     .qcebTrialTypeUsesPointer(trialType)) {
    warning("Frame '", frameName, "' sets cursorVisible = FALSE on trialType '", trialType,
            "', which the participant answers with the mouse. The pointer they need in ",
            "order to respond will be hidden. Use NULL to let the trialType decide.")
  }

  if(is.null(choices)) {
    choices <- character()
  } else if(length(choices) == 1 && choices %in% c("ALL_KEYS", "NO_KEYS")) {
    # jsPsych v8 expects "ALL_KEYS"/"NO_KEYS" as JSON scalar strings, not as
    # 1-element arrays. R wraps length-1 vectors as `["ALL_KEYS"]` by default,
    # which jsPsych interprets as a literal one-key list. Box explicitly so
    # the magic strings round-trip correctly. Real key vectors are unaffected.
    choices <- jsonlite::unbox(choices)
  }
  if(is.null(stimulus)) {
    stimulus <- character()
  }
  if(is.null(stimulus_duration)) {
    stimulus_duration <- numeric()
  }
  # No such normalizer for post_trial_gap: it is validated above as a required
  # single number, so it can never be NULL here. Every frame therefore states
  # its own inter-trial gap, and 0 is how you say "none".

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

  tmpList <- list (trialType = trialType, frameName = frameName, stimulus = stimulus,	stimulus_duration = stimulus_duration, post_trial_gap = post_trial_gap, response_ends_trial = response_ends_trial, choices = choices, background = background, output = output)
  # cursorVisible is emitted ONLY when the researcher set it, for the same reason
  # trial_duration is: omitting the key is how a frame says "not specified", and
  # the engine answers that from the trialType's own declaration. Writing a
  # default here instead would put a literal FALSE on every frame, which OUTRANKS
  # the engine's answer -- so a mouse-driven frame would ship with its pointer
  # hidden unless the researcher remembered a key they had no reason to think
  # about. The key is placed where it has always sat, before `output`, so a frame
  # that does state it produces the shape it always has.
  if (!is.null(cursorVisible)) {
    tmpList <- append(tmpList, list(cursorVisible = cursorVisible),
                      after = which(names(tmpList) == "background"))
  }
  # trial_duration is emitted ONLY when the researcher set it. Omitting the key
  # is what tells the engine "not specified -- inherit stimulus_duration", so a
  # frame that never mentions it produces exactly the JSON it always has. The
  # sentinel is boxed for the same reason "ALL_KEYS"/"NO_KEYS" are: it must
  # arrive as a JSON string, not as a one-element array.
  if (!is.null(trial_duration)) {
    tmpList$trial_duration <- if (isNoLimitDuration) jsonlite::unbox(toupper(trial_duration))
                              else trial_duration
  }
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
