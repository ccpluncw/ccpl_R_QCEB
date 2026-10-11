#' This  function is used to create a QCEB dbfile
#'
#' Function that create a QCEB dbfile. A message left \code{NULL} (or given as \code{""}) is left out of the file, and the engine shows its own text; for a blank screen write a blank, such as \code{"&nbsp;"}. Each argument that sets one value stops the build when it is given several.
#' @param condName A string specifying the condition of that this dbfile represents. It is really just a placeholder that you can use to code anything that you want. It will be output in a column in the datafile. DEFAULT = "defaultCond"
#' @param keyMap A list that specifies the mapping of the keys to their meaning for the experiment. Create this list using the buildKeyMap() and addKeyToKeyMap() functions. DEFAULT = NULL
#' @param randomizeKeyMap a Boolean that specifies whether the maping of the keys to their meaning should be randomized everytime the experiment is run. This is useful if you want to randomize the key to meaning mapping for every subject on a single session experiment. DEFAULT = FALSE.
#' @param presentKeyMapAfterTrialNumbers An integer or vector of integers that specify when the participant will be reminded of the keyMap. The keyMap reminder message will show up after each trial number specified in the option.  So, if you want the keyMap reminder to show up after the first and fifth trial, the option should equal c(1,5). DEFAULT = NULL. If NULL, the key is left out of the file and no reminder is shown.
#' @param defaultBackgroundColor a colour, in hexadecimal. Not written: the engine does not use it, so a value given here draws a warning. Pages and messages are shown on a white background with black text (an experiment's \code{expStyle.css} may change that), and a frame's background is its own \code{background}. Default \code{"#000000"}.
#' @param restTrials An integer or vector of integers that that specify the trial numbers that you want a break to occur after (e.g., 50, 100, 150). DEFAULT = NULL. If NULL, the key is left out of the file and no break is shown.
#' @param speedFeedbackParams A speedFeedbackList that specifies the parameters of the speed Feedback. Create this list using the buildSpeedFeedbackList() function.  DEFAULT = NULL.  If NULL, no speed feedback will be provided.
#' @param instructionFile A string or vector of strings that specifies the name of the html file(s) that contains the instructions.  It will be shown at the begining of the experiment.  If you have multiple instruction files, they should be entered in the order you would like them presented. If this is NULL, then no instructions will be shown. DEFAULT = NULL.
#' @param keyMapInstructionFile  A string that specifies the name of the html file that contains the mapping between the keys and their meaning (e.g., "Press the "d" key to indicate YES).  It will be shown at the begining of the experiment.  DEFAULT = NULL.  If NULL, the engine generates the key map screen itself from the keyMap you supplied, so most experiments should leave this alone. Supply a filename only when you want to replace that generated screen with your own html. NOTE: the string "default" is NOT accepted -- it was a sentinel in engine versions before 9.1, and the current engine reads this field as a literal filename, so "default" would send it looking for a file of that name. Use NULL to mean "generate it for me".
#' @param restMsg A string that specifies the rest message to be shown at the beginning of a break. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, the key is left out of the file and the engine shows its own message, "Please take a self-timed break. Press any key to resume the experiment."
#' @param friendlyReminderMsg A string that specifies the "this is a friendly reminder" message to be shown when presenting the keymap reminder. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, the key is left out of the file and the engine shows its own message, "This is a friendly reminder."
#' @param remindMsg A string that specifies a message that the keymap reminder might be shown again. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, the key is left out of the file and the engine shows its own message, "We may present this screen again during the experiment to remind you of the keys."
#' @param proceedMsg A string that specifies a message to hit any key to proceed. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, the key is left out of the file and the engine shows its own message, "Please hit any key to proceed."
#' @param enableTriggers A boolean that enables fNIRS trigger support for this group (session). When TRUE, the engine connects to a local Python relay (tools/fnirsRelay.py) at session start, forwards trigger codes declared at block/set/trial/frame levels to an LSL stream, and writes a separate `_triggers.finalDat` event log alongside the main behavioral data. Silent fallback if the relay is not running (experiment proceeds normally with no triggers sent). DEFAULT = FALSE -- no overhead and identical behavior to pre-Phase-1.5 experiments.
#' @param triggerRelayPort An integer specifying the port the local Python relay listens on. Only meaningful when enableTriggers = TRUE. Must match the `--port` the relay was started with. DEFAULT = 5678.
#' @param restEveryNMinutes Optional positive numeric -- fire a rest break every N minutes of elapsed task time, in addition to any thresholds in `restTrials`. The engine measures elapsed time from the most recent rest (or first trial completion). NULL means no time-based rests. DEFAULT = NULL.
#' @param restMaxTrial Optional positive integer -- suppress all rest breaks once `trialsShown` reaches this number. Useful for experiments where late-stage rests would disrupt a flow state. NULL means no trial-count cap. DEFAULT = NULL.
#' @param keyMaps Optional named list of keyMap entries (each from buildQCEkeyMapEntry). Phase 3.5 named-keyMap registry -- blocks reference these entries by name via the `keyMapName` arg of addBlockToQCETrialStructureList. NULL means no named keyMaps (legacy single-keyMap experiments work unchanged via the `keyMap` arg). For incremental construction, use addKeyMapToDbfile after this function returns. DEFAULT = NULL.
#' @param customHooksFile Optional single filename ending in `.js` -- the Phase 5 researcher-authored custom-hooks file (defines the global `QCEPHooks` object). NULL means no hooks (legacy experiments take the byte-identical no-hooks code path). Use saveCustomHooksTemplate() to scaffold the file, and copy it into the experiment directory. For incremental construction, use addHooksToQCEgroupDbFile after this function returns. DEFAULT = NULL.
#' @param customHooksColumns Optional character vector of the data columns the hooks will write via `dataAnnotations`. Declaring them lets buildQCEoutputFieldManifest report them and missingQCEoutputFields fail a build when fields.txt does not carry one; they are invisible to every check otherwise, existing only inside JavaScript. Only meaningful when customHooksFile is set. DEFAULT = NULL.
#' @param customHooksStateKeys Optional character vector of state-key names the hooks will write to `qceState.custom`, used by the engine to statically validate `stateRef` showIf conditions (built with buildQCEstateCondition) -- an undeclared stateRef errors at session start as a likely typo. Only meaningful when customHooksFile is set. NULL means no declared keys (engine warns rather than errors on unknown stateRefs). DEFAULT = NULL.
#'
#' @return the QCEBdbfileList
#' @keywords QCE QCEBdbfileList dbfile
#' @export
#' @examples
#' # Standard (no fNIRS)
#' buildQCEgroupDbFile (condName="TestCond", keyMap = myQCEBkeymap, randomizeKeyMap = TRUE,
#'   defaultBackgroundColor = "#000000", instructionFile = "instructions.html",
#'   keyMapInstructionFile = "kmInst.html")
#'
#' # fNIRS-enabled
#' buildQCEgroupDbFile (condName="fNIRS_session", keyMap = myQCEBkeymap,
#'   enableTriggers = TRUE, triggerRelayPort = 5678)
#'
#' # With time-based rests every 10 minutes, suppressed past trial 200
#' buildQCEgroupDbFile (condName="LongSession", keyMap = myQCEBkeymap,
#'   restTrials = c(50, 100, 150),
#'   restEveryNMinutes = 10, restMaxTrial = 200)

buildQCEgroupDbFile <- function (condName="defaultCond", keyMap = NULL, randomizeKeyMap = FALSE, presentKeyMapAfterTrialNumbers = NULL, defaultBackgroundColor = "#000000", restTrials = NULL, speedFeedbackParams = NULL, instructionFile = NULL, keyMapInstructionFile = NULL, restMsg = NULL,  friendlyReminderMsg = NULL, remindMsg = NULL, proceedMsg = NULL, enableTriggers = FALSE, triggerRelayPort = 5678, restEveryNMinutes = NULL, restMaxTrial = NULL, keyMaps = NULL, customHooksFile = NULL, customHooksStateKeys = NULL, customHooksColumns = NULL ) {

  .qcebOneValueArgs("buildQCEgroupDbFile", c("condName", "randomizeKeyMap",
    "keyMapInstructionFile", "restMsg", "friendlyReminderMsg", "remindMsg", "proceedMsg",
    "enableTriggers", "triggerRelayPort", "restEveryNMinutes", "restMaxTrial", "customHooksFile"))
  #the engine does not use it, so it is not written
  .qcebUnusedArgs("buildQCEgroupDbFile", list(defaultBackgroundColor = !missing(defaultBackgroundColor)))

  if(!isSingleString(condName)) {
    stop("condName option must be a single string.  Yours, apparently, is not a single string.")
  }

  if(!is.null(instructionFile)) {
    for(iFile in instructionFile) {
      if(!isValidFilename(iFile, "html")) {
          stop("instructionFile option must be a single filename that ends in '.html' or NULL.  Yours, apparently, is not.")
      }
    }
  }

  #the sentinel "default" is rejected, absence is NULL
  if(!is.null(keyMapInstructionFile)){
    if(isSingleString(keyMapInstructionFile) && keyMapInstructionFile == "default") {
        stop("keyMapInstructionFile = \"default\" is no longer supported.  The engine reads this option as a literal filename, so it would request a file named 'default' and fail.  Use NULL (the default) to have the engine build the key map instruction screen for you, or give the name of an actual '.html' file.")
    }
    if(!isValidFilename(keyMapInstructionFile, "html")) {
        stop("keyMapInstructionFile option must be a single filename that ends in '.html' or NULL.  Yours, apparently, is not.")
    }
  }

  if(!is.null(restMsg) && !isSingleString(restMsg)) {
      stop("restMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the restMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
  }


  if(!is.null(friendlyReminderMsg) && !isSingleString(friendlyReminderMsg)) {
      stop("friendlyReminderMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the friendlyReminderMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
  }

  if(!is.null(remindMsg) && !isSingleString(remindMsg)) {
      stop("remindMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the remindMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
  }

  if(!is.null(proceedMsg) && !isSingleString(proceedMsg)) {
      stop("proceedMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the proceedMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
  }

  if (!is.null(restEveryNMinutes)) {
    if (!isSingleNumeric(restEveryNMinutes) || restEveryNMinutes <= 0) {
      stop("restEveryNMinutes option must be a single positive number (minutes between rest breaks) or NULL.")
    }
  }

  if (!is.null(restMaxTrial)) {
    if (!isSingleNumeric(restMaxTrial) || restMaxTrial <= 0) {
      stop("restMaxTrial option must be a single positive integer (trial number after which rests are suppressed) or NULL.")
    }
  }

  tmpList <- list (condName= condName, keyMap = keyMap, randomizeKeyMap = randomizeKeyMap, presentKeyMapAfterTrialNumbers=presentKeyMapAfterTrialNumbers, restTrials = restTrials, speedFeedbackParams = speedFeedbackParams, instructionFile = instructionFile, keyMapInstructionFile = keyMapInstructionFile, restMsg = restMsg, friendlyReminderMsg = friendlyReminderMsg, remindMsg = remindMsg, proceedMsg = proceedMsg)

  #triggers block is emitted only when enabled
  if (isTRUE(enableTriggers)) {
    tmpList$triggers <- list(enabled = TRUE, relayPort = as.integer(triggerRelayPort))
  }

  #rest extension fields are emitted only when set
  if (!is.null(restEveryNMinutes)) {
    tmpList$restEveryNMinutes <- restEveryNMinutes
  }
  if (!is.null(restMaxTrial)) {
    tmpList$restMaxTrial <- as.integer(restMaxTrial)
  }

  #keyMaps is emitted only when supplied, each entry validated as on add
  if (!is.null(keyMaps)) {
    if (!is.list(keyMaps) || is.null(names(keyMaps)) || any(names(keyMaps) == "")) {
      stop("keyMaps option must be a named list of keyMap entries when present ",
           "(each entry from buildQCEkeyMapEntry, with the list name being the ",
           "name blocks use to reference it).")
    }
    for (kmName in names(keyMaps)) {
      tmpList <- addKeyMapToDbfile(tmpList, kmName, keyMaps[[kmName]])
    }
  }

  #hooks file and state keys are emitted only when supplied, keys alone warn
  if (!is.null(customHooksFile)) {
    tmpList <- addHooksToQCEgroupDbFile(tmpList, customHooksFile, customHooksStateKeys,
                                       customHooksColumns)
  } else if (!is.null(customHooksStateKeys)) {
    warning("customHooksStateKeys was supplied without customHooksFile; ignoring ",
            "(state keys only matter when a hooks file is declared).")
  }


  return(tmpList)

}
