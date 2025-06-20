#' This  function is used to create a QCEB dbfile
#'
#' Function that create a QCEB dbfile.
#' @param condName A string specifying the condition of that this dbfile represents. It is really just a placeholder that you can use to code anything that you want. It will be output in a column in the datafile. DEFAULT = "defaultCond"
#' @param QCEkeyMap A list that specifies the maping of the keys to their meaning for the experiment. Create this lise using the buildKeyMap() and addKeytoKeyMap() functions. DEFAULT = NULL
#' @param randomizeKeyMap a Boolean that specifies whether the maping of the keys to their meaning should be randomized everytime the experiment is run. This is useful if you want to randomize the key to meaning mapping for every subject on a single session experiment. DEFAULT = FALSE.
#' @param presentKeyMapAfterTrialNumbers An integer or vector of integers that specify when the participant will be reminded of the keyMap. The keyMap reminder message will show up after each trial number specified in the option.  So, if you want the keyMap reminder to show up after the first and fifth trial, the option should equal c(1,5). DEFAULT = -1.  If -1, then no reminder will be shown.
#' @param defaultBackgroundColor an rgb color that specifies the default background color of the experiment pages. DEFAULT = "#000000" (black).
#' @param restTrials An integer or vector of integers that that specify the trial numbers that you want a break to occur after (e.g., 50, 100, 150). DEFAULT = -1.  If -1, then no break will be shown.
#' @param speedFeedbackParams A speedFeedbackList that specifies the parameters of the speed Feedback. Create this list using the buildSpeedFeedbackList() function.  DEFAULT = NULL.  If NULL, no speed feedback will be provided.
#' @param instructionFile A string or vector of strings that specifies the name of the html file(s) that contains the instructions.  It will be shown at the begining of the experiment.  If you have multiple instruction files, they should be entered in the order you would like them presented. If this is NULL, then no instructions will be shown. DEFAULT = NULL.
#' @param keyMapInstructionFile  A string that specifies the name of the html file that contains the mapping between the keys and their meaning (e.g., "Press the "d" key to indicate YES).  It will be shown at the begining of the experiment.  If this is NULL, then no keyMap instructions will be shown. DEFAULT = "default". if "default" then the program will build a key map instruction file automatically.
#' @param restMsg A string that specifies the rest message to be shown at the beginning of a break. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Please take a self-timed break. Press any key to resume the experiment."
#' @param friendlyReminderMsg A string that specifies the "this is a friendly reminder" message to be shown when presenting the keymap reminder. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "This is a friendly reminder."
#' @param remindMsg A string that specifies a message that the keymap reminder might be shown again. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "We may present this screen again during the experiment to remind you of the keys."
#' @param proceedMsg A string that specifies a message to hit any key to proceed. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Please hit any key to proceed."
#''
#' @return the QCEBdbfileList
#' @keywords QCE QCEBdbfileList dbfile
#' @export
#' @examples buildQCEdbFile ( condName="TestCond", keyMap = myQCEBkeymap, randomizeKeyMap = TRUE, defaultBackgroundColor = "#000000", instructionFile = "instructions.html", keyMapInstructionFile = "kmInst.html")

buildQCEgroupDbFile <- function (condName="defaultCond", keyMap = NULL, randomizeKeyMap = FALSE, presentKeyMapAfterTrialNumbers = -1, defaultBackgroundColor = "#000000", restTrials = -1, speedFeedbackParams = NULL, instructionFile = NULL, keyMapInstructionFile = "default", restMsg = NULL,  friendlyReminderMsg = NULL, remindMsg = NULL, proceedMsg = NULL ) {

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

  if(keyMapInstructionFile != "default") {
    if(!isValidFilename(keyMapInstructionFile, "html")) {
        stop("keyMapInstructionFile option must be a single filename that ends in '.html' or be 'default'.  Yours, apparently, is not.")
    }
  }

  if(!isColor(defaultBackgroundColor)) {
    stop("defaultBackgroundColor option must be a valid color.")
  }

  if(is.null(restMsg)) {
    restMsg <- "<p style=\"color:white\">Please take a self-timed break. Press any key to resume the experiment.</p>"
  } else {
    if(!isSingleString(restMsg)) {
      stop("restMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the restMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }


  if(is.null(friendlyReminderMsg)) {
    friendlyReminderMsg <- "This is a friendly reminder:<br><br>"
  } else {
    if(!isSingleString(friendlyReminderMsg)) {
      stop("friendlyReminderMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the friendlyReminderMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(is.null(remindMsg)) {
    remindMsg <- "<br>We may present this screen again during the experiment to remind you of the keys."
  } else {
    if(!isSingleString(remindMsg)) {
      stop("remindMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the remindMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(is.null(proceedMsg)) {
    proceedMsg <- "<br><br>Please hit any key to proceed</center>"
  } else {
    if(!isSingleString(proceedMsg)) {
      stop("proceedMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the proceedMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  tmpList <- list (condName= condName, keyMap = keyMap, randomizeKeyMap = randomizeKeyMap, presentKeyMapAfterTrialNumbers=presentKeyMapAfterTrialNumbers, defaultBackgroundColor = defaultBackgroundColor, restTrials = restTrials, speedFeedbackParams = speedFeedbackParams, instructionFile = instructionFile, keyMapInstructionFile = keyMapInstructionFile, restMsg = restMsg, friendlyReminderMsg = friendlyReminderMsg, remindMsg = remindMsg, proceedMsg = proceedMsg)

  return(tmpList)

}
