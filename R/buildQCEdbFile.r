#' This  function is used to create a QCEB dbfile
#'
#' Function that create a QCEB dbfile.
#' @param expName A string specifying the name of the experiment.  It will be output in a column in the datafile. DEFAULT = "defaultExpName"
#' @param condName A string specifying the condition of that this dbfile represents. It is really just a placeholder that you can use to code anything that you want. It will be output in a column in the datafile. DEFAULT = "defaultCond"
#' @param QCEkeyMap A list that specifies the maping of the keys to their meaning for the experiment. Create this lise using the buildKeyMap() and addKeytoKeyMap() functions. DEFAULT = NULL
#' @param condName A string specifying the condition of that this dbfile represents. It is really just a placeholder that you can use to code anything that you want. It will be output in a column in the datafile. DEFAULT = "defaultCond"
#' @param randomizeKeyMap a Boolean that specifies whether the maping of the keys to their meaning should be randomized everytime the experiment is run. This is useful if you want to randomize the key to meaning mapping for every subject on a single session experiment. DEFAULT = FALSE.
#' @param presentKeyMapAfterTrialNumbers An integer or vector of integers that specify when the participant will be reminded of the keyMap. The keyMap reminder message will show up after each trial number specified in the option.  So, if you want the keyMap reminder to show up after the first and fifth trial, the option should equal c(1,5). DEFAULT = -1.  If -1, then no reminder will be shown.
#' @param addQualtricsCode a Boolean that specifies whether to present a time code at the end of the experiment with a message that states asks the user to input the code in a Qualtrics window. This is useful if you want to run the experiment using Qualtrics to randomize conditions and/or assign automatic credits. DEFAULT = FALSE.
#' @param defaultBackgroundColor an rgb color that specifies the default background color of the experiment pages. DEFAULT = "#000000" (black).
#' @param restAfterEveryNTrials An integer or vector of integers that that specify the trial numbers that you want a break to occur after (e.g., 50, 100, 150). DEFAULT = -1.  If -1, then no break will be shown.
#' @param speedFeedbackParams A speedFeedbackList that specifies the parameters of the speed Feedback. Create this list using the buildSpeedFeedbackList() function.  DEFAULT = NULL.  If NULL, no speed feedback will be provided.
#' @param instructionFile A string or vector of strings that specifies the name of the html file(s) that contains the instructions.  It will be shown at the begining of the experiment.  If you have multiple instruction files, they should be entered in the order you would like them presented. If this is NULL, then no instructions will be shown. DEFAULT = NULL.
#' @param keyMapInstructionFile  A string that specifies the name of the html file that contains the mapping between the keys and their meaning (e.g., "Press the "d" key to indicate YES).  It will be shown at the begining of the experiment.  If this is NULL, then no keyMap instructions will be shown. DEFAULT = "default". if "default" then the program will build a key map instruction file automatically.
#' @param getUserNameFile  A string that specifies the name of the html file that collects the users identifying information (e.g., a random number).  It will be shown at the begining of the experiment.  If this is NULL, then this information will not be collected. DEFAULT = NULL.
#' @param getConsentFile  A string that specifies the name of the html file that collects the users consent for participating.  It will be shown at the begining of the experiment.  If this is NULL, then this information will not be collected. DEFAULT = NULL.
#' @param getDemographicsFile  A string that specifies the name of the html file that collects the users demongraphic information (e.g., age).  It will be shown at the begining of the experiment.  If this is NULL, then this information will not be collected. DEFAULT = NULL.
#' @param getGenderFile  A string that specifies the name of the html file that collects the users gender information.  It will be shown at the begining of the experiment.  If this is NULL, then this information will not be collected. DEFAULT = NULL.
#' @param welcomeMsg A string that specifies the welcome message to be shown at the beginning of the experiment. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Welcome to the experiment. Press any key to begin."
#' @param restMsg A string that specifies the rest message to be shown at the beginning of a break. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Please take a self-timed break. Press any key to resume the experiment."
#' @param endOfExpMsg A string that specifies the end of experiment message to be shown at the end of the experiment. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Thank you for taking part in the experiment."
#' @param saveMsg A string that specifies the data is saving message to be shown at the end of the experiment. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Your data is being saved. Please do not close this window until you are told to.  Please press any key to continue."
#''
#' @return the QCEBdbfileList
#' @keywords QCE QCEBdbfileList dbfile
#' @export
#' @examples buildQCEdbFile (expName = "myExp", condName="TestCond", keyMap = myQCEBkeymap, randomizeKeyMap = TRUE, addQualtricsCode = TRUE, defaultBackgroundColor = "#000000", restAfterEveryNTrials = c(50, 100), instructionFile = "instructions.html", keyMapInstructionFile = "kmInst.html", getUserNameFile = NULL, getConsentFile = "consent.html", getDemographicsFile = NULL, getGenderFile = NULL, welcomeMsg = NULL, restMsg = NULL, endOfExpMsg = NULL, saveMsg = NULL)

buildQCEdbFile <- function (expName = "defaultExpName", condName="defaultCond", keyMap = NULL, randomizeKeyMap = FALSE, presentKeyMapAfterTrialNumbers = -1, addQualtricsCode = FALSE, defaultBackgroundColor = "#000000", restAfterEveryNTrials = -1, speedFeedbackParams = NULL, instructionFile = NULL, keyMapInstructionFile = "default", getUserNameFile = NULL, getConsentFile = NULL, getDemographicsFile = NULL, getGenderFile = NULL, welcomeMsg = NULL, restMsg = NULL, endOfExpMsg = NULL, saveMsg = NULL) {

  if(!isSingleString(expName)) {
    stop("expName option must be a single string.  Yours, apparently, is not a single string.")
  }

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

  if(!isValidFilename(getUserNameFile, "html") & !is.null(getUserNameFile)) {
      stop("getUserNameFile option must be a single filename that ends in '.html' or NULL.  Yours, apparently, is not.")
  }

  if(!isValidFilename(getDemographicsFile, "html") & !is.null(getDemographicsFile)) {
      stop("getDemographicsFile option must be a single filename that ends in '.html' or NULL.  Yours, apparently, is not.")
  }

  if(!isValidFilename(getConsentFile, "html") & !is.null(getConsentFile)) {
      stop("getConsentFile option must be a single filename that ends in '.html' or NULL.  Yours, apparently, is not.")
  }

  if(!isValidFilename(getGenderFile, "html") & !is.null(getGenderFile)) {
      stop("getGenderFile option must be a single filename that ends in '.html' or NULL.  Yours, apparently, is not.")
  }

  if(is.null(welcomeMsg)) {
    welcomeMsg <- "<p style=\"color:white\">Welcome to the experiment. Press any key to begin.</p>"
  } else {
    if(!isSingleString(welcomeMsg)) {
      stop("welcomeMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the welcomeMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(is.null(restMsg)) {
    restMsg <- "<p style=\"color:white\">Please take a self-timed break. Press any key to resume the experiment.</p>"
  } else {
    if(!isSingleString(restMsg)) {
      stop("restMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the restMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(is.null(endOfExpMsg)) {
    endOfExpMsg <- "<p style=\"color:white\">Thank you for taking part in the experiment.</p>"
  } else {
    if(!isSingleString(endOfExpMsg)) {
      stop("endOfExpMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the endOfExpMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(is.null(saveMsg)) {
    saveMsg <- "<p style=\"color:white\">Your data is being saved. Please do not close this window until you are told to.  Please press any key to continue</p>"
  } else {
    if(!isSingleString(saveMsg)) {
      stop("saveMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the saveMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }


  if(!is.null(restAfterEveryNTrials)) {
    restAfterEveryNTrials <- as.integer(restAfterEveryNTrials)
    if(any(is.na(restAfterEveryNTrials))) {
      stop("restAfterEveryNTrials option must an integer, a vector of integers, or NULL.")
    }
  }

  tmpList <- list (expName = expName, condName= condName, keyMap = keyMap, randomizeKeyMap = randomizeKeyMap, presentKeyMapAfterTrialNumbers=presentKeyMapAfterTrialNumbers, addQualtricsCode = addQualtricsCode, defaultBackgroundColor = defaultBackgroundColor, restAfterEveryNTrials = restAfterEveryNTrials, speedFeedbackParams = speedFeedbackParams, instructionFile = instructionFile, keyMapInstructionFile = keyMapInstructionFile, getUserNameFile = getUserNameFile, getConsentFile = getConsentFile, getDemographicsFile = getDemographicsFile, getGenderFile = getGenderFile, welcomeMsg = welcomeMsg, restMsg = restMsg, endOfExpMsg = endOfExpMsg, saveMsg= saveMsg)

  return(tmpList)

}
