#' This  function is used to create a QCEB dbfile for the entire experiment
#'
#' Function that create a QCEB dbfile.
#' @param expName A string specifying the name of the experiment.  It will be output in a column in the datafile. DEFAULT = "defaultExpName"
#' @param addQualtricsCode a Boolean that specifies whether to present a time code at the end of the experiment with a message that states asks the user to input the code in a Qualtrics window. This is useful if you want to run the experiment using Qualtrics to randomize conditions and/or assign automatic credits. DEFAULT = FALSE.
#' @param defaultBackgroundColor an rgb color that specifies the default background color of the experiment pages. DEFAULT = "#000000" (black).
#' @param restAfterEveryNTrials An integer or vector of integers that that specify the trial numbers that you want a break to occur after (e.g., 50, 100, 150). DEFAULT = -1.  If -1, then no break will be shown.
#' @param instructionFile A string or vector of strings that specifies the name of the html file(s) that contains the instructions.  It will be shown at the begining of the experiment.  If you have multiple instruction files, they should be entered in the order you would like them presented. If this is NULL, then no instructions will be shown. DEFAULT = NULL.
#' @param getUserNameFile  A string that specifies the name of the html file that collects the users identifying information (e.g., a random number).  It will be shown at the begining of the experiment.  If this is NULL, then this information will not be collected. DEFAULT = NULL.
#' @param getConsentFile  A string that specifies the name of the html file that collects the users consent for participating.  It will be shown at the begining of the experiment.  If this is NULL, then this information will not be collected. DEFAULT = NULL.
#' @param getDemographicsFile  A string that specifies the name of the html file that collects the users demongraphic information (e.g., age).  It will be shown at the begining of the experiment.  If this is NULL, then this information will not be collected. DEFAULT = NULL.
#' @param getGenderFile  A string that specifies the name of the html file that collects the users gender information.  It will be shown at the begining of the experiment.  If this is NULL, then this information will not be collected. DEFAULT = NULL.
#' @param welcomeMsg A string that specifies the welcome message to be shown at the beginning of the experiment. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Welcome to the experiment. Press any key to begin."
#' @param restMsg A string that specifies the rest message to be shown at the beginning of a break. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Please take a self-timed break. Press any key to resume the experiment."
#' @param endOfExpMsg A string that specifies the end of experiment message to be shown at the end of the experiment. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Thank you for taking part in the experiment."
#' @param endOfSessionMsg A string that specifies the end of a session when a new session is comming up message. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "You have just completed the block. Please press any key to start to the next block."
#' @param saveMsg A string that specifies the data is saving message to be shown at the end of the experiment. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Your data is being saved. Please do not close this window until you are told to.  Please press any key to continue."
#' @param closeBrowserMsg A string that tells the user that they may hit Enter and then close the browser (it is the end of the experiment). The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "Please hit the ENTER key and then you may close this browser window"
#' @param fullscreenMsg A string that specifies a message that clicking the button will put the experiment into full screen mode. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "The experiment will switch to full screen mode when you press the button below."
#' @param fullscreenBtn A string that specifies a the text to put on the button in full screen mode.  DEFAULT = "Continue".
#' @param completionRedirect A string that specifies the return URL that redirects the participant to another site - usually for credit participating (e.g., Prolific).  It must be a proper URL. For example, "https://app.prolific.co/submissions/complete?cc=XXXXXXX" If the redirect is for SONA systems, the redirect must take the sona ID as an argument. The program will work if you change the "survey_code" equal to SONA_ID. For example, "https://www.sona-systems.com/webstudy_credit.aspx?experiment_id=769&credit_token=e05ef9d2f821414180dbb0b3f4ae3e59&survey_code=SONA_ID" If it is not appropriate to redirect, then this should be an empty string. DEFAULT = "".
#''
#' @return the QCEBdbfileList
#' @keywords QCE QCEBdbfileList dbfile
#' @export
#' @examples buildQCEdbFile (expName = "myExp", addQualtricsCode = TRUE, defaultBackgroundColor = "#000000", restAfterEveryNTrials = c(50, 100), instructionFile = "instructions.html", keyMapInstructionFile = "kmInst.html", getUserNameFile = NULL, getConsentFile = "consent.html", getDemographicsFile = NULL, getGenderFile = NULL, welcomeMsg = NULL, restMsg = NULL, endOfExpMsg = NULL, saveMsg = NULL)

buildQCEexpDbFile <- function (expName = "defaultExpName", addQualtricsCode = FALSE, defaultBackgroundColor = "#000000", restAfterEveryNTrials = -1, instructionFile = NULL, getUserNameFile = NULL, getConsentFile = NULL, getDemographicsFile = NULL, getGenderFile = NULL, welcomeMsg = NULL, restMsg = NULL, endOfSessionMsg = NULL, endOfExpMsg = NULL, saveMsg = NULL, closeBrowserMsg = NULL, fullscreenMsg = NULL, fullscreenBtn = "Continue", completionRedirect = NULL) {

  if(!isSingleString(expName)) {
    stop("expName option must be a single string.  Yours, apparently, is not a single string.")
  }

  if(!is.null(instructionFile)) {
    for(iFile in instructionFile) {
      if(!isValidFilename(iFile, "html")) {
          stop("instructionFile option must be a single filename that ends in '.html' or NULL.  Yours, apparently, is not.")
      }
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

  if(is.null(endOfSessionMsg)) {
    endOfSessionMsg <- "You have just completed the block. Please press any key to start to the next block"
  } else {
    if(!isSingleString(endOfSessionMsg)) {
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

  if(is.null(fullscreenMsg)) {
    fullscreenMsg <- "<p>The experiment will switch to full screen mode when you press the button below</p>"
  } else {
    if(!isSingleString(fullscreenMsg)) {
      stop("fullscreenMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the fullscreenMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(is.null(closeBrowserMsg)) {
    fullscreenMsg <- "Please hit the ENTER key and then you may close this browser window"
  } else {
    if(!isSingleString(fullscreenMsg)) {
      stop("fullscreenMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the fullscreenMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(!is.null(restAfterEveryNTrials)) {
    restAfterEveryNTrials <- as.integer(restAfterEveryNTrials)
    if(any(is.na(restAfterEveryNTrials))) {
      stop("restAfterEveryNTrials option must an integer, a vector of integers, or NULL.")
    }
  }


  tmpList <- list (expName = expName, addQualtricsCode = addQualtricsCode, defaultBackgroundColor = defaultBackgroundColor, restAfterEveryNTrials = restAfterEveryNTrials,  instructionFile = instructionFile, getUserNameFile = getUserNameFile, getConsentFile = getConsentFile, getDemographicsFile = getDemographicsFile, getGenderFile = getGenderFile, welcomeMsg = welcomeMsg, restMsg = restMsg, endOfSessionMsg = endOfSessionMsg, endOfExpMsg = endOfExpMsg, saveMsg= saveMsg, closeBrowserMsg = closeBrowserMsg, fullscreenMsg = fullscreenMsg, fullscreenBtn = fullscreenBtn, completionRedirect = completionRedirect)

  return(tmpList)

}
