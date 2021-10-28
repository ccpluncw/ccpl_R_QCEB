#' This  function is used to build a speedFeedbackParamsList
#'
#' Function that builds a speedFeedbackParamsList.
#' @param showFeedback  A boolean that specifies whether to show the speedFeedback. This is not a trial-by-trial variable.  It applies for the entire experiment.  So, set it to true if you want to give speedFeedback.  Set it to false if you do not want to give speedFeedback. Default = TRUE.
#' @param speedThresholdUp  A boolean that specifies whether the speedFeedback is intended to speed the participant's responses up (make them faster) or slow the particpant down (make them slower).  Default = TRUE (make the participant faster).
#' @param frameName A string specifying the name of the frame that is want to give speed feedback on.  This frameName should be unique and different from the frames that you do not want to provide speed feedback on. DEFAULT = "test"
#' @param windowSize An integer specifying the number of frames (i.e., trials) on which you want to base your speedFeedback threshold. The program will take the most recent "windowSize" frames to create the RTwindowArray. The RTwindowArray will be used, in part, to determine the RT threshold of the next trial. DEFAULT = 20.
#' @param thresholdArrayPosition An integer specifying the array position in the RTwindowArray, sorted from fastest to slowest, to use as the new potential RT threshold. So, if the thresholdArrayPosition = 18 and windowSize = 20, then the 18th slowest RT would be the new potential RT threshold. DEFAULT = 17.
#' @param thresholdBoundary An integer specifying a hard boundary that the RT threshold cannot cross.  If speedThresholdUp = TRUE, then the RT threshold cannot go below this boundary.  If speedThresholdUp = FALSE, then the RT threshold cannot go above this boundary. DEFAULT = NUll.  If NULL, then: thresholdBoundary = 250 if speedThresholdUp = TRUE and thresholdBoundary = 40000 if speedThresholdUp = FALSE.
#' @param initialWindowRtThreshold An integer specifying the initial value of the RT threshold while the first "windowSize" trials are being accumulated.  DEFAULT = NULL. If NULL then: if speedThresholdUp = TRUE, then the initialWindowRtThreshold  = 999999 (very high so it is not activated).  If speedThresholdUp = FALSE, then  initialWindowRtThreshold = -1 (very low so it is not activated).
#' @param initialWindowMessageDisplay A string that specifies the message to be shown to the participant after every trial during the initial period while the first "windowSize" trials are being accumulated. The string must be in html format.  You can use any html codes.  DEFAULT = "&nbsp" (blank).
#' @param initialWindowMessageOutput A string that will be output in the datafile every trial during the initial period while the first "windowSize" trials are being accumulated. DEFAULT = "initial".
#' @param initialStimulusDuration An integer specifying the time, in ms, that the feedback message should be presented during the initial period while the first "windowSize" trials are being accumulated. DEFAULT = 1 (not noticable)
#' @param initialPostTrialGap An integer specifying the time, in ms, between the feedback message and the next frame during the initial period while the first "windowSize" trials are being accumulated. DEFAULT = 1 (not noticable)
#' @param aboveThresholdMessageDisplay A string that specifies the message to be shown to the participant when the current trial's RT is above the RT threshold. The string must be in html format.  You can use any html codes.  DEFAULT = NUll.  If NULL, then the message will be "Too Slow" if speedThresholdUp = TRUE and "&nbsp" if speedThresholdUp = FALSE.
#' @param aboveThresholdMessageOutput A string that will be output in the datafile when the current trial's RT is above the RT threshold. DEFAULT = "above".
#' @param aboveStimulusDuration An integer specifying the time, in ms, that the feedback message should be presented when the current trial's RT is above the RT threshold. DEFAULT = NUll.  If NULL, then aboveStimulusDuration = 500 if speedThresholdUp = TRUE and aboveStimulusDuration = 1 if speedThresholdUp = FALSE.
#' @param abovePostTrialGap An integer specifying the time, in ms, between the feedback message and the next frame when the current trial's RT is above the RT threshold. DEFAULT = NUll.  If NULL, then abovePostTrialGap = 500 if speedThresholdUp = TRUE and abovePostTrialGap = 1 if speedThresholdUp = FALSE.
#' @param belowThresholdMessageDisplay A string that specifies the message to be shown to the participant when the current trial's RT is below the RT threshold. The string must be in html format.  You can use any html codes.  DEFAULT = NUll.  If NULL, then the message will be "Too Fast" if speedThresholdUp = FALSE and "&nbsp" if speedThresholdUp = TRUE.
#' @param belowThresholdMessageOutput A string that will be output in the datafile when the current trial's RT is below the RT threshold. DEFAULT = "below".
#' @param belowStimulusDuration An integer specifying the time, in ms, that the feedback message should be presented when the current trial's RT is below the RT threshold. DEFAULT = NUll.  If NULL, then belowStimulusDuration = 500 if speedThresholdUp = FALSE and belowStimulusDuration = 1 if speedThresholdUp = TRUE.
#' @param belowPostTrialGap An integer specifying the time, in ms, between the feedback message and the next frame when the current trial's RT is above the RT threshold. DEFAULT = NUll.  If NULL, then belowPostTrialGap = 500 if speedThresholdUp = FALSE and belowPostTrialGap = 1 if speedThresholdUp = TRUE.
#''
#' @return the speedFeedbackList
#' @keywords QCE speed feedback speedFeedback
#' @export
#' @examples buildQCEdbFile (expName = "myExp", condName="TestCond", keyMap = myQCEBkeymap, randomizeKeyMap = TRUE, addQualtricsCode = TRUE, defaultBackgroundColor = "#000000", restAfterEveryNTrials = c(50, 100), instructionFile = "instructions.html", keyMapInstructionFile = "kmInst.html", getUserNameFile = NULL, getConsentFile = "consent.html", getDemographicsFile = NULL, getGenderFile = NULL, welcomeMsg = NULL, restMsg = NULL, endOfExpMsg = NULL, saveMsg = NULL)

buildSpeedFeedbackList <- function (showFeedback = TRUE, speedThresholdUp = TRUE, frameName = "test", windowSize = 20, thresholdArrayPosition = 17, thresholdBoundary = NULL, initialWindowRtThreshold = NULL, initialWindowMessageDisplay = "&nbsp", initialWindowMessageOutput = "initial", initialStimulusDuration = 1, initialPostTrialGap = 1, aboveThresholdMessageDisplay = NULL, aboveThresholdMessageOutput = "above", aboveStimulusDuration = NULL, abovePostTrialGap= NULL, belowThresholdMessageDisplay = NULL, belowThresholdMessageOutput = "below", belowStimulusDuration = NULL, belowPostTrialGap = NULL) {

  if(!is.logical(showFeedback)) {
    stop("showFeedback option must be a boolean (TRUE/FALSE).  Yours, apparently, is not.")
  }
  if(!is.logical(speedThresholdUp)) {
    stop("speedThresholdUp option must be a boolean (TRUE/FALSE).  Yours, apparently, is not.")
  }

  if(!isSingleString(frameName)) {
    stop("frameName option must be a single string.  Yours, apparently, is not a single string.")
  }

  if(!isSingleString(initialWindowMessageDisplay)) {
    stop("initialWindowMessageDisplay option must be a single string.  Yours, apparently, is not a single string.")
  }

  if(!isSingleString(initialWindowMessageOutput)) {
    stop("initialWindowMessageOutput option must be a single string.  Yours, apparently, is not a single string.")
  }

  if(!isSingleString(aboveThresholdMessageOutput)) {
    stop("aboveThresholdMessageOutput option must be a single string.  Yours, apparently, is not a single string.")
  }

  if(!isSingleString(belowThresholdMessageOutput)) {
    stop("belowThresholdMessageOutput option must be a single string.  Yours, apparently, is not a single string.")
  }

  if(!isSingleNumeric(windowSize)) {
    stop("windowSize option must be a single integer.  Yours, apparently, is not.")
  }

  if(!isSingleNumeric(thresholdArrayPosition)) {
    stop("thresholdArrayPosition option must be a single integer.  Yours, apparently, is not.")
  }

  if(!isSingleNumeric(initialStimulusDuration)) {
    stop("initialStimulusDuration option must be a single integer.  Yours, apparently, is not.")
  }

  if(!isSingleNumeric(initialPostTrialGap)) {
    stop("initialPostTrialGap option must be a single integer.  Yours, apparently, is not.")
  }

  if(speedThresholdUp == TRUE) {
    if(is.null(thresholdBoundary)) {
      thresholdBoundary <- 250;
    } else {
      if(!isSingleNumeric(thresholdBoundary)) {
        stop("thresholdBoundary option must be a single integer.  Yours, apparently, is not.")
      }
    }

    if(is.null(initialWindowRtThreshold)) {
      initialWindowRtThreshold <- 999999;
    } else {
      if(!isSingleNumeric(initialWindowRtThreshold)) {
        stop("initialWindowRtThreshold option must be a single integer.  Yours, apparently, is not.")
      }
    }

    if(is.null(aboveThresholdMessageDisplay)) {
      aboveThresholdMessageDisplay <- "Too Slow";
    } else {
      if(!isSingleString(aboveThresholdMessageDisplay)) {
        stop("aboveThresholdMessageDisplay option must be a single string.  Yours, apparently, is not.")
      }
    }

    if(is.null(aboveStimulusDuration)) {
      aboveStimulusDuration <- 500;
    } else {
      if(!isSingleNumeric(aboveStimulusDuration)) {
        stop("aboveStimulusDuration option must be a single integer.  Yours, apparently, is not.")
      }
    }

    if(is.null(abovePostTrialGap)) {
      abovePostTrialGap <- 500;
    } else {
      if(!isSingleNumeric(abovePostTrialGap)) {
        stop("abovePostTrialGap option must be a single integer.  Yours, apparently, is not.")
      }
    }

    if(is.null(belowThresholdMessageDisplay)) {
      belowThresholdMessageDisplay <- "&nbsp";
    } else {
      if(!isSingleString(belowThresholdMessageDisplay)) {
        stop("belowThresholdMessageDisplay option must be a single string.  Yours, apparently, is not.")
      }
    }

    if(is.null(belowStimulusDuration)) {
      belowStimulusDuration <- 1;
    } else {
      if(!isSingleNumeric(belowStimulusDuration)) {
        stop("belowStimulusDuration option must be a single integer.  Yours, apparently, is not.")
      }
    }

    if(is.null(belowPostTrialGap)) {
      belowPostTrialGap <- 1;
    } else {
      if(!isSingleNumeric(belowPostTrialGap)) {
        stop("belowPostTrialGap option must be a single integer.  Yours, apparently, is not.")
      }
    }

  } else {
    if(is.null(thresholdBoundary)) {
      thresholdBoundary <- 40000;
    } else {
      if(!isSingleNumeric(thresholdBoundary)) {
        stop("thresholdBoundary option must be a single integer.  Yours, apparently, is not.")
      }
    }

    if(is.null(initialWindowRtThreshold)) {
      initialWindowRtThreshold <- -1;
    } else {
      if(!isSingleNumeric(initialWindowRtThreshold)) {
        stop("initialWindowRtThreshold option must be a single integer.  Yours, apparently, is not.")
      }
    }

    if(is.null(aboveThresholdMessageDisplay)) {
      aboveThresholdMessageDisplay <- "&nbsp";
    } else {
      if(!isSingleString(aboveThresholdMessageDisplay)) {
        stop("aboveThresholdMessageDisplay option must be a single string.  Yours, apparently, is not.")
      }
    }

    if(is.null(aboveStimulusDuration)) {
      aboveStimulusDuration <- 1;
    } else {
      if(!isSingleNumeric(aboveStimulusDuration)) {
        stop("aboveStimulusDuration option must be a single integer.  Yours, apparently, is not.")
      }
    }

    if(is.null(abovePostTrialGap)) {
      abovePostTrialGap <- 1;
    } else {
      if(!isSingleNumeric(abovePostTrialGap)) {
        stop("abovePostTrialGap option must be a single integer.  Yours, apparently, is not.")
      }
    }

    if(is.null(belowThresholdMessageDisplay)) {
      belowThresholdMessageDisplay <- "Too Fast";
    } else {
      if(!isSingleString(belowThresholdMessageDisplay)) {
        stop("belowThresholdMessageDisplay option must be a single string.  Yours, apparently, is not.")
      }
    }

    if(is.null(belowStimulusDuration)) {
      belowStimulusDuration <- 500;
    } else {
      if(!isSingleNumeric(belowStimulusDuration)) {
        stop("belowStimulusDuration option must be a single integer.  Yours, apparently, is not.")
      }
    }

    if(is.null(belowPostTrialGap)) {
      belowPostTrialGap <- 500;
    } else {
      if(!isSingleNumeric(belowPostTrialGap)) {
        stop("belowPostTrialGap option must be a single integer.  Yours, apparently, is not.")
      }
    }

  }


  tmpList <- list (showFeedback = showFeedback, speedThresholdUp = speedThresholdUp, frameName = frameName, windowSize = windowSize, thresholdArrayPosition = thresholdArrayPosition, thresholdBoundary = thresholdBoundary, initialWindowRtThreshold = initialWindowRtThreshold, initialWindowMessageDisplay = initialWindowMessageDisplay, initialWindowMessageOutput = initialWindowMessageOutput, initialStimulusDuration = initialStimulusDuration, initialPostTrialGap = initialPostTrialGap, aboveThresholdMessageDisplay = aboveThresholdMessageDisplay, aboveThresholdMessageOutput = aboveThresholdMessageOutput, aboveStimulusDuration = aboveStimulusDuration, abovePostTrialGap= abovePostTrialGap, belowThresholdMessageDisplay = belowThresholdMessageDisplay, belowThresholdMessageOutput = belowThresholdMessageOutput, belowStimulusDuration = belowStimulusDuration, belowPostTrialGap = belowPostTrialGap)

  return(tmpList)

}
