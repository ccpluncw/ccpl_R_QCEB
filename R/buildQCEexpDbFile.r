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
#' @param saveDataEveryNTrials A single positive integer specifying how often (in trials) the data is incrementally saved to the server during the experiment. The final save always occurs at the end regardless of this value. DEFAULT = 50.
#' @param completionGate Optional named list gating the end-of-experiment completion redirect (e.g. a SONA credit URL) on an EXPERIMENT-WIDE criterion computed over the whole-run data (every trial from every session). Use exactly ONE of two forms. (1) Formula: `formula` = a list of flat formulas, each a list with `fn` (one of "mean", "median", "proportion", "count", "sum", "min", "max", "sd"), `column` (a data column name, e.g. "correct" or "rt"), `op` (one of ">=", "<=", ">", "<", "==", "!="), `value` (a single finite number; for `proportion` in [0,1]), and optional `where` (a named list of column filters, each a scalar for equality or a list(op=, value=) for a range; an ordering op requires a finite numeric value. Equality normalizes booleans to 1/0, so TRUE/1 and FALSE/0 are interchangeable and a boolean column filters the same way it aggregates; strings are NOT coerced, so "1" does not match TRUE); plus optional `combinator` = "all" (default) or "any". proportion(column) = mean of the column coerced to 0/1; count(column) = number of non-missing rows. (2) Escape hatch: `gateFn` = a single string naming a global JS function called with (custom, experimentData) that returns a boolean. `experimentData` is a PLAIN JavaScript ARRAY of trial row objects (every trial from every session), not a jsPsych DataCollection -- so use standard array methods (filter/map/reduce), not DataCollection query methods such as select(). A gateFn needing the DataCollection API can obtain it directly via myJsPsych.data.get(), which is global. Both forms accept an optional `noCreditMsg` (HTML shown on a fail). The engine evaluates the gate ONCE after all sessions; on failure it suppresses the redirect and shows the no-credit message. Fail-closed: a formula that cannot be evaluated (unknown column, wrong-type value, empty sample) counts as UNKNOWN rather than pass, so it can never grant credit on its own, and it is always logged as a warning. Under `combinator` "all" a single unknown therefore denies. Under "any" a soundly passing formula still grants, since an unevaluable alternative cannot revoke a criterion already met. A gateFn that is missing or errors denies outright. NULL means no gate (the redirect fires unconditionally, as before). DEFAULT = NULL.
#' @param maxExperimentMinutes Optional single positive number: a generous WHOLE-EXPERIMENT wall-clock cap in minutes, stamped once at experiment start. Once exceeded, the engine skips remaining stimuli at stimulus boundaries and ends the run gracefully (into the completion gate + save/end nodes). A backstop against leaving the tab open indefinitely; set well above the task's expected length. NULL means uncapped. DEFAULT = NULL.
#''
#' @return the QCEBdbfileList
#' @keywords QCE QCEBdbfileList dbfile
#' @export
#' @examples buildQCEdbFile (expName = "myExp", addQualtricsCode = TRUE, defaultBackgroundColor = "#000000", restAfterEveryNTrials = c(50, 100), instructionFile = "instructions.html", keyMapInstructionFile = "kmInst.html", getUserNameFile = NULL, getConsentFile = "consent.html", getDemographicsFile = NULL, getGenderFile = NULL, welcomeMsg = NULL, restMsg = NULL, endOfExpMsg = NULL, saveMsg = NULL)

buildQCEexpDbFile <- function (expName = "defaultExpName", addQualtricsCode = FALSE, defaultBackgroundColor = "#000000", restAfterEveryNTrials = -1, instructionFile = NULL, getUserNameFile = NULL, getConsentFile = NULL, getDemographicsFile = NULL, getGenderFile = NULL, welcomeMsg = NULL, restMsg = NULL, endOfSessionMsg = NULL, endOfExpMsg = NULL, saveMsg = NULL, closeBrowserMsg = NULL, fullscreenMsg = NULL, fullscreenBtn = "Continue", completionRedirect = NULL, saveDataEveryNTrials = 50, completionGate = NULL, maxExperimentMinutes = NULL) {

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
    welcomeMsg <- "<p>Welcome to the experiment. Press any key to begin.</p>"
  } else {
    if(!isSingleString(welcomeMsg)) {
      stop("welcomeMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the welcomeMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(is.null(restMsg)) {
    restMsg <- "<p>Please take a self-timed break. Press any key to resume the experiment.</p>"
  } else {
    if(!isSingleString(restMsg)) {
      stop("restMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the restMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(is.null(endOfExpMsg)) {
    endOfExpMsg <- "<p>Thank you for taking part in the experiment.</p>"
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
    saveMsg <- "<p>Your data is being saved. Please do not close this window until you are told to.  Please press any key to continue</p>"
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
    closeBrowserMsg <- "Please hit the ENTER key and then you may close this browser window"
  } else {
    if(!isSingleString(closeBrowserMsg)) {
      stop("closeBrowserMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the closeBrowserMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(!is.null(restAfterEveryNTrials)) {
    restAfterEveryNTrials <- as.integer(restAfterEveryNTrials)
    if(any(is.na(restAfterEveryNTrials))) {
      stop("restAfterEveryNTrials option must an integer, a vector of integers, or NULL.")
    }
  }

  saveDataEveryNTrials <- as.integer(saveDataEveryNTrials)
  if(length(saveDataEveryNTrials) != 1 || is.na(saveDataEveryNTrials) || saveDataEveryNTrials < 1) {
    stop("saveDataEveryNTrials option must be a single positive integer.")
  }


  tmpList <- list (expName = expName, addQualtricsCode = addQualtricsCode, defaultBackgroundColor = defaultBackgroundColor, restAfterEveryNTrials = restAfterEveryNTrials,  instructionFile = instructionFile, getUserNameFile = getUserNameFile, getConsentFile = getConsentFile, getDemographicsFile = getDemographicsFile, getGenderFile = getGenderFile, welcomeMsg = welcomeMsg, restMsg = restMsg, endOfSessionMsg = endOfSessionMsg, endOfExpMsg = endOfExpMsg, saveMsg= saveMsg, closeBrowserMsg = closeBrowserMsg, fullscreenMsg = fullscreenMsg, fullscreenBtn = fullscreenBtn, completionRedirect = completionRedirect, saveDataEveryNTrials = saveDataEveryNTrials)

  # Experiment-wide completion gate -- emit only when supplied, so legacy dbfiles
  # are byte-identical and the redirect fires unconditionally as before. Exactly
  # one form: a non-empty `formula` list (flat aggregate criteria over the whole-run
  # data) OR a `gateFn` string (escape hatch). Mirrors the engine's config-time
  # validation so a bad gate fails here at BUILD time, not only at experiment start.
  # The engine evaluates it once after all sessions.
  if (!is.null(completionGate)) {
    if (!is.list(completionGate)) {
      stop("completionGate must be a named list (formula | gateFn, plus optional noCreditMsg).")
    }
    hasFn <- !is.null(completionGate$gateFn)
    hasFormula <- !is.null(completionGate$formula) && length(completionGate$formula) > 0
    if (hasFn && hasFormula) {
      stop("completionGate must have EITHER a 'formula' list OR a 'gateFn' string, not both.")
    }
    if (!hasFn && !hasFormula) {
      stop("completionGate needs a non-empty 'formula' list or a 'gateFn' string.")
    }
    if (hasFn) {
      if (!isSingleString(completionGate$gateFn)) {
        stop("completionGate$gateFn must be a single string naming a global JS function.")
      }
    } else {
      validOps <- c(">=", "<=", ">", "<", "==", "!=")
      validFns <- c("mean", "median", "proportion", "count", "sum", "min", "max", "sd")
      for (i in seq_along(completionGate$formula)) {
        f <- completionGate$formula[[i]]
        if (!is.list(f) || is.null(f$fn) || is.null(f$column) || is.null(f$op) || is.null(f$value)) {
          stop(sprintf("completionGate formula %d must be a list with 'fn', 'column', 'op', and 'value'.", i))
        }
        if (!(f$fn %in% validFns)) {
          stop(sprintf("completionGate formula %d has invalid fn '%s'. Valid: %s.",
                       i, as.character(f$fn), paste(validFns, collapse = " ")))
        }
        if (!(f$op %in% validOps)) {
          stop(sprintf("completionGate formula %d has invalid op '%s'. Valid: %s.",
                       i, as.character(f$op), paste(validOps, collapse = " ")))
        }
        if (!is.numeric(f$value) || length(f$value) != 1 || !is.finite(f$value)) {
          stop(sprintf("completionGate formula %d 'value' must be a single finite number.", i))
        }
        if (f$fn == "proportion" && (f$value < 0 || f$value > 1)) {
          stop(sprintf("completionGate formula %d (proportion) 'value' must be in [0,1].", i))
        }
        if (!is.null(f$where)) {
          if (!is.list(f$where)) {
            stop(sprintf("completionGate formula %d 'where' must be a named list of column filters.", i))
          }
          for (wc in names(f$where)) {
            spec <- f$where[[wc]]
            if (is.list(spec) && !is.null(spec$op) && !(spec$op %in% validOps)) {
              stop(sprintf("completionGate formula %d 'where$%s' has invalid op '%s'.",
                           i, wc, as.character(spec$op)))
            }
            # An ordering op compares numerically, so a non-numeric bound can never
            # match a row -- it would silently filter the sample to empty at run
            # time. Rejected here as well as in the engine.
            if (is.list(spec) && !is.null(spec$op) && !(spec$op %in% c("==", "!="))) {
              if (is.null(spec$value)) {
                stop(sprintf("completionGate formula %d 'where$%s' is missing a 'value'.", i, wc))
              }
              wv <- spec$value[[1]]
              if (is.logical(wv) || length(wv) != 1 || is.na(suppressWarnings(as.numeric(wv)))) {
                stop(sprintf(paste0("completionGate formula %d 'where$%s' uses ordering op '%s' ",
                                    "so its 'value' must be a single finite number."),
                             i, wc, as.character(spec$op)))
              }
            }
          }
        }
      }
      if (!is.null(completionGate$combinator) &&
          !(completionGate$combinator %in% c("all", "any"))) {
        stop("completionGate$combinator must be 'all' or 'any'.")
      }
    }
    tmpList$completionGate <- completionGate
  }

  # Whole-experiment hard deadline (minutes) -- emit only when supplied; absent
  # means uncapped (legacy). The engine stamps the wall-clock end-time once at
  # experiment start and skips remaining stimuli once past it.
  if (!is.null(maxExperimentMinutes)) {
    if (!is.numeric(maxExperimentMinutes) || length(maxExperimentMinutes) != 1 || maxExperimentMinutes <= 0) {
      stop("maxExperimentMinutes must be a single positive number of minutes.")
    }
    tmpList$maxExperimentMinutes <- maxExperimentMinutes
  }

  return(tmpList)

}
