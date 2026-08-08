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
#' @param closeBrowserMsg A string presented on its own final screen, after the end-of-experiment message, telling the user that they may close the browser. The screen accepts no keypress and remains until the window is closed, so this message should not instruct the user to press a key. It is shown only when no completion redirect will fire, so it never appears on a run that navigates the user elsewhere. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "You may now close this browser window."
#' @param fullscreenMsg A string that specifies a message that clicking the button will put the experiment into full screen mode. The string must be in html format.  You can use any html codes.  DEFAULT = NULL. If NULL, then the following message will be presented, "The experiment will switch to full screen mode when you press the button below."
#' @param fullscreenBtn A string that specifies a the text to put on the button in full screen mode.  DEFAULT = "Continue".
#' @param completionRedirect A string that specifies the return URL that redirects the participant to another site - usually for credit participating (e.g., Prolific).  It must be a proper URL. For example, "https://app.prolific.co/submissions/complete?cc=XXXXXXX" If the redirect is for SONA systems, the redirect must take the sona ID as an argument. The program will work if you change the "survey_code" equal to SONA_ID. For example, "https://www.sona-systems.com/webstudy_credit.aspx?experiment_id=769&credit_token=e05ef9d2f821414180dbb0b3f4ae3e59&survey_code=SONA_ID" If it is not appropriate to redirect, then this should be an empty string. DEFAULT = "".
#' @param saveDataEveryNTrials A single positive integer specifying how often (in trials) the data is incrementally saved to the server during the experiment. The final save always occurs at the end regardless of this value. DEFAULT = 50.
#' @param completionGate Optional named list gating the end-of-experiment completion redirect (e.g. a SONA credit URL) on an EXPERIMENT-WIDE criterion computed over the whole-run data (every trial from every session). Use exactly ONE of two forms. (1) Formula: `formula` = a list of flat formulas, each a list with `fn` (one of "mean", "median", "proportion", "count", "sum", "min", "max", "sd"), `column` (a data column name, e.g. "correct" or "rt"), `op` (one of ">=", "<=", ">", "<", "==", "!="), `value` (a single finite number; for `proportion` in [0,1]), and optional `where` (a named list of column filters, each a scalar for equality or a list(op=, value=) for a range; an ordering op requires a finite numeric value. Equality normalizes booleans to 1/0, so TRUE/1 and FALSE/0 are interchangeable and a boolean column filters the same way it aggregates; strings are NOT coerced, so "1" does not match TRUE); plus optional `combinator` = "all" (default) or "any". proportion(column) = mean of the column coerced to 0/1; count(column) = number of non-missing rows. (2) Escape hatch: `gateFn` = a single string naming a global JS function called with (custom, experimentData) that returns a boolean. `experimentData` is a PLAIN JavaScript ARRAY of trial row objects (every trial from every session), not a jsPsych DataCollection -- so use standard array methods (filter/map/reduce), not DataCollection query methods such as select(). A gateFn needing the DataCollection API can obtain it directly via myJsPsych.data.get(), which is global. Both forms accept an optional `noCreditMsg` (HTML shown on a fail). The engine evaluates the gate ONCE after all sessions; on failure it suppresses the redirect and shows the no-credit message. Fail-closed: a formula that cannot be evaluated (unknown column, wrong-type value, empty sample) counts as UNKNOWN rather than pass, so it can never grant credit on its own, and it is always logged as a warning. Under `combinator` "all" a single unknown therefore denies. Under "any" a soundly passing formula still grants, since an unevaluable alternative cannot revoke a criterion already met. A gateFn that is missing or errors denies outright. NULL means no gate (the redirect fires unconditionally, as before). Three further entries, accepted by BOTH forms, govern repeat attempts. `attemptsAllowed` = a whole number of 1 or more: how many times one participant may be admitted to this material. ABSENT MEANS ONE, which is how every gated study behaved before attempts existed, so an older config is unchanged. `retryPrompt` = a named list with `text`, `yesLabel` and `noLabel`, the question a participant is asked after a failing run to offer the next attempt. The two are only meaningful together and are cross-checked: allowing more than one attempt without a prompt makes the extra attempts unreachable, and offering a prompt while allowing one attempt promises something that cannot be granted. `supersededMsg` = HTML shown when the claim finds the record already settled by a later run. DEFAULT = NULL.
#' @param maxExperimentMinutes Optional single positive number: a generous WHOLE-EXPERIMENT wall-clock cap in minutes, stamped once at experiment start. Once exceeded, the engine skips remaining stimuli at stimulus boundaries and ends the run gracefully (into the completion gate + save/end nodes). A backstop against leaving the tab open indefinitely; set well above the task's expected length. NULL means uncapped. DEFAULT = NULL.
#' @param saveTimeoutMs Optional single positive number: the per-request timeout in milliseconds applied to every data-save POST. A save that neither succeeds nor fails within this window is treated as a failure so the serialized save chain proceeds instead of hanging behind it. Set below any final-save watchdog. NULL uses the engine default (20000). DEFAULT = NULL.
#' @param saveCanary Optional single Boolean gating the start-of-run save health check. When enabled (the engine default), the experiment probes that the save path is writable BEFORE building the timeline and halts the participant before any work if it is not -- bounding a save outage on an unattended run to the cohort already in flight rather than crediting empty runs. Set FALSE to opt a run out. NULL uses the engine default (enabled). DEFAULT = NULL.
#' @param saveUnavailableMsg A string shown on the terminal halt screen when the start-of-run save canary fails. The string must be in html format. You can use any html codes. NULL uses the engine default, which asks the participant to close the window and try again in about 24 hours. DEFAULT = NULL.
#' @param warnOnLeave Optional single Boolean gating the browser's leave-the-page confirmation during a run. When enabled (the engine default), closing the tab or navigating away raises the browser's own "leave site?" dialog, so a participant does not discard an in-progress run with one stray click. The guard is armed only once the experiment itself begins -- the preliminary screens and the file loading are free to leave, and guarding them is noise that teaches participants to dismiss the dialog -- and it is released when the run ends, so it never fires on the final screens. The dialog's wording is fixed by the browser and cannot be set from configuration; this option only turns it on or off. Set FALSE to opt a run out. NULL uses the engine default (enabled). DEFAULT = NULL.
#' @param strictGroupAssignment Optional single Boolean controlling what a multi-group experiment does when it cannot obtain a group assignment from the server. Server-side assignment is what makes the chosen group durable across a reload and what lets the server withhold groups a participant has already completed. When strict, a run that cannot obtain one refuses to start and tells the participant that nothing has been recorded and they may try again; when not strict (the engine default), it falls back to drawing a group in the browser, which is how multi-group experiments behaved before assignment existed but leaves the choice recorded nowhere. Has no effect on a single-group experiment, which never asks the server. Strict is forced on regardless of this setting for repeat-session links, where the recorded group is part of the credit key. Set TRUE to opt in. NULL uses the engine default (not strict). DEFAULT = NULL.
#' @param creditClaimTimeoutMs Optional single number, at least 1000: the timeout in milliseconds on the credit claim, the one request that writes the credit record and returns the grant-or-deny verdict at the end of a gated run. NULL uses the engine default (10000), which is the right choice unless a deployment is known to be slow. ⚠ A value the browser cannot use does not relax the timeout, it REMOVES it -- the underlying field treats zero as "no limit" -- and an unbounded claim against a server that accepts the connection and never answers leaves the participant on a blank screen with the final save unrun. A very small value fails the other way: every claim times out, and the claim fails open, so credit is granted with no record written. Both are refused here. DEFAULT = NULL.
#''
#' @return the QCEBdbfileList
#' @keywords QCE QCEBdbfileList dbfile
#' @export
#' @examples buildQCEdbFile (expName = "myExp", addQualtricsCode = TRUE, defaultBackgroundColor = "#000000", restAfterEveryNTrials = c(50, 100), instructionFile = "instructions.html", keyMapInstructionFile = "kmInst.html", getUserNameFile = NULL, getConsentFile = "consent.html", getDemographicsFile = NULL, getGenderFile = NULL, welcomeMsg = NULL, restMsg = NULL, endOfExpMsg = NULL, saveMsg = NULL)

buildQCEexpDbFile <- function (expName = "defaultExpName", addQualtricsCode = FALSE, defaultBackgroundColor = "#000000", restAfterEveryNTrials = -1, instructionFile = NULL, getUserNameFile = NULL, getConsentFile = NULL, getDemographicsFile = NULL, getGenderFile = NULL, welcomeMsg = NULL, restMsg = NULL, endOfSessionMsg = NULL, endOfExpMsg = NULL, saveMsg = NULL, closeBrowserMsg = NULL, fullscreenMsg = NULL, fullscreenBtn = "Continue", completionRedirect = NULL, saveDataEveryNTrials = 50, completionGate = NULL, maxExperimentMinutes = NULL, saveTimeoutMs = NULL, saveCanary = NULL, saveUnavailableMsg = NULL, warnOnLeave = NULL, strictGroupAssignment = NULL, creditClaimTimeoutMs = NULL) {

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
      stop("endOfSessionMsg option must be a single string composed in html or NULL.  I won't check your html grammer, but I will check to see that the endOfSessionMsg option is a single string or NULL.  Yours, apparently, is neither a single string or NULL.")
    }
  }

  if(is.null(saveMsg)) {
    ## The saving screen takes no keypress: it ends itself once the write has
    ## landed. Default text must not invite a control the participant lacks.
    saveMsg <- "<p>Your data is being saved. Please do not close this window. This screen will continue on its own when the save is finished.</p>"
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
    closeBrowserMsg <- "You may now close this browser window."
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
      # Vocabulary and the shared aggregator/where checks live in QCEButils.r so
      # the gate and card fields cannot drift apart. The op/value rules below are
      # the gate's own: a card field displays a number, it does not compare one.
      validOps <- .qcebValidCompareOps
      for (i in seq_along(completionGate$formula)) {
        f <- completionGate$formula[[i]]
        if (!is.list(f) || is.null(f$fn) || is.null(f$column) || is.null(f$op) || is.null(f$value)) {
          stop(sprintf("completionGate formula %d must be a list with 'fn', 'column', 'op', and 'value'.", i))
        }
        validateQCEaggregateFn(f, sprintf("completionGate formula %d", i))
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
        validateQCEwhereFilter(f$where, sprintf("completionGate formula %d", i))
      }
      if (!is.null(completionGate$combinator) &&
          !(completionGate$combinator %in% c("all", "any"))) {
        stop("completionGate$combinator must be 'all' or 'any'.")
      }
    }

    # ⚠ ATTEMPTS APPLY TO BOTH FORMS. The engine validates them after the shape
    # check, whichever form the gate took, so a gateFn gate offers retries on
    # exactly the same terms as a formula gate. Validating inside the formula
    # branch would leave the escape hatch unchecked.
    validateQCEattemptsPolicy(completionGate)

    # Participant-facing messages. Neither is required -- the engine supplies a
    # default for each -- but an EMPTY one is a different thing from an absent
    # one: it reaches the participant as a blank screen at the moment they are
    # being told they were not credited.
    for (m in c("noCreditMsg", "supersededMsg")) {
      if (!is.null(completionGate[[m]]) &&
          (!isSingleString(completionGate[[m]]) || nchar(completionGate[[m]]) == 0)) {
        stop(sprintf(paste0("completionGate$%s must be a single non-empty string composed ",
                            "in html, or absent to use the engine's default."), m))
      }
    }

    # ⚠⚠ THE ONE MISTAKE NOTHING ELSE CATCHES. A misspelled key is not rejected
    # anywhere -- it is simply dropped, and the gate then runs under a policy
    # nobody wrote. `attemptsAllowd` alone leaves the engine's cross-check to
    # notice the orphaned retryPrompt, but misspell BOTH keys and the gate
    # silently becomes a one-attempt gate that never offers a retry, which is
    # exactly the failure the retry feature exists to prevent. Warn rather than
    # stop: a newer engine may define a key this version of QCEB has not heard of,
    # and refusing would make the package the reason a valid study cannot build.
    unknown <- setdiff(names(completionGate), .qcebGateKeys)
    if (length(unknown) > 0) {
      warning("completionGate has unrecognized entries, which the engine will ignore: ",
              paste(unknown, collapse = ", "),
              ". Check the spelling against ", paste(.qcebGateKeys, collapse = ", "),
              " -- a misspelled key is dropped silently, so the gate would run without it.")
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

  # Save-resilience knobs -- all optional, emitted only when supplied so legacy
  # dbfiles stay byte-identical and the engine's own defaults apply when absent.

  # Per-request save timeout (ms). A save that neither resolves nor rejects
  # within this window is treated as a failure so the serialized save chain is
  # not blocked behind one hung request.
  if (!is.null(saveTimeoutMs)) {
    if (!is.numeric(saveTimeoutMs) || length(saveTimeoutMs) != 1 || saveTimeoutMs <= 0) {
      stop("saveTimeoutMs must be a single positive number of milliseconds.")
    }
    tmpList$saveTimeoutMs <- saveTimeoutMs
  }

  # Start-of-run save canary. FALSE opts a run out of the pre-timeline
  # writability probe that otherwise halts a participant before any work when
  # the save path is down.
  if (!is.null(saveCanary)) {
    if (!is.logical(saveCanary) || length(saveCanary) != 1 || is.na(saveCanary)) {
      stop("saveCanary must be a single Boolean (TRUE or FALSE).")
    }
    tmpList$saveCanary <- saveCanary
  }

  # Message for the terminal halt screen shown when the canary fails.
  if (!is.null(saveUnavailableMsg)) {
    if (!isSingleString(saveUnavailableMsg)) {
      stop("saveUnavailableMsg must be a single string composed in html or NULL.")
    }
    tmpList$saveUnavailableMsg <- saveUnavailableMsg
  }

  # Run-integrity knobs -- also optional and also emitted only when supplied, so
  # a config that does not mention them produces the same JSON it always did and
  # takes the engine's own default. Both are read by the engine as flags, which
  # tolerates the string spellings of a Boolean because dbfiles are hand-authored
  # too; this builder does not. A string here is a mistake in R code, and passing
  # it through would emit a value whose meaning depends on a coercion table
  # rather than on what was written.

  # Leave-the-page guard. FALSE opts a run out of the browser confirmation the
  # engine otherwise arms for the duration of the timeline.
  if (!is.null(warnOnLeave)) {
    if (!is.logical(warnOnLeave) || length(warnOnLeave) != 1 || is.na(warnOnLeave)) {
      stop("warnOnLeave must be a single Boolean (TRUE or FALSE).")
    }
    tmpList$warnOnLeave <- warnOnLeave
  }

  # Group-assignment strictness. TRUE refuses to start a multi-group run whose
  # server assignment could not be obtained, rather than drawing a group in the
  # browser and leaving the choice unrecorded.
  if (!is.null(strictGroupAssignment)) {
    if (!is.logical(strictGroupAssignment) || length(strictGroupAssignment) != 1 ||
        is.na(strictGroupAssignment)) {
      stop("strictGroupAssignment must be a single Boolean (TRUE or FALSE).")
    }
    tmpList$strictGroupAssignment <- strictGroupAssignment
  }

  # Timeout on the credit claim -- the single request that writes the credit
  # record and returns the grant-or-deny verdict at the end of a gated run.
  #
  # ⚠⚠ THE FLOOR AND THE TYPE CHECK ARE BOTH LOAD-BEARING, in opposite directions.
  # The browser's timeout field is an unsigned integer in which ZERO MEANS NO
  # LIMIT, so a value it cannot use -- a string, or a plain 0 written meaning "do
  # not time out" -- removes the timeout instead of relaxing it, and a server that
  # accepts the connection and never answers then leaves the participant on a
  # blank screen with the final save unrun. A value of a few milliseconds fails
  # the opposite way: every claim fails, and the claim FAILS OPEN, so credit is
  # granted with no record written. Absent is the safe state; a stated value is
  # checked.
  if (!is.null(creditClaimTimeoutMs)) {
    if (!is.numeric(creditClaimTimeoutMs) || length(creditClaimTimeoutMs) != 1 ||
        !is.finite(creditClaimTimeoutMs) || creditClaimTimeoutMs < 1000) {
      stop("creditClaimTimeoutMs must be a single finite number of milliseconds of at least 1000.")
    }
    tmpList$creditClaimTimeoutMs <- creditClaimTimeoutMs
  }

  return(tmpList)

}
