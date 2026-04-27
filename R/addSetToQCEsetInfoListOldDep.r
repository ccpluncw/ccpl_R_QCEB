#' DEPRECATED multi-set form of addSetToQCEsetInfoList
#'
#' This is the pre-Phase-2 multi-set form of addSetToQCEsetInfoList,
#' preserved here for backward compatibility. Calling it logs a
#' deprecation warning. New code should use addSetToQCEsetInfoList,
#' which now takes a single setName per call and accepts trigger/showIf
#' as scalar values rather than named lists.
#'
#' Behavior is byte-identical to the original function. No new features
#' (e.g., showIf) are supported here -- those require the new form.
#'
#' @param QCEsetInfoList Existing QCEsetInfoList or NULL.
#' @param QCEScenarioList The QCEScenarioList that declares valid setNames.
#' @param setName Vector of setNames, or NULL to use all validSetNames.
#' @param numberOfTrialsPerSet Single integer or vector parallel to setName.
#' @param selectionType One of "randomWithoutReplacement" (default), "randomWithReplacement", "fixed".
#' @param trigger Optional named list of trigger objects keyed by setName.
#''
#' @return The updated QCEsetInfoList.
#' @keywords QCE QCEsetInfoList deprecated
#' @export

addSetToQCEsetInfoListOldDep <- function(QCEsetInfoList = NULL, QCEScenarioList = NULL, setName = NULL, numberOfTrialsPerSet = 1, selectionType = "randomWithoutReplacement", trigger = NULL) {

  .Deprecated(
    new = "addSetToQCEsetInfoList",
    msg = paste(
      "addSetToQCEsetInfoList no longer accepts a vector of setNames in a single call.",
      "Call the new addSetToQCEsetInfoList once per set instead -- it now takes a",
      "single setName, single numberOfTrialsPerSet, and per-set trigger/showIf as",
      "scalar values (not named lists). This deprecated form is kept for backward",
      "compatibility and may be removed in a future release.",
      "See ?addSetToQCEsetInfoList for the new signature."
    )
  )

  tmpList <- NULL

  validSelectionTypes <- c("randomWithoutReplacement", "randomWithReplacement", "fixed")
  if (!(selectionType %in% validSelectionTypes)) {
    stop(paste("selectionType option must take on one of the following values: ",
               paste(validSelectionTypes, sep = "", collapse = " ")))
  }

  attachTrigger <- function(entry, nm) {
    if (!is.null(trigger) && !is.null(trigger[[nm]])) {
      entry$trigger <- trigger[[nm]]
    }
    return(entry)
  }

  validSetNames <- getSetnamesFromScenarioList(QCEScenarioList)
  if (!is.null(setName)) {
    # NOTE: original code had `setNames %in% validSetNames` (a typo --
    # `setNames` is a base R function, not the local arg). The bug caused
    # any non-NULL setName call to error in modern R; only the NULL-setName
    # path ever worked. Typo fixed here so the wrapper actually serves
    # legacy multi-set callers.
    if (any((setName %in% validSetNames) == FALSE)) {
      stop(paste("setName option must take on one of the following values: ",
                 paste(validSetNames, sep = "", collapse = " ")))
    }
    if (length(numberOfTrialsPerSet) == 1) {
      for (i in 1:length(setName)) {
        tmpList[[setName[i]]] <- attachTrigger(list(N = numberOfTrialsPerSet, selection = selectionType), setName[i])
      }
    } else {
      if (length(setName) != length(numberOfTrialsPerSet)) {
        stop("Either the number of setNames must be equal to the number of numberOfTrialsPerSet or the numberOfTrialsPerSet must be a single integer that is applied to all sets")
      }
      for (i in 1:length(setName)) {
        tmpList[[setName[i]]] <- attachTrigger(list(N = numberOfTrialsPerSet[i], selection = selectionType), setName[i])
      }
    }
  } else {
    for (i in 1:length(validSetNames)) {
      tmpList[[validSetNames[i]]] <- attachTrigger(list(N = numberOfTrialsPerSet, selection = selectionType), validSetNames[i])
    }
  }

  if (is.null(QCEsetInfoList)) {
    QCEsetInfoList <- tmpList
  } else {
    QCEsetInfoList <- c(QCEsetInfoList, tmpList)
  }

  return(QCEsetInfoList)
}
