#' Build a single showIf condition for QCEP dynamic experiments
#'
#' Creates a condition list in the shape consumed by the QCEP engine's
#' evaluateCondition (see customScripts/v9/dynamicEngine.js). The returned
#' list is passed to the `showIf` parameter of builder functions
#' (addScenarioToQCEscenarioList, addSetToQCEsetInfoList,
#' addBlockToQCETrialStructureList) or wrapped by buildQCEshowIfCompound
#' inside an `all`/`any` group.
#'
#' Operators fall into two groups:
#'   - Value-comparison (require both `value` and `field`):
#'       equals, notEquals, greaterThan, lessThan,
#'       greaterThanOrEqual, lessThanOrEqual, contains
#'   - Stimulus-presence (only `stimRef` and `operator`):
#'       wasShown, wasNotShown
#'
#' @param stimRef Single string. The identifier of the scenario whose data
#'   the condition reads. Must match a `stimRef` declared on some scenario
#'   via addScenarioToQCEscenarioList.
#' @param operator Single string. One of: equals, notEquals, greaterThan,
#'   lessThan, greaterThanOrEqual, lessThanOrEqual, contains, wasShown,
#'   wasNotShown.
#' @param value The value to compare against. Required for value-comparison
#'   operators; ignored for wasShown/wasNotShown. DEFAULT = NULL.
#' @param field Single string -- the name of the data field on the referenced
#'   stimRef's trial data to read for comparison (e.g., "Response", "RT").
#'   Required for value-comparison operators; ignored for
#'   wasShown/wasNotShown. DEFAULT = NULL.
#''
#' @return A list of the form list(stimRef, operator, value, field) for
#'   value-comparison operators, or list(stimRef, operator) for
#'   wasShown/wasNotShown.
#' @keywords QCE showIf condition dynamic
#' @export
#' @examples
#' # Value comparison
#' c1 <- buildQCEshowIfCondition("smoker", "equals", "Yes", "Response")
#'
#' # Stimulus presence
#' c2 <- buildQCEshowIfCondition("instructions", "wasShown")

buildQCEshowIfCondition <- function(stimRef, operator, value = NULL, field = NULL) {

  valueOps <- c("equals", "notEquals", "greaterThan", "lessThan",
                "greaterThanOrEqual", "lessThanOrEqual", "contains")
  shownOps <- c("wasShown", "wasNotShown")
  validOps <- c(valueOps, shownOps)

  if (!isSingleString(stimRef)) {
    stop("stimRef option must be a single string. Yours, apparently, is not a single string.")
  }

  if (!isSingleString(operator)) {
    stop("operator option must be a single string. Yours, apparently, is not a single string.")
  }

  if (!(operator %in% validOps)) {
    stop(paste("operator option must take on one of the following values:",
               paste(validOps, collapse = " ")))
  }

  if (operator %in% valueOps) {
    if (is.null(value) || is.null(field)) {
      stop(paste0("Operator '", operator, "' compares a stimulus value, so 'value' and 'field' are required. ",
                  "If you meant 'has the participant seen this stimulus?', use operator='wasShown' or 'wasNotShown' instead."))
    }
    if (!isSingleString(field)) {
      stop("field option must be a single string. Yours, apparently, is not a single string.")
    }
    return(list(stimRef = stimRef, operator = operator, value = value, field = field))
  }

  # shownOps: surface the conceptual mistake if value/field were passed,
  # but don't break the call -- drop them and warn.
  if (!is.null(value) || !is.null(field)) {
    warning(paste0("Operator '", operator, "' ignores 'value' and 'field' (it only checks whether the stimRef has been shown). ",
                   "You passed one or both; they are being dropped. ",
                   "If you meant a value comparison, use an operator like 'equals' instead."))
  }
  return(list(stimRef = stimRef, operator = operator))
}
