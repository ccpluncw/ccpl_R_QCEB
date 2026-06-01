#' Build a hook-state showIf condition (stateRef leaf) for QCEP
#'
#' Phase 5 (Custom Hooks): builds a showIf leaf condition that reads from the
#' engine's `qceState.custom[stateRef]` scratchpad -- the channel by which a
#' custom hook influences declarative flow. A hook (onTrialFinish, onBlockEnd,
#' etc.) writes a value into `ctx.qceState.custom[key]`; a downstream scenario,
#' set, or block gates itself on that value via this condition. This is how
#' "skip / branch based on something a hook computed" is expressed -- the
#' decision stays declarative and is captured in the engine's showIf log,
#' rather than a hook imperatively vanishing a trial.
#'
#' Operators fall into two groups:
#'   - Value-comparison (require `value`):
#'       equals, notEquals, greaterThan, lessThan,
#'       greaterThanOrEqual, lessThanOrEqual, contains
#'   - Presence (only `stateRef` and `operator`, no `value`):
#'       isSet, isNotSet
#'
#' Unlike buildQCEshowIfCondition there is no `field`: a stateRef reads the
#' value the hook stored at `qceState.custom[stateRef]` directly. A key the
#' hook has not set yet is "not set" -- `isSet` is false, `isNotSet` is true,
#' and value operators evaluate false (no error). A key set to 0 or FALSE
#' counts as set.
#'
#' Returned condition is interchangeable with buildQCEshowIfCondition's and
#' buildQCEblockSwitchedCondition's output -- you can mix stimRef, blockRef,
#' and stateRef leaves inside the same buildQCEshowIfCompound all/any group.
#'
#' Static validation: if the dbfile declares `customHooksStateKeys` (see
#' addHooksToQCEgroupDbFile), the engine errors at session start on a stateRef
#' not in that list (typo guard); if no keys are declared, it warns instead.
#'
#' @param stateRef Single non-empty string. The key the hook writes to
#'   `qceState.custom`. Should match a name in the dbfile's
#'   `customHooksStateKeys` when that list is declared.
#' @param operator Single string. One of: equals, notEquals, greaterThan,
#'   lessThan, greaterThanOrEqual, lessThanOrEqual, contains, isSet, isNotSet.
#' @param value The value to compare against. Required for value-comparison
#'   operators; ignored for isSet/isNotSet. DEFAULT = NULL.
#''
#' @return A list of the form list(stateRef, operator, value) for
#'   value-comparison operators, or list(stateRef, operator) for
#'   isSet/isNotSet.
#' @keywords QCE showIf condition stateRef hooks dynamic
#' @export
#' @examples
#' # Gate a block on a flag a hook set after the practice block
#' c1 <- buildQCEstateCondition("reachedCriterion", "equals", TRUE)
#'
#' # Show a bonus scenario only once the hook has recorded a score
#' c2 <- buildQCEstateCondition("blockScore", "isSet")
#'
#' # Mix with a stimRef leaf in a compound
#' compound <- buildQCEshowIfCompound("all", list(
#'   buildQCEshowIfCondition("consent", "equals", "Yes", "Response"),
#'   buildQCEstateCondition("blockScore", "greaterThan", 0.8)
#' ))

buildQCEstateCondition <- function(stateRef, operator, value = NULL) {

  valueOps <- c("equals", "notEquals", "greaterThan", "lessThan",
                "greaterThanOrEqual", "lessThanOrEqual", "contains")
  presenceOps <- c("isSet", "isNotSet")
  validOps <- c(valueOps, presenceOps)

  if (missing(stateRef) || !isSingleString(stateRef) || nchar(stateRef) == 0) {
    stop("stateRef option must be a non-empty single string ",
         "(the qceState.custom key a hook writes and this condition reads).")
  }

  if (missing(operator) || !isSingleString(operator) || !(operator %in% validOps)) {
    stop(paste("operator option must take on one of the following values:",
               paste(validOps, collapse = " ")))
  }

  if (operator %in% valueOps) {
    if (is.null(value)) {
      stop(paste0("Operator '", operator, "' compares the stored hook value, so 'value' is required. ",
                  "If you meant 'has the hook set this key?', use operator='isSet' or 'isNotSet' instead."))
    }
    return(list(stateRef = stateRef, operator = operator, value = value))
  }

  # presenceOps: surface the conceptual mistake if value was passed, but don't
  # break the call -- drop it and warn.
  if (!is.null(value)) {
    warning(paste0("Operator '", operator, "' ignores 'value' (it only checks whether the hook has set this key). ",
                   "You passed a value; it is being dropped. ",
                   "If you meant a value comparison, use an operator like 'equals' instead."))
  }
  return(list(stateRef = stateRef, operator = operator))
}
