#' Build a single switch rule for QCEP dynamic experiments
#'
#' Creates a switch-rule list in the shape consumed by the QCEP engine's
#' executeSwitch (see customScripts/v9/dynamicEngine.js). The returned list
#' is passed alongside other rules to the `switchRules` parameter of
#' addBlockToQCETrialStructureList.
#'
#' Switch rules govern intra-block flow: when a configurable count condition
#' has been met `threshold` times within the currently-watching set, the
#' rule fires -- it ends the current set early and (optionally) jumps to a
#' destination set. See `customScripts/v9/dynamicEngine.js` for runtime
#' semantics; the locked design decisions are summarized in
#' `DYNAMIC_EXPERIMENTS_PLAN.md` "JSON Schema Additions".
#'
#' Count condition: pick exactly one of two forms (Decision 1 / hybrid):
#'   - `countResponse` (sugar): a single string. Sugar for
#'     `countWhen = list(field="Key", operator="equals", value=<x>)`.
#'     Use this for the common 2AFC pattern ("count when participant
#'     pressed Yes").
#'   - `countWhen`: a list `list(field, operator, value)` with operator in
#'     the showIf value-comparison vocabulary (equals, notEquals,
#'     greaterThan, lessThan, greaterThanOrEqual, lessThanOrEqual,
#'     contains). Use this for non-Key-based or non-equality counting
#'     (e.g., RT < 500ms, NumberLine value > 5).
#'
#' Decision 3 — terminal behavior:
#'   - `switchToSet` present: rule fires, current set ends, destination set
#'     is built + pushed. Block continues with the new set.
#'   - `switchToSet` absent (NULL): rule fires, current set ends early; no
#'     redirect. Block falls through to the next set in natural order
#'     (early-stop). Useful for "run training until criterion, then proceed
#'     normally."
#'
#' Decision 6 (Semantic C) — `excludePreviouslyPresented` is NOT a switch-rule
#' parameter; it is a property of the destination set's pool, declared on
#' the set via `addSetToQCEsetInfoList(..., excludePreviouslyPresented = TRUE)`.
#'
#' Rule sequencing (Decision 3 amendment, Bug Fix #20): Rules are sequential.
#' The order rules appear in `switchRules` is the order in which they watch
#' the natural set sequence: rule[1] watches the first set, rule[2] watches
#' the second, etc. A rule expires when its watched set ends -- either
#' because the rule fired (and switched), or because the set ended naturally
#' without the count threshold being reached. Once expired, a rule cannot
#' fire again, and counting in subsequent sets is governed by the next rule
#' (if any). If there are fewer rules than sets, sets past the last rule
#' run with no switching. Counting is per-rule -- each rule's counter
#' starts at 0 when it becomes active and is reset on rule expiration.
#'
#' @param threshold A threshold spec, typically from buildQCEswitchThreshold.
#'   Required.
#' @param countResponse Single non-empty string. Sugar for matching the
#'   trial's Key field. Mutually exclusive with countWhen. DEFAULT = NULL.
#' @param countWhen A list `list(field, operator, value)` describing a more
#'   general count condition. Mutually exclusive with countResponse.
#'   DEFAULT = NULL.
#' @param switchToSet Optional single non-empty string -- the destination
#'   set name. Must reference a setName declared in the same block's
#'   setInfo (the engine validates this at session start). NULL means
#'   "early-stop without redirect" (Decision 3). DEFAULT = NULL.
#' @param switchInstruction Optional single non-empty string -- HTML
#'   filename of an instruction page to display between source and
#'   destination sets when this rule fires. NULL means no instruction.
#'   DEFAULT = NULL.
#''
#' @return A list of the form list(threshold, countResponse, switchToSet, ...)
#'   with only the supplied fields included (NULL defaults are omitted).
#' @keywords QCE switchRule dynamic
#' @export
#' @examples
#' # 2AFC sugar: switch to SetB after 5 "Yes" responses
#' r1 <- buildQCEswitchRule(
#'   countResponse = "Yes",
#'   threshold     = buildQCEswitchThreshold(values = 5, rule = "fixed"),
#'   switchToSet   = "SetB"
#' )
#'
#' # Full form: count fast RTs; early-stop without redirect
#' r2 <- buildQCEswitchRule(
#'   countWhen = list(field = "RT", operator = "lessThan", value = 500),
#'   threshold = buildQCEswitchThreshold(values = 10, rule = "fixed")
#' )
#'
#' # With instruction page between sets
#' r3 <- buildQCEswitchRule(
#'   countResponse     = "Prefer Left",
#'   threshold         = buildQCEswitchThreshold(values = 3, rule = "fixed"),
#'   switchToSet       = "SetB",
#'   switchInstruction = "switch_instruct.html"
#' )

buildQCEswitchRule <- function(threshold,
                                countResponse = NULL, countWhen = NULL,
                                switchToSet = NULL, switchInstruction = NULL) {

  # XOR validation: exactly one of countResponse or countWhen
  hasResp <- !is.null(countResponse)
  hasWhen <- !is.null(countWhen)

  if (!hasResp && !hasWhen) {
    stop("Must provide either countResponse (string sugar) or countWhen ",
         "(full condition list). countResponse is sugar for ",
         "countWhen=list(field='Key', operator='equals', value=<x>).")
  }
  if (hasResp && hasWhen) {
    stop("Pass either countResponse or countWhen, not both. countResponse ",
         "is sugar for countWhen=list(field='Key', operator='equals', value=<x>).")
  }

  # countResponse shape
  if (hasResp) {
    if (!isSingleString(countResponse) || nchar(countResponse) == 0) {
      stop("countResponse option must be a non-empty single string.")
    }
  }

  # countWhen shape (delegates to internal validator in QCEButils.r)
  if (hasWhen) {
    validateSwitchCountWhenShape(countWhen, "countWhen")
  }

  # threshold required + shape-checked
  if (missing(threshold) || is.null(threshold)) {
    stop("threshold option is required. Use buildQCEswitchThreshold(values, rule) ",
         "to construct one, or pass a list(values, rule) directly.")
  }
  validateSwitchThresholdShape(threshold, "threshold")

  # switchToSet optional non-empty string
  if (!is.null(switchToSet)) {
    if (!isSingleString(switchToSet) || nchar(switchToSet) == 0) {
      stop("switchToSet option must be a non-empty single string when present ",
           "(omit for early-stop without redirect).")
    }
  }

  # switchInstruction optional non-empty string
  if (!is.null(switchInstruction)) {
    if (!isSingleString(switchInstruction) || nchar(switchInstruction) == 0) {
      stop("switchInstruction option must be a non-empty single string when present.")
    }
  }

  # Build the rule list -- only include supplied fields so the JSON shape
  # exactly matches what the engine expects (NULL defaults omitted).
  rule <- list(threshold = threshold)
  if (hasResp) rule$countResponse <- countResponse
  if (hasWhen) rule$countWhen     <- countWhen
  if (!is.null(switchToSet))       rule$switchToSet       <- switchToSet
  if (!is.null(switchInstruction)) rule$switchInstruction <- switchInstruction

  return(rule)
}
