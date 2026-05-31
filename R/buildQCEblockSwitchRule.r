#' Build a single block-to-block switch rule for QCEP dynamic experiments
#'
#' Creates a block-switch-rule list in the shape consumed by the QCEP engine's
#' executeBlockSwitch (see customScripts/v9.1/dynamicEngine.js, Phase 4 Step 2).
#' The returned list is passed alongside other rules to the `switchRules`
#' parameter of \code{\link{addBlockSwitchRulesToQCETrialStructureList}}, which
#' attaches them at the TOP LEVEL of the trial structure (not on a block).
#'
#' Block switch rules govern BLOCK-to-BLOCK flow: when a configurable count
#' condition has been met `threshold` times anywhere within the watched block
#' (whole-block scope, across whatever sets play), the rule fires -- it aborts
#' the rest of that block and jumps forward to a destination block (or ends the
#' session's block sequence). This is distinct from \code{\link{buildQCEswitchRule}},
#' which governs SET-to-SET flow within a single block (destination = a set).
#' The naming convention is by DESTINATION: \emph{set} switch rules switch to a
#' set (`switchToSet`); \emph{block} switch rules switch to a block (`switchToBlock`).
#'
#' Count condition: pick exactly one of two forms (same vocabulary as the
#' set-level builder):
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
#' Terminal behavior (Decision 9):
#'   - `switchToBlock` present: rule fires, the watched block ends early, and
#'     the engine jumps FORWARD to the named block (skipping any blocks in
#'     between). The destination block's own `entryInstruction` (if any) shows
#'     on arrival, exactly as on a natural arrival.
#'   - `switchToBlock` absent (NULL): rule fires, the watched block ends early,
#'     and the engine ENDS the session's remaining block sequence (advancing to
#'     the next session if the experiment has more). Mirror of the set-level
#'     `switchToSet = NULL` early-stop, but at block scope.
#'
#' Direction is FORWARD-ONLY (Decision 6): `switchToBlock` may target only a
#' block declared LATER than `watchBlock`. Backward jumps / revisits are
#' rejected. This is enforced by the engine at session start (not here -- see
#' "Validation split" below).
#'
#' Branching (Decision 8): multiple rules MAY name the same `watchBlock`, each
#' with its own counter. All rules watching the current block are live
#' simultaneously; the first to cross threshold fires, with declaration order
#' breaking a same-trial tie. (The set-level builder forbids this; block rules
#' allow it because they are named by `watchBlock` rather than positional.)
#'
#' Validation split: this builder validates only the SHAPE of the rule
#' (watchBlock present, count XOR, threshold shape, switchToBlock shape). It
#' deliberately does NOT check cross-references -- that `watchBlock` /
#' `switchToBlock` name real blocks, that the jump is forward-only, or that the
#' watched block has `blockIterator.N == 1`. The QCEP engine enforces all of
#' those in `validateSessionSwitchRules` at session start, where it has the full
#' trial structure in hand. This matches the set-level division of labor
#' (`buildQCEswitchRule` likewise defers `switchToSet` existence to the engine).
#'
#' @param threshold A threshold spec, typically from buildQCEswitchThreshold.
#'   Required.
#' @param watchBlock Single non-empty string -- the name of the block this rule
#'   counts responses within (must match a `blockName` declared via
#'   addBlockToQCETrialStructureList; the engine validates existence). Required.
#' @param countResponse Single non-empty string. Sugar for matching the
#'   trial's Key field. Mutually exclusive with countWhen. DEFAULT = NULL.
#' @param countWhen A list `list(field, operator, value)` describing a more
#'   general count condition. Mutually exclusive with countResponse.
#'   DEFAULT = NULL.
#' @param switchToBlock Optional single non-empty string -- the destination
#'   block name. Must reference a `blockName` declared LATER than `watchBlock`
#'   (forward-only; the engine validates this at session start). NULL means
#'   "end the session's block sequence early" (Decision 9). DEFAULT = NULL.
#'
#' @return A list of the form list(watchBlock, threshold, countResponse|countWhen,
#'   switchToBlock) with only the supplied fields included (NULL defaults omitted).
#' @keywords QCE switchRule blockSwitch dynamic
#' @seealso \code{\link{addBlockSwitchRulesToQCETrialStructureList}} to attach
#'   the rules, \code{\link{buildQCEswitchThreshold}} for the threshold,
#'   \code{\link{buildQCEswitchRule}} for set-to-set (intra-block) switching.
#' @export
#' @examples
#' # 2AFC sugar: after 5 "Yes" responses in block T2_yesno, jump to T3_spacebar
#' r1 <- buildQCEblockSwitchRule(
#'   watchBlock    = "T2_yesno",
#'   countResponse = "Yes",
#'   threshold     = buildQCEswitchThreshold(values = 5, rule = "fixed"),
#'   switchToBlock = "T3_spacebar"
#' )
#'
#' # Full form: count fast RTs in a practice block; end the session early
#' # (no destination) once criterion is reached
#' r2 <- buildQCEblockSwitchRule(
#'   watchBlock = "Practice",
#'   countWhen  = list(field = "RT", operator = "lessThan", value = 500),
#'   threshold  = buildQCEswitchThreshold(values = 10, rule = "fixed")
#' )
#'
#' # Branching: two rules watch the same block, jumping to different
#' # destinations depending on which response the participant favors
#' rA <- buildQCEblockSwitchRule("T1", countResponse = "Left",  switchToBlock = "LeftPath",
#'   threshold = buildQCEswitchThreshold(values = 5, rule = "fixed"))
#' rB <- buildQCEblockSwitchRule("T1", countResponse = "Right", switchToBlock = "RightPath",
#'   threshold = buildQCEswitchThreshold(values = 5, rule = "fixed"))

buildQCEblockSwitchRule <- function(threshold, watchBlock,
                                     countResponse = NULL, countWhen = NULL,
                                     switchToBlock = NULL) {

  # watchBlock required non-empty string
  if (missing(watchBlock) || is.null(watchBlock)) {
    stop("watchBlock option is required -- the name of the block this rule ",
         "counts responses within (must match a blockName declared via ",
         "addBlockToQCETrialStructureList).")
  }
  if (!isSingleString(watchBlock) || nchar(watchBlock) == 0) {
    stop("watchBlock option must be a non-empty single string.")
  }

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

  # switchToBlock optional non-empty string (NULL = end session sequence)
  if (!is.null(switchToBlock)) {
    if (!isSingleString(switchToBlock) || nchar(switchToBlock) == 0) {
      stop("switchToBlock option must be a non-empty single string when present ",
           "(omit for end-session-early without a destination). Forward-only: ",
           "it must name a block declared later than watchBlock (engine-enforced).")
    }
  }

  # Build the rule list -- only include supplied fields so the JSON shape
  # exactly matches what the engine expects (NULL defaults omitted).
  rule <- list(watchBlock = watchBlock, threshold = threshold)
  if (hasResp) rule$countResponse <- countResponse
  if (hasWhen) rule$countWhen     <- countWhen
  if (!is.null(switchToBlock)) rule$switchToBlock <- switchToBlock

  return(rule)
}
