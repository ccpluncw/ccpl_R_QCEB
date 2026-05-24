#' Build a block-fired-switch condition for QCEP showIf gating
#'
#' Phase 3.5 Decision G: builds a showIf leaf condition that reads from the
#' engine's `qceState.blockSwitched[blockRef]` marker (stamped at runtime
#' the first time any switchRule fires in the named block). Use this to
#' gate downstream blocks or sets on whether an earlier block's switch
#' fired -- e.g., "only run T3 if T2's switch fired", "skip the bonus
#' block if the practice block didn't reach criterion".
#'
#' Returned condition is interchangeable with buildQCEshowIfCondition's
#' output in any context that accepts a showIf condition -- you can mix
#' stimRef and blockRef leaves inside the same buildQCEshowIfCompound
#' all/any group, e.g.:
#' \preformatted{
#'   buildQCEshowIfCompound("all", list(
#'     buildQCEshowIfCondition("consent", "equals", "Yes", "Response"),
#'     buildQCEblockSwitchedCondition("Block_T2", "switchFired")
#'   ))
#' }
#'
#' Static validation: the engine checks at session start that (a) blockRef
#' references a block declared in the same trial structure, (b) the
#' referenced block is declared BEFORE the gated scope (otherwise the
#' marker can't be stamped by evaluation time), and (c) the referenced
#' block declares at least one switchRule (warning, not error -- the
#' condition would otherwise always evaluate false).
#'
#' @param blockRef Single non-empty string. The name of the block whose
#'   switch status the condition reads. Must match a `blockName` declared
#'   on an earlier addBlockToQCETrialStructureList call.
#' @param operator Single string. One of: switchFired, switchNotFired.
#''
#' @return A list of the form list(blockRef, operator), shape-compatible
#'   with QCEP's evaluateCondition blockRef leaf branch.
#' @keywords QCE showIf condition blockRef dynamic
#' @export
#' @examples
#' # Gate Block_T3 on whether Block_T2 fired its switch
#' c1 <- buildQCEblockSwitchedCondition("Block_T2", "switchFired")
#'
#' # Gate a bonus block on the practice block NOT firing (i.e., participant
#' # didn't reach criterion in practice)
#' c2 <- buildQCEblockSwitchedCondition("Practice", "switchNotFired")

buildQCEblockSwitchedCondition <- function(blockRef, operator) {

  validOps <- c("switchFired", "switchNotFired")

  if (missing(blockRef) || !isSingleString(blockRef) || nchar(blockRef) == 0) {
    stop("blockRef option must be a non-empty single string ",
         "(the name of the block whose switch status this condition reads).")
  }

  if (missing(operator) || !isSingleString(operator) || !(operator %in% validOps)) {
    stop(paste("operator option must take on one of the following values:",
               paste(validOps, collapse = " ")))
  }

  return(list(blockRef = blockRef, operator = operator))
}
