#' Attach block-to-block switch rules to a QCETrialStructureList
#'
#' Adds a top-level `switchRules` key to an existing QCETrialStructureList. These
#' are BLOCK-to-BLOCK switch rules (Phase 4 Step 2): each rule (from
#' \code{\link{buildQCEblockSwitchRule}}) watches one block and, on crossing its
#' threshold, jumps forward to another block (or ends the session sequence).
#'
#' This is the block-scope analogue of the `switchRules` argument on
#' \code{\link{addBlockToQCETrialStructureList}}, which attaches SET-to-SET rules
#' INSIDE a single block. Block rules live at the trial-structure (session)
#' level, as a sibling of the numbered block entries, so they get their own
#' attach function rather than a per-block argument. The engine reads them from
#' the tsfile's top-level `switchRules` key (`trialStructure.switchRules`) and
#' validates them in `validateSessionSwitchRules` at session start.
#'
#' \strong{Call this LAST, after every block has been added.}
#' \code{addBlockToQCETrialStructureList} derives each new block's key from the
#' current list length, so if `switchRules` is already attached, a subsequently
#' added block would be mis-numbered. The QCETrialStructureList must already
#' contain at least one block (this function errors otherwise).
#'
#' Validation here is SHAPE-only (each rule has a watchBlock, a valid count
#' condition, a valid threshold, and a well-formed switchToBlock if present).
#' Cross-references -- that watchBlock / switchToBlock name real blocks, that
#' every jump is forward-only, and that watched blocks have blockIterator.N == 1
#' -- are enforced by the engine at session start, where the full block list is
#' available. See \code{\link{buildQCEblockSwitchRule}} for the rationale.
#'
#' @param QCETrialStructureList A QCETrialStructureList that already contains all
#'   of its blocks (built up via repeated addBlockToQCETrialStructureList calls).
#'   Required -- must be non-NULL with at least one block.
#' @param switchRules A list of block-switch-rule lists, each produced by
#'   \code{\link{buildQCEblockSwitchRule}}. Must contain at least one rule.
#'   Multiple rules may name the same watchBlock (branching, Decision 8).
#'
#' @return the updated QCETrialStructureList, with a top-level `switchRules`
#'   element added.
#' @keywords QCE QCETrialStructureList switchRule blockSwitch dynamic
#' @seealso \code{\link{buildQCEblockSwitchRule}} to build the rules,
#'   \code{\link{addBlockToQCETrialStructureList}} for set-to-set switching.
#' @export
#' @examples
#' # Build blocks first ...
#' TS <- addBlockToQCETrialStructureList(NULL, setInfoA, blockItA, blockName = "T2_yesno")
#' TS <- addBlockToQCETrialStructureList(TS,  setInfoB, blockItB, blockName = "T3_spacebar")
#'
#' # ... then attach the block switch rule LAST.
#' rule <- buildQCEblockSwitchRule(
#'   watchBlock    = "T2_yesno",
#'   countResponse = "Yes",
#'   threshold     = buildQCEswitchThreshold(values = 5, rule = "fixed"),
#'   switchToBlock = "T3_spacebar"
#' )
#' TS <- addBlockSwitchRulesToQCETrialStructureList(TS, list(rule))

addBlockSwitchRulesToQCETrialStructureList <- function(QCETrialStructureList, switchRules) {

  if (missing(QCETrialStructureList) || is.null(QCETrialStructureList)) {
    stop("QCETrialStructureList option is required and must already contain at ",
         "least one block. Add all blocks via addBlockToQCETrialStructureList ",
         "first, then attach switch rules LAST.")
  }
  if (!is.list(QCETrialStructureList) || length(QCETrialStructureList) < 1) {
    stop("QCETrialStructureList must be a non-empty list of blocks. Add blocks ",
         "via addBlockToQCETrialStructureList before attaching switch rules.")
  }

  # Guard against attaching twice / attaching before blocks are present:
  # there must be at least one actual block entry (not just a switchRules key).
  blockKeys <- setdiff(names(QCETrialStructureList), "switchRules")
  if (length(blockKeys) < 1) {
    stop("QCETrialStructureList has no blocks to watch. Add at least one block ",
         "via addBlockToQCETrialStructureList before attaching switch rules.")
  }

  # Shape validation (cross-references deferred to the engine).
  validateBlockSwitchRulesShape(switchRules, "switchRules")

  QCETrialStructureList$switchRules <- switchRules

  return(QCETrialStructureList)
}
