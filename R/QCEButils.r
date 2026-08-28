#' This function tests whether a variable contains a single string
#'
#' Function tests whether a variable contains a single string.
#' @param input A variable to be tested.
#'
#' @return a boolean (TRUE or FALSE) identifying whether the input contains a single string (TRUE) or not (FALSE)
#' @keywords is string single
#' @export
#' @examples isSingleString ("hello")

isSingleString <- function(input) {
    is.character(input) & length(input) == 1
}


#' This function tests whether a variable contains a single numeric
#'
#' Function tests whether a variable contains a single numeric.
#' @param input A variable to be tested.
#'
#' @return a boolean (TRUE or FALSE) identifying whether the input contains a single numeric (TRUE) or not (FALSE)
#' @keywords is numeric single
#' @export
#' @examples isSingleNumeric (100)

isSingleNumeric <- function(input) {
    is.numeric(input) & length(input) == 1
}

#' This function tests whether a variable contains a valid color
#'
#' Function tests whether a variable contains a valid color.
#' @param input A variable to be tested.
#'
#' @return a boolean (TRUE or FALSE) identifying whether the input contains a valid color (TRUE) or not (FALSE)
#' @keywords is color valid
#' @export
#' @examples isColor ('#000000')

isColor <- function(input)
{
  res <- try(col2rgb(input),silent=TRUE)
  return(!"try-error"%in%class(res))
}

#' This function tests whether a variable is a valid filename
#'
#' Function tests whether a variable is a valid filename.
#' @param filename A variable to be tested.
#' @param extension A string that represents the file extension you are testing against (e.g. "html").
#'
#' @return a boolean (TRUE or FALSE) identifying whether the input is a valid filename (TRUE) or not (FALSE)
#' @keywords is filename valid
#' @export
#' @examples isValidFilename ('myfile.html', "html")

isValidFilename <- function (filename, extension) {

  out <- FALSE

  filename <- tolower(filename)
  extension <- tolower(extension)

  if(stringi::stri_sub(extension,1,1) != ".") {
    extension <- paste(".", extension, sep="")
  }

  strN <- stringi::stri_length(extension)

  if(isSingleString(filename)) {
    if(stringr::str_sub(filename,-1*strN) == extension) {
      out <- TRUE
    }
  }

  return (out)

}


# Internal helper (not exported): shape-validate a showIf condition.
# Used by addScenarioToQCEscenarioList, addSetToQCEsetInfoList, and
# addBlockToQCETrialStructureList to reject malformed hand-rolled lists.
# Same shape rule buildQCEshowIfCompound applies to its children.
#
# Phase 3.5 Decision G (2026-05-24): blockRef+operator leaf added as a
# second valid single-condition shape, alongside the original stimRef+
# operator leaf. A leaf is one OR the other, never both. Compound (all/any)
# groups can contain a mix.
#
# Phase 5 (2026-06-01): stateRef+operator leaf added as a third valid single-
# condition shape (the hook->state->showIf channel; built by
# buildQCEstateCondition). Still XOR per leaf; compound groups can mix all three.
validateShowIfShape <- function(x, paramName = "showIf") {
  if (!is.list(x)) {
    stop(paramName, " must be a list (output of buildQCEshowIfCondition, ",
         "buildQCEblockSwitchedCondition, buildQCEstateCondition, or ",
         "buildQCEshowIfCompound).")
  }
  isStimLeaf  <- !is.null(x$stimRef)  && !is.null(x$operator)
  isBlockLeaf <- !is.null(x$blockRef) && !is.null(x$operator)
  isStateLeaf <- !is.null(x$stateRef) && !is.null(x$operator)
  isCompound  <- !is.null(x$all) || !is.null(x$any)
  if (!isStimLeaf && !isBlockLeaf && !isStateLeaf && !isCompound) {
    stop(paramName, " is not a valid showIf condition: ",
         "expected stimRef+operator (single), blockRef+operator (single, ",
         "Phase 3.5), stateRef+operator (single, Phase 5), or all/any ",
         "(compound). Did you forget to wrap with buildQCEshowIfCondition, ",
         "buildQCEblockSwitchedCondition, buildQCEstateCondition, or ",
         "buildQCEshowIfCompound?")
  }
  invisible(TRUE)
}


# Internal helper (not exported): shape-validate a switch-rule countWhen
# condition. Mirrors the engine's countWhen validation in
# dynamicEngine.js::validateSwitchRules. wasShown / wasNotShown are excluded
# (countWhen evaluates against a single just-completed trial's data row,
# whereas wasShown/wasNotShown is a state check across the dataIndex).
validateSwitchCountWhenShape <- function(x, paramName = "countWhen") {
  validOps <- c("equals", "notEquals", "greaterThan", "lessThan",
                "greaterThanOrEqual", "lessThanOrEqual", "contains")
  if (!is.list(x)) {
    stop(paramName, " must be a list of {field, operator, value}.")
  }
  if (is.null(x$field) || !isSingleString(x$field) || nchar(x$field) == 0) {
    stop(paramName, ".field must be a non-empty single string.")
  }
  if (is.null(x$operator) || !isSingleString(x$operator) || !(x$operator %in% validOps)) {
    stop(paramName, ".operator must be one of: ", paste(validOps, collapse = ", "), ".")
  }
  if (is.null(x$value)) {
    stop(paramName, ".value is required.")
  }
  invisible(TRUE)
}


# Internal helper (not exported): shape-validate a switch-rule threshold spec.
# Mirrors resolveThreshold + validateSwitchRules in dynamicEngine.js.
validateSwitchThresholdShape <- function(x, paramName = "threshold") {
  validRules <- c("fixed", "randomFromList", "randomIntBetween")
  if (!is.list(x)) {
    stop(paramName, " must be a list of {values, rule}.")
  }
  if (is.null(x$values)) {
    stop(paramName, ".values is required.")
  }
  if (!is.numeric(x$values) || length(x$values) < 1) {
    stop(paramName, ".values must be a numeric vector with at least one element.")
  }
  if (is.null(x$rule) || !isSingleString(x$rule) || !(x$rule %in% validRules)) {
    stop(paramName, ".rule must be one of: ", paste(validRules, collapse = ", "), ".")
  }
  if (x$rule == "randomIntBetween" && length(x$values) != 2) {
    stop(paramName, ".rule='randomIntBetween' requires exactly 2 values [lo, hi]; got ",
         length(x$values), ".")
  }
  invisible(TRUE)
}


# Internal helper (not exported): shape-validate a list of switch rules
# attached to a block. Reused by addBlockToQCETrialStructureList. Each rule
# must look like a buildQCEswitchRule output: XOR on countResponse vs
# countWhen, threshold present and well-shaped.
validateSwitchRulesShape <- function(rules, paramName = "switchRules") {
  if (!is.list(rules)) {
    stop(paramName, " must be a list of switch-rule lists (each from buildQCEswitchRule).")
  }
  if (length(rules) < 1) {
    stop(paramName, " must contain at least one rule.")
  }
  for (i in seq_along(rules)) {
    rule  <- rules[[i]]
    label <- paste0(paramName, "[[", i, "]]")
    if (!is.list(rule)) {
      stop(label, " must be a list (output of buildQCEswitchRule).")
    }
    hasResp <- !is.null(rule$countResponse)
    hasWhen <- !is.null(rule$countWhen)
    if (!hasResp && !hasWhen) {
      stop(label, " missing both countResponse and countWhen. ",
           "Did you forget to wrap with buildQCEswitchRule?")
    }
    if (hasResp && hasWhen) {
      stop(label, " has both countResponse and countWhen -- pick one ",
           "(countResponse is sugar for countWhen=list(field='Key', operator='equals', value=<x>)).")
    }
    if (hasResp) {
      if (!isSingleString(rule$countResponse) || nchar(rule$countResponse) == 0) {
        stop(label, ".countResponse must be a non-empty single string.")
      }
    }
    if (hasWhen) {
      validateSwitchCountWhenShape(rule$countWhen, paste0(label, ".countWhen"))
    }
    if (is.null(rule$threshold)) {
      stop(label, " missing threshold. Did you forget to wrap with buildQCEswitchRule?")
    }
    validateSwitchThresholdShape(rule$threshold, paste0(label, ".threshold"))
    if (!is.null(rule$switchToSet)) {
      if (!isSingleString(rule$switchToSet) || nchar(rule$switchToSet) == 0) {
        stop(label, ".switchToSet must be a non-empty single string when present ",
             "(omit for early-stop without redirect).")
      }
    }
    if (!is.null(rule$switchInstruction)) {
      if (!isSingleString(rule$switchInstruction) || nchar(rule$switchInstruction) == 0) {
        stop(label, ".switchInstruction must be a non-empty single string when present.")
      }
    }
  }
  invisible(TRUE)
}


# Internal helper (not exported): shape-validate a list of BLOCK-to-BLOCK
# switch rules (Phase 4 Step 2). Reused by
# addBlockSwitchRulesToQCETrialStructureList. Mirrors validateSwitchRulesShape
# but for the block-scope schema: each rule must name a watchBlock and may
# carry a switchToBlock (instead of the set-level switchToSet /
# switchInstruction). Cross-references (block existence, forward-only,
# blockIterator.N == 1) are NOT checked here -- the engine's
# validateSessionSwitchRules enforces them at session start, where the full
# trial structure is available.
validateBlockSwitchRulesShape <- function(rules, paramName = "switchRules") {
  if (!is.list(rules)) {
    stop(paramName, " must be a list of block-switch-rule lists (each from buildQCEblockSwitchRule).")
  }
  if (length(rules) < 1) {
    stop(paramName, " must contain at least one rule.")
  }
  for (i in seq_along(rules)) {
    rule  <- rules[[i]]
    label <- paste0(paramName, "[[", i, "]]")
    if (!is.list(rule)) {
      stop(label, " must be a list (output of buildQCEblockSwitchRule).")
    }
    # watchBlock required -- this is the field that distinguishes a block rule
    # from a set rule (set rules have no watchBlock; association is positional).
    if (is.null(rule$watchBlock)) {
      stop(label, " missing watchBlock. Block-to-block rules must name the ",
           "block they count within. Did you forget to wrap with ",
           "buildQCEblockSwitchRule?")
    }
    if (!isSingleString(rule$watchBlock) || nchar(rule$watchBlock) == 0) {
      stop(label, ".watchBlock must be a non-empty single string.")
    }
    hasResp <- !is.null(rule$countResponse)
    hasWhen <- !is.null(rule$countWhen)
    if (!hasResp && !hasWhen) {
      stop(label, " missing both countResponse and countWhen. ",
           "Did you forget to wrap with buildQCEblockSwitchRule?")
    }
    if (hasResp && hasWhen) {
      stop(label, " has both countResponse and countWhen -- pick one ",
           "(countResponse is sugar for countWhen=list(field='Key', operator='equals', value=<x>)).")
    }
    if (hasResp) {
      if (!isSingleString(rule$countResponse) || nchar(rule$countResponse) == 0) {
        stop(label, ".countResponse must be a non-empty single string.")
      }
    }
    if (hasWhen) {
      validateSwitchCountWhenShape(rule$countWhen, paste0(label, ".countWhen"))
    }
    if (is.null(rule$threshold)) {
      stop(label, " missing threshold. Did you forget to wrap with buildQCEblockSwitchRule?")
    }
    validateSwitchThresholdShape(rule$threshold, paste0(label, ".threshold"))
    if (!is.null(rule$switchToBlock)) {
      if (!isSingleString(rule$switchToBlock) || nchar(rule$switchToBlock) == 0) {
        stop(label, ".switchToBlock must be a non-empty single string when present ",
             "(omit for end-session-early without a destination).")
      }
    }
  }
  invisible(TRUE)
}

# --- Shared vocabulary for the completion gate, card fields, pages ------------

# The aggregators the engine's qcepComputeAggregate implements, and the
# comparison operators its evaluator accepts. Named once so the completion gate,
# card fields, and any future consumer cannot drift apart.
.qcebValidAggregateFns <- c("mean", "median", "proportion", "count", "sum", "min", "max", "sd")
.qcebValidCompareOps   <- c(">=", "<=", ">", "<", "==", "!=")

# The five events at which a page may be played or a card mounted/unmounted.
# Pages and cards share one vocabulary; naming it once is what stops the two
# from drifting apart, as they previously did over "experimentStart".
.qcebAnchorEvents       <- c("experimentStart", "sessionStart", "sessionEnd", "entry", "exit")
# The events that name a block boundary, and so require a session and a block.
.qcebAnchorScopedEvents <- c("entry", "exit")

# Why `x` is not a usable anchor, or NULL when it is. Returns the message rather
# than stopping so each caller can prefix it with its own argument name.
#
# An anchor is a LIST of fields, not a string: list(at=, session=, block=, set=).
# A string grammar would need delimiters, and block and set names are
# unconstrained free text, so a parsed form would impose a character restriction
# on a namespace nothing else restricts. Level -- block boundary versus set
# boundary -- is carried by whether `set` is present, so it cannot contradict a
# separate level field.
qcebAnchorProblem <- function(x) {
  if (!is.list(x) || is.null(x$at)) {
    return(paste0("must be an anchor list built by QCEanchor(), e.g. ",
                  "QCEanchor(\"entry\", session = \"1\", block = \"practice\")."))
  }
  known <- c("at", "session", "block", "set")
  extra <- setdiff(names(x), known)
  if (length(extra) > 0) {
    return(paste0("has unknown field(s) '", paste(extra, collapse = "', '"),
                  "'. An anchor carries only: ", paste(known, collapse = ", "), "."))
  }
  for (f in known) {
    v <- x[[f]]
    if (!is.null(v) && (!isSingleString(v) || is.na(v))) {
      return(paste0("field '", f, "' must be a single non-NA string."))
    }
  }
  if (!(x$at %in% .qcebAnchorEvents)) {
    return(paste0("has an unknown 'at' value '", x$at, "'. Valid: ",
                  paste(.qcebAnchorEvents, collapse = ", "), "."))
  }
  if (x$at %in% .qcebAnchorScopedEvents) {
    if (is.null(x$session)) {
      return(paste0("is '", x$at, "' but names no session. A block name is unique only ",
                    "within a session, so the session is part of the block's address."))
    }
    if (is.null(x$block)) {
      return(paste0("is '", x$at, "' but names no block.",
                    if (!is.null(x$set))
                      paste0(" A set named without its block addresses EVERY block containing ",
                             "a set called '", x$set, "', which is almost never intended.")
                    else ""))
    }
    return(NULL)
  }
  if (!is.null(x$block) || !is.null(x$set)) {
    return(paste0("is '", x$at, "', which names no block or set, but carries one. ",
                  "Use at = 'entry' or 'exit' to address a block or set boundary."))
  }
  if (identical(x$at, "experimentStart") && !is.null(x$session)) {
    return(paste0("is 'experimentStart' but names a session. experimentStart plays once ",
                  "for the whole run, before any session begins. Use 'sessionStart' to ",
                  "place something at the top of a session."))
  }
  NULL
}

isValidQCEanchor <- function(x) is.null(qcebAnchorProblem(x))

# Can two anchors ever fire at the same moment? Used to reject a card whose
# mount and unmount coincide. Equality is the wrong test: an optional `session`
# means an anchor with none and one naming session 3 both fire at session 3's
# boundary without being equal, and field order would defeat identical().
qcebAnchorsOverlap <- function(a, b) {
  if (!identical(a$at, b$at)) return(FALSE)
  if (!identical(a$block, b$block)) return(FALSE)
  if (!identical(a$set, b$set)) return(FALSE)
  is.null(a$session) || is.null(b$session) || identical(a$session, b$session)
}

# Validate the aggregator half of a formula -- the parts a completion-gate
# formula and a card-field formula have in common. `label` prefixes every
# message so each caller keeps its own phrasing. Gate-only concerns (op, value)
# stay with the caller, since a card field displays a number rather than
# comparing one.
validateQCEaggregateFn <- function(f, label) {
  if (!(f$fn %in% .qcebValidAggregateFns)) {
    stop(sprintf("%s has invalid fn '%s'. Valid: %s.",
                 label, as.character(f$fn), paste(.qcebValidAggregateFns, collapse = " ")))
  }
  if (!isSingleString(f$column) || nchar(f$column) == 0) {
    stop(sprintf("%s 'column' must be a single non-empty string naming a data column.", label))
  }
  invisible(TRUE)
}

# Validate a `where` row filter: a named list whose entries are either a bare
# scalar (loose equality) or a list(op, value).
validateQCEwhereFilter <- function(where, label) {
  if (is.null(where)) return(invisible(TRUE))
  if (!is.list(where) || is.null(names(where)) || any(names(where) == "")) {
    stop(sprintf("%s 'where' must be a named list of column filters.", label))
  }
  for (wc in names(where)) {
    spec <- where[[wc]]
    if (is.list(spec) && !is.null(spec$op) && !(spec$op %in% .qcebValidCompareOps)) {
      stop(sprintf("%s 'where$%s' has invalid op '%s'.", label, wc, as.character(spec$op)))
    }
    # An ordering op compares numerically, so a non-numeric bound can never match
    # a row -- it would silently filter the sample to empty at run time. Rejected
    # here as well as in the engine.
    if (is.list(spec) && !is.null(spec$op) && !(spec$op %in% c("==", "!="))) {
      if (is.null(spec$value)) {
        stop(sprintf("%s 'where$%s' is missing a 'value'.", label, wc))
      }
      wv <- spec$value[[1]]
      if (is.logical(wv) || length(wv) != 1 || is.na(suppressWarnings(as.numeric(wv)))) {
        stop(sprintf(paste0("%s 'where$%s' uses ordering op '%s' so its 'value' must be ",
                            "a single finite number."), label, wc, as.character(spec$op)))
      }
    }
  }
  invisible(TRUE)
}

# The keys a completionGate may carry. Anything else is almost certainly a
# misspelling, and a misspelled key is DROPPED SILENTLY -- the gate then runs
# with a policy nobody wrote. Kept as data so the check cannot drift from the
# validators beside it.
#
# ⚠ THIS LIST MUST TRACK THE ENGINE, or the warning it feeds accuses valid
# configs: a key the engine reads but this list omits is reported as one "the
# engine will ignore", and a builder who believes that removes a working part
# of their gate. That happened -- creditMsg, unscoredMsg and scoredBy were
# all engine-read and all missing here.
.qcebGateKeys <- c("formula", "gateFn", "combinator", "scoredBy",
                   "creditMsg", "noCreditMsg", "unscoredMsg",
                   "attemptsAllowed", "retryPrompt", "supersededMsg",
                   "duplicateMsg")

# Validate the attempts half of a completion gate: how many tries the
# participant gets, and the question that offers the next one.
#
# ⚠ ABSENT MEANS ONE. A gate that says nothing about attempts gets a single
# attempt, matching what the engine and the assignment endpoint both do when the
# key is missing. So `attemptsAllowed` is never required -- but a value that IS
# written and is not a whole number of 1 or more is broken rather than silent,
# and is refused rather than rounded into something plausible.
#
# The two cross-checks exist because the two keys are only meaningful together:
# a gate that allows a second attempt and never asks for one can never grant it,
# and a gate that asks while allowing one promises the participant something it
# cannot deliver, at the moment they have just been told they failed.
#
# This mirrors the engine's own check, which runs at experiment start. Running it
# here as well moves the diagnosis to build time, where no participant is waiting
# on it -- the same reason the formula validators are duplicated.
validateQCEattemptsPolicy <- function(cfg, label = "completionGate") {
  if (is.null(cfg) || !is.list(cfg)) return(invisible(TRUE))

  declared <- !is.null(cfg$attemptsAllowed)
  n <- if (declared) cfg$attemptsAllowed else 1
  if (declared &&
      (!is.numeric(n) || length(n) != 1 || !is.finite(n) || n != floor(n) || n < 1)) {
    stop(sprintf("%s 'attemptsAllowed' must be a whole number of 1 or more.", label))
  }
  n <- as.numeric(n)

  prompt <- cfg$retryPrompt
  hasPrompt <- !is.null(prompt)
  if (hasPrompt && (!is.list(prompt) || is.null(names(prompt)))) {
    stop(sprintf("%s 'retryPrompt' must be a named list with 'text', 'yesLabel' and 'noLabel'.",
                 label))
  }

  if (n > 1 && !hasPrompt) {
    stop(sprintf(paste0("%s allows %s attempts but declares no 'retryPrompt', so a ",
                        "participant who fails could never be asked whether they want ",
                        "another attempt -- the extra attempts are unreachable. Add ",
                        "retryPrompt = list(text=, yesLabel=, noLabel=), or set ",
                        "attemptsAllowed to 1."), label, format(n)))
  }
  if (n == 1 && hasPrompt) {
    # Two different mistakes arrive here and they need different advice. A gate
    # that WROTE 1 meant one attempt and should drop the prompt; a gate that
    # wrote nothing probably meant to allow a second and forgot the key. Naming
    # the wrong one sends the researcher to the wrong edit.
    stop(sprintf(paste0("%s declares a 'retryPrompt' but %s, so the participant would be ",
                        "offered a second attempt that cannot be granted. %s"),
                 label,
                 if (declared) "allows only 1 attempt"
                 else "no 'attemptsAllowed', which means one attempt",
                 if (declared) "Raise attemptsAllowed, or remove retryPrompt."
                 else "Add attemptsAllowed (2 offers the same material once more), or remove retryPrompt."))
  }
  if (hasPrompt) {
    for (k in c("text", "yesLabel", "noLabel")) {
      v <- prompt[[k]]
      if (!isSingleString(v) || nchar(v) == 0) {
        stop(sprintf("%s 'retryPrompt$%s' must be a single non-empty string.", label, k))
      }
    }
    extra <- setdiff(names(prompt), c("text", "yesLabel", "noLabel"))
    if (length(extra) > 0) {
      warning(sprintf("%s 'retryPrompt' has unrecognized entries the engine will ignore: %s.",
                      label, paste(extra, collapse = ", ")))
    }
  }
  invisible(TRUE)
}
