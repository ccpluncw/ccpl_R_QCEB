#' This function tests whether a variable contains a single string
#'
#' Function tests whether a variable contains a single string.
#' @param input A variable to be tested.
#''
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
#''
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
#''
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
#''
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
validateShowIfShape <- function(x, paramName = "showIf") {
  if (!is.list(x)) {
    stop(paramName, " must be a list (output of buildQCEshowIfCondition, ",
         "buildQCEblockSwitchedCondition, or buildQCEshowIfCompound).")
  }
  isStimLeaf  <- !is.null(x$stimRef)  && !is.null(x$operator)
  isBlockLeaf <- !is.null(x$blockRef) && !is.null(x$operator)
  isCompound  <- !is.null(x$all) || !is.null(x$any)
  if (!isStimLeaf && !isBlockLeaf && !isCompound) {
    stop(paramName, " is not a valid showIf condition: ",
         "expected stimRef+operator (single), blockRef+operator (single, ",
         "Phase 3.5), or all/any (compound). Did you forget to wrap with ",
         "buildQCEshowIfCondition, buildQCEblockSwitchedCondition, or ",
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
