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
    if(stringi::stri_sub(filename,-1*strN) == extension) {
      out <- TRUE
    }
  }

  return (out)

}


#showIf leaf: stimRef, blockRef or stateRef plus operator, exactly one kind
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


#countWhen tests one just-completed row, so wasShown and wasNotShown are out
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


#mirrors the engine's threshold resolution and switch rule validation
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


#each rule has countResponse xor countWhen plus a well-shaped threshold
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


#block rules name a watchBlock, the engine checks block references at start
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
    #watchBlock tells a block rule from a set rule
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


#aggregators and comparison operators the engine evaluates
.qcebValidAggregateFns <- c("mean", "median", "proportion", "count", "sum", "min", "max", "sd")
.qcebValidCompareOps   <- c(">=", "<=", ">", "<", "==", "!=")

#events at which a page plays or a card mounts or unmounts
.qcebAnchorEvents       <- c("experimentStart", "sessionStart", "sessionEnd", "entry", "exit")
#events that name a block boundary and so need a session and a block
.qcebAnchorScopedEvents <- c("entry", "exit")

#reason x is not a usable anchor, or NULL, anchor is a list of fields
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

#overlap not equality, an absent session matches every session
qcebAnchorsOverlap <- function(a, b) {
  if (!identical(a$at, b$at)) return(FALSE)
  if (!identical(a$block, b$block)) return(FALSE)
  if (!identical(a$set, b$set)) return(FALSE)
  is.null(a$session) || is.null(b$session) || identical(a$session, b$session)
}

#aggregator checks shared by completion gate and card field formulas
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

#where entries are a bare scalar or list(op, value)
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
    #an ordering op compares numerically, so a non-numeric bound never matches
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

#keys a completionGate may carry, kept in step with the engine's reads
.qcebGateKeys <- c("formula", "gateFn", "combinator", "scoredBy",
                   "creditMsg", "noCreditMsg", "unscoredMsg",
                   "attemptsAllowed", "retryPrompt", "supersededMsg",
                   "duplicateMsg")

#attempts half of a completion gate, absent attemptsAllowed means one
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
    #a written 1 and a missing key need different advice
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
