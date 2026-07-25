#' Describe one value shown on a persistent card
#'
#' A card is an always-visible panel that lives outside the trial display, so it
#' survives trial transitions and re-renders from live experiment state. Each
#' named field it shows is either computed from the data so far, or read from a
#' state key a custom hook writes. Give exactly one of \code{formula} or
#' \code{bind}.
#'
#' A \code{formula} is aggregated over the whole run by the same evaluator the
#' completion gate uses, so a card can show the very number the gate will judge.
#' Note it takes no \code{op} or \code{value}: a gate compares a number, a card
#' merely displays one.
#'
#' Nothing a card shows is the source of truth for anything. The gate re-derives
#' its verdict from the dataset independently, so a card that degrades or fails
#' to render cannot affect a participant's outcome.
#'
#' @param formula A list with \code{fn} (one of "mean", "median", "proportion",
#'   "count", "sum", "min", "max", "sd"), \code{column} (the data column to
#'   aggregate), and optionally \code{where} (a named list of row filters, each
#'   entry either a bare value for equality or \code{list(op=, value=)}).
#'   \code{count} counts rows with a value present; \code{proportion} is the mean
#'   of a 0/1 column. Give this OR \code{bind}. DEFAULT = NULL.
#' @param bind A single string naming a key in the card view object that a custom
#'   hook writes. The escape hatch for values the dataset cannot express. Give
#'   this OR \code{formula}. DEFAULT = NULL.
#' @param digits An integer: decimal places for display rounding. DEFAULT = NULL
#'   (no rounding).
#' @param emptyValue What to display before there is anything to compute -- at
#'   the start of a run an aggregate over zero rows has no honest value. Without
#'   this the field renders blank. DEFAULT = NULL.
#'
#' @return A list describing one card field.
#' @keywords QCE cards card field formula
#' @export
#' @examples
#' # How many questions have been answered so far
#' buildQCEcardField(formula = list(fn = "count", column = "selectedValue"),
#'                   emptyValue = 0)
#'
#' # Accuracy over the multiple-choice trials only, to 2 decimal places
#' buildQCEcardField(
#'   formula = list(fn = "proportion", column = "correct",
#'                  where = list(respType = "mcKeys")),
#'   digits = 2, emptyValue = "-")
#'
#' # A value a custom hook publishes
#' buildQCEcardField(bind = "partnerName")
buildQCEcardField <- function(formula = NULL, bind = NULL, digits = NULL,
                              emptyValue = NULL) {

  hasFormula <- !is.null(formula)
  hasBind    <- !is.null(bind)

  if (hasFormula && hasBind) {
    stop("Give EITHER a 'formula' OR a 'bind', not both -- a field has one source.")
  }
  if (!hasFormula && !hasBind) {
    stop("A card field needs either a 'formula' (computed from the data) or a ",
         "'bind' (a state key a custom hook writes).")
  }

  if (hasBind && (!isSingleString(bind) || nchar(bind) == 0)) {
    stop("bind option must be a single non-empty string naming a card-view state key.")
  }

  if (hasFormula) {
    if (!is.list(formula) || is.null(formula$fn) || is.null(formula$column)) {
      stop("formula option must be a list with 'fn' and 'column' (and optionally 'where').")
    }
    if (!is.null(formula$op) || !is.null(formula$value)) {
      stop("A card field's formula takes no 'op' or 'value' -- a card displays a ",
           "number rather than comparing one. Those belong on a completionGate formula.")
    }
    validateQCEaggregateFn(formula, "card field formula")
    validateQCEwhereFilter(formula$where, "card field formula")
  }

  if (!is.null(digits)) {
    if (!isSingleNumeric(digits) || is.na(digits) || digits < 0 || digits != round(digits)) {
      stop("digits option must be a single non-negative whole number, or NULL.")
    }
  }

  if (!is.null(emptyValue) && length(emptyValue) != 1) {
    stop("emptyValue option must be a single value, or NULL.")
  }

  tmpList <- list()
  if (hasFormula) {
    tmpList$formula <- formula
  } else {
    tmpList$bind <- bind
  }
  if (!is.null(digits)) {
    tmpList$digits <- digits
  }
  if (!is.null(emptyValue)) {
    tmpList$emptyValue <- emptyValue
  }

  return(tmpList)
}
