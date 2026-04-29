#' Build a switch-rule threshold specification for QCEP dynamic experiments
#'
#' Creates a threshold spec in the shape consumed by the QCEP engine's
#' resolveThreshold (see customScripts/v9/dynamicEngine.js). The returned
#' list is passed to the `threshold` parameter of buildQCEswitchRule.
#'
#' Three resolution rules are supported:
#'   - "fixed":             threshold = values[1]. Provide a single value.
#'   - "randomFromList":    threshold = sample(values, 1). Provide >= 1 value.
#'   - "randomIntBetween":  threshold = random int in [values[1], values[2]]
#'                          inclusive. Provide exactly 2 values.
#'
#' Threshold is resolved per-rule when the manager initializes that rule,
#' so each session can see a different draw under randomFromList /
#' randomIntBetween (variability across participants).
#'
#' @param values Numeric vector. Length depends on rule:
#'   1+ for "fixed" (only first element is used) and "randomFromList";
#'   exactly 2 for "randomIntBetween" (interpreted as [lo, hi]).
#' @param rule Single string. One of "fixed", "randomFromList",
#'   "randomIntBetween". DEFAULT = "fixed".
#''
#' @return A list of the form list(values, rule), shape-validated.
#' @keywords QCE switchRule threshold dynamic
#' @export
#' @examples
#' # Fixed threshold of 5 hits before firing
#' th1 <- buildQCEswitchThreshold(values = 5, rule = "fixed")
#'
#' # Random pick from {3, 5, 7}
#' th2 <- buildQCEswitchThreshold(values = c(3, 5, 7), rule = "randomFromList")
#'
#' # Random integer between 3 and 7 inclusive
#' th3 <- buildQCEswitchThreshold(values = c(3, 7), rule = "randomIntBetween")

buildQCEswitchThreshold <- function(values, rule = "fixed") {

  validRules <- c("fixed", "randomFromList", "randomIntBetween")

  if (!isSingleString(rule)) {
    stop("rule option must be a single string.")
  }
  if (!(rule %in% validRules)) {
    stop(paste("rule option must take on one of the following values:",
               paste(validRules, collapse = " ")))
  }

  if (missing(values) || is.null(values)) {
    stop("values option is required.")
  }
  if (!is.numeric(values) || length(values) < 1) {
    stop("values option must be a numeric vector with at least one element.")
  }
  if (rule == "randomIntBetween" && length(values) != 2) {
    stop("rule='randomIntBetween' requires exactly 2 values [lo, hi]; got ",
         length(values), ".")
  }

  return(list(values = values, rule = rule))
}
