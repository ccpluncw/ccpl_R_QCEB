#' Build a compound showIf condition (all / any) for QCEP dynamic experiments
#'
#' Wraps a list of conditions (each from buildQCEshowIfCondition or another
#' buildQCEshowIfCompound for nesting) under an `all` or `any` aggregator,
#' matching the compound shape consumed by evaluateCondition in
#' customScripts/v9/dynamicEngine.js.
#'
#' @param kind Single string: "all" (every child must be true) or "any"
#'   (at least one child must be true).
#' @param conditions A list() of conditions or compounds. Each element must
#'   have the right shape (output of buildQCEshowIfCondition or
#'   buildQCEshowIfCompound). At least one element required.
#''
#' @return A list of the form list(all = list(c1, c2, ...)) or
#'   list(any = list(c1, c2, ...)).
#' @keywords QCE showIf condition compound dynamic
#' @export
#' @examples
#' # all-of: smoker AND over18
#' compound <- buildQCEshowIfCompound("all", list(
#'   buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"),
#'   buildQCEshowIfCondition("over18", "equals", "Yes", "Response")
#' ))
#'
#' # Nested: smoker AND (vapes OR chewsTob)
#' nested <- buildQCEshowIfCompound("all", list(
#'   buildQCEshowIfCondition("smoker", "equals", "Yes", "Response"),
#'   buildQCEshowIfCompound("any", list(
#'     buildQCEshowIfCondition("vapes",    "equals", "Yes", "Response"),
#'     buildQCEshowIfCondition("chewsTob", "equals", "Yes", "Response")
#'   ))
#' ))

buildQCEshowIfCompound <- function(kind, conditions) {

  validKinds <- c("all", "any")

  if (!isSingleString(kind)) {
    stop("kind option must be a single string. Yours, apparently, is not a single string.")
  }
  if (!(kind %in% validKinds)) {
    stop(paste("kind option must take on one of the following values:",
               paste(validKinds, collapse = " ")))
  }

  if (!is.list(conditions)) {
    stop("conditions option must be a list of conditions (use list(...) to wrap them).")
  }
  if (length(conditions) < 1) {
    stop("conditions option must contain at least one condition.")
  }

  # Shape-validate each child: must be a list with either
  # (a) stimRef + operator (single condition), or
  # (b) all or any (nested compound).
  for (i in seq_along(conditions)) {
    child <- conditions[[i]]
    if (!is.list(child)) {
      stop(paste0("conditions[[", i, "]] must be a list (output of buildQCEshowIfCondition or buildQCEshowIfCompound)."))
    }
    isSingle   <- !is.null(child$stimRef) && !is.null(child$operator)
    isCompound <- !is.null(child$all) || !is.null(child$any)
    if (!isSingle && !isCompound) {
      stop(paste0("conditions[[", i, "]] is not a valid showIf condition: ",
                  "expected stimRef+operator (single) or all/any (compound). ",
                  "Did you forget to wrap with buildQCEshowIfCondition or buildQCEshowIfCompound?"))
    }
  }

  out <- list()
  out[[kind]] <- conditions
  return(out)
}
