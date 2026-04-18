#' DEPRECATED — use addFixationToQCEframeList() instead
#'
#' Thin wrapper that issues a .Deprecated() warning and delegates to
#' addFixationToQCEframeList(). The v7-suffixed name was dropped because the
#' jsPsych 7 vs. 8 distinction no longer matters at the JSON schema level;
#' the unsuffixed function works for both. All existing callers of
#' addFixationToQCEframeList_7() keep working (with a one-time deprecation
#' warning per R session).
#'
#' @param ... All arguments are forwarded verbatim to addFixationToQCEframeList().
#'   See ?addFixationToQCEframeList for the full parameter list, which is a
#'   superset (adds the `trigger` param for fNIRS support).
#' @return the updated QCEframeList (from addFixationToQCEframeList)
#' @keywords QCE QCEframeList fixation deprecated
#' @export

addFixationToQCEframeList_7 <- function (...) {
  .Deprecated("addFixationToQCEframeList",
              msg = "addFixationToQCEframeList_7() is deprecated. Use addFixationToQCEframeList() — same signature plus a new optional `trigger` parameter for fNIRS support.")
  addFixationToQCEframeList(...)
}
