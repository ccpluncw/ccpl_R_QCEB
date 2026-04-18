#' DEPRECATED — use addFrameToQCEframeList() instead
#'
#' Thin wrapper that issues a .Deprecated() warning and delegates to
#' addFrameToQCEframeList(). The v7-suffixed name was dropped because the
#' jsPsych 7 vs. 8 distinction no longer matters at the JSON schema level;
#' the unsuffixed function works for both. All existing callers of
#' addFrameToQCEframeList_7() keep working (with a one-time deprecation
#' warning per R session).
#'
#' @param ... All arguments are forwarded verbatim to addFrameToQCEframeList().
#'   See ?addFrameToQCEframeList for the full parameter list, which is a
#'   superset (adds the `trigger` param for fNIRS support).
#' @return the updated QCEframeList (from addFrameToQCEframeList)
#' @keywords QCE QCEframeList deprecated
#' @export

addFrameToQCEframeList_7 <- function (...) {
  .Deprecated("addFrameToQCEframeList",
              msg = "addFrameToQCEframeList_7() is deprecated. Use addFrameToQCEframeList() — same signature plus a new optional `trigger` parameter for fNIRS support.")
  addFrameToQCEframeList(...)
}
