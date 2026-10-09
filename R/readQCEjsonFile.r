#' Read a QCEB-written JSON config back into a list you can save again
#'
#' Reads a JSON file that \code{saveJsonFile()} wrote and returns it in the same
#' shape the package's builders produce, so it can be extended and saved again
#' without changing anything that was already in it:
#' \code{saveJsonFile(readQCEjsonFile(f), f)} leaves the file unchanged.
#'
#' A bare value is read as a vector of length one. A list of one item is read as
#' that item wrapped in \code{I()}, which is how a list of one is marked for
#' \code{saveJsonFile()}: a setting the description lists as one value is still
#' written bare, and a study's own value, an output variable or a plugin parameter
#' written as a list of one stays a list of one. A longer list of single values is
#' read as a vector, a list of objects as a list of lists, and nested lists stay
#' nested; objects and lists are never collapsed into data frames or matrices.
#'
#' A file written by QCEB 0.0.5 or earlier holds every single value as a list of
#' one. Read it with \code{unboxLists = TRUE}, which reads every list of one as the
#' value inside it, so that saving it again writes each single value bare.
#'
#' @param filename A string giving the path of the JSON file to read.
#' @param unboxLists A single Boolean. \code{TRUE} reads every list of one as the value inside it, for a file written by QCEB 0.0.5 or earlier. Default \code{FALSE}.
#'
#' @return A list in the same shape the package's builders produce.
#' @keywords QCE json read config round trip
#' @export
#' @examples
#' \dontrun{
#' cfg <- readQCEjsonFile("myExp_Tsfile.json")
#' cfg <- addBlockToQCETrialStructureList(cfg, ...)
#' saveJsonFile(cfg, "myExp_Tsfile.json")
#' }

readQCEjsonFile <- function (filename, unboxLists = FALSE) {

  if(!isSingleString(filename)) {
    stop("filename option must be a single string giving the path of the file to read.")
  }

  if(!file.exists(filename)) {
    stop(paste0("readQCEjsonFile cannot find the file '", filename, "'."))
  }

  if(!(is.logical(unboxLists) && length(unboxLists) == 1 && !is.na(unboxLists))) {
    stop("unboxLists option must be TRUE or FALSE.")
  }

  #read unsimplified, so a bare value and a list of one stay distinct
  .qceRestoreJsonShape(
    jsonlite::fromJSON(filename,
                       simplifyVector    = FALSE,
                       simplifyDataFrame = FALSE,
                       simplifyMatrix    = FALSE),
    unboxLists
  )
}

#an unsimplified parse in the shape the builders produce
.qceRestoreJsonShape <- function (x, unboxLists = FALSE) {

  if (is.list(x)) {
    if (length(x) == 0L) {
      return(x)
    }

    nms <- names(x)
    if (!is.null(nms) && any(nzchar(nms))) {
      out <- lapply(x, .qceRestoreJsonShape, unboxLists = unboxLists)
      names(out) <- nms
      return(out)
    }

    #a list of single values is a vector
    allScalar <- all(vapply(x, function (e) {
      is.null(e) || (!is.list(e) && is.atomic(e) && length(e) == 1L)
    }, logical(1)))
    if (allScalar) {
      v <- unlist(lapply(x, function (e) if (is.null(e)) NA else e), use.names = FALSE)
      if (length(v) == 1L && !unboxLists) return(I(v))
      return(v)
    }
    return(lapply(x, .qceRestoreJsonShape, unboxLists = unboxLists))
  }

  if (is.null(x)) return(NA)
  x
}
