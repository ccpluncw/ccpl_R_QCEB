#' Read a QCEB-written JSON config back into a list you can save again
#'
#' Reads a JSON file that \code{saveJsonFile()} wrote and returns it in the same
#' shape the package's builders produce, so it can be extended and saved again
#' without changing the structure of anything that was already in it.
#'
#' This exists because reading and writing are not symmetric by default.
#' \code{saveJsonFile()} deliberately writes every scalar as a one-element array,
#' because that is what the experiment engine's reader expects. A plain
#' \code{jsonlite::fromJSON(simplifyVector = FALSE)} hands those back as
#' one-element \emph{lists}, and saving again wraps them a second time:
#' \code{"set": ["someSet"]} becomes \code{"set": [["someSet"]]}. Every scalar in
#' the file is affected. Nothing reports it -- the save succeeds, the file is
#' valid JSON, and the build prints its usual summary -- but a set name nested one
#' level too deep matches no setInfo entry, so the first symptom is a session that
#' runs no trials.
#'
#' Reading with this function instead makes the round trip lossless:
#' \code{saveJsonFile(readQCEjsonFile(f), f)} leaves the file unchanged.
#'
#' Values the package writes \emph{unboxed} on purpose -- a bare
#' \code{"choices": "NO_KEYS"} rather than \code{["NO_KEYS"]} -- are read back
#' still unboxed, so re-saving does not turn a scalar the engine expects into a
#' one-element array. That distinction is only visible before the JSON is
#' simplified, which is why this reads the file unsimplified and restores the
#' shapes itself.
#'
#' Arrays of objects are kept as lists of lists rather than collapsed into data
#' frames, and equal-length nested arrays are kept as nested lists rather than
#' collapsed into matrices. Both of those simplifications would lose the config's
#' structure and are switched off.
#'
#' @param filename A string giving the path of the JSON file to read.
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

readQCEjsonFile <- function (filename) {

  if(!isSingleString(filename)) {
    stop("filename option must be a single string giving the path of the file to read.")
  }

  if(!file.exists(filename)) {
    stop(paste0("readQCEjsonFile cannot find the file '", filename, "'."))
  }

  # Read unsimplified, then restore the shapes by hand. jsonlite's own
  # simplification cannot do this job: it maps both "x" and ["x"] onto the same
  # length-1 vector, so the difference between a value written unboxed and a
  # value written boxed is gone before this function could act on it -- and that
  # difference is exactly what has to survive the round trip.
  .qceRestoreJsonShape(
    jsonlite::fromJSON(filename,
                       simplifyVector    = FALSE,
                       simplifyDataFrame = FALSE,
                       simplifyMatrix    = FALSE)
  )
}

# Turn an unsimplified parse back into the shape the builders produce, so that
# saving it again reproduces the file it came from.
#
# Unsimplified, a JSON array is always an unnamed list and a JSON object is
# always a named one, so the two are distinguishable -- which is what makes the
# mapping below possible:
#
#   ["a","b"] -> list("a","b")  -> c("a","b")     toJSON writes the array back
#   ["x"]     -> list("x")      -> "x"            toJSON re-boxes the scalar
#   "x"       -> "x"            -> unbox("x")     stays a bare scalar
#
# Without the last line, a value the package deliberately unboxed would come
# back as a one-element array, silently changing what the engine is handed.
.qceRestoreJsonShape <- function (x) {

  if (is.list(x)) {
    if (length(x) == 0L) {
      return(x)                       # [] and {} both survive as they are
    }

    nms <- names(x)
    if (!is.null(nms) && any(nzchar(nms))) {
      out <- lapply(x, .qceRestoreJsonShape)   # JSON object -- recurse, keep names
      names(out) <- nms
      return(out)
    }

    # JSON array. If every element is a single primitive it was an array of
    # primitives, which is what an atomic vector serialises back to.
    allScalar <- all(vapply(x, function (e) {
      !is.list(e) && is.atomic(e) && length(e) == 1L
    }, logical(1)))
    if (allScalar) {
      return(unlist(x, use.names = FALSE))
    }

    return(lapply(x, .qceRestoreJsonShape))    # array of objects/arrays
  }

  # A primitive that was NOT inside an array: it was written unboxed, and has to
  # be marked so it is written unboxed again.
  if (is.atomic(x) && length(x) == 1L) {
    return(jsonlite::unbox(x))
  }

  x
}
