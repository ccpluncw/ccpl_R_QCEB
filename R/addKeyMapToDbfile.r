#' Register a named keyMap on a QCEdbfile (incremental)
#'
#' Phase 3.5: appends one named keyMap entry to a QCEdbfile's `keyMaps`
#' dictionary. Mirrors the incremental pattern of addKeyToKeyMap +
#' addBlockToQCETrialStructureList + addSetToQCEsetInfoList -- pass NULL
#' or a partially-built dbfile and chain calls.
#'
#' Pair this with buildQCEkeyMapEntry to construct each entry. For one-shot
#' construction (declare all keyMaps at dbfile-build time), pass the same
#' entries as the `keyMaps` argument to buildQCEgroupDbFile; that path
#' delegates internally to this function in a loop.
#'
#' Initialization: if the dbfile has no `keyMaps` field yet (the typical
#' case the first time this is called), the field is created. Subsequent
#' calls append.
#'
#' Duplicate names: if a keyMap with the same name is already registered
#' on the dbfile, this function warns and overwrites. The warning catches
#' the researcher footgun of accidentally redefining a keyMap (typo in the
#' name, copy-pasted block that wasn't fully edited). To suppress the
#' warning intentionally, remove the existing entry first
#' (`QCEdbfile$keyMaps[[name]] <- NULL`).
#'
#' @param QCEdbfile A QCEdbfile (output of buildQCEgroupDbFile). Required.
#' @param name Single non-empty string -- the name blocks will use to
#'   reference this keyMap (via the new `keyMapName` argument on
#'   addBlockToQCETrialStructureList).
#' @param entry A keyMap entry list (output of buildQCEkeyMapEntry).
#''
#' @return The updated QCEdbfile, with the new keyMap registered under
#'   `$keyMaps[[name]]`.
#' @keywords QCE keyMap dbfile dynamic
#' @export
#' @examples
#' dbfile <- buildQCEgroupDbFile(condName = "multiTask",
#'   keyMap = buildKeyMap(data.frame(Yes = "y", No = "n")))
#'
#' # Register two named keyMaps for blocks to reference
#' km_yesNo <- buildKeyMap(data.frame(Yes = "y", No = "n"))
#' km_dir   <- buildKeyMap(data.frame(Left = "j", Right = "k"))
#' dbfile <- addKeyMapToDbfile(dbfile, "yesNo",
#'             buildQCEkeyMapEntry(map = km_yesNo, randomize = TRUE))
#' dbfile <- addKeyMapToDbfile(dbfile, "directional",
#'             buildQCEkeyMapEntry(map = km_dir))

addKeyMapToDbfile <- function(QCEdbfile, name, entry) {

  if (is.null(QCEdbfile) || !is.list(QCEdbfile)) {
    stop("QCEdbfile option must be a QCEdbfile (output of buildQCEgroupDbFile).")
  }

  if (missing(name) || !isSingleString(name) || nchar(name) == 0) {
    stop("name option must be a non-empty single string -- the name blocks ",
         "will use to reference this keyMap via keyMapName.")
  }

  if (missing(entry) || is.null(entry) || !is.list(entry) || is.null(entry$map)) {
    stop("entry option must be a keyMap entry (output of buildQCEkeyMapEntry).")
  }

  # Initialize the keyMaps slot on first add. Legacy dbfiles built before
  # Phase 3.5 have no $keyMaps field; adding the slot here keeps JSON
  # byte-identical for dbfiles that never receive a keyMap (no $keyMaps
  # field = no `keyMaps` key in the output JSON).
  if (is.null(QCEdbfile$keyMaps)) {
    QCEdbfile$keyMaps <- list()
  }

  # Duplicate-name warning. Catches researcher typos and copy-paste errors.
  if (!is.null(QCEdbfile$keyMaps[[name]])) {
    warning(sprintf(
      "keyMap '%s' already defined on dbfile; overwriting with new entry.",
      name))
  }

  QCEdbfile$keyMaps[[name]] <- entry
  return(QCEdbfile)
}
