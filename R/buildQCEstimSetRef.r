#' Build a reference to a stimulus set
#'
#' Function that reads a locked stimulus set's \code{manifest.json} and selects the items a block shows. A set is a directory \code{<stimuliDir>/<stimSet>/} holding \code{manifest.json} (schema version 1) and, for a set of files, \code{files/}; the platform places it in the study before the build. The reference is passed to \code{addStimSetToQCEscenarioList}, which writes one scenario per selected item, and to \code{savePreloadFiles}, which preloads the set's files. Files are served only inside a running session, through the engine's stimulus endpoint, so a set needs engine 10.0 or later.
#'
#' A filter keeps an item when every named attribute matches: a single value is an equality test, and \code{list(min = , max = )} (either bound may be left out) is a range test on a number attribute. An item with no value for a filtered attribute does not match.
#' @param stimSet A single string naming the set: its directory under \code{stimuliDir} and the \code{set.name} in its manifest.
#' @param where A named list filtering the items on their attributes, for example \code{list(gender = "f", rating = list(min = 3))}. \code{NULL} keeps every item. Default \code{NULL}.
#' @param n A single whole number: how many of the matching items each participant sees, drawn at random per participant by the engine. \code{NULL} shows every matching item. Pass \code{ref$n} as \code{numberOfTrialsPerSet} to \code{addSetToQCEsetInfoList} with \code{selectionType = "randomWithoutReplacement"}. Default \code{NULL}.
#' @param stimuliDir A string giving the directory that holds the study's sets, relative to the working directory the build runs in. Default \code{"stimuli"}.
#' @param engineVersion A string naming the engine the study runs. Sets are refused before engine 10.0, which has no stimulus endpoint. Default \code{"10.0"}.
#'
#' @return A list describing the reference: \code{stimSet}, the set's name; \code{version}, its locked version; \code{kind}, \code{"files"} or \code{"text"}; \code{attributes}, the declared attributes, each a list with \code{name}, \code{type} and, for a category, \code{levels}; \code{items}, the matching items, each a list with \code{id}, \code{attrs} and either \code{file} and \code{mediaType} or \code{text}; \code{n}, how many of them each participant sees; \code{where}, the filter as given.
#' @keywords QCE stimulus set reference manifest
#' @export
#' @examples
#' \dontrun{
#' faces <- buildQCEstimSetRef("faces", where = list(gender = "f"), n = 15)
#' }

buildQCEstimSetRef <- function(stimSet, where = NULL, n = NULL, stimuliDir = "stimuli", engineVersion = "10.0") {

  if (!isSingleString(stimSet) || !grepl("^[a-z][a-z0-9_]{0,39}$", stimSet)) {
    stop("stimSet must be a single set name: lower-case letters, digits and _, starting with a letter.")
  }
  if (!isSingleString(engineVersion)) {
    stop("engineVersion must be a single string such as \"10.0\".")
  }
  major <- suppressWarnings(as.integer(sub("\\..*$", "", engineVersion)))
  if (is.na(major) || major < 10) {
    stop("stimulus set \"", stimSet, "\" needs engine 10.0 or later; engine ", engineVersion,
         " cannot serve a set's files. Run the study on engine 10.0.")
  }

  manifestFile <- file.path(stimuliDir, stimSet, "manifest.json")
  if (!file.exists(manifestFile)) {
    have <- if (dir.exists(stimuliDir)) list.dirs(stimuliDir, full.names = FALSE, recursive = FALSE) else character()
    stop("there is no stimulus set \"", stimSet, "\" in ", stimuliDir, "/. ",
         if (length(have)) paste0("The study carries: ", paste(have, collapse = ", "), ".")
         else "The study carries no stimulus sets.")
  }
  doc <- jsonlite::fromJSON(manifestFile, simplifyVector = FALSE)
  if (!identical(doc$manifestVersion, 1L) && !identical(doc$manifestVersion, 1)) {
    stop("stimulus set \"", stimSet, "\" has manifest version ", format(doc$manifestVersion),
         "; only version 1 is read.")
  }
  if (!identical(doc$set$name, stimSet)) {
    stop("the manifest in ", stimuliDir, "/", stimSet, "/ names the set \"", doc$set$name, "\".")
  }
  kind <- doc$set$kind
  if (!(identical(kind, "files") || identical(kind, "text"))) {
    stop("stimulus set \"", stimSet, "\" is of kind \"", format(kind), "\"; only files and text sets are read.")
  }

  attributes <- lapply(doc$attributes, function(a) {
    out <- list(name = a$name, type = a$type)
    if (identical(a$type, "category")) out$levels <- unlist(a$levels)
    out
  })
  attrNames <- vapply(attributes, function(a) a$name, "")

  if (!is.null(where)) {
    if (!is.list(where) || is.null(names(where)) || any(names(where) == "")) {
      stop("where must be a named list of attribute filters, such as list(gender = \"f\").")
    }
    for (nm in names(where)) {
      if (!(nm %in% attrNames)) {
        stop("stimulus set \"", stimSet, "\" has no attribute \"", nm, "\"; its attributes are ",
             if (length(attrNames)) paste(attrNames, collapse = ", ") else "none", ".")
      }
      a <- attributes[[match(nm, attrNames)]]
      f <- where[[nm]]
      if (is.list(f)) {
        if (!identical(a$type, "number")) {
          stop("where$", nm, ": a min/max range fits only a number attribute; \"", nm, "\" is a ", a$type, ".")
        }
        if (length(f) == 0 || !all(names(f) %in% c("min", "max")) ||
            !all(vapply(f, isSingleNumeric, TRUE))) {
          stop("where$", nm, " must be list(min = <number>, max = <number>), either bound optional.")
        }
      } else {
        if (length(f) != 1 || is.na(f)) {
          stop("where$", nm, " must be a single value, or list(min = , max = ) for a number.")
        }
        if (identical(a$type, "category") && !(as.character(f) %in% a$levels)) {
          stop("where$", nm, " is \"", f, "\", which is not one of its levels: ",
               paste(a$levels, collapse = ", "), ".")
        }
        if (identical(a$type, "number") && !is.numeric(f)) {
          stop("where$", nm, " must be a number: \"", nm, "\" is a number attribute.")
        }
      }
    }
  }

  matches <- function(it) {
    for (nm in names(where)) {
      v <- it$attrs[[nm]]
      if (is.null(v)) return(FALSE)
      f <- where[[nm]]
      if (is.list(f)) {
        if (!is.null(f$min) && v < f$min) return(FALSE)
        if (!is.null(f$max) && v > f$max) return(FALSE)
      } else if (!identical(as.character(v), as.character(f))) {
        return(FALSE)
      }
    }
    TRUE
  }

  items <- Filter(matches, doc$items)
  items <- lapply(items, function(it) {
    out <- list(id = it$id, attrs = if (is.null(it$attrs)) list() else it$attrs)
    if (identical(kind, "files")) {
      out$file <- it$file
      out$mediaType <- it$mediaType
    } else {
      out$text <- it$text
    }
    out
  })
  if (length(items) == 0) {
    stop("no item of stimulus set \"", stimSet, "\" matches the filter.")
  }

  if (is.null(n)) {
    n <- length(items)
  } else {
    if (!isSingleNumeric(n) || n != round(n) || n < 1) {
      stop("n must be a single whole number of at least 1, or NULL for every matching item.")
    }
    if (n > length(items)) {
      stop("n is ", n, " but only ", length(items), " items match in stimulus set \"", stimSet, "\".")
    }
  }

  list(stimSet = stimSet, version = doc$set$version, kind = kind, attributes = attributes,
       items = items, n = as.integer(n), where = where)
}
