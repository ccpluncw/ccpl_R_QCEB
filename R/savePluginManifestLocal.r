#' This function writes an experiment-local plugin manifest
#'
#' Function that writes \code{pluginManifest.local.json} into the working directory. The engine loads this file additively beside its central plugin manifest: an experiment can ADD plugins of its own here, but a name that also exists in the central manifest loads the central entry and the local one is ignored with a console error -- a local manifest can never shadow or alter a shared plugin. Declare the plugin name in a session's \code{plugins} array (see \code{addSessionToSessionList}) exactly as for a central plugin.
#'
#' The file is serialized with scalars unboxed: the engine reads this manifest directly (no unwrapping layer), so a value written as a one-element array would be refused at run time.
#' @param plugins A named list of manifest entries, one per plugin, each built by \code{buildQCElocalPluginEntry}. The names are the plugin names sessions declare.
#' @param filename A string giving the file to write. Defaults to \code{"pluginManifest.local.json"}, the only name the engine reads; override only to stage the file somewhere else before deployment.
#'
#' @return the json data
#' @keywords QCE plugin local manifest save
#' @export
#' @examples savePluginManifestLocal (list(myTrialType = buildQCElocalPluginEntry("myTrialTypeRegister.js")))

savePluginManifestLocal <- function (plugins, filename = "pluginManifest.local.json") {

  if (!is.list(plugins) || length(plugins) == 0) {
    stop("plugins must be a non-empty named list of entries from buildQCElocalPluginEntry()")
  }
  nm <- names(plugins)
  if (is.null(nm) || any(is.na(nm)) || any(nchar(nm) == 0)) {
    stop("every element of plugins must be named -- the name is the plugin name sessions declare")
  }
  if (anyDuplicated(nm)) {
    stop(paste0("duplicate plugin name(s): ", paste(unique(nm[duplicated(nm)]), collapse = ", ")))
  }
  for (n in nm) {
    entry <- plugins[[n]]
    if (!is.list(entry) || is.null(entry$register)) {
      stop(paste0("plugin '", n, "' has no register script -- build entries with buildQCElocalPluginEntry()"))
    }
    # Files may be written later in the build; absence is a warning here and a
    # hard failure at preflight. An asset with neither key is a structural
    # mistake (only reachable with a hand-built entry) and must fail with a
    # message naming it, not with vapply's length-zero error.
    paths <- c(entry$register,
               vapply(seq_along(entry$assets), function (i) {
                 a <- entry$assets[[i]]
                 p <- if (!is.null(a$src)) a$src else a$href
                 if (is.null(p)) {
                   stop(paste0("plugin '", n, "': asset ", i,
                               " has neither 'src' nor 'href' -- build entries with buildQCElocalPluginEntry()"))
                 }
                 p
               }, character(1)))
    for (p in paths) {
      if (!file.exists(p)) {
        warning(paste0("plugin '", n, "': file not found in the working directory (write it before preflight): ", p))
      }
    }
  }

  # Unboxed on purpose: the engine parses this file directly, with no
  # unwrapping layer, so a one-element array where a scalar belongs would be
  # refused at run time. Asset lists stay arrays -- lists are never unboxed.
  jsonData <- jsonlite::toJSON(list(plugins = plugins), pretty = TRUE, auto_unbox = TRUE)
  write(jsonData, filename, append = FALSE)

  return(jsonData)
}
