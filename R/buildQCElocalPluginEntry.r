#' This function builds one entry for an experiment-local plugin manifest
#'
#' Function that assembles the manifest entry for a plugin that ships inside the experiment directory itself (see \code{savePluginManifestLocal}). The engine confines every path in a local entry to the experiment directory: each one must be a plain relative path with forward slashes -- no URL, no absolute path, no drive letter, no '..' segment -- and the engine refuses (with a console error) any path that breaks those rules or does not resolve to an existing file inside the directory.
#' @param registerScript A string giving the experiment-relative path of the register script. This script must call \code{registerTrialType()}; it is loaded after the entry's assets.
#' @param scriptAssets An optional character vector of experiment-relative paths of script files to load before the register script (a plugin's library bundle, for example), in order.
#' @param cssAssets An optional character vector of experiment-relative paths of stylesheet files. The engine imports each into its vendor cascade layer, below the experiment's own stylesheet.
#' @param description An optional string describing the plugin, carried in the manifest for the next reader.
#'
#' @return a list holding the manifest entry, ready for \code{savePluginManifestLocal}
#' @keywords QCE plugin local manifest
#' @export
#' @examples buildQCElocalPluginEntry ("myPluginRegister.js", scriptAssets = c("myPluginBundle.js"))

buildQCElocalPluginEntry <- function (registerScript, scriptAssets = NULL, cssAssets = NULL, description = NULL) {

  validatePath <- function (p, what) {
    if (!is.character(p) || length(p) != 1 || is.na(p) || nchar(p) == 0) {
      stop(paste0(what, " must be a single non-empty string, got: ", deparse(p)))
    }
    if (grepl("://", p, fixed = TRUE) || startsWith(p, "//")) {
      stop(paste0(what, " may not be a URL -- a local plugin loads no external code: ", p))
    }
    if (startsWith(p, "/") || grepl("\\\\", p) || grepl("^[A-Za-z]:", p)) {
      stop(paste0(what, " must be a relative path with forward slashes: ", p))
    }
    if (grepl("(^|/)\\.\\.(/|$)", p)) {
      stop(paste0(what, " may not contain '..' -- every path resolves inside the experiment directory: ", p))
    }
    invisible(p)
  }

  validatePath(registerScript, "registerScript")

  assets <- list()
  for (src in scriptAssets) {
    validatePath(src, "each scriptAssets path")
    assets[[length(assets) + 1]] <- list(type = "script", src = src)
  }
  for (href in cssAssets) {
    validatePath(href, "each cssAssets path")
    assets[[length(assets) + 1]] <- list(type = "css", href = href)
  }

  entry <- list()
  if (!is.null(description)) {
    if (!is.character(description) || length(description) != 1) {
      stop("description must be a single string")
    }
    entry$description <- description
  }
  if (length(assets) > 0) entry$assets <- assets
  entry$register <- registerScript

  return(entry)
}
