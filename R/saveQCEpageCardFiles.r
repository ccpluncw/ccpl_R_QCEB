#' Write a page placement map and its sidecars
#'
#' Writes the placement map to \code{pagesFile} and each sidecar to
#' \code{<name>.page.json} in the same directory. The HTML files themselves are
#' yours to author -- this writes only the JSON that describes them.
#'
#' The sidecar name must match the \code{file} you used when placing the page, so
#' that \code{addPageToQCEpagePlacement(..., file = "consent")} pairs with
#' \code{sidecars = list(consent = ...)}.
#'
#' A page needs no sidecar if it collects nothing and its continue button already
#' carries the engine's default id; the engine falls back cleanly when one is
#' absent.
#'
#' @param QCEpagePlacement A placement map from
#'   \code{\link{addPageToQCEpagePlacement}}.
#' @param pagesFile Path to write the placement map to, e.g. "pagesA.json". Name
#'   it whatever you referenced from the group's \code{pages} argument.
#' @param sidecars A NAMED list of \code{\link{buildQCEpageSidecar}} results, the
#'   names being page base names. NULL writes the placement map only. DEFAULT =
#'   NULL.
#' @param dir Directory to write into. DEFAULT = "." (the working directory).
#'
#' @return Invisibly, a character vector of the paths written.
#' @keywords QCE pages page save write
#' @export
#' @examples
#' pages <- addPageToQCEpagePlacement(NULL, "sessionStart", "consent")
#' saveQCEpageFiles(pages, "pagesA.json",
#'                  sidecars = list(consent = buildQCEpageSidecar(contBtn = "agreeBtn")),
#'                  dir = tempdir())
saveQCEpageFiles <- function(QCEpagePlacement, pagesFile, sidecars = NULL, dir = ".") {

  if (missing(QCEpagePlacement) || !is.list(QCEpagePlacement) ||
      length(QCEpagePlacement) == 0) {
    stop("QCEpagePlacement option must be a non-empty placement map (output of ",
         "addPageToQCEpagePlacement).")
  }
  if (missing(pagesFile) || !isSingleString(pagesFile) || nchar(pagesFile) == 0) {
    stop("pagesFile option must be a single non-empty filename, e.g. 'pagesA.json'.")
  }
  if (!isSingleString(dir)) {
    stop("dir option must be a single string naming a directory.")
  }

  # Every page named in the map, so a sidecar for a page that is never placed can
  # be caught -- almost always a typo in one name or the other.
  # Iterate the placement LIST, not the map's names: the map is one named
  # element holding an unnamed list of entries, so names() says nothing about
  # which pages were placed.
  placed <- character(0)
  for (p in QCEpagePlacement$placements) {
    placed <- c(placed, as.character(p$file))
  }
  placed <- unique(placed)

  written <- character(0)
  path <- file.path(dir, pagesFile)
  saveJsonFile(QCEpagePlacement, path)
  written <- c(written, path)

  if (!is.null(sidecars)) {
    nms <- names(sidecars)
    if (!is.list(sidecars) || is.null(nms) || any(nms == "")) {
      stop("sidecars option must be a NAMED list, the names being page base names ",
           "matching the 'file' values used when placing each page.")
    }
    orphans <- setdiff(nms, placed)
    if (length(orphans) > 0) {
      stop("sidecar(s) named '", paste(orphans, collapse = "', '"),
           "' are not placed anywhere in this map (placed: '",
           paste(placed, collapse = "', '"), "'). Check for a typo -- a sidecar ",
           "whose name does not match its placement is never loaded.")
    }
    for (nm in nms) {
      sp <- file.path(dir, paste0(nm, ".page.json"))
      saveJsonFile(sidecars[[nm]], sp)
      written <- c(written, sp)
    }
  }

  invisible(written)
}


#' Write a card placement list and its sidecars
#'
#' Writes the placement list to \code{cardsFile} and each sidecar to
#' \code{<name>.card.json} in the same directory. An optional \code{<name>.html}
#' shell is yours to author if you want one; a card renders from its template
#' without it.
#'
#' Unlike a page, a card's sidecar is not optional -- it carries the template and
#' fields that are the card's entire content.
#'
#' @param QCEcardPlacement A placement list from
#'   \code{\link{addCardToQCEcardPlacement}}.
#' @param cardsFile Path to write the placement list to, e.g. "cards1.json". Name
#'   it whatever you referenced from the group's \code{cards} argument.
#' @param sidecars A NAMED list of \code{\link{buildQCEcardSidecar}} results, the
#'   names being card base names.
#' @param dir Directory to write into. DEFAULT = "." (the working directory).
#'
#' @return Invisibly, a character vector of the paths written.
#' @keywords QCE cards card save write
#' @export
#' @examples
#' cards <- addCardToQCEcardPlacement(NULL, "progress")
#' saveQCEcardFiles(cards, "cards1.json",
#'                  sidecars = list(progress = buildQCEcardSidecar(
#'                    template = "<div>{deadlineRemaining}</div>", refreshMs = 1000)),
#'                  dir = tempdir())
saveQCEcardFiles <- function(QCEcardPlacement, cardsFile, sidecars, dir = ".") {

  if (missing(QCEcardPlacement) || !is.list(QCEcardPlacement) ||
      length(QCEcardPlacement) == 0) {
    stop("QCEcardPlacement option must be a non-empty placement list (output of ",
         "addCardToQCEcardPlacement).")
  }
  if (missing(cardsFile) || !isSingleString(cardsFile) || nchar(cardsFile) == 0) {
    stop("cardsFile option must be a single non-empty filename, e.g. 'cards1.json'.")
  }
  if (!isSingleString(dir)) {
    stop("dir option must be a single string naming a directory.")
  }

  placed <- unique(vapply(QCEcardPlacement, function(p) as.character(p$card), character(1)))

  if (missing(sidecars) || !is.list(sidecars) || length(sidecars) == 0) {
    stop("sidecars option is required: a card's sidecar carries its template and ",
         "fields, so a placed card with no sidecar would render empty.")
  }
  nms <- names(sidecars)
  if (is.null(nms) || any(nms == "")) {
    stop("sidecars option must be a NAMED list, the names being card base names ",
         "matching the 'card' values used when placing each card.")
  }
  missingSidecars <- setdiff(placed, nms)
  if (length(missingSidecars) > 0) {
    stop("card(s) '", paste(missingSidecars, collapse = "', '"),
         "' are placed but have no sidecar, so they would render empty.")
  }
  orphans <- setdiff(nms, placed)
  if (length(orphans) > 0) {
    stop("sidecar(s) named '", paste(orphans, collapse = "', '"),
         "' are not placed anywhere in this list (placed: '",
         paste(placed, collapse = "', '"), "'). Check for a typo.")
  }

  written <- character(0)
  path <- file.path(dir, cardsFile)
  saveJsonFile(QCEcardPlacement, path)
  written <- c(written, path)

  for (nm in nms) {
    sp <- file.path(dir, paste0(nm, ".card.json"))
    saveJsonFile(sidecars[[nm]], sp)
    written <- c(written, sp)
  }

  invisible(written)
}
