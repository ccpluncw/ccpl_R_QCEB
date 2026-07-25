#' Build the sidecar that describes one persistent card
#'
#' A card's sidecar (\code{<card>.card.json}) says what the card IS -- its markup,
#' the values it shows, how often it re-renders -- while the placement list built
#' by \code{\link{addCardToQCEcardPlacement}} says where it sits and between which
#' anchors it lives.
#'
#' The \code{template} is HTML with \code{\{fieldName\}} placeholders. Each
#' placeholder is replaced at every tick by the matching entry in \code{fields}.
#' Two names resolve without being declared: \code{\{deadlineRemaining\}}, the
#' whole seconds left on a per-trial deadline, published by any plugin that runs
#' one; and any key a custom hook writes to the card view.
#'
#' Templates are treated as trusted researcher-authored content and are not
#' HTML-escaped, so a placeholder can carry markup.
#'
#' @param template A single string of HTML with \code{\{fieldName\}} placeholders.
#'   DEFAULT = NULL (the card renders whatever \code{renderFn} produces).
#' @param fields A NAMED list of \code{\link{buildQCEcardField}} results. The
#'   names are what the template's placeholders refer to. NULL for a card that
#'   shows only a countdown or hook-written state. DEFAULT = NULL.
#' @param refreshMs How often the card re-renders, in milliseconds. A countdown
#'   needs 1000 to tick once a second; a card showing only running totals can
#'   refresh far less often, or rely on the re-render each trial triggers.
#'   DEFAULT = NULL (engine default).
#' @param position A list describing where the card sits, e.g.
#'   \code{list(region = "top-right")}. A placement may override this. DEFAULT =
#'   NULL.
#' @param widgets Optional list of extra display widgets, passed through to the
#'   card engine. DEFAULT = NULL.
#' @param renderFn A single string naming a global JS function that renders the
#'   card, for cards whose display is too dynamic for a template. DEFAULT = NULL.
#'
#' @return A list: the card sidecar, ready for \code{\link{saveQCEcardFiles}}.
#' @keywords QCE cards card sidecar template
#' @export
#' @examples
#' progress <- buildQCEcardSidecar(
#'   template = paste0("<div><b>Progress</b><br>Answered: {answered}",
#'                     "<br>Time left: {deadlineRemaining}</div>"),
#'   fields = list(
#'     answered = buildQCEcardField(
#'       formula = list(fn = "count", column = "correct",
#'                      where = list(respType = "mcKeys")),
#'       emptyValue = 0)
#'   ),
#'   refreshMs = 1000)
buildQCEcardSidecar <- function(template = NULL, fields = NULL, refreshMs = NULL,
                                position = NULL, widgets = NULL, renderFn = NULL) {

  if (!is.null(template) && (!isSingleString(template) || nchar(template) == 0)) {
    stop("template option must be a single non-empty HTML string, or NULL.")
  }

  if (!is.null(renderFn) && (!isSingleString(renderFn) || nchar(renderFn) == 0)) {
    stop("renderFn option must be a single non-empty string naming a global JS function, or NULL.")
  }

  if (is.null(template) && is.null(renderFn) && is.null(widgets)) {
    stop("A card needs something to draw: give a 'template', a 'renderFn', or 'widgets'.")
  }

  if (!is.null(refreshMs)) {
    if (!isSingleNumeric(refreshMs) || !is.finite(refreshMs) || refreshMs <= 0) {
      stop("refreshMs option must be a single positive number of milliseconds, or NULL.")
    }
  }

  if (!is.null(position) && !is.list(position)) {
    stop("position option must be a list, e.g. list(region = 'top-right'), or NULL.")
  }

  if (!is.null(fields)) {
    # A NAMED list, so it serializes as a JSON object keyed by field name -- that
    # is what the template's {placeholders} resolve against. This is the one place
    # cards and pages differ structurally: page fields are an unnamed array.
    if (!is.list(fields) || length(fields) == 0) {
      stop("fields option must be a non-empty NAMED list of buildQCEcardField() results, or NULL.")
    }
    nms <- names(fields)
    if (is.null(nms) || any(nms == "") || anyDuplicated(nms)) {
      stop("fields option must be a NAMED list with unique names -- the names are what ",
           "the template's {placeholders} refer to. Use list(answered = ..., score = ...).")
    }
    for (nm in nms) {
      f <- fields[[nm]]
      if (!is.list(f) || (is.null(f$formula) && is.null(f$bind))) {
        stop(sprintf(paste0("fields$%s does not look like a buildQCEcardField() result ",
                            "(no 'formula' or 'bind') -- did you forget to wrap it?"), nm))
      }
    }

    # A placeholder with no field behind it renders empty at run time, which is a
    # silent hole in the display rather than an error. Caught here instead.
    if (!is.null(template)) {
      used <- regmatches(template, gregexpr("\\{[^{}[:space:]]+\\}", template))[[1]]
      used <- gsub("^\\{|\\}$", "", used)
      # deadlineRemaining is injected by the card engine; hook-written keys cannot
      # be known here, so only warn rather than fail.
      unknown <- setdiff(used, c(nms, "deadlineRemaining"))
      if (length(unknown) > 0) {
        warning("card template refers to {", paste(unknown, collapse = "}, {"),
                "} which no field declares. If a custom hook writes ",
                if (length(unknown) == 1) "it" else "them",
                " to the card view this is fine; otherwise ",
                if (length(unknown) == 1) "it" else "they",
                " will render blank.")
      }
    }
  }

  tmpList <- list()
  if (!is.null(template))  tmpList$template  <- template
  if (!is.null(fields))    tmpList$fields    <- fields
  if (!is.null(refreshMs)) tmpList$refreshMs <- refreshMs
  if (!is.null(position))  tmpList$position  <- position
  if (!is.null(widgets))   tmpList$widgets   <- widgets
  if (!is.null(renderFn))  tmpList$renderFn  <- renderFn

  return(tmpList)
}
