#' Build one SurveyJS question (any type)
#'
#' Schema-agnostic builder for a single SurveyJS question. Every SurveyJS
#' question of every type is structurally \code{{type, name, title, ...props}},
#' so this one function expresses ALL of them -- current and future -- by
#' passing arbitrary SurveyJS properties through \code{...}. There is no
#' per-type function to fall out of date.
#'
#' The result is validated SOFTLY against \code{\link{surveyTypeCatalog}}: a
#' typo'd type, or a known type missing a required property (e.g. a
#' \code{radiogroup} with no \code{choices}), emits a \code{warning} but is
#' still returned and serialized. QCEB never blocks a question on type grounds;
#' the authoritative checks are the engine's \code{validateTrialTypes} (is the
#' survey plugin loaded?) and SurveyJS's own in-browser schema validation.
#'
#' @param type A non-empty single string -- the SurveyJS question type
#'   (e.g. "radiogroup", "rating", "matrix", "checkbox", "text", "html"). See
#'   \code{names(surveyTypeCatalog())} for the curated set; any other valid
#'   SurveyJS type also works.
#' @param name A single string -- the question's name. This becomes the data
#'   column (or column prefix, for matrix/multipletext) in the saved data.
#'   Required for input questions; optional for display-only types
#'   (html/image/expression). DEFAULT = NULL.
#' @param title A single string -- the question text shown to the participant.
#'   DEFAULT = NULL (SurveyJS falls back to showing the name).
#' @param isRequired A single boolean. When TRUE, SurveyJS forces an answer
#'   before the page can be submitted. Emitted only when TRUE. DEFAULT = FALSE.
#' @param ... Any further SurveyJS properties for this question, passed through
#'   verbatim: \code{choices}, \code{columns}, \code{rows}, \code{rateMin},
#'   \code{rateMax}, \code{rateValues}, \code{inputType}, \code{html},
#'   \code{isAllRowRequired}, etc. Collection properties (choices/columns/rows/
#'   rateValues/items/...) are guaranteed to serialize as JSON arrays even when
#'   length 1 -- see \code{\link{addSurveyFrameToQCEframeList}}.
#'
#' @return A named list representing one SurveyJS question (an "element").
#' @keywords QCE survey question SurveyJS
#' @export
#' @examples
#' # A 1-7 rating with anchored endpoints:
#' surveyQuestion("rating", name = "mvs1",
#'   title = "How desirable a partner are you?",
#'   isRequired = TRUE, rateMin = 1, rateMax = 7,
#'   minRateDescription = "Not at all", maxRateDescription = "Extremely")
#'
#' # A single-select list:
#' surveyQuestion("radiogroup", name = "sex", title = "Your sex:",
#'   choices = c("Female", "Male", "Other"))
#'
#' # A matrix (rows/columns are passed straight through):
#' surveyQuestion("matrix", name = "RSE", title = "How much do you agree?",
#'   isAllRowRequired = TRUE,
#'   columns = list(list(value = "A", text = "Agree"),
#'                  list(value = "D", text = "Disagree")),
#'   rows = list(list(value = "rse1", text = "I am satisfied with myself.")))
surveyQuestion <- function(type, name = NULL, title = NULL, isRequired = FALSE, ...) {
  if (!isSingleString(type) || nchar(type) == 0) {
    stop("surveyQuestion: type must be a non-empty single string ",
         "(a SurveyJS question type, e.g. 'radiogroup', 'rating', 'matrix').")
  }
  q <- list(type = type)
  if (!is.null(name))  q$name  <- name
  if (!is.null(title)) q$title <- title
  if (!is.null(isRequired) && length(isRequired) == 1 && !is.na(isRequired) && isRequired) {
    q$isRequired <- TRUE
  }
  extra <- list(...)
  if (length(extra) > 0) {
    if (is.null(names(extra)) || any(names(extra) == "")) {
      stop("surveyQuestion: every extra SurveyJS property passed via ... must be ",
           "named (e.g. choices = ..., rateMax = 7).")
    }
    q <- modifyList(q, extra)
  }
  .validateSurveyQuestion(q)
  q
}

#' Build one SurveyJS page from questions
#'
#' Groups questions into a SurveyJS page. A model can have one or many pages;
#' QCEP presents the whole model as a single survey trial.
#'
#' @param name A single string -- the page name (appears in SurveyJS internals;
#'   not usually shown to participants).
#' @param ... One or more question lists from \code{\link{surveyQuestion}} (or
#'   hand-built question lists). These become the page's \code{elements}, in
#'   order.
#' @param pageProps An optional named list of page-level SurveyJS properties to
#'   merge in (e.g. \code{list(title = "Part 1", description = "...")}). Kept
#'   separate from \code{...} so question objects and page settings never
#'   collide. DEFAULT = NULL.
#'
#' @return A named list representing one SurveyJS page.
#' @keywords QCE survey page SurveyJS
#' @export
#' @examples
#' q1 <- surveyQuestion("text", "age", "Your age:")
#' q2 <- surveyQuestion("radiogroup", "sex", "Your sex:", choices = c("F", "M"))
#' surveyPage("demographics", q1, q2, pageProps = list(title = "About you"))
surveyPage <- function(name, ..., pageProps = NULL) {
  if (!isSingleString(name) || nchar(name) == 0) {
    stop("surveyPage: name must be a non-empty single string.")
  }
  elements <- list(...)
  if (length(elements) == 0) {
    stop("surveyPage('", name, "'): a page must contain at least one question. ",
         "Pass questions built with surveyQuestion() via ...")
  }
  page <- list(name = name, elements = elements)
  if (!is.null(pageProps)) {
    if (!is.list(pageProps) || (length(pageProps) > 0 && is.null(names(pageProps)))) {
      stop("surveyPage: pageProps must be a named list of SurveyJS page properties.")
    }
    page <- modifyList(page, pageProps)
  }
  page
}

#' Build a complete SurveyJS model from pages
#'
#' Assembles one or more pages into a top-level SurveyJS model -- the object
#' that \code{\link{addSurveyFrameToQCEframeList}} serializes into a survey
#' frame's stimulus.
#'
#' @param ... One or more page lists from \code{\link{surveyPage}}. These become
#'   the model's \code{pages}, in order. (You may also pass a single page; a
#'   one-page model is the common case.)
#' @param showQuestionNumbers SurveyJS numbering mode: "off", "on", or
#'   "onPage". DEFAULT = "off".
#' @param modelProps An optional named list of model-level SurveyJS properties
#'   to merge in (e.g. \code{list(showProgressBar = "top", title = "...")}).
#'   DEFAULT = NULL.
#'
#' @return A named list representing a complete SurveyJS model.
#' @keywords QCE survey model SurveyJS
#' @export
#' @examples
#' p <- surveyPage("p1", surveyQuestion("text", "age", "Your age:"))
#' surveyModel(p)
#' surveyModel(p, showQuestionNumbers = "on",
#'             modelProps = list(showProgressBar = "top"))
surveyModel <- function(..., showQuestionNumbers = "off", modelProps = NULL) {
  pages <- list(...)
  if (length(pages) == 0) {
    stop("surveyModel: a model must contain at least one page. ",
         "Pass pages built with surveyPage() via ...")
  }
  model <- list(showQuestionNumbers = showQuestionNumbers, pages = pages)
  if (!is.null(modelProps)) {
    if (!is.list(modelProps) || (length(modelProps) > 0 && is.null(names(modelProps)))) {
      stop("surveyModel: modelProps must be a named list of SurveyJS model properties.")
    }
    model <- modifyList(model, modelProps)
  }
  model
}

# --- serialization ----------------------------------------------------------
# Properties whose VALUE must always serialize as a JSON array, even when it
# holds a single element. jsonlite::toJSON(auto_unbox = TRUE) collapses a
# length-1 atomic vector to a scalar -- which would turn a single-choice list
# (choices = "Yes") into "Yes" and break SurveyJS. We coerce these to R lists
# before serializing so they always render as arrays. The set is intentionally
# broad and applies at ANY depth (a matrixdropdown column can carry its own
# choices). Genuine scalars (title, name, rateMax, ...) are untouched and unbox
# normally.
.surveyArrayKeys <- c("pages", "elements", "choices", "columns", "rows",
                      "rateValues", "items", "templateElements", "cells",
                      "validators", "triggers", "calculatedValues")

# Recursively walk a model, coercing atomic vectors found under an array-key
# into lists so auto_unbox keeps them as arrays. Lists (objects / already-arrays)
# are recursed into and left as lists.
.coerceSurveyArrays <- function(x, keyName = NULL) {
  if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms)) nms <- rep("", length(x))
    for (i in seq_along(x)) {
      x[[i]] <- .coerceSurveyArrays(x[[i]], nms[i])
    }
    return(x)
  }
  if (!is.null(keyName) && nzchar(keyName) && keyName %in% .surveyArrayKeys &&
      is.atomic(x) && !is.null(x) && length(x) >= 1) {
    # length-1 atomic -> list(of 1) -> JSON array of 1 after auto_unbox.
    return(as.list(x))
  }
  x
}

# Internal (not exported): serialize a survey model list to a JSON STRING,
# array-forcing collection keys. This string is what the survey frame's
# `stimulus` carries (shape (A) in surveyTrialType.js -- a pre-serialized JSON
# string, the unambiguous form).
.serializeSurveyModel <- function(model) {
  as.character(jsonlite::toJSON(.coerceSurveyArrays(model), auto_unbox = TRUE))
}
