#' Add one scenario per item of a stimulus set to a QCEScenarioList
#'
#' Function that expands a stimulus-set reference from \code{buildQCEstimSetRef} into scenarios: one per selected item, all in the set named \code{setName}, each a copy of \code{QCEframeList} with the item written into every frame's stimulus. Two placeholders mark where: \code{\{\{stimulus\}\}} becomes the item itself -- an image, sound or video element for a file, the escaped text for a text item -- and \code{\{\{stimulusUrl\}\}} becomes the file's address alone, for a frame that writes its own element. At least one frame must carry a placeholder.
#'
#' Every scenario records \code{stimSet}, \code{stimSetVersion}, \code{stimId} and one \code{stim_<attribute>} column per declared attribute (empty when the item has no value) in the data. A file's address is the engine's stimulus endpoint, \code{stimFile.php?set=<set>&id=<id>}, relative to the page; the endpoint serves the file only to a running session of the study.
#' @param QCEScenarioList The QCEScenarioList to add to, or \code{NULL} to start a new one.
#' @param stimSetRef A reference from \code{buildQCEstimSetRef}.
#' @param QCEframeList The frames each scenario shows, from \code{addFrameToQCEframeList}, with a placeholder in at least one frame's stimulus.
#' @param QCEfeebackList A feedback list, from \code{createFeedbackList}, for every scenario.
#' @param setName A single string naming the set the scenarios belong to; pass it to \code{addSetToQCEsetInfoList}.
#' @param QCEoutvariableList A named list of further columns written for every scenario. Its names may not repeat the stimulus columns. Default \code{NULL}.
#' @param trigger Optional trial-level triggers from \code{buildQCETriggerList}, given to every scenario. Default \code{NULL}.
#'
#' @return the updated QCEScenarioList
#' @keywords QCE QCEScenarioList stimulus set scenario
#' @export
#' @examples
#' \dontrun{
#' faces <- buildQCEstimSetRef("faces", where = list(gender = "f"), n = 6)
#' fr <- addFrameToQCEframeList(trialType = "key", frameName = "face",
#'   stimulus = "<div>{{stimulus}}</div>", post_trial_gap = 250, choices = c("d", "k"))
#' scenarios <- addStimSetToQCEscenarioList(NULL, faces, fr, createFeedbackList(), "faceSet")
#' }

addStimSetToQCEscenarioList <- function(QCEScenarioList, stimSetRef, QCEframeList, QCEfeebackList, setName, QCEoutvariableList = NULL, trigger = NULL) {

  if (!is.list(stimSetRef) || is.null(stimSetRef$items) || is.null(stimSetRef$kind) ||
      is.null(stimSetRef$version) || !isSingleString(stimSetRef$stimSet)) {
    stop("stimSetRef must be a reference from buildQCEstimSetRef().")
  }
  if (!isSingleString(setName)) {
    stop("setName must be a single string.")
  }
  stims <- vapply(QCEframeList, function(fr) paste(fr$stimulus, collapse = ""), "")
  if (!any(grepl("{{stimulus}}", stims, fixed = TRUE) | grepl("{{stimulusUrl}}", stims, fixed = TRUE))) {
    stop("no frame's stimulus carries the placeholder {{stimulus}} or {{stimulusUrl}}, ",
         "so no item of stimulus set \"", stimSetRef$stimSet, "\" would be shown.")
  }
  isText <- identical(stimSetRef$kind, "text")
  if (isText && any(grepl("{{stimulusUrl}}", stims, fixed = TRUE))) {
    stop("{{stimulusUrl}} names a file, and \"", stimSetRef$stimSet, "\" is a text set; use {{stimulus}}.")
  }

  attrNames <- vapply(stimSetRef$attributes, function(a) a$name, "")
  stimCols <- c("stimSet", "stimSetVersion", "stimId", paste0("stim_", attrNames))
  clash <- intersect(names(QCEoutvariableList), stimCols)
  if (length(clash)) {
    stop("QCEoutvariableList repeats the stimulus column(s) ", paste(clash, collapse = ", "),
         ", which the set writes itself.")
  }

  escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    gsub("'", "&#39;", x, fixed = TRUE)
  }

  for (it in stimSetRef$items) {
    if (isText) {
      shown <- escape(it$text)
      url <- NULL
    } else {
      #the address sits inside an attribute, so its ampersand is escaped
      url <- paste0("stimFile.php?set=", stimSetRef$stimSet, "&amp;id=", it$id)
      family <- sub("/.*$", "", it$mediaType)
      shown <- switch(family,
                      image = paste0("<img src=\"", url, "\" alt=\"\">"),
                      audio = paste0("<audio src=\"", url, "\" autoplay></audio>"),
                      video = paste0("<video src=\"", url, "\" autoplay playsinline></video>"),
                      stop("item ", it$id, " has media type ", it$mediaType, ", which no frame can show."))
    }
    frames <- lapply(QCEframeList, function(fr) {
      if (length(fr$stimulus) == 1) {
        s <- gsub("{{stimulus}}", shown, fr$stimulus, fixed = TRUE)
        if (!is.null(url)) s <- gsub("{{stimulusUrl}}", url, s, fixed = TRUE)
        fr$stimulus <- s
      }
      fr
    })
    vals <- vapply(attrNames, function(a) {
      v <- it$attrs[[a]]
      if (is.null(v)) "" else as.character(v)
    }, "")
    attrCols <- as.list(vals)
    names(attrCols) <- paste0("stim_", attrNames)
    ov <- c(list(stimSet = stimSetRef$stimSet,
                 stimSetVersion = as.character(stimSetRef$version),
                 stimId = it$id),
            attrCols, QCEoutvariableList)
    QCEScenarioList <- addScenarioToQCEscenarioList(QCEScenarioList, frames, QCEfeebackList,
                                                    ov, setName, trigger = trigger)
  }

  return(QCEScenarioList)
}
