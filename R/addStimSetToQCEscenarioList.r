#' Add one scenario per item of a stimulus set to a QCEScenarioList
#'
#' Function that expands a stimulus-set reference from \code{buildQCEstimSetRef} into scenarios: one per selected item, all in the set named \code{setName}, each a copy of \code{QCEframeList} with the item written into every frame's stimulus. Three placeholders mark where: \code{\{\{stimulus\}\}} becomes the item itself -- an image, sound or video element for a file, and for a text item its text -- \code{\{\{stimulusUrl\}\}} becomes the file's address alone, for a frame that writes its own element, and \code{\{\{stimulus:<attribute>\}\}} becomes the item's value of that attribute, a number or level as the data column writes it. A text item or text value is written escaped for where it stands in the frame's markup: in its text inside \code{<span dir='auto' style='white-space:pre-wrap'>}, so its spacing and line breaks show as typed in its own script's direction; in a tag's attribute, a \code{<textarea>} or \code{<title>}, an option or SVG text as character references alone (a space in an attribute as \code{&#32;}). Every character that could end or escape a JSON string is a character reference (a line break \code{&#10;}, a tab \code{&#9;}, a backslash \code{&#92;}), so the frame stays valid inside a JSON string. A placeholder in a frame's script, style or HTML comment is refused: read an item in a hook from the trial's data. A survey frame (\code{addSurveyFrameToQCEframeList}) shows its text as text, so there a text item or value is written as plain words escaped for the string of the survey's JSON model it stands in, the placeholder's opening bracket written as its JSON escape so an item never takes a placeholder's form. At least one frame must carry a placeholder.
#'
#' Every scenario records \code{stimSet}, \code{stimSetVersion}, \code{stimId} and one \code{stim_<attribute>} column per declared attribute (empty when the item has no value) in the data; a number is written in fixed notation with up to 15 significant digits, never in scientific notation. An attribute value holding a tab, line break, other control character or Unicode line or paragraph separator is refused, since it would split a row of the data file; a text item may hold line breaks and tabs, and any other control character or separator is refused. A file's address is the engine's stimulus endpoint, \code{stimFile.php?set=<set>&id=<id>&v=<version>}, relative to the page; the endpoint serves the file only to a running session of the study, and the version in the address keeps a browser from showing a cached file of an earlier version.
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
  attrPattern <- "\\{\\{stimulus:([A-Za-z][A-Za-z0-9_]*)\\}\\}"
  if (!any(grepl("{{stimulus}}", stims, fixed = TRUE) | grepl("{{stimulusUrl}}", stims, fixed = TRUE) |
           grepl(attrPattern, stims))) {
    stop("no frame's stimulus carries the placeholder {{stimulus}}, {{stimulusUrl}} or {{stimulus:<attribute>}}, ",
         "so no item of stimulus set \"", stimSetRef$stimSet, "\" would be shown.")
  }
  isText <- identical(stimSetRef$kind, "text")
  if (isText && any(grepl("{{stimulusUrl}}", stims, fixed = TRUE))) {
    stop("{{stimulusUrl}} names a file, and \"", stimSetRef$stimSet, "\" is a text set; use {{stimulus}}.")
  }

  attrNames <- vapply(stimSetRef$attributes, function(a) a$name, "")
  attrTypes <- vapply(stimSetRef$attributes, function(a) a$type, "")
  #the attributes a frame shows through {{stimulus:<attribute>}}
  shownAttrs <- unique(unlist(lapply(regmatches(stims, gregexpr(attrPattern, stims)),
                                     function(m) sub("^\\{\\{stimulus:(.*)\\}\\}$", "\\1", m))))
  unknown <- setdiff(shownAttrs, attrNames)
  if (length(unknown)) {
    stop("a frame shows {{stimulus:", unknown[1], "}}, but stimulus set \"", stimSetRef$stimSet,
         "\" has no attribute \"", unknown[1], "\"; its attributes are ",
         if (length(attrNames)) paste(attrNames, collapse = ", ") else "none", ".")
  }
  #paste0 turns an empty vector into "stim_"
  attrCols <- if (length(attrNames)) paste0("stim_", attrNames) else character(0)
  stimCols <- c("stimSet", "stimSetVersion", "stimId", attrCols)
  clash <- intersect(names(QCEoutvariableList), stimCols)
  if (length(clash)) {
    stop("QCEoutvariableList repeats the stimulus column(s) ", paste(clash, collapse = ", "),
         ", which the set writes itself.")
  }

  #each placeholder of each frame, with where it stands in the frame's markup
  slotPattern <- "\\{\\{stimulus(Url|:[A-Za-z][A-Za-z0-9_]*)?\\}\\}"
  slots <- lapply(QCEframeList, function(fr) {
    if (length(fr$stimulus) != 1) return(NULL)
    s <- fr$stimulus
    m <- gregexpr(slotPattern, s)[[1]]
    if (m[1] < 0) return(NULL)
    survey <- identical(fr$trialType, "survey")
    parts <- if (survey) NULL else .stimParts(s)
    lapply(seq_along(m), function(k) {
      at <- m[k]
      tok <- substr(s, at, at + attr(m, "match.length")[k] - 1)
      key <- sub("^\\{\\{stimulus:?(.*)\\}\\}$", "\\1", tok)
      p <- if (survey) list(kind = "survey", name = "") else .stimPartAt(parts, at)
      words <- (key == "" && isText) || (!key %in% c("", "Url") && identical(attrTypes[match(key, attrNames)], "text"))
      if (words && is.null(.stimWordsIn(p$kind, p$name, "x")) && p$kind != "survey") {
        stop("frame \"", fr$frameName, "\" has ", tok, " ", .stimPlaceOf(p$kind, p$name),
             ", where a set's words are never written. Show an item in a frame's text, a tag's attribute, ",
             "a textarea, an option or SVG text, and read it in a hook from the trial's data (stim_<attribute>).")
      }
      list(start = at, end = at + nchar(tok), key = key, kind = p$kind, name = p$name)
    })
  })
  #an item's text, or a value, as it stands in a slot
  fill <- function(sl, it, vals, shown, url) {
    if (sl$key == "Url") return(if (is.null(url)) paste0("{{stimulusUrl}}") else url)
    if (sl$key == "") {
      if (!isText) return(shown)
      return(if (sl$kind == "survey") .stimPlain(it$text) else .stimWordsIn(sl$kind, sl$name, it$text))
    }
    v <- vals[[match(sl$key, attrNames)]]
    if (sl$kind == "survey") return(.stimPlain(v))
    if (identical(attrTypes[[match(sl$key, attrNames)]], "text")) return(.stimWordsIn(sl$kind, sl$name, v))
    .stimEscape(v)
  }

  #a tab or newline in a value splits a row of the data file
  control <- "[\\p{Cc}\\p{Zl}\\p{Zp}]"
  #a text item is markup, where only its line breaks and tabs may stand
  textControl <- "[\\p{Zl}\\p{Zp}]|(?![\\t\\n\\r])\\p{Cc}"
  for (it in stimSetRef$items) {
    for (a in attrNames) {
      v <- it$attrs[[a]]
      if (is.character(v) && any(grepl(control, enc2utf8(v), perl = TRUE))) {
        stop("item ", it$id, ": \"", a, "\" holds a tab, line break or other control character; ",
             "a value is one line of plain text.")
      }
    }
    if (isText && any(grepl(textControl, enc2utf8(it$text), perl = TRUE))) {
      stop("item ", it$id, ": the text holds a control character other than a line break or a tab.")
    }
    if (isText) {
      shown <- NULL
      url <- NULL
    } else {
      #the address sits inside an attribute, so its ampersand is escaped
      url <- paste0("stimFile.php?set=", stimSetRef$stimSet, "&amp;id=", it$id,
                    "&amp;v=", stimSetRef$version)
      family <- sub("/.*$", "", it$mediaType)
      shown <- switch(family,
                      image = paste0("<img src=\"", url, "\" alt=\"\">"),
                      audio = paste0("<audio src=\"", url, "\" autoplay></audio>"),
                      video = paste0("<video src=\"", url, "\" autoplay playsinline></video>"),
                      stop("item ", it$id, " has media type ", it$mediaType, ", which no frame can show."))
    }
    vals <- vapply(attrNames, function(a) {
      v <- it$attrs[[a]]
      if (is.null(v)) return("")
      #fixed notation, so a column never holds 1e-04
      if (is.numeric(v)) format(v, digits = 15, scientific = FALSE, trim = TRUE) else as.character(v)
    }, "")
    frames <- lapply(seq_along(QCEframeList), function(k) {
      fr <- QCEframeList[[k]]
      if (is.null(slots[[k]])) return(fr)
      s <- fr$stimulus
      out <- ""
      at <- 1
      for (sl in slots[[k]]) {
        out <- paste0(out, substr(s, at, sl$start - 1), fill(sl, it, vals, shown, url))
        at <- sl$end
      }
      fr$stimulus <- paste0(out, substr(s, at, nchar(s)))
      fr
    })
    names(frames) <- names(QCEframeList)
    attrVals <- as.list(unname(vals))
    names(attrVals) <- attrCols
    ov <- c(list(stimSet = stimSetRef$stimSet,
                 stimSetVersion = as.character(stimSetRef$version),
                 stimId = it$id),
            attrVals, QCEoutvariableList)
    QCEScenarioList <- addScenarioToQCEscenarioList(QCEScenarioList, frames, QCEfeebackList,
                                                    ov, setName, trigger = trigger)
  }

  return(QCEScenarioList)
}
