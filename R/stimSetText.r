#the words of a stimulus set as the platform shows them

#a placeholder the platform fills after the build
.stimToken <- "^\u27e6[a-z][a-z0-9_]{0,39}:[A-Za-z0-9_-]{1,40}(:[A-Za-z][A-Za-z0-9_]{0,39})?\u27e7$"

#escaped for html
.stimEscape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x <- gsub("\\", "&#92;", x, fixed = TRUE)
  #an item never takes the form of the platform's placeholder
  gsub("\u27e6", "&#10214;", x, fixed = TRUE)
}

#single-quoted, so the element stands inside a JSON string unchanged
.stimMark <- function(key, inner) {
  paste0("<span data-qcep-item='", key, "' dir='auto' style='white-space:pre-wrap'>", inner, "</span>")
}

#a survey shows its text as text, inside a string of its JSON model
.stimPlain <- function(x) {
  #a placeholder the platform fills after the build is written as it is
  if (grepl(.stimToken, x, perl = TRUE)) return(x)
  x <- as.character(jsonlite::toJSON(gsub("\r\n?", "\n", x), auto_unbox = TRUE))
  gsub("\u27e6", "\\u27e6", substr(x, 2, nchar(x) - 1), fixed = TRUE)
}

#line breaks and tabs become markup, so the page never holds a raw one
.stimRender <- function(x) {
  #a placeholder the platform fills after the build is written as it is
  if (grepl(.stimToken, x, perl = TRUE)) return(x)
  x <- .stimEscape(gsub("\r\n?", "\n", x))
  x <- gsub("\n", "<br>", x, fixed = TRUE)
  gsub("\t", "&#9;", x, fixed = TRUE)
}
