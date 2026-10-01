#the words of a stimulus set as the platform writes them

#a placeholder the platform fills after the build
.stimToken <- "^⟦[a-z][a-z0-9_]{0,39}:[A-Za-z0-9_-]{1,40}(:[A-Za-z][A-Za-z0-9_]{0,39})?⟧$"

#character references that stand anywhere in markup and inside a json string
.stimEscape <- function(x) {
  x <- gsub("\r\n?", "\n", x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x <- gsub("\\", "&#92;", x, fixed = TRUE)
  #an item never takes the form of the platform's placeholder
  x <- gsub("⟦", "&#10214;", x, fixed = TRUE)
  x <- gsub("\n", "&#10;", x, fixed = TRUE)
  gsub("\t", "&#9;", x, fixed = TRUE)
}

#a survey shows its text as text, inside a string of its JSON model
.stimPlain <- function(x) {
  #a placeholder the platform fills after the build is written as it is
  if (grepl(.stimToken, x, perl = TRUE)) return(x)
  x <- as.character(jsonlite::toJSON(gsub("\r\n?", "\n", x), auto_unbox = TRUE))
  gsub("⟦", "\\u27e6", substr(x, 2, nchar(x) - 1), fixed = TRUE)
}

#an attribute whose value is code: a handler, a style, a javascript: address
.stimCodeAttr <- function(place) {
  grepl("^on", place$attr) || place$attr == "style" || grepl("^\\s*javascript:", place$value, ignore.case = TRUE)
}

#text as it stands at a place in markup, or NULL where words are never written
.stimWordsIn <- function(place, x) {
  #a placeholder the platform fills after the build is written as it is
  if (grepl(.stimToken, x, perl = TRUE)) return(x)
  kind <- place$kind
  if (kind == "text") {
    #spacing and line breaks shown as typed, in the text's own direction
    return(paste0("<span dir='auto' style='white-space:pre-wrap'>", .stimEscape(x), "</span>"))
  }
  if (kind == "tag") {
    if (is.null(place$attr) || .stimCodeAttr(place)) return(NULL)
    return(gsub(" ", "&#32;", .stimEscape(x), fixed = TRUE))
  }
  if (kind == "inert" || (kind == "raw" && place$name %in% c("textarea", "title"))) return(.stimEscape(x))
  NULL
}

#where a place in markup is, for a message
.stimPlaceOf <- function(place) {
  if (place$kind == "raw") return(paste0("inside a <", place$name, ">"))
  if (place$kind == "comment") return("in an HTML comment")
  if (place$kind == "tag" && !is.null(place$attr)) return(paste0("inside the ", place$attr, " attribute, whose value is code"))
  if (place$kind == "tag") return("inside a tag, outside an attribute's value")
  "here"
}

#the parts of an html text as a browser reads them, each kind, start and end
.stimParts <- function(s) {
  n <- nchar(s)
  ch <- strsplit(s, "")[[1]]
  rawNames <- c("script", "style", "textarea", "title", "xmp", "iframe", "noembed", "noframes", "noscript", "plaintext")
  inertNames <- c("select", "datalist", "option", "optgroup", "svg", "math")
  depth <- stats::setNames(integer(length(inertNames)), inertNames)
  parts <- list()
  add <- function(kind, a, b, name = "") parts[[length(parts) + 1]] <<- list(kind = kind, start = a, end = b, name = name)
  textStart <- 1
  text <- function(b) if (b > textStart) add(if (any(depth > 0)) "inert" else "text", textStart, b)
  from <- function(pat, at, ...) {
    if (at > n) return(-1)
    m <- regexpr(pat, substr(s, at, n), ...)
    if (m < 0) -1 else at + m - 1
  }
  i <- 1
  while (i <= n) {
    lt <- from("<", i, fixed = TRUE)
    if (lt < 0) break
    if (substr(s, lt, lt + 3) == "<!--") {
      text(lt)
      close <- from("-->", lt + 4, fixed = TRUE)
      end <- if (close < 0) n + 1 else close + 3
      add("comment", lt, end)
      i <- textStart <- end
      next
    }
    nxt <- if (lt < n) ch[lt + 1] else ""
    if (nxt %in% c("!", "?")) {
      text(lt)
      close <- from(">", lt, fixed = TRUE)
      end <- if (close < 0) n + 1 else close + 1
      add("tag", lt, end)
      i <- textStart <- end
      next
    }
    isEnd <- nxt == "/"
    nameAt <- if (isEnd) lt + 2 else lt + 1
    #a < that starts no tag is text
    if (nameAt > n || !grepl("[A-Za-z]", ch[nameAt])) {
      i <- lt + 1
      next
    }
    text(lt)
    j <- nameAt
    quote <- ""
    last <- ""
    while (j <= n) {
      c1 <- ch[j]
      if (nzchar(quote)) {
        if (c1 == quote) {
          quote <- ""
          last <- c1
        }
      } else if (c1 %in% c("\"", "'") && last == "=") {
        quote <- c1
      } else if (c1 == ">") {
        break
      } else if (!grepl("\\s", c1)) {
        last <- c1
      }
      j <- j + 1
    }
    end <- if (j <= n) j + 1 else n + 1
    name <- tolower(regmatches(substr(s, nameAt, n), regexpr("^[A-Za-z][^\\s/>]*", substr(s, nameAt, n), perl = TRUE)))
    add("tag", lt, end)
    i <- textStart <- end
    if (isEnd) {
      if (name %in% inertNames && depth[[name]] > 0) depth[[name]] <- depth[[name]] - 1L
      #closing a select or a list closes the options it holds
      if (name %in% c("select", "datalist", "optgroup")) depth[["option"]] <- 0L
      if (name %in% c("select", "datalist")) depth[["optgroup"]] <- 0L
      next
    }
    if (name %in% inertNames && !(end - 2 >= 1 && ch[end - 2] == "/")) {
      #an option or group ends the option before it
      if (name %in% c("option", "optgroup")) depth[["option"]] <- 0L
      depth[[name]] <- depth[[name]] + 1L
    }
    if (name %in% rawNames) {
      close <- from(paste0("</", name, "(?=[\\s/>]|$)"), end, perl = TRUE, ignore.case = TRUE)
      rawEnd <- if (close < 0) n + 1 else close
      if (rawEnd > end) add("raw", end, rawEnd, name)
      i <- textStart <- rawEnd
    }
  }
  text(n + 1)
  parts
}

#where character position at stands: its part and, in a tag, the attribute
.stimPlaceAt <- function(s, parts, at) {
  for (p in parts) {
    if (at >= p$start && at < p$end) {
      if (p$kind != "tag") return(p)
      pre <- substr(s, p$start, at - 1)
      m <- regmatches(pre, regexec("([^\\s\"'<>/=]+)\\s*=\\s*(\"[^\"]*|'[^']*|[^\\s\"'>]*)$", pre, perl = TRUE))[[1]]
      if (length(m)) {
        p$attr <- tolower(m[2])
        p$value <- sub("^[\"']", "", m[3])
      }
      return(p)
    }
  }
  list(kind = "text", name = "")
}
