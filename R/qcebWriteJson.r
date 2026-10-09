#the one writer of every settings file a build produces

.qcebDescriptionCache <- new.env(parent = emptyenv())

#the settings description shipped in inst/
.qcebDescription <- function() {
  if (is.null(.qcebDescriptionCache$d)) {
    p <- system.file("settingsDescription.json", package = "QCEB")
    if (!nzchar(p)) {
      stop("QCEB: the settings description (settingsDescription.json) is missing from the package.")
    }
    .qcebDescriptionCache$d <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  }
  .qcebDescriptionCache$d
}

#the readable name of each settings file, for messages
.qcebFileLabels <- c(expInfo = "expInfo.json", expDBfile = "expDBfile.json",
                     groupSettings = "the group settings file",
                     trialStructure = "the trial-structure file",
                     stimulus = "the stimulus file", preload = "preloadFile.json",
                     pages = "the pages file", pageSidecar = "the page sidecar",
                     cards = "the cards file", cardSidecar = "the card sidecar",
                     localPluginManifest = "pluginManifest.local.json",
                     unplacedPages = "unplacedPages.json",
                     stimSetList = "the stimulus-set list")

#the value written for a key that is present but not set, by file and key pattern
.qcebNotSetWritten <- list(stimulus = c("*.frame.*.choices" = "NO_KEYS"))

#keys the engine shows whole, so several values are written and never stopped
.qcebNeverStopped <- list(stimulus = c("*.frame.*.stimulus", "*.feedback.feedback_key.*.displayText"))

.qcebAbsent <- structure(list(), class = "qcebAbsent")

.qcebIsAbsent <- function(x) inherits(x, "qcebAbsent")

#a list whose elements carry no names is an array
.qcebIsArrayList <- function(v) {
  is.list(v) && !is.data.frame(v) && (is.null(names(v)) || all(names(v) == ""))
}

.qcebIsObjectList <- function(v) {
  is.list(v) && !is.data.frame(v) && !is.null(names(v)) && any(names(v) != "")
}

#the shapes that say a setting was not given
.qcebNotSet <- function(v) {
  if (is.null(v) || .qcebIsAbsent(v)) return(TRUE)
  if (is.data.frame(v)) return(nrow(v) == 0)
  if (is.list(v)) {
    if (length(v) == 0) return(TRUE)
    return(.qcebIsArrayList(v) && length(v) == 1 && .qcebNotSet(v[[1]]))
  }
  if (!is.atomic(v)) return(FALSE)
  if (length(v) == 0) return(TRUE)
  length(v) == 1 && (is.na(v) || identical(as.character(v), ""))
}

#a plain vector, without I() and without factor levels
.qcebPlain <- function(v) {
  if (is.factor(v)) v <- as.character(v)
  if (inherits(v, "AsIs")) class(v) <- setdiff(class(v), "AsIs")
  v
}

#the rows of a data frame as a list of named lists
.qcebRows <- function(df) {
  lapply(seq_len(nrow(df)), function(i) {
    lapply(as.list(df[i, , drop = FALSE]), function(x) .qcebPlain(x))
  })
}

#a value written whole: one value bare, several a list, I() a list, NA null
.qcebWhole <- function(v) {
  if (is.null(v)) return(jsonlite::unbox(NA))
  if (is.data.frame(v)) v <- .qcebRows(v)
  if (is.list(v)) {
    out <- lapply(v, .qcebWhole)
    names(out) <- names(v)
    if (.qcebIsObjectList(v) && length(out) == 0) out <- structure(list(), names = character(0))
    return(out)
  }
  if (!is.atomic(v)) return(v)
  asis <- inherits(v, "AsIs")
  v <- .qcebPlain(v)
  if (length(v) == 1 && !asis) return(jsonlite::unbox(v))
  v
}

#a key's description entry with its shared piece filled in
.qcebResolve <- function(node, d) {
  if (is.null(node$ref)) return(node)
  piece <- d$pieces[[node$ref]]
  for (k in setdiff(names(node), "ref")) piece[[k]] <- node[[k]]
  piece
}

.qcebJoin <- function(base, k) if (identical(base, "")) as.character(k) else paste0(base, ".", k)

#the values a call was given, short enough for a message
.qcebShowValues <- function(v) {
  v <- if (is.list(v)) vapply(v, function(x) paste(format(unlist(x)), collapse = " "), character(1))
       else .qcebPlain(v)
  shown <- if (is.character(v)) paste0("\"", utils::head(v, 10), "\"") else format(utils::head(v, 10))
  paste0(paste(shown, collapse = ", "), if (length(v) > 10) ", ..." else "")
}

#stop for a one-value setting given several values, named by the call that set it
.qcebStopSeveral <- function(fn, what, v) {
  stop(sprintf("%s: %s is a setting that takes one value, but it was given %d: %s.",
               fn, what, length(v), .qcebShowValues(v)), call. = FALSE)
}

#stop when an argument for a one-value setting holds several values
#args is a named list of values, or the names of the caller's arguments
.qcebOneValueArgs <- function(fn, args, env = parent.frame()) {
  if (is.character(args)) {
    given <- args[!vapply(args, function(a) eval(call("missing", as.name(a)), env), logical(1))]
    args <- structure(lapply(given, function(a) get(a, envir = env)), names = given)
  }
  for (a in names(args)) {
    v <- args[[a]]
    if (is.null(v) || is.data.frame(v)) next
    if ((is.atomic(v) || .qcebIsArrayList(v)) && length(v) > 1) .qcebStopSeveral(fn, a, v)
  }
  invisible(TRUE)
}

#warn for each argument given a value that the engine does not use
.qcebUnusedArgs <- function(fn, given) {
  for (a in names(given)[unlist(given)]) {
    warning(sprintf("%s: %s is not used by the engine, so it is not written to the file.", fn, a),
            call. = FALSE)
  }
  invisible(TRUE)
}

#which of a key's described forms an R value takes
.qcebPickForm <- function(node, v, d) {
  forms <- lapply(node$anyOf, .qcebResolve, d = d)
  kinds <- vapply(forms, function(f) if (is.null(f$kind)) "" else f$kind, character(1))
  listForm <- which(kinds %in% c("list", "table"))
  if (is.data.frame(v)) v <- .qcebRows(v)
  if (.qcebIsArrayList(v)) {
    if (length(listForm) > 0) return(forms[[listForm[1]]])
    if (length(v) == 1) v <- v[[1]]
  }
  if (.qcebIsObjectList(v)) {
    best <- 0
    bestScore <- -1
    for (i in which(kinds == "group")) {
      score <- sum(names(v) %in% names(forms[[i]]$keys))
      if (score > bestScore) {
        best <- i
        bestScore <- score
      }
    }
    if (best > 0) return(forms[[best]])
    return(forms[[1]])
  }
  if (is.atomic(v)) {
    asis <- inherits(v, "AsIs")
    pv <- .qcebPlain(v)
    if ((asis || length(pv) > 1) && length(listForm) > 0) return(forms[[listForm[1]]])
    one <- which(kinds == "one")
    for (i in one) {
      if (!is.null(forms[[i]]$oneOf) && length(pv) == 1 &&
          as.character(pv) %in% as.character(unlist(forms[[i]]$oneOf))) return(forms[[i]])
    }
    for (i in one) {
      if (is.null(forms[[i]]$oneOf)) return(forms[[i]])
    }
    if (length(listForm) > 0) return(forms[[listForm[1]]])
    if (length(one) > 0) return(forms[[one[1]]])
  }
  forms[[1]]
}

#one value of a file, by its description entry
.qcebShape <- function(v, node, ctx) {
  node <- .qcebResolve(node, ctx$d)
  if (.qcebNotSet(v)) return(.qcebAbsent)
  if (!is.null(node$anyOf)) return(.qcebShape(v, .qcebPickForm(node, v, ctx$d), ctx))
  if (identical(node$kind, "byTrialType")) {
    tt <- .qcebPlain(unlist(ctx$parent$trialType))[1]
    form <- if (!is.null(tt) && !is.na(tt) && tt %in% names(node$byTrialType)) node$byTrialType[[tt]]
            else if (!is.null(node$otherTypes)) node$otherTypes
            else list(open = TRUE)
    return(.qcebShape(v, form, ctx))
  }
  if (!is.null(node$studyOwnUnwrap)) return(.qcebStudyOwnGroup(v, node, ctx))
  if (isTRUE(node$open) || isTRUE(node$data)) return(.qcebWhole(v))
  switch(if (is.null(node$kind)) "" else node$kind,
         one = .qcebOne(v, ctx),
         list = .qcebList(v, node, ctx),
         table = .qcebTable(v, node, ctx),
         group = .qcebGroup(v, node, ctx),
         .qcebWhole(v))
}

.qcebOne <- function(v, ctx) {
  if (is.data.frame(v)) return(.qcebWhole(v))
  if (.qcebIsArrayList(v)) {
    if (length(v) == 1) return(.qcebOne(v[[1]], ctx))
    if (ctx$strict) .qcebStopSeveral(ctx$fn, ctx$what, v)
    return(.qcebWhole(v))
  }
  if (is.list(v)) return(.qcebWhole(v))
  v <- .qcebPlain(v)
  if (length(v) > 1) {
    if (ctx$strict) .qcebStopSeveral(ctx$fn, ctx$what, v)
    return(v)
  }
  jsonlite::unbox(v)
}

#a list: one item is still a list
.qcebList <- function(v, node, ctx) {
  if (is.data.frame(v)) v <- .qcebRows(v)
  items <- if (is.list(v)) {
    if (.qcebIsArrayList(v)) v else list(v)
  } else {
    as.list(.qcebPlain(v))
  }
  out <- lapply(seq_along(items), function(i) {
    if (is.null(node$items)) return(.qcebWhole(items[[i]]))
    r <- .qcebShape(items[[i]], node$items, .qcebCtxInto(ctx, i - 1, "*", NULL))
    if (.qcebIsAbsent(r)) jsonlite::unbox(NA) else r
  })
  #a list of single values is written as one vector
  if (length(out) > 0 && all(vapply(out, function(x) inherits(x, "scalar"), logical(1)))) {
    flat <- unlist(lapply(out, unclass))
    if (!is.list(flat) && length(flat) == length(out)) return(flat)
  }
  out
}

#a table: one row is still a table
.qcebTable <- function(v, node, ctx) {
  if (is.data.frame(v)) v <- .qcebRows(v)
  if (!is.list(v)) {
    warning(sprintf("%s: '%s' in %s should be a list of rows, but it holds %s; it is written as given.",
                    ctx$fn, ctx$path, ctx$label, .qcebShowValues(v)), call. = FALSE)
    return(.qcebWhole(v))
  }
  rows <- if (.qcebIsArrayList(v)) v else list(v)
  row <- .qcebResolve(node$row, ctx$d)
  out <- lapply(seq_along(rows), function(i) {
    r <- rows[[i]]
    rctx <- .qcebCtxInto(ctx, i - 1, "*", NULL)
    if (.qcebIsArrayList(r) && length(r) == 1 && .qcebIsObjectList(r[[1]])) r <- r[[1]]
    if (is.null(row) || !.qcebIsObjectList(r)) return(.qcebWhole(r))
    if (identical(row$kind, "group") && is.null(row$anyOf)) return(.qcebGroupObject(r, row, rctx))
    s <- .qcebShape(r, row, rctx)
    if (.qcebIsAbsent(s)) .qcebWhole(r) else s
  })
  out
}

#a group of settings: an object inside a list of one is the object
.qcebGroup <- function(v, node, ctx) {
  if (.qcebIsArrayList(v) && length(v) == 1) v <- v[[1]]
  if (!.qcebIsObjectList(v)) {
    warning(sprintf("%s: '%s' in %s should be a named list of settings, but it holds %s; it is written as given.",
                    ctx$fn, ctx$path, ctx$label, .qcebShowValues(v)), call. = FALSE)
    return(.qcebWhole(v))
  }
  out <- .qcebGroupObject(v, node, ctx)
  if (length(out) == 0) .qcebAbsent else out
}

#the context for a value one level down
.qcebCtxInto <- function(ctx, k, patternKey, parent) {
  ctx$path <- .qcebJoin(ctx$path, k)
  ctx$pattern <- .qcebJoin(ctx$pattern, patternKey)
  ctx$what <- sprintf("'%s' in %s", ctx$path, ctx$label)
  ctx$parent <- parent
  ctx
}

#the description entry for key k of a group, or NULL when it is not listed
.qcebChild <- function(node, k) {
  if (!is.null(node$keys) && k %in% names(node$keys)) return(node$keys[[k]])
  if (!is.null(node$reservedKeys) && k %in% names(node$reservedKeys)) return(node$reservedKeys[[k]])
  if (!is.null(node$keysNamedBy) && !is.null(node$entry)) return(node$entry)
  o <- node$otherKeys
  if (!is.null(o) && (!is.null(o$kind) || !is.null(o$anyOf) || !is.null(o$ref))) return(o)
  NULL
}

.qcebGroupObject <- function(v, node, ctx) {
  out <- structure(list(), names = character(0))
  named <- !is.null(node$keysNamedBy) && !is.null(node$entry)
  for (k in names(v)) {
    x <- v[[k]]
    child <- .qcebChild(node, k)
    described <- k %in% names(node$keys) || k %in% names(node$reservedKeys)
    kctx <- .qcebCtxInto(ctx, k, if (named && !described) "*" else k, v)
    #a key the description does not list is the study's own
    if (is.null(child)) {
      if (!is.null(x)) out[[k]] <- .qcebWhole(x)
      next
    }
    resolved <- .qcebResolve(child, ctx$d)
    if (identical(resolved$qcebWrites, FALSE)) next
    if (kctx$pattern %in% .qcebNeverStopped[[ctx$file]]) kctx$strict <- FALSE
    r <- .qcebShape(x, child, kctx)
    notSet <- .qcebNotSetWritten[[ctx$file]]
    if (.qcebIsAbsent(r) && kctx$pattern %in% names(notSet)) {
      r <- jsonlite::unbox(notSet[[kctx$pattern]])
    }
    #an entry the study named is kept even when nothing in it is set
    if (.qcebIsAbsent(r) && named && !described && .qcebIsObjectList(x) &&
        identical(resolved$kind, "group") && is.null(resolved$anyOf)) {
      r <- .qcebGroupObject(x, resolved, kctx)
    }
    if (!.qcebIsAbsent(r)) out[[k]] <- r
  }
  out
}

#a group whose keys belong to a plugin or the study: described keys as described
.qcebStudyOwnGroup <- function(v, node, ctx) {
  if (.qcebIsArrayList(v) && length(v) == 1 && .qcebIsObjectList(v[[1]])) v <- v[[1]]
  if (!.qcebIsObjectList(v)) return(.qcebWhole(v))
  ctx$strict <- FALSE
  out <- structure(list(), names = character(0))
  for (k in names(v)) {
    if (!is.null(node$keys) && k %in% names(node$keys)) {
      r <- .qcebShape(v[[k]], node$keys[[k]], .qcebCtxInto(ctx, k, k, v))
      if (!.qcebIsAbsent(r)) out[[k]] <- r
    } else if (!is.null(v[[k]])) {
      out[[k]] <- .qcebWhole(v[[k]])
    }
  }
  out
}

#which settings file a list is, from its name or its shape
.qcebFileKind <- function(data, filename = NULL) {
  if (!is.null(filename)) {
    b <- basename(filename)
    byName <- c(expDBfile.json = "expDBfile", expInfo.json = "expInfo",
                preloadFile.json = "preload", pluginManifest.local.json = "localPluginManifest",
                unplacedPages.json = "unplacedPages")
    if (b %in% names(byName)) return(unname(byName[b]))
    if (grepl("\\.page\\.json$", b)) return("pageSidecar")
    if (grepl("\\.card\\.json$", b)) return("cardSidecar")
  }
  if (!is.list(data) || length(data) == 0) return(NA_character_)
  if (.qcebIsArrayList(data)) {
    if (all(vapply(data, function(r) is.list(r) && !is.null(r$card), logical(1)))) return("cards")
    return(NA_character_)
  }
  if (!is.null(data$placements)) return("pages")
  entries <- data[setdiff(names(data), "switchRules")]
  allHave <- function(keys) {
    length(entries) > 0 && all(vapply(entries, function(e) is.list(e) && any(keys %in% names(e)), logical(1)))
  }
  if (allHave("frame")) return("stimulus")
  if (allHave(c("setInfo", "blockIterator"))) return("trialStructure")
  if (allHave("sessions")) return("expInfo")
  d <- .qcebDescription()
  score <- function(kind) sum(names(data) %in% names(d$files[[kind]]$root$keys))
  s <- c(expDBfile = score("expDBfile"), groupSettings = score("groupSettings"),
         stimSetList = score("stimSetList"))
  if (max(s) == 0) return(NA_character_)
  names(s)[which.max(s)]
}

#a settings list in the form the description gives it, ready for toJSON
.qcebSettingsForm <- function(data, fileKind, fn = "saveJsonFile") {
  d <- .qcebDescription()
  if (is.na(fileKind) || is.null(d$files[[fileKind]])) {
    return(.qcebWhole(data))
  }
  label <- .qcebFileLabels[[fileKind]]
  ctx <- list(d = d, file = fileKind, path = "", pattern = "", fn = fn, label = label,
              what = label, strict = TRUE, parent = NULL)
  root <- .qcebResolve(d$files[[fileKind]]$root, d)
  out <- if (identical(root$kind, "table")) {
    r <- .qcebTable(data, root, ctx)
    if (.qcebIsAbsent(r)) list() else r
  } else if (.qcebIsObjectList(data)) {
    .qcebGroupObject(data, root, ctx)
  } else if (is.list(data) && length(data) == 0) {
    structure(list(), names = character(0))
  } else {
    .qcebWhole(data)
  }
  if (identical(fileKind, "expDBfile")) {
    out <- c(list(settingsForm = jsonlite::unbox(2L)), out[names(out) != "settingsForm"])
  }
  out
}

#the json text of a list already in its written form
.qcebToJson <- function(x, pretty = TRUE) {
  jsonlite::toJSON(x, auto_unbox = FALSE, digits = NA, null = "null", na = "null", pretty = pretty)
}

#write a settings list as json, in the form the description gives each key
.qcebWriteSettings <- function(data, filename, fileKind = NULL, fn = "saveJsonFile", prefix = NULL) {
  if (is.null(fileKind)) fileKind <- .qcebFileKind(data, filename)
  if (is.na(fileKind)) {
    warning(sprintf("%s: %s is not a settings file QCEB recognises, so every value in it is written as given: one value bare, several as a list.",
                    fn, filename), call. = FALSE)
  }
  jsonData <- .qcebToJson(.qcebSettingsForm(data, fileKind, fn))
  if (is.null(prefix)) {
    write(jsonData, filename, append = FALSE)
  } else {
    write(prefix, filename)
    write(jsonData, filename, append = TRUE)
  }
  jsonData
}
