#every described key, handed every shape an R value can take, written by the one writer
desc <- jsonlite::fromJSON(system.file("settingsDescription.json", package = "QCEB"),
                           simplifyVector = FALSE)

resolve <- function(node) {
  if (is.null(node$ref)) return(node)
  piece <- desc$pieces[[node$ref]]
  for (k in setdiff(names(node), "ref")) piece[[k]] <- node[[k]]
  piece
}

#every key of every file, as the steps that lead to it
keyPaths <- function() {
  out <- list()
  walk <- function(node, file, steps, refs) {
    if (!is.null(node$ref)) {
      if (node$ref %in% refs) return(invisible())
      refs <- c(refs, node$ref)
    }
    node <- resolve(node)
    if (length(steps) > 0) out[[length(out) + 1]] <<- list(file = file, steps = steps, node = node)
    for (k in names(node$keys)) walk(node$keys[[k]], file, c(steps, list(list(t = "key", k = k))), refs)
    for (k in names(node$reservedKeys)) walk(node$reservedKeys[[k]], file, c(steps, list(list(t = "key", k = k))), refs)
    if (!is.null(node$keysNamedBy) && !is.null(node$entry)) {
      walk(node$entry, file, c(steps, list(list(t = "named"))), refs)
    }
    if (!is.null(node$row)) walk(node$row, file, c(steps, list(list(t = "row"))), refs)
    if (!is.null(node$items)) walk(node$items, file, c(steps, list(list(t = "item"))), refs)
    for (i in seq_along(node$anyOf)) {
      f <- resolve(node$anyOf[[i]])
      if (identical(f$kind, "group")) walk(node$anyOf[[i]], file, c(steps, list(list(t = "form", f = f))), refs)
    }
    for (tt in names(node$byTrialType)) {
      f <- resolve(node$byTrialType[[tt]])
      if (!is.null(f$anyOf)) {
        for (i in seq_along(f$anyOf)) {
          g <- resolve(f$anyOf[[i]])
          if (identical(g$kind, "group") && !is.null(g$keys)) {
            walk(f$anyOf[[i]], file, c(steps, list(list(t = "tt", tt = tt), list(t = "form", f = g))), refs)
          }
        }
      }
    }
  }
  for (f in names(desc$files)) walk(desc$files[[f]]$root, f, list(), character(0))
  out
}

#two sample values of a described one-value entry
sampleValues <- function(node) {
  node <- resolve(node)
  if (!is.null(node$anyOf)) {
    one <- Filter(function(f) identical(resolve(f)$kind, "one"), node$anyOf)
    return(sampleValues(one[[1]]))
  }
  v <- if (is.null(node$value)) "any" else node$value
  switch(v,
         number = c(0.123456, 2),
         integer = c(3, 4),
         boolean = c(TRUE, FALSE),
         oneOf = {
           o <- unlist(node$oneOf)
           if (length(o) == 1) c(o, o) else o[1:2]
         },
         jsonText = c("{\"a\":1}", "{\"b\":2}"),
         character = c("d", "k"),
         keyName = c("d", "k"),
         c("a", "b"))
}

#an entry the description lists as a list of single values
isListOfOne <- function(node) {
  identical(node$kind, "list") && (is.null(node$items) || identical(resolve(node$items)$kind, "one"))
}

#a file holding value at the end of steps, with a form's other keys filled in
buildFile <- function(steps, value) {
  x <- value
  for (i in rev(seq_along(steps))) {
    s <- steps[[i]]
    if (s$t == "key") {
      x <- structure(list(x), names = s$k)
      #a stimulus takes its form from the frame's trial type
      if (i < length(steps) && steps[[i + 1]]$t == "tt") x <- c(list(trialType = steps[[i + 1]]$tt), x)
    } else if (s$t == "named") {
      x <- list(n1 = x)
    } else if (s$t == "row" || s$t == "item") {
      x <- list(x)
    } else if (s$t == "form") {
      for (k in rev(names(s$f$keys))) {
        kn <- resolve(s$f$keys[[k]])
        if (!(k %in% names(x)) && identical(kn$kind, "one")) x <- c(structure(list(sampleValues(kn)[1]), names = k), x)
      }
    }
  }
  x
}

#the written value at the end of steps, or a marker when it is absent
pick <- function(j, steps) {
  for (s in steps) {
    if (s$t == "key") {
      if (!is.list(j) || is.null(names(j)) || !(s$k %in% names(j))) return("<absent>")
      j <- j[[s$k]]
    } else if (s$t == "named") {
      if (!is.list(j) || !("n1" %in% names(j))) return("<absent>")
      j <- j[["n1"]]
    } else if (s$t == "row" || s$t == "item") {
      if (!is.list(j) || length(j) < 1) return("<absent>")
      j <- j[[1]]
    }
  }
  j
}

written <- function(file, data) {
  txt <- QCEB:::.qcebToJson(QCEB:::.qcebSettingsForm(data, file, fn = "table"))
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

pattern <- function(steps) {
  paste(vapply(steps, function(s) switch(s$t, key = s$k, named = "*", row = "*", item = "*", ""),
               character(1))[vapply(steps, function(s) s$t %in% c("key", "named", "row", "item"), logical(1))],
        collapse = ".")
}

test_that("every described key is written in its form for every shape R can hand it", {
  paths <- keyPaths()
  n <- 0
  for (p in paths) {
    node <- p$node
    if (!is.null(node$anyOf) && any(vapply(node$anyOf, function(f) identical(resolve(f)$kind, "list"), logical(1)))) next
    if (identical(p$steps[[length(p$steps)]]$t, "item")) next
    kind <- if (!is.null(node$kind)) node$kind else if (!is.null(node$anyOf)) "one" else NULL
    if (is.null(kind) || !(kind %in% c("one", "list"))) next
    if (kind == "list" && !isListOfOne(node)) next
    if (isTRUE(node$open) || isTRUE(node$data) || !is.null(node$studyOwnUnwrap)) next
    pat <- pattern(p$steps)
    if (identical(pat, "settingsForm")) next
    inStim <- grepl("(^|\\.)frame\\.\\*\\.stimulus($|\\.)", pat) || grepl("displayText$", pat) ||
              grepl("pluginParams", pat)
    strict <- !inStim
    dead <- identical(node$qcebWrites, FALSE)
    v <- sampleValues(if (kind == "list") node$items else node)
    if (kind == "list" && is.null(node$items)) v <- c("a", "b")
    shapes <- list(null = NULL, na = NA, empty = character(0), blank = "",
                   one = v[1], asis = I(v[1]), two = v, list = as.list(v))
    for (sh in names(shapes)) {
      label <- sprintf("%s: %s given %s", p$file, pat, sh)
      data <- buildFile(p$steps, shapes[[sh]])
      if (sh %in% c("two", "list") && kind == "one" && strict && !dead) {
        expect_error(written(p$file, data), "takes one value", info = label)
        n <- n + 1
        next
      }
      got <- pick(written(p$file, data), p$steps)
      expected <- if (dead || sh %in% c("null", "na", "empty", "blank")) {
        "<absent>"
      } else if (kind == "list" || sh %in% c("two", "list")) {
        if (sh %in% c("one", "asis")) list(v[[1]]) else as.list(v)
      } else {
        v[[1]]
      }
      expect_equal(got, expected, info = label)
      n <- n + 1
    }
  }
  #the table covers the description, not a sample of it
  expect_gt(n, 1500)
})

test_that("a frame's choices is a list of keys or one of the two words", {
  fr <- function(ch) list(n1 = list(frame = list(n1 = list(trialType = "key", choices = ch))))
  ch <- function(x) written("stimulus", fr(x))$n1$frame$n1$choices
  expect_identical(ch(NULL), "NO_KEYS")
  expect_identical(ch(character(0)), "NO_KEYS")
  expect_identical(ch(NA), "NO_KEYS")
  expect_identical(ch(""), "NO_KEYS")
  expect_identical(ch("d"), list("d"))
  expect_identical(ch(I("d")), list("d"))
  expect_identical(ch(c("d", "k")), list("d", "k"))
  expect_identical(ch(list("d", "k")), list("d", "k"))
  expect_identical(ch("NO_KEYS"), "NO_KEYS")
  expect_identical(ch("ALL_KEYS"), "ALL_KEYS")
  expect_identical(ch(I("ALL_KEYS")), list("ALL_KEYS"))
})

test_that("data and a study's own values are written whole", {
  sc <- list(n1 = list(set = "A",
                       outputVariables = list(none = NULL, na = NA, empty = character(0), blank = "",
                                              one = "x", asis = I("x"), two = c("x", "y"),
                                              lst = list("x", "y"), num = "0.123456"),
                       myOwn = c(1, 2), myOne = I(7), myNA = NA,
                       frame = list(n1 = list(trialType = "key",
                                              pluginParams = list(p = c("PA", "PB"), q = "x", r = I("y"))))))
  j <- written("stimulus", sc)$n1
  ov <- j$outputVariables
  expect_identical(ov$none, NULL)
  expect_true("none" %in% names(ov))
  expect_identical(ov$na, NULL)
  expect_true("na" %in% names(ov))
  expect_identical(ov$empty, list())
  expect_identical(ov$blank, "")
  expect_identical(ov$one, "x")
  expect_identical(ov$asis, list("x"))
  expect_identical(ov$two, list("x", "y"))
  expect_identical(ov$lst, list("x", "y"))
  expect_identical(ov$num, "0.123456")
  expect_identical(j$myOwn, list(1L, 2L))
  expect_identical(j$myOne, list(7L))
  expect_true("myNA" %in% names(j))
  pp <- j$frame$n1$pluginParams
  expect_identical(pp$p, list("PA", "PB"))
  expect_identical(pp$q, "x")
  expect_identical(pp$r, list("y"))
})

test_that("numbers are written at full precision", {
  db <- list(condName = "c", restEveryNMinutes = 0.123456789, sessionNumber = "1")
  txt <- QCEB:::.qcebToJson(QCEB:::.qcebSettingsForm(db, "groupSettings"))
  expect_true(grepl("0.123456789", txt, fixed = TRUE))
})

test_that("the experiment settings file carries settingsForm 2 first", {
  j <- written("expDBfile", list(expName = "x"))
  expect_identical(names(j)[1], "settingsForm")
  expect_identical(j$settingsForm, 2L)
})

test_that("the writer's stop names the function, the key and the values", {
  expect_error(written("expDBfile", list(expName = c("a", "b"))),
               "table: 'expName' in expDBfile.json is a setting that takes one value, but it was given 2: \"a\", \"b\"",
               fixed = TRUE)
})
