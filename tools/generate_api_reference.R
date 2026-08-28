#!/usr/bin/env Rscript

# Regenerate the generated API section of BUILDER_REFERENCE.md from man/*.Rd.
#
# The reference document is authored by hand EXCEPT the block between the
# markers below, which this script rewrites from the package's Rd files so the
# API listing can never drift from the documented source. Run from the package
# root (or pass the package root as the first argument) after any change that
# regenerates man/:
#
#   Rscript tools/generate_api_reference.R [package_root]
#
# Only functions exported in NAMESPACE are listed. Deprecated functions
# (name ending in Dep/OldDep/_7, or a title announcing deprecation) are
# quarantined in a terminal "do not use" list rather than omitted, so a reader
# meeting one in an old script can identify it and find the replacement.

BEGIN_MARKER <- "<!-- BEGIN GENERATED API — do not edit by hand; run tools/generate_api_reference.R -->"
END_MARKER   <- "<!-- END GENERATED API -->"

args <- commandArgs(trailingOnly = TRUE)
pkg_root <- if (length(args) >= 1) args[[1]] else "."
man_dir  <- file.path(pkg_root, "man")
ns_file  <- file.path(pkg_root, "NAMESPACE")
out_file <- file.path(pkg_root, "BUILDER_REFERENCE.md")
desc_file <- file.path(pkg_root, "DESCRIPTION")

if (!dir.exists(man_dir)) stop("man/ not found under: ", pkg_root)
if (!file.exists(ns_file)) stop("NAMESPACE not found under: ", pkg_root)
if (!file.exists(out_file)) {
  stop("BUILDER_REFERENCE.md not found under: ", pkg_root,
       " — this script only fills the generated section of an existing file.")
}

## ---- exported names ---------------------------------------------------------

ns_lines <- readLines(ns_file, warn = FALSE)
exports <- sub("^export\\((.*)\\)$", "\\1",
               grep("^export\\(", ns_lines, value = TRUE))

## ---- Rd flattening ----------------------------------------------------------

# Flatten an Rd fragment to markdown-ish plain text. Handles the tags roxygen2
# emits; unknown tags degrade to their flattened contents rather than erroring.
rd_flatten <- function(x) {
  if (is.character(x)) return(paste(x, collapse = ""))
  tag <- attr(x, "Rd_tag")
  kids <- function() paste(vapply(x, rd_flatten, character(1)), collapse = "")
  if (is.null(tag)) return(kids())
  switch(tag,
    "TEXT" = , "RCODE" = , "VERB" = kids(),
    "\\code" = , "\\env" = , "\\option" = , "\\samp" = , "\\verb" =
      paste0("`", kids(), "`"),
    "\\emph" = paste0("*", kids(), "*"),
    "\\strong" = , "\\bold" = paste0("**", kids(), "**"),
    "\\link" = kids(),
    "\\href" = {
      # first element is the URL, second the label
      if (length(x) >= 2) rd_flatten(x[[2]]) else kids()
    },
    "\\url" = kids(),
    "\\dontrun" = , "\\donttest" = kids(),
    "\\itemize" = , "\\enumerate" = kids(),
    # An \item carrying two blocks is a \describe pair — \item{name}{desc} —
    # and must keep its name separated from its description. An \item with
    # any other arity is a plain list bullet. The two-element form occurs in
    # \arguments (handled separately, below) and in \describe blocks anywhere
    # in an Rd file; without the separator the name runs straight into the
    # description and the pair reads as one mangled word.
    "\\item" = if (length(x) == 2) {
      item_name <- tidy(rd_flatten(x[[1]]))
      if (!grepl("^`", item_name)) item_name <- paste0("`", item_name, "`")
      paste0("\n- ", item_name, " — ", rd_flatten(x[[2]]))
    } else {
      paste0("\n- ", kids())
    },
    "\\describe" = kids(),
    "\\dQuote" = , "\\sQuote" = paste0("\"", kids(), "\""),
    "\\eqn" = , "\\deqn" = if (length(x) >= 1) rd_flatten(x[[1]]) else "",
    "COMMENT" = "",
    kids()
  )
}

# Collapse runs of blank space that flattening leaves behind.
tidy <- function(s) {
  s <- gsub("[ \t]+", " ", s)
  s <- gsub("\n ", "\n", s, fixed = TRUE)
  s <- gsub("\n{3,}", "\n\n", s)
  trimws(s)
}

rd_section <- function(rd, tag) {
  hits <- Filter(function(el) identical(attr(el, "Rd_tag"), tag), rd)
  if (length(hits) == 0) return(NULL)
  hits[[1]]
}

parse_one <- function(path) {
  rd <- tools::parse_Rd(path)
  name_el <- rd_section(rd, "\\name")
  if (is.null(name_el)) return(NULL)
  name <- tidy(rd_flatten(name_el))
  title <- tidy(rd_flatten(rd_section(rd, "\\title")))
  usage_el <- rd_section(rd, "\\usage")
  usage <- if (is.null(usage_el)) "" else trimws(paste(
    vapply(usage_el, function(el) {
      if (is.character(el)) paste(el, collapse = "") else rd_flatten(el)
    }, character(1)), collapse = ""))
  args_el <- rd_section(rd, "\\arguments")
  arg_lines <- character(0)
  if (!is.null(args_el)) {
    for (el in args_el) {
      if (identical(attr(el, "Rd_tag"), "\\item") && length(el) == 2) {
        arg_name <- tidy(rd_flatten(el[[1]]))
        arg_desc <- tidy(rd_flatten(el[[2]]))
        arg_desc <- gsub("\n", " ", arg_desc)
        arg_lines <- c(arg_lines, sprintf("- `%s` — %s", arg_name, arg_desc))
      }
    }
  }
  value <- rd_section(rd, "\\value")
  value_txt <- if (is.null(value)) NULL else tidy(rd_flatten(value))
  details <- rd_section(rd, "\\details")
  details_txt <- if (is.null(details)) NULL else tidy(rd_flatten(details))
  desc <- rd_section(rd, "\\description")
  desc_txt <- if (is.null(desc)) NULL else tidy(rd_flatten(desc))
  list(name = name, title = title, usage = usage, args = arg_lines,
       value = value_txt, details = details_txt, description = desc_txt)
}

## ---- categorization ---------------------------------------------------------

# Curated map from function-name pattern to reference section, in the order a
# build script uses them. Anything unmatched lands in "Other exported
# functions" so a new builder cannot silently vanish from the reference.
categories <- list(
  list(title = "Stimfile — scenarios and frames",
       pattern = "^(addFrameToQCEframeList|addFixationToQCEframeList|addSurveyFrameToQCEframeList|addScenarioToQCEscenarioList|getSetnamesFromScenarioList)$"),
  list(title = "Survey models",
       pattern = "^(surveyModel|surveyPage|surveyQuestion|surveyTypeCatalog|surveyUniversalProperties)$"),
  list(title = "Tsfile — blocks, sets, and trial order",
       pattern = "^(addBlockToQCETrialStructureList|addSetToQCEsetInfoList|createBlockIteratorList)$"),
  list(title = "Conditional display and dynamic rules (showIf, switch rules)",
       pattern = "^(buildQCEshowIfCondition|buildQCEshowIfCompound|buildQCEstateCondition|buildQCEblockSwitchedCondition|buildQCEswitchRule|buildQCEswitchThreshold|buildQCEblockSwitchRule|addBlockSwitchRulesToQCETrialStructureList)$"),
  list(title = "Key maps and response keys",
       pattern = "^(buildKeyMap|addKeyToKeyMap|buildQCEkeyMapEntry|addKeyMapToDbfile|getKeyChoicesFromKeyMap|reverseTwoChoiceFeedbackKey)$"),
  list(title = "Feedback",
       pattern = "^(createFeedbackList|addKeyToFeedbackKeyList|buildSpeedFeedbackList)$"),
  list(title = "Dbfiles — experiment- and group-level settings",
       pattern = "^(buildQCEexpDbFile|buildQCEgroupDbFile|addHooksToQCEgroupDbFile|buildQCETriggerList)$"),
  list(title = "Groups, sessions, and expInfo",
       pattern = "^(addSessionToSessionList|addSessionListToQCEGroupList)$"),
  list(title = "Pages and cards",
       pattern = "^(buildQCEpageField|buildQCEpageSidecar|addPageToQCEpagePlacement|saveQCEpageFiles|buildQCEcardField|buildQCEcardSidecar|addCardToQCEcardPlacement|saveQCEcardFiles)$"),
  list(title = "Trial-type registry",
       pattern = "^(registerQCEBtrialType|getRegisteredQCEBtrialTypes|isRegisteredQCEBtrialType)$"),
  list(title = "Output fields and data manifest",
       pattern = "^(createQCEoutputVariableList|buildQCEoutputFieldManifest|missingQCEoutputFields|buildQCEhookRowManifest|promotedQCEhookRows)$"),
  list(title = "Writing and reading the config files",
       pattern = "^(saveDbFile|saveStimFile|saveTSFile|saveJsonFile|readQCEjsonFile|readQCEBjsonFileToList|savePreloadFiles|savePreloadImages|saveCustomHooksTemplate)$"),
  list(title = "Utilities",
       pattern = "^(QCEanchor|isColor|isSingleNumeric|isSingleString|isValidFilename)$")
)

is_deprecated <- function(fn) {
  if (grepl("(Dep|OldDep|_7)$", fn$name)) return(TRUE)
  isTRUE(grepl("deprecat", fn$title, ignore.case = TRUE))
}

## ---- build the section ------------------------------------------------------

rd_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
parsed <- Filter(Negate(is.null), lapply(rd_files, parse_one))
parsed <- Filter(function(fn) fn$name %in% exports, parsed)

dep  <- Filter(is_deprecated, parsed)
live <- Filter(Negate(is_deprecated), parsed)

render_fn <- function(fn) {
  out <- c(sprintf("### `%s`", fn$name), "", fn$title, "")
  if (nzchar(fn$usage)) out <- c(out, "```r", fn$usage, "```", "")
  if (!is.null(fn$description) && !identical(tidy(fn$description), tidy(fn$title))) {
    out <- c(out, fn$description, "")
  }
  if (length(fn$args) > 0) out <- c(out, fn$args, "")
  if (!is.null(fn$details)) out <- c(out, "**Details.** ", fn$details, "")
  if (!is.null(fn$value)) out <- c(out, paste0("**Returns.** ", gsub("\n", " ", fn$value)), "")
  out
}

section <- c(
  sprintf("*Generated from `man/` on %s — %d exported functions (%d current, %d deprecated).*",
          format(Sys.Date()), length(parsed), length(live), length(dep)),
  ""
)

used <- character(0)
for (cat in categories) {
  members <- Filter(function(fn) grepl(cat$pattern, fn$name), live)
  if (length(members) == 0) next
  members <- members[order(vapply(members, `[[`, character(1), "name"))]
  section <- c(section, sprintf("## %s", cat$title), "")
  for (fn in members) section <- c(section, render_fn(fn))
  used <- c(used, vapply(members, `[[`, character(1), "name"))
}

other <- Filter(function(fn) !(fn$name %in% used), live)
if (length(other) > 0) {
  other <- other[order(vapply(other, `[[`, character(1), "name"))]
  section <- c(section, "## Other exported functions", "")
  for (fn in other) section <- c(section, render_fn(fn))
}

if (length(dep) > 0) {
  dep <- dep[order(vapply(dep, `[[`, character(1), "name"))]
  section <- c(section,
    "## Deprecated — do not use in new code", "",
    "These remain exported for backward compatibility with existing build",
    "scripts. New code must not call them; each one's documentation names its",
    "replacement.", "")
  for (fn in dep) {
    section <- c(section, sprintf("- `%s` — %s", fn$name, gsub("\n", " ", fn$title)))
  }
  section <- c(section, "")
}

## ---- splice between the markers --------------------------------------------

doc <- readLines(out_file, warn = FALSE)
begin_at <- which(doc == BEGIN_MARKER)
end_at   <- which(doc == END_MARKER)
if (length(begin_at) != 1 || length(end_at) != 1 || end_at <= begin_at) {
  stop("BUILDER_REFERENCE.md must contain exactly one BEGIN/END generated-API marker pair.")
}

new_doc <- c(doc[1:begin_at], "", section, doc[end_at:length(doc)])
writeLines(new_doc, out_file)
cat(sprintf("Wrote %s: %d current + %d deprecated exported functions.\n",
            out_file, length(live), length(dep)))
