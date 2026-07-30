# Tests for the pages + cards builders, their writers, the group wiring, and the
# output-field manifest scanner.

# --- page fields -------------------------------------------------------------

test_that("buildQCEpageField emits input/type/required and omits optional keys", {
    f <- buildQCEpageField("age")
    expect_equal(f$input, "age")
    expect_equal(f$type, "text")
    expect_false(f$required)
    expect_null(f$as)
    expect_null(f$emptyValue)
})

test_that("buildQCEpageField carries as/emptyValue when given", {
    f <- buildQCEpageField("birth_year", type = "number", as = "Birth",
                           required = TRUE, emptyValue = 0)
    expect_equal(f$as, "Birth")
    expect_equal(f$emptyValue, 0)
    expect_true(f$required)
})

test_that("buildQCEpageField carries requiredMessage only when given", {
    expect_null(buildQCEpageField("age", required = TRUE)$requiredMessage)
    f <- buildQCEpageField("age", required = TRUE, requiredMessage = "Set your age.")
    expect_equal(f$requiredMessage, "Set your age.")
})

test_that("buildQCEpageField rejects a bad requiredMessage and warns when it cannot fire", {
    expect_error(buildQCEpageField("age", required = TRUE, requiredMessage = ""),
                 "non-empty string")
    expect_error(buildQCEpageField("age", required = TRUE, requiredMessage = c("a", "b")),
                 "non-empty string")
    # A message on an optional field is dead config, not an error.
    expect_warning(buildQCEpageField("age", required = FALSE, requiredMessage = "hi"),
                   "never be shown")
})

test_that("buildQCEpageField rejects bad input, type, and required", {
    expect_error(buildQCEpageField(""), "non-empty string")
    expect_error(buildQCEpageField(c("a", "b")), "non-empty string")
    expect_error(buildQCEpageField("x", type = "slider"), "type option must be one of")
    expect_error(buildQCEpageField("x", required = NA), "TRUE or FALSE")
    expect_error(buildQCEpageField("x", as = ""), "non-empty string")
})

# --- page sidecar ------------------------------------------------------------

test_that("buildQCEpageSidecar defaults to global scope and no fields", {
    s <- buildQCEpageSidecar()
    expect_equal(s$dataScope, "global")
    expect_null(s$fields)
    expect_null(s$contBtn)
})

test_that("page sidecar fields must be an UNNAMED list (JSON array)", {
    f <- buildQCEpageField("a")
    expect_error(buildQCEpageSidecar(fields = list(one = f)), "UNNAMED list")
    expect_silent(buildQCEpageSidecar(fields = list(f)))
})

test_that("page sidecar rejects two fields writing the same column", {
    expect_error(
        buildQCEpageSidecar(fields = list(
            buildQCEpageField("a", as = "Age"),
            buildQCEpageField("b", as = "Age"))),
        "already writes to")
    # the same collision via the input fallback
    expect_error(
        buildQCEpageSidecar(fields = list(
            buildQCEpageField("Age"),
            buildQCEpageField("b", as = "Age"))),
        "already writes to")
})

test_that("page sidecar rejects unwrapped junk and bad dataScope", {
    expect_error(buildQCEpageSidecar(fields = list(list(nope = 1))), "did you forget to wrap")
    expect_error(buildQCEpageSidecar(dataScope = "row-ish"), "dataScope option must be one of")
})

test_that("page sidecar fields serialize as a JSON array", {
    s <- buildQCEpageSidecar(fields = list(buildQCEpageField("a")))
    js <- as.character(jsonlite::toJSON(s))
    expect_true(grepl('"fields":\\[', js))
})

# --- page placement ----------------------------------------------------------

test_that("addPageToQCEpagePlacement keys by anchor and preserves order", {
    p <- addPageToQCEpagePlacement(NULL, "sessionStart", "consent")
    p <- addPageToQCEpagePlacement(p, "sessionStart", "demographics")
    p <- addPageToQCEpagePlacement(p, "sessionEnd", "debrief")
    expect_equal(names(p), c("sessionStart", "sessionEnd"))
    expect_length(p$sessionStart, 2)
    expect_equal(p$sessionStart[[1]]$file, "consent")
    expect_equal(p$sessionStart[[2]]$file, "demographics")
    expect_false(p$sessionStart[[1]]$playOnce)
})

test_that("a single page at an anchor still serializes as an array", {
    p <- addPageToQCEpagePlacement(NULL, "sessionEnd", "debrief")
    js <- as.character(jsonlite::toJSON(p))
    expect_true(grepl('"sessionEnd":\\[\\{', js))
})

test_that("page placement accepts the full anchor vocabulary", {
    for (a in c("experimentStart", "sessionStart", "sessionEnd",
                "entry(block:practice)", "exit(block:practice)",
                "entry(set:s1)", "exit(set:s1)")) {
        expect_silent(addPageToQCEpagePlacement(NULL, a, "pg"))
    }
})

test_that("experimentStart and sessionStart are distinct anchors", {
    p <- addPageToQCEpagePlacement(NULL, "experimentStart", "consent")
    p <- addPageToQCEpagePlacement(p, "sessionStart", "reminder")
    expect_named(p, c("experimentStart", "sessionStart"))
    expect_equal(p$experimentStart[[1]]$file, "consent")
    expect_equal(p$sessionStart[[1]]$file, "reminder")
})

test_that("cards accept experimentStart as a mount/unmount point", {
    expect_silent(addCardToQCEcardPlacement(NULL, "scoreCard",
                                            mount = "experimentStart",
                                            unmount = "sessionEnd"))
})

test_that("page placement rejects unknown anchors and extensioned filenames", {
    expect_error(addPageToQCEpagePlacement(NULL, "blockStart", "pg"), "anchor option must be")
    expect_error(addPageToQCEpagePlacement(NULL, "entry(trial:1)", "pg"), "anchor option must be")
    expect_error(addPageToQCEpagePlacement(NULL, "sessionStart", "consent.html"), "NOT carry an extension")
    expect_error(addPageToQCEpagePlacement(NULL, "sessionStart", ""), "non-empty string")
})

# --- card fields -------------------------------------------------------------

test_that("buildQCEcardField requires exactly one source", {
    expect_error(buildQCEcardField(), "either a 'formula'")
    expect_error(buildQCEcardField(formula = list(fn = "count", column = "x"), bind = "y"),
                 "not both")
})

test_that("card field accepts a formula and a bind", {
    a <- buildQCEcardField(formula = list(fn = "count", column = "correct"), emptyValue = 0)
    expect_equal(a$formula$fn, "count")
    expect_equal(a$emptyValue, 0)
    b <- buildQCEcardField(bind = "partnerName")
    expect_equal(b$bind, "partnerName")
    expect_null(b$formula)
})

test_that("card field rejects gate-only op/value", {
    expect_error(
        buildQCEcardField(formula = list(fn = "proportion", column = "correct",
                                         op = ">=", value = 0.75)),
        "takes no 'op' or 'value'")
})

test_that("card field validates fn, column, and where via the shared checks", {
    expect_error(buildQCEcardField(formula = list(fn = "average", column = "x")), "invalid fn")
    expect_error(buildQCEcardField(formula = list(fn = "count", column = "")), "non-empty string")
    expect_error(
        buildQCEcardField(formula = list(fn = "count", column = "x",
                                         where = list(respType = list(op = "~=", value = 1)))),
        "invalid op")
    expect_error(
        buildQCEcardField(formula = list(fn = "count", column = "x",
                                         where = list(Trial = list(op = ">", value = "abc")))),
        "must be a single finite number")
    expect_silent(
        buildQCEcardField(formula = list(fn = "count", column = "x",
                                         where = list(respType = "mcKeys"))))
})

test_that("card field validates digits", {
    expect_error(buildQCEcardField(bind = "x", digits = -1), "non-negative whole number")
    expect_error(buildQCEcardField(bind = "x", digits = 1.5), "non-negative whole number")
    expect_equal(buildQCEcardField(bind = "x", digits = 2)$digits, 2)
})

# --- card sidecar ------------------------------------------------------------

test_that("card sidecar needs something to draw", {
    expect_error(buildQCEcardSidecar(), "needs something to draw")
})

test_that("card sidecar fields must be a NAMED list (JSON object)", {
    f <- buildQCEcardField(bind = "x")
    expect_error(buildQCEcardSidecar(template = "{x}", fields = list(f)), "NAMED list")
    expect_silent(buildQCEcardSidecar(template = "{x}", fields = list(x = f)))
})

test_that("card sidecar fields serialize as a JSON object keyed by name", {
    s <- buildQCEcardSidecar(template = "{x}", fields = list(x = buildQCEcardField(bind = "x")))
    js <- as.character(jsonlite::toJSON(s))
    expect_true(grepl('"fields":\\{"x":', js))
})

test_that("card sidecar warns about a placeholder no field declares", {
    expect_warning(
        buildQCEcardSidecar(template = "<div>{answered} {mystery}</div>",
                            fields = list(answered = buildQCEcardField(bind = "a"))),
        "which no field declares")
})

test_that("deadlineRemaining needs no declaration and raises no warning", {
    expect_silent(
        buildQCEcardSidecar(template = "<div>{deadlineRemaining} {answered}</div>",
                            fields = list(answered = buildQCEcardField(bind = "a"))))
})

test_that("card sidecar validates refreshMs and position", {
    expect_error(buildQCEcardSidecar(template = "x", refreshMs = 0), "positive number")
    expect_error(buildQCEcardSidecar(template = "x", refreshMs = -5), "positive number")
    expect_error(buildQCEcardSidecar(template = "x", position = "top-right"), "must be a list")
    expect_equal(buildQCEcardSidecar(template = "x", refreshMs = 1000)$refreshMs, 1000)
})

# --- card placement ----------------------------------------------------------

test_that("addCardToQCEcardPlacement builds an unnamed list with defaults", {
    c1 <- addCardToQCEcardPlacement(NULL, "progress")
    expect_null(names(c1))
    expect_length(c1, 1)
    expect_equal(c1[[1]]$card, "progress")
    expect_equal(c1[[1]]$mount, "sessionStart")
    expect_equal(c1[[1]]$unmount, "sessionEnd")
})

test_that("card placement serializes as a JSON array", {
    c1 <- addCardToQCEcardPlacement(NULL, "progress", position = list(region = "top-right"))
    js <- as.character(jsonlite::toJSON(c1))
    expect_true(startsWith(js, "[{"))
    expect_true(grepl('"region":\\["top-right"\\]', js))
})

test_that("card placement refuses identical mount and unmount", {
    expect_error(
        addCardToQCEcardPlacement(NULL, "progress", mount = "sessionEnd", unmount = "sessionEnd"),
        "never be seen")
})

test_that("card placement rejects bad anchors and extensioned names", {
    expect_error(addCardToQCEcardPlacement(NULL, "progress", mount = "blockStart"), "mount option must be")
    expect_error(addCardToQCEcardPlacement(NULL, "progress", unmount = "nope"), "unmount option must be")
    expect_error(addCardToQCEcardPlacement(NULL, "progress.card.json"), "NOT carry an extension")
})

test_that("several cards accumulate in order", {
    cs <- addCardToQCEcardPlacement(NULL, "progress")
    cs <- addCardToQCEcardPlacement(cs, "tally", mount = "entry(block:test)",
                                    unmount = "exit(block:test)")
    expect_length(cs, 2)
    expect_equal(cs[[2]]$card, "tally")
    expect_equal(cs[[2]]$mount, "entry(block:test)")
})

# --- writers -----------------------------------------------------------------

test_that("saveQCEpageFiles writes the map and its sidecars", {
    d <- file.path(tempdir(), "pgtest"); dir.create(d, showWarnings = FALSE)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    p <- addPageToQCEpagePlacement(NULL, "sessionStart", "consent")
    saveQCEpageFiles(p, "pagesA.json",
                     sidecars = list(consent = buildQCEpageSidecar(contBtn = "agreeBtn")),
                     dir = d)
    expect_true(file.exists(file.path(d, "pagesA.json")))
    expect_true(file.exists(file.path(d, "consent.page.json")))
    back <- jsonlite::fromJSON(file.path(d, "consent.page.json"))
    expect_equal(back$contBtn, "agreeBtn")
})

test_that("saveQCEpageFiles catches a sidecar whose name matches no placement", {
    d <- file.path(tempdir(), "pgtest2"); dir.create(d, showWarnings = FALSE)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    p <- addPageToQCEpagePlacement(NULL, "sessionStart", "consent")
    expect_error(
        saveQCEpageFiles(p, "pagesA.json",
                         sidecars = list(concent = buildQCEpageSidecar(contBtn = "x")),
                         dir = d),
        "not placed anywhere")
})

test_that("saveQCEcardFiles requires a sidecar for every placed card", {
    d <- file.path(tempdir(), "cdtest"); dir.create(d, showWarnings = FALSE)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    cs <- addCardToQCEcardPlacement(NULL, "progress")
    expect_error(saveQCEcardFiles(cs, "cards1.json",
                                  sidecars = list(other = buildQCEcardSidecar(template = "x")),
                                  dir = d),
                 "placed but have no sidecar")
    saveQCEcardFiles(cs, "cards1.json",
                     sidecars = list(progress = buildQCEcardSidecar(template = "{deadlineRemaining}",
                                                                    refreshMs = 1000)),
                     dir = d)
    expect_true(file.exists(file.path(d, "cards1.json")))
    expect_true(file.exists(file.path(d, "progress.card.json")))
})

# --- group wiring ------------------------------------------------------------

test_that("group list omits pages/cards keys when not given (byte-identical)", {
    g <- addSessionListToQCEGroupList(NULL, list(a = 1), "grp")
    expect_equal(names(g[[1]]), c("sessions", "groupName"))
})

test_that("group list carries pages and cards when given", {
    g <- addSessionListToQCEGroupList(NULL, list(a = 1), "grp",
                                      pages = "pagesA.json", cards = "cards1.json")
    expect_equal(g[[1]]$pages, "pagesA.json")
    expect_equal(g[[1]]$cards, "cards1.json")
})

test_that("group list validates pages/cards filenames", {
    expect_error(addSessionListToQCEGroupList(NULL, list(a = 1), "g", pages = ""), "pages option")
    expect_error(addSessionListToQCEGroupList(NULL, list(a = 1), "g", cards = c("a", "b")), "cards option")
})

# --- registry outputColumns --------------------------------------------------

test_that("core types declare the columns their extractors write", {
    expect_equal(QCEB:::.qcebTrialTypeOutputColumns("mcKeys"),
                 c("qid", "shownOrder", "pressedKey", "selectedLabel",
                   "selectedValue", "correctValue", "correct", "timedOut"))
    expect_equal(QCEB:::.qcebTrialTypeOutputColumns("key"), c("Key", "FeedBack", "Response"))
    # survey derives its columns from question names, so it declares none
    expect_null(QCEB:::.qcebTrialTypeOutputColumns("survey"))
    expect_null(QCEB:::.qcebTrialTypeOutputColumns("nosuchtype"))
})

# --- the manifest scanner ----------------------------------------------------

makeTestExp <- function(d, withHooks = FALSE) {
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    stim <- list(
        s1 = list(frame = list(`1` = list(trialType = "mcKeys", frameName = "q")),
                  outputVariables = list(myVar = "v"),
                  feedback = list(feedback_key = "fbKey")),
        s2 = list(frame = list(`1` = list(trialType = "key", frameName = "read")))
    )
    jsonlite::write_json(stim, file.path(d, "e_stimfile.json"), auto_unbox = FALSE)
    db <- list(welcomeMsg = "hi")
    if (withHooks) db$customHooksFile <- "myHooks.js"
    jsonlite::write_json(db, file.path(d, "e_dbfile.json"), auto_unbox = FALSE)
    p <- buildQCEpageSidecar(fields = list(
        buildQCEpageField("birth_year", type = "number", as = "Birth", required = TRUE)))
    jsonlite::write_json(p, file.path(d, "intake.page.json"), auto_unbox = FALSE)
    d
}

test_that("the manifest finds trial types, outputVariables, feedback keys and page columns", {
    d <- makeTestExp(file.path(tempdir(), "mftest"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    out <- buildQCEoutputFieldManifest(d, outFile = NULL)
    txt <- paste(out, collapse = "\n")
    expect_true(grepl("this experiment uses \\(key, mcKeys\\)", txt))
    expect_true("selectedValue" %in% out)   # from the mcKeys registry entry
    expect_true("Response" %in% out)        # from the key registry entry
    expect_true("myVar" %in% out)           # outputVariables
    expect_true("fbKey" %in% out)           # feedback key
    expect_true("Birth" %in% out)           # page field's `as` column
    expect_true(grepl("WHITELIST", txt))
})

test_that("the manifest writes a file by default and returns the lines", {
    d <- makeTestExp(file.path(tempdir(), "mftest2"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    out <- buildQCEoutputFieldManifest(d)
    expect_true(file.exists(file.path(d, "output_fields_manifest.txt")))
    expect_true(length(out) > 10)
})

test_that("the manifest flags custom hooks loudly and stays quiet without them", {
    d1 <- makeTestExp(file.path(tempdir(), "mfhooks"), withHooks = TRUE)
    d2 <- makeTestExp(file.path(tempdir(), "mfnohooks"), withHooks = FALSE)
    on.exit({unlink(d1, recursive = TRUE); unlink(d2, recursive = TRUE)}, add = TRUE)
    t1 <- paste(buildQCEoutputFieldManifest(d1, outFile = NULL), collapse = "\n")
    t2 <- paste(buildQCEoutputFieldManifest(d2, outFile = NULL), collapse = "\n")
    expect_true(grepl("CHECK THIS!", t1))
    expect_true(grepl("myHooks.js", t1))
    expect_false(grepl("CHECK THIS!", t2))
})

test_that("the manifest reports an undeclared trial type rather than dropping it", {
    d <- makeTestExp(file.path(tempdir(), "mfundecl"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    registerQCEBtrialType("mysteryPlugin", requiresKeymap = FALSE)
    stim <- list(s1 = list(frame = list(`1` = list(trialType = "mysteryPlugin"))))
    jsonlite::write_json(stim, file.path(d, "e_stimfile.json"), auto_unbox = FALSE)
    txt <- paste(buildQCEoutputFieldManifest(d, outFile = NULL), collapse = "\n")
    expect_true(grepl("\\[mysteryPlugin\\] declares no outputColumns", txt))
})

test_that("the manifest diff names columns missing from fields.txt", {
    d <- makeTestExp(file.path(tempdir(), "mfdiff"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    ff <- file.path(d, "fields.txt")
    writeLines(c("Exp_Name", "sn", "Trial"), ff)   # deliberately short
    txt <- paste(buildQCEoutputFieldManifest(d, outFile = NULL, fieldsFile = ff), collapse = "\n")
    expect_true(grepl("CHECK THIS! Expected columns MISSING", txt))
    expect_true(grepl("selectedValue", txt))
    expect_true(grepl("Birth", txt))
})

test_that("the manifest diff is silent when fields.txt is complete", {
    d <- makeTestExp(file.path(tempdir(), "mfdiff2"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    expected <- buildQCEoutputFieldManifest(d, outFile = NULL)
    cols <- expected[!grepl("^#", expected) & nchar(expected) > 0]
    ff <- file.path(d, "fields.txt")
    writeLines(cols, ff)
    txt <- paste(buildQCEoutputFieldManifest(d, outFile = NULL, fieldsFile = ff), collapse = "\n")
    expect_true(grepl("No expected column is missing", txt))
})

makeSurveyExp <- function(d) {
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    model <- list(pages = list(list(name = "p1", elements = list(
        list(type = "html", name = "blurb", html = "<p>hi</p>"),
        list(type = "rating", name = "MVS_goodCatch", title = "How good a catch?"),
        list(type = "matrix", name = "PANAS",
             rows = list(list(value = "Alert", text = "Alert"),
                         list(value = "Upset", text = "Upset"))),
        list(type = "panel", name = "pnl", elements = list(
            list(type = "text", name = "nested_q")))
    ))))
    stim <- list(s1 = list(frame = list(`1` = list(
        trialType = "survey", frameName = "MVS",
        stimulus = jsonlite::toJSON(model, auto_unbox = TRUE)))))
    jsonlite::write_json(stim, file.path(d, "s_stimfile.json"), auto_unbox = FALSE)
    d
}

test_that("the manifest reads a survey's question names as columns", {
    d <- makeSurveyExp(file.path(tempdir(), "mfsurvey"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    out <- buildQCEoutputFieldManifest(d, outFile = NULL)
    expect_true("MVS_goodCatch" %in% out)
    expect_true("nested_q" %in% out)          # inside a panel
    expect_false("blurb" %in% out)            # display-only html element
})

test_that("the manifest expands a matrix into the per-item columns the data carries", {
    d <- makeSurveyExp(file.path(tempdir(), "mfmatrix"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    out <- buildQCEoutputFieldManifest(d, outFile = NULL)
    # The trial type flattens {PANAS: {Alert: 3}} to PANAS_Alert, so reporting a
    # bare "PANAS" would name a column that never appears in the data.
    expect_true(all(c("PANAS_Alert", "PANAS_Upset") %in% out))
    expect_false("PANAS" %in% out)
})

test_that("the manifest does not call the survey type undeclared once it has read it", {
    d <- makeSurveyExp(file.path(tempdir(), "mfsurvdecl"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    txt <- paste(buildQCEoutputFieldManifest(d, outFile = NULL), collapse = "\n")
    expect_false(grepl("\\[survey\\] declares no outputColumns", txt))
    expect_true(grepl("survey questions", txt))
})

test_that("missingQCEoutputFields returns the gap as data, not prose", {
    d <- makeTestExp(file.path(tempdir(), "mfgaps"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    ff <- file.path(d, "fields.txt")
    writeLines(c("Exp_Name", "sn", "Trial"), ff)
    gaps <- missingQCEoutputFields(d, ff)
    expect_true(is.character(gaps))
    expect_true(all(c("Birth", "selectedValue") %in% gaps))
    expect_false(any(grepl("^#", gaps)))
    # Columns already present must not be reported as gaps.
    expect_false("Trial" %in% gaps)
})

test_that("missingQCEoutputFields is empty when fields.txt is complete", {
    d <- makeTestExp(file.path(tempdir(), "mfgaps2"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    report <- buildQCEoutputFieldManifest(d, outFile = NULL)
    cols <- report[!grepl("^#", report) & nchar(report) > 0]
    ff <- file.path(d, "fields.txt")
    writeLines(cols, ff)
    expect_length(missingQCEoutputFields(d, ff), 0)
})

test_that("missingQCEoutputFields does not mistake the 'extra columns' list for a gap", {
    d <- makeTestExp(file.path(tempdir(), "mfgaps3"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    report <- buildQCEoutputFieldManifest(d, outFile = NULL)
    cols <- report[!grepl("^#", report) & nchar(report) > 0]
    ff <- file.path(d, "fields.txt")
    # Complete, PLUS a hook-written column the scan cannot see. That lands in the
    # report's second list, which must not be read back as missing.
    writeLines(c(cols, "aHookColumn"), ff)
    expect_length(missingQCEoutputFields(d, ff), 0)
})

test_that("the manifest rejects a bad directory or a missing fields.txt", {
    expect_error(buildQCEoutputFieldManifest(file.path(tempdir(), "nope_not_here")),
                 "existing experiment directory")
    d <- makeTestExp(file.path(tempdir(), "mfbad"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    expect_error(buildQCEoutputFieldManifest(d, fieldsFile = file.path(d, "absent.txt")),
                 "does not exist")
})

test_that("an unset config field (serialized as {}) is not read as a declared screen", {
    d <- file.path(tempdir(), "mfempty"); dir.create(d, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    stim <- list(s1 = list(frame = list(`1` = list(trialType = "mcKeys"))))
    jsonlite::write_json(stim, file.path(d, "e_stimfile.json"), auto_unbox = FALSE)
    # buildQCEexpDbFile emits NULL params as empty objects, not as absent keys.
    db <- list(expName = "x", getDemographicsFile = setNames(list(), character(0)),
               getGenderFile = setNames(list(), character(0)),
               customHooksFile = setNames(list(), character(0)))
    jsonlite::write_json(db, file.path(d, "expDBfile.json"), auto_unbox = FALSE)
    txt <- paste(buildQCEoutputFieldManifest(d, outFile = NULL), collapse = "\n")
    expect_false(grepl("Birth", txt))
    expect_false(grepl("Gender", txt))
    expect_false(grepl("CHECK THIS!", txt))   # no hooks declared
})
