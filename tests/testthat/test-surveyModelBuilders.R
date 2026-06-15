# Tests for the schema-agnostic SurveyJS model builders + array-forcing
# serializer (surveyModelBuilders.r) and the soft catalog (surveyTypeCatalog.r).

test_that("surveyQuestion builds a minimal element with passthrough props", {
    q <- surveyQuestion("rating", name = "mvs1", title = "How are you?",
                        isRequired = TRUE, rateMin = 1, rateMax = 7)
    expect_equal(q$type, "rating")
    expect_equal(q$name, "mvs1")
    expect_equal(q$title, "How are you?")
    expect_true(q$isRequired)
    expect_equal(q$rateMin, 1)
    expect_equal(q$rateMax, 7)
})

test_that("surveyQuestion omits isRequired when FALSE (default)", {
    q <- surveyQuestion("text", name = "age", title = "Age")
    expect_null(q$isRequired)
})

test_that("surveyQuestion passes through ANY property (schema-agnostic)", {
    # unknown type warns softly but still builds -- suppress the soft warning
    q <- suppressWarnings(surveyQuestion("brandNewType", name = "x", futureProp = list(1, 2, 3)))
    expect_equal(q$type, "brandNewType")
    expect_equal(q$futureProp, list(1, 2, 3))
})

test_that("unknown type warns but still returns the element", {
    expect_warning(q <- surveyQuestion("notARealType", name = "x"),
                   "not in the QCEB SurveyJS")
    expect_equal(q$type, "notARealType")
})

test_that("known type missing a required prop warns softly", {
    expect_warning(surveyQuestion("radiogroup", name = "sex"),
                   "missing recommended")
})

test_that("missing name on an input type warns", {
    expect_warning(surveyQuestion("text"), "name.*missing")
})

test_that("display-only type without name does NOT warn about name", {
    expect_silent(surveyQuestion("html", html = "<p>hi</p>"))
})

test_that("surveyQuestion rejects an empty type", {
    expect_error(surveyQuestion(""), "non-empty single string")
})

test_that("surveyQuestion rejects unnamed ... props", {
    # fill type/name/title/isRequired so the stray value lands in ...
    expect_error(surveyQuestion("text", "a", "T", FALSE, "stray"),
                 "must be")
})

test_that("surveyPage requires at least one question", {
    expect_error(surveyPage("p1"), "at least one question")
})

test_that("surveyPage assembles elements in order", {
    q1 <- surveyQuestion("text", "a", "A")
    q2 <- surveyQuestion("text", "b", "B")
    p <- surveyPage("p1", q1, q2)
    expect_equal(p$name, "p1")
    expect_equal(length(p$elements), 2L)
    expect_equal(p$elements[[1]]$name, "a")
    expect_equal(p$elements[[2]]$name, "b")
})

test_that("surveyPage merges pageProps", {
    p <- surveyPage("p1", surveyQuestion("text", "a"), pageProps = list(title = "Part 1"))
    expect_equal(p$title, "Part 1")
})

test_that("surveyModel requires at least one page", {
    expect_error(surveyModel(), "at least one page")
})

test_that("surveyModel sets defaults and assembles pages", {
    p <- surveyPage("p1", surveyQuestion("text", "a"))
    m <- surveyModel(p)
    expect_equal(m$showQuestionNumbers, "off")
    expect_equal(length(m$pages), 1L)
})

# --- serializer: the array-forcing guarantee --------------------------------

test_that("single choice serializes as a JSON ARRAY, not a scalar", {
    q <- surveyQuestion("radiogroup", "x", choices = "OnlyOne")
    m <- surveyModel(surveyPage("p1", q))
    json <- QCEB:::.serializeSurveyModel(m)
    expect_match(json, '"choices":["OnlyOne"]', fixed = TRUE)
})

test_that("multi choices serialize as a JSON array", {
    q <- surveyQuestion("checkbox", "x", choices = c("a", "b", "c"))
    m <- surveyModel(surveyPage("p1", q))
    json <- QCEB:::.serializeSurveyModel(m)
    expect_match(json, '"choices":["a","b","c"]', fixed = TRUE)
})

test_that("single-page model still emits pages as an array of one", {
    m <- surveyModel(surveyPage("p1", surveyQuestion("text", "a")))
    json <- QCEB:::.serializeSurveyModel(m)
    expect_match(json, '"pages":[{', fixed = TRUE)
})

test_that("genuine scalars unbox normally", {
    q <- surveyQuestion("rating", "x", rateMax = 7)
    m <- surveyModel(surveyPage("p1", q))
    json <- QCEB:::.serializeSurveyModel(m)
    expect_match(json, '"rateMax":7', fixed = TRUE)
    expect_match(json, '"showQuestionNumbers":"off"', fixed = TRUE)
})

test_that("nested matrix columns/rows survive as arrays", {
    q <- surveyQuestion("matrix", "RSE",
        columns = list(list(value = "A", text = "Agree")),
        rows = list(list(value = "r1", text = "Item one.")))
    m <- surveyModel(surveyPage("p1", q))
    json <- QCEB:::.serializeSurveyModel(m)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    el <- parsed$pages[[1]]$elements[[1]]
    expect_equal(length(el$columns), 1L)
    expect_equal(el$columns[[1]]$value, "A")
    expect_equal(el$rows[[1]]$text, "Item one.")
})

test_that("round-trip: serialized model parses back to the same structure", {
    m <- surveyModel(surveyPage("p1",
        surveyQuestion("rating", "mvs1", "Q1", isRequired = TRUE, rateMin = 1, rateMax = 7),
        surveyQuestion("radiogroup", "sex", "Sex", choices = c("F", "M"))))
    json <- QCEB:::.serializeSurveyModel(m)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_equal(parsed$pages[[1]]$elements[[1]]$rateMax, 7)
    expect_equal(parsed$pages[[1]]$elements[[2]]$choices[[1]], "F")
})

test_that("surveyTypeCatalog covers the documented core SurveyJS types", {
    cat <- surveyTypeCatalog()
    for (t in c("text", "comment", "radiogroup", "checkbox", "dropdown",
                "tagbox", "ranking", "imagepicker", "rating", "boolean",
                "matrix", "matrixdropdown", "matrixdynamic", "multipletext",
                "panel", "paneldynamic", "file", "signaturepad", "html",
                "image", "expression")) {
        expect_true(t %in% names(cat), info = paste("missing catalog type:", t))
    }
})

test_that("every catalog entry has desc/required/properties/docUrl with the right shapes", {
    cat <- surveyTypeCatalog()
    for (t in names(cat)) {
        e <- cat[[t]]
        expect_true(is.character(e$desc) && length(e$desc) == 1,
                    info = paste(t, "desc"))
        expect_true(is.character(e$required), info = paste(t, "required"))
        expect_true(is.list(e$properties) && !is.null(names(e$properties)) &&
                    all(nchar(names(e$properties)) > 0),
                    info = paste(t, "properties is a named list"))
        # every property carries a single-string description
        expect_true(all(vapply(e$properties, function(d) is.character(d) && length(d) == 1, logical(1))),
                    info = paste(t, "property descriptions"))
        # every required prop is also documented in properties
        expect_true(all(e$required %in% names(e$properties)),
                    info = paste(t, "required props documented in properties"))
        expect_match(e$docUrl, "^https://surveyjs\\.io/form-library/documentation/api-reference/",
                     info = paste(t, "docUrl"))
    }
})

test_that("surveyUniversalProperties returns a documented, linked property set", {
    up <- surveyUniversalProperties()
    expect_true(is.list(up$properties) && length(up$properties) > 10)
    expect_true(all(c("name", "title", "visibleIf", "isRequired", "validators") %in%
                    names(up$properties)))
    expect_true(all(vapply(up$properties, function(d) is.character(d) && length(d) == 1, logical(1))))
    expect_match(up$docUrl, "api-reference/question$")
})
