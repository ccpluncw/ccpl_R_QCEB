#' The SurveyJS question-type catalog used for soft validation + reference
#'
#' Returns the hand-curated catalog of SurveyJS question types that QCEB knows
#' about. It serves two purposes:
#' \enumerate{
#'   \item it powers \emph{soft} validation in \code{\link{surveyQuestion}} (a
#'     \code{warning()}, never a \code{stop()}); and
#'   \item it is a built-in REFERENCE for someone unfamiliar with the SurveyJS
#'     plugin -- each type documents its properties (common and uncommon) and
#'     links to the authoritative SurveyJS API page.
#' }
#' QCEB is a thin, schema-agnostic passthrough over SurveyJS: an unknown
#' \code{type}, or an unknown property on a known type, still serializes and
#' ships. The catalog never limits which types or properties you can use; it
#' just surfaces common mistakes early and tells you what each type accepts.
#'
#' Each entry is a list with:
#' \describe{
#'   \item{desc}{a one-line human description of the type.}
#'   \item{required}{character vector of properties SurveyJS needs for the type
#'     to render meaningfully (beyond the universal \code{type}/\code{name}). A
#'     missing one triggers a soft warning.}
#'   \item{properties}{a named list mapping each type-specific property (common
#'     AND uncommon) to a one-line description. Reference only -- not enforced.}
#'   \item{docUrl}{the authoritative SurveyJS API-reference URL for the type.}
#' }
#'
#' Properties shared by EVERY question type (name, title, visibleIf, isRequired,
#' validators, ...) are not repeated in each entry; see
#' \code{\link{surveyUniversalProperties}}.
#'
#' Authoritative reference index:
#' \url{https://surveyjs.io/form-library/documentation/api-reference/question}.
#' Descriptions are a curated starting point and can drift across SurveyJS
#' releases -- follow each entry's \code{docUrl} for the definitive list.
#'
#' @return A named list, one entry per known SurveyJS type.
#' @keywords QCE survey catalog SurveyJS reference
#' @export
#' @examples
#' names(surveyTypeCatalog())
#' surveyTypeCatalog()[["matrix"]]$properties
#' surveyTypeCatalog()[["rating"]]$docUrl
surveyTypeCatalog <- function() {
  u <- function(slug) paste0(.surveyDocBase, slug)
  list(
    # --- single-value text inputs -----------------------------------------
    text = list(
      desc = "A single-line input; inputType picks text/number/email/date/etc.",
      required = character(0),
      properties = list(
        inputType    = "HTML input type: text/number/email/date/datetime-local/time/tel/url/password/range/color/month/week.",
        placeholder  = "Grey hint text shown while the field is empty.",
        min          = "Minimum allowed value (for number/date inputType).",
        max          = "Maximum allowed value (for number/date inputType).",
        minErrorText = "Error message shown when the value is below min.",
        maxErrorText = "Error message shown when the value is above max.",
        step         = "Increment for number/range inputType.",
        maxLength    = "Maximum characters (0 = unlimited, -1 = survey default).",
        size         = "Visible width of the input, in characters.",
        autocomplete = "HTML autocomplete token (e.g. 'email', 'name').",
        dataList     = "Vector of autocomplete suggestions (HTML datalist).",
        textUpdateMode = "When the value updates: 'default' / 'onBlur' / 'onTyping'."
      ),
      docUrl = u("text-entry-question-model")),

    comment = list(
      desc = "A multi-line free-text box.",
      required = character(0),
      properties = list(
        placeholder = "Grey hint text shown while the box is empty.",
        rows        = "Number of visible text lines.",
        cols        = "Visible width in characters.",
        maxLength   = "Maximum characters (0 = unlimited, -1 = survey default).",
        autoGrow    = "Auto-expand the box height as the participant types.",
        acceptCarriageReturn = "Allow Enter to insert a newline.",
        textUpdateMode = "When the value updates: 'default' / 'onBlur' / 'onTyping'."
      ),
      docUrl = u("comment-field-model")),

    # --- choice-based (need `choices`) ------------------------------------
    radiogroup = list(
      desc = "Single-select radio buttons.",
      required = "choices",
      properties = list(
        choices            = "Option list: a vector of strings, or a list of {value,text} objects.",
        choicesByUrl       = "Load choices from a REST endpoint: list(url, path, valueName, titleName).",
        choicesFromQuestion = "Reuse the choices defined on another question (by name).",
        choicesOrder       = "Display order: 'none' / 'asc' / 'desc' / 'random'.",
        colCount           = "Number of columns the options are arranged in (0-5).",
        showOtherItem      = "Add an 'Other' option with a free-text box.",
        otherText          = "Caption of the 'Other' option.",
        otherPlaceholder   = "Hint text in the 'Other' free-text box.",
        otherErrorText     = "Error when 'Other' is chosen but left blank.",
        showNoneItem       = "Add a 'None' option.",
        noneText           = "Caption of the 'None' option.",
        showRefuseItem     = "Add a 'Refuse to answer' option.",
        showDontKnowItem   = "Add a 'Don't know' option.",
        separateSpecialChoices = "Visually separate None/Other/etc. from regular choices."
      ),
      docUrl = u("radio-button-question-model")),

    checkbox = list(
      desc = "Multi-select checkboxes (records an array).",
      required = "choices",
      properties = list(
        choices            = "Option list: vector of strings, or list of {value,text} objects.",
        choicesByUrl       = "Load choices from a REST endpoint.",
        choicesFromQuestion = "Reuse another question's choices (by name).",
        choicesOrder       = "Display order: 'none' / 'asc' / 'desc' / 'random'.",
        colCount           = "Number of columns the options are arranged in (0-5).",
        maxSelectedChoices = "Maximum options the participant may check (0 = no limit).",
        minSelectedChoices = "Minimum options that must be checked.",
        showSelectAllItem  = "Add a 'Select All' option.",
        selectAllText      = "Caption of the 'Select All' option.",
        showOtherItem      = "Add an 'Other' option with a free-text box.",
        showNoneItem       = "Add a 'None' option.",
        separateSpecialChoices = "Visually separate None/Other/SelectAll from regular choices."
      ),
      docUrl = u("checkbox-question-model")),

    dropdown = list(
      desc = "Single-select dropdown list.",
      required = "choices",
      properties = list(
        choices       = "Option list: vector of strings, or list of {value,text} objects.",
        choicesByUrl  = "Load choices from a REST endpoint.",
        placeholder   = "Text shown before a selection is made.",
        choicesOrder  = "Display order: 'none' / 'asc' / 'desc' / 'random'.",
        allowClear    = "Show a button to clear the current selection.",
        searchEnabled = "Allow type-to-filter within the list.",
        showOtherItem = "Add an 'Other' option with a free-text box.",
        choicesMin    = "Auto-generate numeric choices starting here.",
        choicesMax    = "Auto-generate numeric choices ending here.",
        choicesStep   = "Step for the auto-generated numeric choices."
      ),
      docUrl = u("dropdown-menu-model")),

    tagbox = list(
      desc = "Multi-select dropdown (records an array).",
      required = "choices",
      properties = list(
        choices            = "Option list: vector of strings, or list of {value,text} objects.",
        choicesByUrl       = "Load choices from a REST endpoint.",
        placeholder        = "Text shown before any selection is made.",
        maxSelectedChoices = "Maximum options the participant may select (0 = no limit).",
        allowClear         = "Show a button to clear all selections.",
        searchEnabled      = "Allow type-to-filter within the list.",
        hideSelectedItems  = "Remove chosen items from the dropdown list.",
        closeOnSelect      = "Close the dropdown after each selection.",
        showSelectAllItem  = "Add a 'Select All' option.",
        selectAllText      = "Caption of the 'Select All' option."
      ),
      docUrl = u("dropdown-tag-box-model")),

    ranking = list(
      desc = "Drag-to-order ranking (records an ordered array).",
      required = "choices",
      properties = list(
        choices             = "Items to be ranked: vector of strings, or list of {value,text}.",
        selectToRankEnabled = "Two-list mode: drag items from an unranked pool into a ranked list.",
        selectToRankAreasLayout = "Layout of the two lists: 'horizontal' / 'vertical'.",
        longTap             = "Require a long press before dragging (touch devices).",
        choicesOrder        = "Initial order: 'none' / 'asc' / 'desc' / 'random'."
      ),
      docUrl = u("ranking-question-model")),

    imagepicker = list(
      desc = "Pick one/more images; each choice has an imageLink.",
      required = "choices",
      properties = list(
        choices     = "List of {value, imageLink, text} objects.",
        multiSelect = "Allow selecting more than one image.",
        contentMode = "Choice content: 'image' or 'video'.",
        imageFit    = "CSS object-fit for each thumbnail (cover/contain/fill/none).",
        imageHeight = "Thumbnail height (px or CSS value).",
        imageWidth  = "Thumbnail width (px or CSS value).",
        showLabel   = "Show each choice's text under its image.",
        colCount    = "Number of columns the images are arranged in."
      ),
      docUrl = u("image-picker-question-model")),

    # --- scales / booleans ------------------------------------------------
    rating = list(
      desc = "A 1..N rating scale; rateValues for custom labeled points.",
      required = character(0),
      properties = list(
        rateMin            = "Lowest value of a numeric range (default 1).",
        rateMax            = "Highest value of a numeric range (default 5).",
        rateStep           = "Increment between numeric points.",
        rateValues         = "Explicit list of {value,text} points; overrides rateMin/rateMax for custom labels.",
        rateCount          = "Number of items when using stars/smileys.",
        rateType           = "Visual style: 'labels' (default) / 'stars' / 'smileys'.",
        displayMode        = "'auto' / 'buttons' / 'dropdown' rendering.",
        minRateDescription = "Caption shown under the lowest point.",
        maxRateDescription = "Caption shown under the highest point.",
        rateDescriptionLocation = "Where the min/max captions sit relative to the scale.",
        scaleColorMode     = "Color treatment of the scale ('monochrome' / 'colored')."
      ),
      docUrl = u("rating-scale-question-model")),

    boolean = list(
      desc = "A yes/no toggle.",
      required = character(0),
      properties = list(
        labelTrue  = "Caption for the 'yes' / true side.",
        labelFalse = "Caption for the 'no' / false side.",
        valueTrue  = "Value saved when 'yes' is chosen (default TRUE).",
        valueFalse = "Value saved when 'no' is chosen (default FALSE).",
        renderAs   = "Visual style: 'default' (switch) / 'radio' / 'checkbox'.",
        swapOrder  = "Show 'No' before 'Yes'."
      ),
      docUrl = u("boolean-question-model")),

    # --- matrices ---------------------------------------------------------
    matrix = list(
      desc = "Single-select grid: one radio choice per row (records {row: col}).",
      required = c("columns", "rows"),
      properties = list(
        columns          = "Answer options shown as column headers: strings or {value,text}.",
        rows             = "The statements/items down the side: strings or {value,text}.",
        isAllRowRequired = "Require an answer in every row.",
        rowsOrder        = "Row display order: 'initial' / 'random'.",
        alternateRows    = "Shade every other row.",
        showHeader       = "Show the column-header row.",
        columnMinWidth   = "Minimum column width (CSS value).",
        rowTitleWidth    = "Width of the row-title (left) column.",
        hideIfRowsEmpty  = "Hide the whole question if it has no rows."
      ),
      docUrl = u("matrix-table-question-model")),

    matrixdropdown = list(
      desc = "Grid whose cells are themselves inputs (dropdown/text/checkbox/...).",
      required = "columns",
      properties = list(
        columns          = "Each column is an input definition: {name,title,cellType,choices,...}.",
        rows             = "Row headers: strings or {value,text}.",
        choices          = "Default choice list shared by dropdown/checkbox/radiogroup cells.",
        cellType         = "Default cell editor: 'dropdown'/'text'/'checkbox'/'radiogroup'/'comment'/'boolean'.",
        isAllRowRequired = "Require an answer in every row.",
        totalText        = "Label for the totals row (when columns define totals).",
        columnLayout     = "Orientation: 'horizontal' (default) / 'vertical'.",
        horizontalScroll = "Enable horizontal scrolling for wide grids.",
        transposeData    = "Swap rows and columns in the recorded data."
      ),
      docUrl = u("matrix-table-with-dropdown-list")),

    matrixdynamic = list(
      desc = "Grid where the participant adds/removes rows at runtime.",
      required = "columns",
      properties = list(
        columns         = "Each column is an input definition: {name,title,cellType,choices,...}.",
        rowCount        = "Initial number of rows.",
        minRowCount     = "Minimum rows the participant must keep.",
        maxRowCount     = "Maximum rows the participant may add.",
        cellType        = "Default cell editor (see matrixdropdown).",
        addRowText      = "Caption of the add-row button.",
        removeRowText   = "Caption of the remove-row button.",
        addRowLocation  = "Where the add-row button sits: 'top'/'bottom'/'topBottom'.",
        allowAddRows    = "Whether participants may add rows.",
        allowRemoveRows = "Whether participants may remove rows.",
        confirmDelete   = "Ask for confirmation before deleting a row.",
        defaultRowValue = "Values pre-filled into each newly added row."
      ),
      docUrl = u("dynamic-matrix-table-question-model")),

    # --- composite / container --------------------------------------------
    multipletext = list(
      desc = "Several labeled single-line inputs as one question (records a nested object).",
      required = "items",
      properties = list(
        items         = "List of sub-field definitions: {name, title, inputType, ...}.",
        colCount      = "Number of columns the items are arranged in.",
        itemSize      = "Visible width of each item's input, in characters.",
        itemTitleWidth = "Width of the item-title (label) column."
      ),
      docUrl = u("multiple-text-entry-question-model")),

    panel = list(
      desc = "A visual group of questions (its elements are nested).",
      required = character(0),
      properties = list(
        elements       = "The questions/panels contained in this panel.",
        title          = "Panel heading.",
        description    = "Secondary text under the panel heading.",
        state          = "'default' / 'collapsed' / 'expanded' for a collapsible panel.",
        showNumber     = "Show a number before the panel title.",
        questionsOrder = "Order of contained questions: 'initial' / 'random'.",
        innerIndent    = "Indentation level of the contained questions."
      ),
      docUrl = u("panel-model")),

    paneldynamic = list(
      desc = "A repeatable panel the participant adds/removes.",
      required = "templateElements",
      properties = list(
        templateElements = "The questions repeated inside every panel instance.",
        templateTitle    = "Title template per panel; supports {panelIndex} etc.",
        panelCount       = "Initial number of panels.",
        minPanelCount    = "Minimum panels the participant must keep.",
        maxPanelCount    = "Maximum panels the participant may add.",
        panelAddText     = "Caption of the add-panel button.",
        panelRemoveText  = "Caption of the remove-panel button.",
        allowAddPanel    = "Whether participants may add panels.",
        allowRemovePanel = "Whether participants may remove panels.",
        renderMode       = "Layout: 'list'/'progressTop'/'progressBottom'/'tab'.",
        panelsState      = "Initial collapsed/expanded state of panels."
      ),
      docUrl = u("dynamic-panel-model")),

    # --- file / signature -------------------------------------------------
    file = list(
      desc = "File upload.",
      required = character(0),
      properties = list(
        acceptedTypes      = "Accepted MIME types / extensions, e.g. 'image/*,.pdf'.",
        allowMultiple      = "Allow uploading more than one file.",
        maxSize            = "Maximum file size in bytes (0 = no limit).",
        storeDataAsText    = "Store content as Base64 text in the data (vs. an upload handler).",
        sourceType         = "Input source: 'file' / 'camera' / 'file-camera'.",
        allowImagesPreview = "Show thumbnail previews of uploaded images.",
        imageHeight        = "Preview height.",
        imageWidth         = "Preview width.",
        waitForUpload      = "Block survey completion until uploads finish."
      ),
      docUrl = u("file-model")),

    signaturepad = list(
      desc = "A draw-your-signature canvas.",
      required = character(0),
      properties = list(
        penColor        = "Stroke color.",
        backgroundColor = "Canvas background color.",
        penMinWidth     = "Minimum stroke width in px.",
        penMaxWidth     = "Maximum stroke width in px.",
        signatureWidth  = "Canvas width in px.",
        signatureHeight = "Canvas height in px.",
        dataFormat      = "Saved image format: 'png' / 'jpeg' / 'svg'.",
        allowClear      = "Show a button to clear the canvas.",
        backgroundImage = "Image shown behind the signature (e.g. a line to sign on)."
      ),
      docUrl = u("signature-pad-model")),

    # --- display-only (no response) ---------------------------------------
    html = list(
      desc = "Static HTML; collects no response.",
      required = "html",
      properties = list(
        html = "The raw HTML markup to render."
      ),
      docUrl = u("add-custom-html-to-survey")),

    image = list(
      desc = "A static image or video; collects no response.",
      required = character(0),
      properties = list(
        imageLink   = "URL or Base64 of the image/video to display.",
        contentMode = "'image' / 'video' / 'youtube' / 'auto'.",
        imageFit    = "CSS object-fit (cover/contain/fill/none).",
        imageHeight = "Display height (px or CSS value).",
        imageWidth  = "Display width (px or CSS value).",
        altText     = "Alt text passed to the underlying <img> element."
      ),
      docUrl = u("add-image-to-survey")),

    expression = list(
      desc = "A computed read-only value derived from other answers.",
      required = "expression",
      properties = list(
        expression  = "Expression evaluated to produce the displayed value, e.g. '{q1} + {q2}'.",
        displayStyle = "Number formatting: 'none'/'decimal'/'currency'/'percent'/'date'.",
        currency    = "ISO currency code when displayStyle = 'currency'.",
        format      = "Format string with {0} as the placeholder, e.g. 'Total: {0}'.",
        maximumFractionDigits = "Max decimal places shown.",
        minimumFractionDigits = "Min decimal places shown."
      ),
      docUrl = u("expression-model"))
  )
}

#' Properties shared by every SurveyJS question type
#'
#' Many SurveyJS properties apply to ALL question types (name, title,
#' visibility, requiredness, validation, ...). Rather than repeat them in every
#' entry of \code{\link{surveyTypeCatalog}}, they are documented once here. They
#' may be passed to \code{\link{surveyQuestion}} for any \code{type} via
#' \code{...}.
#'
#' Authoritative reference (base Question class):
#' \url{https://surveyjs.io/form-library/documentation/api-reference/question}.
#'
#' @return A list with \code{desc}, a named \code{properties} list
#'   (propertyName -> description), and \code{docUrl}.
#' @keywords QCE survey catalog SurveyJS reference
#' @export
#' @examples
#' names(surveyUniversalProperties()$properties)
#' surveyUniversalProperties()$properties$visibleIf
surveyUniversalProperties <- function() {
  list(
    desc = "Properties accepted by every SurveyJS question type.",
    properties = list(
      name              = "Unique identifier; becomes the data column for this question's answer.",
      title             = "Question text shown to the participant (falls back to name if absent).",
      titleLocation     = "Title placement: 'default'/'top'/'bottom'/'left'/'hidden'.",
      description       = "Secondary explanatory text under the title.",
      descriptionLocation = "Where the description sits: 'default'/'underTitle'/'underInput'.",
      isRequired        = "Force an answer before the page can be submitted.",
      requiredErrorText = "Message shown when a required question is left blank.",
      requiredIf        = "Expression that makes the question conditionally required.",
      visible           = "Whether the question is shown (default TRUE).",
      visibleIf         = "Expression controlling visibility, e.g. '{age} > 18'.",
      enableIf          = "Expression controlling whether the question is editable.",
      readOnly          = "Show the question but disallow editing.",
      defaultValue      = "Pre-filled answer.",
      defaultValueExpression = "Expression that computes the default answer.",
      valueName         = "Store the answer under a different data key than name.",
      startWithNewLine  = "Begin on a new line vs. sharing a row with the previous question.",
      indent            = "Left indentation level (0-3).",
      width             = "CSS width within its row.",
      minWidth          = "CSS minimum width.",
      maxWidth          = "CSS maximum width.",
      hideNumber        = "Hide this question's auto-generated number.",
      validators        = "List of validation rules (numeric range, regex, email, expression, ...).",
      clearIfInvisible  = "When to clear a hidden question's value: 'none'/'onComplete'/'onHidden'/'onHiddenContainer'.",
      correctAnswer     = "Expected answer, for quiz-style scoring."
    ),
    docUrl = paste0(.surveyDocBase, "question")
  )
}

# Base URL for all SurveyJS API-reference pages (verified against the live site,
# June 2026). Each catalog entry's docUrl is this base + the type's slug.
.surveyDocBase <- "https://surveyjs.io/form-library/documentation/api-reference/"

# Internal (not exported): soft-validate one question list against the catalog.
# NEVER stops -- only warns. An unknown type, or a known type missing a
# required prop, warns once and passes through unchanged. This keeps QCEB
# schema-agnostic (all SurveyJS types work, including future ones) while still
# catching the most common authoring slips.
.validateSurveyQuestion <- function(q) {
  if (is.null(q$type) || !isSingleString(q$type)) {
    warning("surveyQuestion: 'type' is missing or not a single string; ",
            "SurveyJS will not render this element correctly.")
    return(invisible(FALSE))
  }
  catalog <- surveyTypeCatalog()
  if (!(q$type %in% names(catalog))) {
    warning("surveyQuestion: type '", q$type, "' is not in the QCEB SurveyJS ",
            "catalog. It will still be passed through verbatim -- if it is a ",
            "valid SurveyJS type (or a newer one), ignore this warning; ",
            "otherwise check for a typo. Known types: ",
            paste(names(catalog), collapse = ", "), ".")
    return(invisible(FALSE))
  }
  # Display-only types do not need a name; input types should have one.
  displayOnly <- c("html", "image", "expression")
  if (!(q$type %in% displayOnly) && (is.null(q$name) || !isSingleString(q$name) || nchar(q$name) == 0)) {
    warning("surveyQuestion(type='", q$type, "'): 'name' is missing. The name ",
            "becomes the data column for this item -- supply one so the response ",
            "can be scored.")
  }
  needed <- catalog[[q$type]]$required
  missingProps <- needed[!(needed %in% names(q))]
  if (length(missingProps) > 0) {
    warning("surveyQuestion(type='", q$type, "', name='",
            if (is.null(q$name)) "" else q$name,
            "'): missing recommended propert", if (length(missingProps) > 1) "ies: " else "y: ",
            paste(missingProps, collapse = ", "),
            ". SurveyJS may render an empty question. (Passing through anyway.)")
  }
  invisible(TRUE)
}
