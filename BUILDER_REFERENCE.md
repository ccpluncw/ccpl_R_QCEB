# QCEB Builder Reference

> **Targets:** QCEP engine **v9.1** · QCEP engine specification (`QCEP_SPEC.md`
> in the QCEP repository) **v0.1** · QCEB package **0.0.1**.
> If any of those move, regenerate and re-verify this document.

This is the reference for building QCEP experiment configurations with the
QCEB R package. It covers the **R call surface**: which builder functions
exist, how they compose into a build script, and the serialization rules that
make the emitted JSON acceptable to the engine.

It deliberately does **not** re-explain what the configuration *means* at
runtime. The engine's contract — the config file structure, trial lifecycle,
conditional-display and switch-rule semantics, data output — is specified once,
in the QCEP engine specification (`QCEP_SPEC.md`, in the QCEP repository).
References flow one direction: this document points at the specification;
the specification never depends on this document. Where a rule below spans
both sides (the engine expects X, so the builder must do Y), the engine
expectation is the specification's; only the R mechanics are stated here.

## Installing and loading

```r
# install
devtools::install_github("ccpluncw/ccpl_R_QCEB")
library(QCEB)

# or, when working from a source checkout
pkgload::load_all("path/to/QCEB")
```

When results disagree with this document, confirm the loaded package matches
the source tree you think you are using (`packageVersion("QCEB")`), and prefer
`pkgload::load_all()` on a checkout during development.

## The build workflow

A build script assembles an experiment from the inside out, then writes one
JSON file per configuration role:

1. **Frames → scenarios (Stimfile).** `addFrameToQCEframeList()` builds each
   display/response frame; `addFixationToQCEframeList()` is the fixation
   convenience wrapper; `addSurveyFrameToQCEframeList()` embeds a survey
   model. Frames are grouped into scenarios with
   `addScenarioToQCEscenarioList()`. `saveJsonFile()` writes the result.
2. **Sets → blocks (Tsfile).** `addSetToQCEsetInfoList()` declares which
   scenarios a set draws on and how; `addBlockToQCETrialStructureList()`
   assembles sets into blocks (key maps, entry instructions, switch rules
   attach here). `saveJsonFile()` writes the result.
3. **Group-level settings (group dbfile).** `buildQCEgroupDbFile()` — key
   maps, feedback, triggers, rest breaks, hooks. One per session's `dbFile`.
4. **Experiment-level settings (expDBfile).** `buildQCEexpDbFile()` —
   welcome/end messages, consent/demographics pointers, fullscreen,
   completion gate, redirects. `saveJsonFile()` writes both dbfile kinds.
5. **Sessions → groups (expInfo).** `addSessionToSessionList()` wires each
   session to its three files; `addSessionListToQCEGroupList()` assembles
   groups (in QCEP, "groups" are tasks/modules, not participant cohorts —
   see the specification). `saveJsonFile()` writes the result.
6. **Manifests and checks.** `buildQCEoutputFieldManifest()` reports the data
   columns the built experiment will produce; `missingQCEoutputFields()`
   cross-checks an expected-column list.

**One writer for every configuration file: `saveJsonFile(data, filename)`.**
It emits plain JSON and takes the output path as an argument, so a build can
name its files whatever the deployment expects. `saveStimFile()`,
`saveTSFile()` and `saveDbFile()` are legacy writers kept for backward
compatibility: each wraps the JSON in a JavaScript assignment
(`var scenarios = …`) and writes to a fixed filename, and an engine that
requires plain JSON cannot load what they produce. Do not use them in new
builds.

The worked examples below show the full pattern end to end.

## Hard rules (violating any of these produces a config the engine misreads or rejects)

1. **Never hand-edit the emitted JSON.** Serialization is part of the
   contract: the writers box every scalar as a one-element array except where
   a builder deliberately unboxes, and the engine reads both forms only where
   it is specified to. Rebuilds also revert hand edits. Change the build
   script, not the output.
2. **Round-trip JSON only with `readQCEjsonFile()`.** Generic readers
   (`jsonlite::fromJSON`) either double-box on re-save or destroy deliberate
   scalars (turning `"choices": "NO_KEYS"` into `["NO_KEYS"]`), both of which
   change engine behavior.
3. **`contBtn` arguments take an HTML element `id`, not a button label.**
4. **`post_trial_gap` is required on frames.** Supply it explicitly.
5. **`cursorVisible` is three-state.** Leave it `NULL` (the default) to let
   the engine decide from the trial type; `TRUE`/`FALSE` are overrides. An
   explicit `FALSE` on a mouse-driven trial type produces a trial that cannot
   be answered; the builder warns.
6. **Do not pass legacy sentinels.** `keyMapInstructionFile = "default"` is a
   pre-9.1 sentinel and now a hard error; the field is a literal filename.
7. **Deprecated builders are never used in new code.** See the terminal
   section of the generated API listing.
8. **Unknown keys are the weakest evidence of support.** Some builders pass
   unrecognized list keys through to the JSON verbatim (with a warning where
   validation exists). A key reaching the file does not mean the engine reads
   it; the specification is the authority on what the engine reads.

<!-- BEGIN GENERATED API — do not edit by hand; run tools/generate_api_reference.R -->

*Generated from `man/` on 2026-08-28 — 73 exported functions (66 current, 7 deprecated).*

## Stimfile — scenarios and frames

### `addFixationToQCEframeList`

This function is used to add a fixation frameto a QCEframeList

```r
addFixationToQCEframeList(
  QCEframeList = NULL,
  frameSymbol = "+",
  fixationFontSize = "50px",
  fixationColor = "#FF6464",
  stimulus_duration = 500,
  post_trial_gap = 500,
  stimTableWidth = 100,
  background = "#000000",
  output = FALSE,
  trigger = NULL
)
```

Function that creates or modifys an QCEframeList by adding a fixation frame.

- `QCEframeList` — A list that specifies the frames to show a participant in a single scenario. These frames are presented in succession: 1, 2, ... N. If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEframeList that you are adding an effect to. DEFAULT = NULL
- `frameSymbol` — A string that specifies the character symbol to use for the fixation. DEFAULT = "+"
- `fixationFontSize` — A string that specifies the font size of the fixation. Font size is specified by an integer followed by px. For example, "50px" DEFAULT = "50px"
- `fixationColor` — an RGB color, specified in hexadecimal, that controls the color of the fixation symbol. DEFAULT = "#FF6464" (reddish).
- `stimulus_duration` — An integer that specifies how long to present the fixation in milliseconds. A NULL will present the stimulus until their is a user input. DEFAULT = 500
- `post_trial_gap` — An integer that specifies how long to present a blank frame after this frame in milliseconds. DEFAULT = 500
- `stimTableWidth` — An integer specifying the width of the stimuli in the other frames. This is generally not necessary. Rather, the fixation will be centered on the screen. DEFAULT = 100.
- `background` — an RGB color, specified in hexadecimal, that controls the background color of the frame page. DEFAULT = "#000000" (black).
- `output` — a boolean that specifies whether to output the data from the frame into the dataset. Often fixation frames do not need to be output. DEFAULT = FALSE.
- `trigger` — Optional list produced by buildQCETriggerList() specifying fNIRS trigger codes that fire at the fixation frame's boundaries — onset in on_start, offset in on_finish. Rarely used at this level for fNIRS (analysts usually mark the stimulus frame, not fixation), but provided for parity with addFrameToQCEframeList. Recommended code range: 10000-99999 (5 digits). DEFAULT = NULL.

**Returns.** the updated QCEframeList

### `addFrameToQCEframeList`

This function is used to create or modify a QCEframeList

```r
addFrameToQCEframeList(
  QCEframeList = NULL,
  trialType = "key",
  frameName = NULL,
  stimulus = NULL,
  stimulus_duration = NULL,
  post_trial_gap = NULL,
  response_ends_trial = TRUE,
  choices = "ALL_KEYS",
  kind = "string",
  background = "#000000",
  cursorVisible = NULL,
  output = TRUE,
  trigger = NULL,
  pluginParams = NULL,
  trial_duration = NULL
)
```

Function that creates or modifies a QCEframeList by adding frames to the list one at a time.

- `QCEframeList` — A list that specifies the frames to show a participant in a single scenario. These frames are presented in succession: 1, 2, ... N. If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEframeList that you are adding an effect to. DEFAULT = NULL
- `trialType` — A string that specifies the response type that you will be collecting. It can take on one of the following values: "key", "textbox", "numberline", or "angleline". "key" indicates a key press (or no input) to move on to the next frame. "textbox" presents a textbox for participants to input text. "numberline" presents a draggable number-line response plugin. "angleline" presents a draggable angle-line response plugin. DEFAULT = "key".
- `frameName` — A string that specifies the name of the frame that will be output in the datafile, to indicate the data collected for this particular frame. One row is output in the datafile for each frame, so the frameName helps you keep track of the frame. DEFAULT = NULL. A NULL will force the frameName to equal "frame#" where # is the frame number.
- `stimulus` — A string that specifies the stimulus to be presented on this frame. The stimulus must be in html format. You can use any html codes. IMPORTANT: if the trialType = "key" you cannot have an input box of any kind. If the trialType is "textbox" you must contain a textbox input field specified in html. The fields for the html textbox MUST contain the following: <label id = TIN for="Text_In"> and  <input id ="Text_In" …> DEFAULT = NULL. A NULL will present a blank screen.
- `stimulus_duration` — An integer that specifies how long the stimulus is VISIBLE, in milliseconds. A NULL leaves it visible until there is a user input. By default the frame also ENDS when the stimulus disappears; set trial_duration to separate the two. DEFAULT = NULL
- `post_trial_gap` — An integer that specifies how long to present a blank frame after this frame in milliseconds. REQUIRED: pass 0 when you want no gap. The NULL in the signature is not a working default -- omitting this argument is an error, so that a frame's inter-trial timing is always something the researcher stated rather than something inherited silently. NO DEFAULT.
- `response_ends_trial` — A boolean that specifies whether the key response ends the trial. A frame must have SOME way to end: if this is FALSE, the frame needs a stimulus_duration or a trial_duration to end it. The same applies when choices is NULL, empty, or "NO_KEYS", since those leave no key to press. Trial types whose plugin supplies its own response surface (textbox, numberline, angleline, survey, mcKeys) can always be ended by the participant and are exempt. DEFAULT = TRUE
- `choices` — Specifies the keyboard keys accepted as a response. Interpretation depends on trialType. For trialType = "key": a character vector of allowed key names (e.g., c("a", "b", " ", "Enter")), or the sentinel "ALL_KEYS" to accept any key. NULL or an empty vector disables the keyboard response path entirely — the trial then advances on stimulus_duration only. For trialType = "textbox" with kind = "string" or "number": the default "ALL_KEYS" is appropriate (the plugin handles character filtering internally via the kind argument). For trialType = "textbox" with kind = "other": you MUST provide a character vector of specific allowed keys (e.g., c("y", "n")). The sentinel "ALL_KEYS" will NOT work in this case — the plugin tests membership against the vector, so "ALL_KEYS" would match nothing. For trialType = "numberline" or "angleline": ignored (these plugins handle their own interaction model). choices does not control what can be typed into an html textbox input field — that is controlled by the html input code plus the kind argument. DEFAULT = "ALL_KEYS".
- `kind` — A string that specifies the type of allowable input in a textbox. Use "string" to allow all input, "number" to allow numbers, and "other" to restrict the textbox to the subset of keys specified in "choices". Only meaningful when trialType = "textbox"; silently ignored (not emitted to JSON) for other trial types. DEFAULT = "string".
- `background` — an RGB color, specified in hexadecimal, that controls the background color of the frame page. DEFAULT = "#000000" (black).
- `cursorVisible` — Three-state, like trial_duration. NULL (the default) omits the key, which tells the engine to decide from the trialType: hidden for a keyboard-response frame, where a pointer sitting over the stimulus is a distractor, and shown for a type that declares usesPointer because the participant answers it with the mouse. TRUE or FALSE overrides that decision for this frame. ⚠ An explicit FALSE on a pointer-driven type hides the cursor the participant needs in order to respond, so it warns. DEFAULT = NULL (let the trialType decide).
- `output` — A boolean that specifies whether to output the data from the frame into the dataset. Many times frames such as fixation and mask frames do not need to be output. DEFAULT = TRUE.
- `trigger` — Optional list produced by buildQCETriggerList() specifying the fNIRS trigger codes that fire at this frame's boundaries — onset fires in the frame's on_start, offset fires in the frame's on_finish (even for non-response frames like fixation). NULL means no frame-level triggers. Recommended code range: 10000-99999 (5 digits). DEFAULT = NULL.
- `pluginParams` — A named list of plugin-specific parameters passed through to the jsPsych plugin for this frame. For textbox trials, the 'kind' argument (above) is automatically merged into this list — passing 'kind' both as a named argument AND inside pluginParams is an error. For future custom plugins (Cyberball etc.), pass their specific parameters here. DEFAULT = NULL.
- `trial_duration` — An integer that specifies how long the frame lasts before it ends on its own, in milliseconds. Three states: NULL means "not specified", and the frame ends when its stimulus does (it inherits stimulus_duration). This is the long-standing behavior and what every frame that does not mention trial_duration still gets. The sentinel "NO_LIMIT" removes the time limit entirely, so the frame ends only when the participant responds. This is the only way to hide a stimulus partway through (via stimulus_duration) while continuing to accept input, since the inheritance above otherwise ties the two durations together. A positive number is that many milliseconds, independent of stimulus_duration. A value LARGER than stimulus_duration gives a limited-exposure stimulus with a response window that outlives it. DEFAULT = NULL

**Returns.** the updated QCEframeList

### `addScenarioToQCEscenarioList`

This function is used to create or modify a QCEScenarioList

```r
addScenarioToQCEscenarioList(
  QCEScenarioList,
  QCEframeList,
  QCEfeebackList,
  QCEoutvariableList,
  setName,
  trigger = NULL,
  stimRef = NULL,
  showIf = NULL
)
```

Function that creates or modifys an QCEScenarioList by adding scenarios to the list one at a time.

- `QCEScenarioList` — A list that specifies all the possible scenarios that participants might see. A scenario is, essentially, a trial. It is composed of a series of frames, some potential response, and maybe feedback. Included in each scenario are an output variable list to code in the datafile and a setName that is used for presentation rules (see trialStructure.json). REQUIRED -- this argument has no default: pass NULL explicitly to start a new list, or pass the QCEScenarioList you are adding this scenario to.
- `QCEframeList` — A list that specifies the frames to show a participant in a single scenario. These frames are presented in succession: 1, 2, ... N.
- `QCEfeebackList` — A feedback list, from createFeedbackList(), giving the feedback shown for this scenario's response. Note the spelling of the argument name, which is retained for backward compatibility. Pass createFeedbackList() with no keys added for a scenario that shows no feedback.
- `QCEoutvariableList` — A list of variable names and their contents that will be output in the datafile for this trial. DEFAULT = NULL.
- `setName` — A string that specifies the name of the set that this scenario belongs to. Set names are used for selecting scenarios to show participants. The rules are set in trialStructure.json.
- `trigger` — Optional list produced by buildQCETriggerList() specifying the fNIRS trigger codes that fire at this trial's boundaries -- onset fires on the scenario's first frame; offset fires on the response frame. NULL means no trial-level triggers. Recommended code range: 1000-9999 (4 digits). DEFAULT = NULL.
- `stimRef` — Optional single string -- a data-lookup tag for this scenario. When this scenario completes at runtime, its trial data is recorded under this tag in qceState.conditions.dataIndex. Other scenarios/sets/blocks with a `showIf` referencing this stimRef will read that data. Most scenarios don't need a stimRef; only tag the ones whose data drives downstream conditional logic. DEFAULT = NULL.
- `showIf` — Optional condition (output of buildQCEshowIfCondition or buildQCEshowIfCompound) that gates whether this scenario runs at all. Evaluated at trial entry -- if FALSE, the scenario is skipped entirely. NULL means always show. DEFAULT = NULL.

**Returns.** the updated QCEScenarioList

### `addSurveyFrameToQCEframeList`

Add a SurveyJS survey frame to a QCEframeList

```r
addSurveyFrameToQCEframeList(
  QCEframeList = NULL,
  surveyModel,
  frameName = NULL,
  stimulus_duration = NULL,
  post_trial_gap = 300,
  background = "#FFFFFF",
  cursorVisible = TRUE,
  output = TRUE,
  trigger = NULL
)
```

Convenience builder for a `trialType = "survey"` frame. It serializes a
SurveyJS model (from `surveyModel`) into the frame's
`stimulus` as a JSON string and appends the frame to a QCEframeList,
exactly like `addFrameToQCEframeList` does for the core trial
types. The resulting scenario is added with
`addScenarioToQCEscenarioList` like any other.

- `QCEframeList` — An existing QCEframeList to append to, or NULL to start a new one. DEFAULT = NULL.
- `surveyModel` — A SurveyJS model list, normally from `surveyModel`. The serializer guarantees that collection properties (choices/rows/columns/rateValues/...) render as JSON arrays even when length 1.
- `frameName` — A single string naming this frame in the data file. NULL makes it "frame#" by position (consistent with `addFrameToQCEframeList`). DEFAULT = NULL.
- `stimulus_duration` — Integer ms to force-advance even without a submit, or NULL to wait for the participant's survey submit button (the normal case). DEFAULT = NULL.
- `post_trial_gap` — Integer ms of blank screen after the survey. DEFAULT = 300.
- `background` — Page background color (hex). DEFAULT = "#FFFFFF" (surveys are typically shown on a light background, unlike the black stimulus frames).
- `cursorVisible` — Boolean; surveys are mouse-driven so the cursor must be visible. DEFAULT = TRUE.
- `output` — Boolean; whether to record this frame's data. DEFAULT = TRUE.
- `trigger` — Optional buildQCETriggerList() output for fNIRS frame-level triggers. DEFAULT = NULL.

**Details.** 
The survey plugin (surveyTrialType.js, Phase 6) renders the model and records
one data column per question (matrix/multipletext questions flatten to one
column per cell). For the data to be saved you must (1) load the plugin by
passing `plugins = c("survey")` to `addSessionToSessionList`,
and (2) list the resulting column names in your fields.txt.

Survey blocks have no key input, so give the BLOCK `keyMapName = "none"`
(the engine's explicit no-keymap sentinel) in
`addBlockToQCETrialStructureList` so no keymap-instruction screen
fires before the survey.

**Returns.** The updated QCEframeList.

### `getSetnamesFromScenarioList`

This function is used to get all the set names from a QCEScenarioList

```r
getSetnamesFromScenarioList(QCEScenarioList = NULL)
```

Function that gets all the set names from a QCEScenarioList.

- `QCEScenarioList` — A list that specifies all the possible scenarios that participants might see. A scenario is, essentially, a trial. It is composed of a series of frames, some potential response, and maybe feedback. Included in each scenario are an output variable list to code in the datafile and a setName that is used for presentation rules (see trialStructure.json).

**Returns.** a vector containing setnames

## Survey models

### `surveyModel`

Build a complete SurveyJS model from pages

```r
surveyModel(..., showQuestionNumbers = "off", modelProps = NULL)
```

Assembles one or more pages into a top-level SurveyJS model -- the object
that `addSurveyFrameToQCEframeList` serializes into a survey
frame's stimulus.

- `...` — One or more page lists from `surveyPage`. These become the model's `pages`, in order. (You may also pass a single page; a one-page model is the common case.)
- `showQuestionNumbers` — SurveyJS numbering mode: "off", "on", or "onPage". DEFAULT = "off".
- `modelProps` — An optional named list of model-level SurveyJS properties to merge in (e.g. `list(showProgressBar = "top", title = "...")`). DEFAULT = NULL.

**Returns.** A named list representing a complete SurveyJS model.

### `surveyPage`

Build one SurveyJS page from questions

```r
surveyPage(name, ..., pageProps = NULL)
```

Groups questions into a SurveyJS page. A model can have one or many pages;
QCEP presents the whole model as a single survey trial.

- `name` — A single string -- the page name (appears in SurveyJS internals; not usually shown to participants).
- `...` — One or more question lists from `surveyQuestion` (or hand-built question lists). These become the page's `elements`, in order.
- `pageProps` — An optional named list of page-level SurveyJS properties to merge in (e.g. `list(title = "Part 1", description = "...")`). Kept separate from `...` so question objects and page settings never collide. DEFAULT = NULL.

**Returns.** A named list representing one SurveyJS page.

### `surveyQuestion`

Build one SurveyJS question (any type)

```r
surveyQuestion(type, name = NULL, title = NULL, isRequired = FALSE, ...)
```

Schema-agnostic builder for a single SurveyJS question. Every SurveyJS
question of every type is structurally `{type, name, title, ...props}`,
so this one function expresses ALL of them -- current and future -- by
passing arbitrary SurveyJS properties through `...`. There is no
per-type function to fall out of date.

- `type` — A non-empty single string -- the SurveyJS question type (e.g. "radiogroup", "rating", "matrix", "checkbox", "text", "html"). See `names(surveyTypeCatalog())` for the curated set; any other valid SurveyJS type also works.
- `name` — A single string -- the question's name. This becomes the data column (or column prefix, for matrix/multipletext) in the saved data. Required for input questions; optional for display-only types (html/image/expression). DEFAULT = NULL.
- `title` — A single string -- the question text shown to the participant. DEFAULT = NULL (SurveyJS falls back to showing the name).
- `isRequired` — A single boolean. When TRUE, SurveyJS forces an answer before the page can be submitted. Emitted only when TRUE. DEFAULT = FALSE.
- `...` — Any further SurveyJS properties for this question, passed through verbatim: `choices`, `columns`, `rows`, `rateMin`, `rateMax`, `rateValues`, `inputType`, `html`, `isAllRowRequired`, etc. Collection properties (choices/columns/rows/ rateValues/items/...) are guaranteed to serialize as JSON arrays even when length 1 -- see `addSurveyFrameToQCEframeList`.

**Details.** 
The result is validated SOFTLY against `surveyTypeCatalog`: a
typo'd type, or a known type missing a required property (e.g. a
`radiogroup` with no `choices`), emits a `warning` but is
still returned and serialized. QCEB never blocks a question on type grounds;
the authoritative checks are the engine's `validateTrialTypes` (is the
survey plugin loaded?) and SurveyJS's own in-browser schema validation.

**Returns.** A named list representing one SurveyJS question (an "element").

### `surveyTypeCatalog`

The SurveyJS question-type catalog used for soft validation + reference

```r
surveyTypeCatalog()
```

Returns the hand-curated catalog of SurveyJS question types that QCEB knows
about. It serves two purposes:

- it powers *soft* validation in `surveyQuestion` (a
`warning()`, never a `stop()`); and

- it is a built-in REFERENCE for someone unfamiliar with the SurveyJS
plugin -- each type documents its properties (common and uncommon) and
links to the authoritative SurveyJS API page.

QCEB is a thin, schema-agnostic passthrough over SurveyJS: an unknown
`type`, or an unknown property on a known type, still serializes and
ships. The catalog never limits which types or properties you can use; it
just surfaces common mistakes early and tells you what each type accepts.

**Details.** 
Each entry is a list with:

- `desc` — a one-line human description of the type.

- `required` — character vector of properties SurveyJS needs for the type
to render meaningfully (beyond the universal `type`/`name`). A
missing one triggers a soft warning.

- `properties` — a named list mapping each type-specific property (common
AND uncommon) to a one-line description. Reference only -- not enforced.

- `docUrl` — the authoritative SurveyJS API-reference URL for the type.

Properties shared by EVERY question type (name, title, visibleIf, isRequired,
validators, ...) are not repeated in each entry; see
`surveyUniversalProperties`.

Authoritative reference index:
https://surveyjs.io/form-library/documentation/api-reference/question.
Descriptions are a curated starting point and can drift across SurveyJS
releases -- follow each entry's `docUrl` for the definitive list.

**Returns.** A named list, one entry per known SurveyJS type.

### `surveyUniversalProperties`

Properties shared by every SurveyJS question type

```r
surveyUniversalProperties()
```

Many SurveyJS properties apply to ALL question types (name, title,
visibility, requiredness, validation, ...). Rather than repeat them in every
entry of `surveyTypeCatalog`, they are documented once here. They
may be passed to `surveyQuestion` for any `type` via
`...`.

**Details.** 
Authoritative reference (base Question class):
https://surveyjs.io/form-library/documentation/api-reference/question.

**Returns.** A list with `desc`, a named `properties` list (propertyName -> description), and `docUrl`.

## Tsfile — blocks, sets, and trial order

### `addBlockToQCETrialStructureList`

This function is used to create or modify a QCETrialStructureList

```r
addBlockToQCETrialStructureList(
  QCETrialStructureList = NULL,
  QCEsetInfoList,
  QCEblockIteratorList,
  blockNumber = -1,
  blockName = "blockName",
  trigger = NULL,
  showIf = NULL,
  switchRules = NULL,
  keyMapName = NULL,
  entryInstruction = NULL,
  excludePreviouslyPresented = NULL,
  showKeyMapInstruction = NULL
)
```

Function that creates or modifys an QCETrialStructureList by adding blocks to the list one at a time.

- `QCETrialStructureList` — A list that specifies how the trials will be presented in the experiment. This list specifies the selection of stimuli from stimFile.json, the ordering of stimuli, the blocking structure, etc. If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCETrialStructureList that you are adding an effect to. DEFAULT = NULL
- `QCEsetInfoList` — A list that specifies the the setInfo information. This includes, for each setName, : N (the number of trials per set) and selection (the method of selecting the scenarios from the QCEScenarioList).
- `QCEblockIteratorList` — A list that specifies the the block iteration information. This includes, for each block, the number of times to repeat each block, and the presentation order of the sets and trials.
- `blockNumber` — An integer specifying the ordinal position of this block relative to all the other blocks. If this is set to -1, then it will be randomly placed in the one of the non-specified positions. DEFAULT = -1.
- `blockName` — A string specifying the name of this block. It is used for the experimenter to identify block condition names. DEFAULT = "blockName".
- `trigger` — Optional list produced by buildQCETriggerList() specifying the fNIRS trigger codes that fire at this block's boundaries (onset before the first trial, offset after the last). NULL means no block-level triggers. Recommended code range: 1-99 (1-2 digits). DEFAULT = NULL.
- `showIf` — Optional condition (output of buildQCEshowIfCondition or buildQCEshowIfCompound) that gates whether this block runs. Evaluated at block entry -- if FALSE, the entire block is skipped. NULL means always run. DEFAULT = NULL.
- `switchRules` — Optional list of switch-rule lists (each from buildQCEswitchRule). Rules are sequential -- they fire in array order, advancing activeRuleIndex on each fire. Switch rules govern intra-block flow (set switching at threshold); they are scoped per-block (Phase 3 Decision 4). NULL means no switching. DEFAULT = NULL.
- `keyMapName` — Optional single string -- the name of a named keyMap (declared on the dbfile via addKeyMapToDbfile or the dbfile's `keyMaps` arg) that this block uses for its key-response trials. Three cases: (1) NULL / omitted -- the block falls back to the dbfile's legacy single keyMap (`mydB.keyMap`); (2) the reserved string "none" -- the block has NO keyMap (use this for survey/click-only blocks with no key-response trials, so no keymap-instruction screen fires and nothing is scored against a keyMap); (3) any other string -- the name of an entry in `mydB.keyMaps`. Phase 3.5 (Decision A/B): cascade resolves at IsBlockItFirst -- "none" -> no keyMap, else `mydB.keyMaps[name]` if name set, else `mydB.keyMap` default. Because R drops NULL list elements, NULL cannot serialize to JSON null; "none" is the R-emittable way to declare an explicit no-keymap block. DEFAULT = NULL.
- `entryInstruction` — Optional character vector of HTML filenames. Each fires as a jsPsychExternalHtml trial at block entry (one screen per file, in order). Phase 3.5 Decision D -- use for per-task framing screens ("Welcome to Part 2") that should display before the keymap-instruction screen and the first trial. Each HTML file must contain a button with id="Go" (current engine convention; Phase 5.5 forms refactor will harmonize). NULL means no block-level entry screens. DEFAULT = NULL.
- `excludePreviouslyPresented` — Optional single boolean. When TRUE, this block's trial pool is filtered at runtime to exclude any scenarioID the participant has already been shown anywhere in the experiment. Phase 3.5 Decision H -- extends the set-level flag (already on addSetToQCEsetInfoList) to apply uniformly across ALL sets in the block. Honored in both switchRules blocks (via buildSetNode cascade) and regular non-switchRules blocks (runtime trial-list filter at IsBlockItFirst). NULL means do not filter at block level. DEFAULT = NULL.
- `showKeyMapInstruction` — Optional single string controlling whether the keymap-instruction screen fires at THIS block's entry (the policy is block-based, not keyMap-based). Three values: "auto" (show only when this block's active keyMap differs from the last one shown -- the smart default that avoids re-showing an unchanged keymap); "always" (re-show even if unchanged -- use for consecutive blocks that share one keyMap but should each re-orient the participant); "never" (suppress even if the keyMap changed). Boolean TRUE/FALSE are accepted as aliases for "always"/"never" and normalized to the string form. In every mode the engine still requires the block to contain at least one key-response frame -- a survey/click-only block never shows a keymap screen. NULL / omitted means "auto", which is byte-identical to the pre-existing engine behavior. DEFAULT = NULL.

**Returns.** the updated QCETrialStructureList

### `addSetToQCEsetInfoList`

Add a single set's configuration to a QCEsetInfoList

```r
addSetToQCEsetInfoList(
  QCEsetInfoList = NULL,
  QCEScenarioList = NULL,
  setName,
  numberOfTrialsPerSet = 1,
  selectionType = "randomWithoutReplacement",
  trigger = NULL,
  showIf = NULL,
  excludePreviouslyPresented = NULL,
  entryInstruction = NULL
)
```

Adds one set entry to a QCEsetInfoList. Call this function once per set
you want in the trial structure.

- `QCEsetInfoList` — Existing QCEsetInfoList to append to. Pass NULL to start a new one. DEFAULT = NULL.
- `QCEScenarioList` — The QCEScenarioList that declares which setNames are valid.
- `setName` — A single string -- the name of the set being added. Must match a setName declared on at least one scenario in QCEScenarioList.
- `numberOfTrialsPerSet` — Single integer -- number of stimuli to select for this set. DEFAULT = 1.
- `selectionType` — String -- "randomWithoutReplacement" (default), "randomWithReplacement", or "fixed". See documentation for semantics.
- `trigger` — Optional list produced by buildQCETriggerList() specifying fNIRS trigger codes for this set's onset/offset. NULL means no triggers on this set. DEFAULT = NULL.
- `showIf` — Optional condition (output of buildQCEshowIfCondition or buildQCEshowIfCompound) that gates whether this set runs. Evaluated at set entry -- if FALSE, the entire set is skipped. NULL means always run. DEFAULT = NULL.
- `excludePreviouslyPresented` — Optional single boolean. When TRUE, this set's pool is filtered at runtime to exclude any scenarioID the participant has already been shown anywhere in the experiment. Honored on regular blocks as well as on sets built inside switchRules blocks: the flag is OR-ed with the block-level exclusion flag, so setting it here filters this set's pool even when the block itself does not ask for filtering. Phase 3 Decision 6 (Semantic C) -- the flag is a property of the destination set's pool, not of the firing rule, so any path that builds the set (rule firing, natural fallthrough, recursive next-natural) honors it consistently. NULL means do not filter. DEFAULT = NULL.
- `entryInstruction` — Optional character vector of HTML filenames. Each fires as a jsPsychExternalHtml trial at set entry (one screen per file, in order). Phase 3.5 Decision D / Decision 7 close-out -- use for per-set transition screens that should display before the set's first trial. The same screens fire whether the set is reached by rule-fire (executeSwitch path), natural fallthrough (on_timeline_finish path), or as the first set in a switchRules block, OR as a regular set in a non-switchRules block (IsSetFirst hook). Each HTML file must contain a button with id="Go" (current engine convention; Phase 5.5 forms refactor will harmonize). NULL means no set-level entry screens. DEFAULT = NULL.

**Details.** 
Note: prior to Phase 2 this function accepted a vector of setNames in
a single call. That multi-set form is now deprecated -- see
addSetToQCEsetInfoListOldDep for the legacy behavior.

**Returns.** The updated QCEsetInfoList, with the new set appended.

### `createBlockIteratorList`

This function creates a QCEblockIteratorList

```r
createBlockIteratorList(
  numberOfIterations = 1,
  randomizeTrialInSetOrder = TRUE,
  randomizeSetOrder = "randomAll",
  randomizeAllTrials = FALSE
)
```

Function that creates a QCEblockIteratorList .

- `numberOfIterations` — An integer specifying how many times the current block should be repeated. DEFAULT = 1
- `randomizeTrialInSetOrder` — A boolean that specifies whether the order of the trials in each set will be randomized on every iteration. TRUE = Randomize the order. FALSE = Present the trials in the order they were selected in the first iteration. DEFAULT = TRUE.
- `randomizeSetOrder` — A string that specifies how the sets shoud be randomized. "randomFirst" = the order of the sets in first iteration will be randomized, and the remaining iterations will be fixed to this order. "randomAll" = the order of the sets will be randomized. "fixed" = the order of the sets will be presented in the order they appear in setInfo. DEFAULT = "randomAll"
- `randomizeAllTrials` — A boolean that specifies whether all the trials from all the sets are placed in a large vector and are randomized. TRUE = Randomize all the trials, ignoring set boundaries. FALSE = the randomization follows the two rules above instead: `randomizeSetOrder` ("fixed", "randomFirst" or "randomAll") for the order of the sets, and `randomizeTrialInSetOrder` (TRUE or FALSE) for the order of the trials within each set. DEFAULT = FALSE

**Returns.** the blockIterator list

## Conditional display and dynamic rules (showIf, switch rules)

### `addBlockSwitchRulesToQCETrialStructureList`

Attach block-to-block switch rules to a QCETrialStructureList

```r
addBlockSwitchRulesToQCETrialStructureList(QCETrialStructureList, switchRules)
```

Adds a top-level `switchRules` key to an existing QCETrialStructureList. These
are BLOCK-to-BLOCK switch rules (Phase 4 Step 2): each rule (from
`buildQCEblockSwitchRule`) watches one block and, on crossing its
threshold, jumps forward to another block (or ends the session sequence).

- `QCETrialStructureList` — A QCETrialStructureList that already contains all of its blocks (built up via repeated addBlockToQCETrialStructureList calls). Required -- must be non-NULL with at least one block.
- `switchRules` — A list of block-switch-rule lists, each produced by `buildQCEblockSwitchRule`. Must contain at least one rule. Multiple rules may name the same watchBlock (branching, Decision 8).

**Details.** 
This is the block-scope analogue of the `switchRules` argument on
`addBlockToQCETrialStructureList`, which attaches SET-to-SET rules
INSIDE a single block. Block rules live at the trial-structure (session)
level, as a sibling of the numbered block entries, so they get their own
attach function rather than a per-block argument. The engine reads them from
the tsfile's top-level `switchRules` key (`trialStructure.switchRules`) and
validates them in `validateSessionSwitchRules` at session start.

**Call this LAST, after every block has been added.**
`addBlockToQCETrialStructureList` derives each new block's key from the
current list length, so if `switchRules` is already attached, a subsequently
added block would be mis-numbered. The QCETrialStructureList must already
contain at least one block (this function errors otherwise).

Validation here is SHAPE-only (each rule has a watchBlock, a valid count
condition, a valid threshold, and a well-formed switchToBlock if present).
Cross-references -- that watchBlock / switchToBlock name real blocks, that
every jump is forward-only, and that watched blocks have blockIterator.N == 1
-- are enforced by the engine at session start, where the full block list is
available. See `buildQCEblockSwitchRule` for the rationale.

**Returns.** the updated QCETrialStructureList, with a top-level `switchRules` element added.

### `buildQCEblockSwitchedCondition`

Build a block-fired-switch condition for QCEP showIf gating

```r
buildQCEblockSwitchedCondition(blockRef, operator)
```

Phase 3.5 Decision G: builds a showIf leaf condition that reads from the
engine's `qceState.blockSwitched[blockRef]` marker (stamped at runtime
the first time any switchRule fires in the named block). Use this to
gate downstream blocks or sets on whether an earlier block's switch
fired -- e.g., "only run T3 if T2's switch fired", "skip the bonus
block if the practice block didn't reach criterion".

- `blockRef` — Single non-empty string. The name of the block whose switch status the condition reads. Must match a `blockName` declared on an earlier addBlockToQCETrialStructureList call.
- `operator` — Single string. One of: switchFired, switchNotFired.

**Details.** 
Returned condition is interchangeable with buildQCEshowIfCondition's
output in any context that accepts a showIf condition -- you can mix
stimRef and blockRef leaves inside the same buildQCEshowIfCompound
all/any group, e.g.:

buildQCEshowIfCompound("all", list(
buildQCEshowIfCondition("consent", "equals", "Yes", "Response"),
buildQCEblockSwitchedCondition("Block_T2", "switchFired")
))

Static validation: the engine checks at session start that (a) blockRef
references a block declared in the same trial structure, (b) the
referenced block is declared BEFORE the gated scope (otherwise the
marker can't be stamped by evaluation time), and (c) the referenced
block declares at least one switchRule (warning, not error -- the
condition would otherwise always evaluate false).

**Returns.** A list of the form list(blockRef, operator), shape-compatible with QCEP's evaluateCondition blockRef leaf branch.

### `buildQCEblockSwitchRule`

Build a single block-to-block switch rule for QCEP dynamic experiments

```r
buildQCEblockSwitchRule(
  threshold,
  watchBlock,
  countResponse = NULL,
  countWhen = NULL,
  switchToBlock = NULL
)
```

Creates a block-switch-rule list in the shape consumed by the QCEP engine's
executeBlockSwitch (see customScripts/v9.1/dynamicEngine.js, Phase 4 Step 2).
The returned list is passed alongside other rules to the `switchRules`
parameter of `addBlockSwitchRulesToQCETrialStructureList`, which
attaches them at the TOP LEVEL of the trial structure (not on a block).

- `threshold` — A threshold spec, typically from buildQCEswitchThreshold. Required.
- `watchBlock` — Single non-empty string -- the name of the block this rule counts responses within (must match a `blockName` declared via addBlockToQCETrialStructureList; the engine validates existence). Required.
- `countResponse` — Single non-empty string. Sugar for matching the trial's Key field. Mutually exclusive with countWhen. DEFAULT = NULL.
- `countWhen` — A list `list(field, operator, value)` describing a more general count condition. Mutually exclusive with countResponse. DEFAULT = NULL.
- `switchToBlock` — Optional single non-empty string -- the destination block name. Must reference a `blockName` declared LATER than `watchBlock` (forward-only; the engine validates this at session start). NULL means "end the session's block sequence early" (Decision 9). DEFAULT = NULL.

**Details.** 
Block switch rules govern BLOCK-to-BLOCK flow: when a configurable count
condition has been met `threshold` times anywhere within the watched block
(whole-block scope, across whatever sets play), the rule fires -- it aborts
the rest of that block and jumps forward to a destination block (or ends the
session's block sequence). This is distinct from `buildQCEswitchRule`,
which governs SET-to-SET flow within a single block (destination = a set).
The naming convention is by DESTINATION: *set* switch rules switch to a
set (`switchToSet`); *block* switch rules switch to a block (`switchToBlock`).

Count condition: pick exactly one of two forms (same vocabulary as the
set-level builder):
- `countResponse` (sugar): a single string. Sugar for
`countWhen = list(field="Key", operator="equals", value=<x>)`.
Use this for the common 2AFC pattern ("count when participant
pressed Yes").
- `countWhen`: a list `list(field, operator, value)` with operator in
the showIf value-comparison vocabulary (equals, notEquals,
greaterThan, lessThan, greaterThanOrEqual, lessThanOrEqual,
contains). Use this for non-Key-based or non-equality counting
(e.g., RT < 500ms, NumberLine value > 5).

Terminal behavior (Decision 9):
- `switchToBlock` present: rule fires, the watched block ends early, and
the engine jumps FORWARD to the named block (skipping any blocks in
between). The destination block's own `entryInstruction` (if any) shows
on arrival, exactly as on a natural arrival.
- `switchToBlock` absent (NULL): rule fires, the watched block ends early,
and the engine ENDS the session's remaining block sequence (advancing to
the next session if the experiment has more). Mirror of the set-level
`switchToSet = NULL` early-stop, but at block scope.

Direction is FORWARD-ONLY (Decision 6): `switchToBlock` may target only a
block declared LATER than `watchBlock`. Backward jumps / revisits are
rejected. This is enforced by the engine at session start (not here -- see
"Validation split" below).

Branching (Decision 8): multiple rules MAY name the same `watchBlock`, each
with its own counter. All rules watching the current block are live
simultaneously; the first to cross threshold fires, with declaration order
breaking a same-trial tie. (The set-level builder forbids this; block rules
allow it because they are named by `watchBlock` rather than positional.)

Validation split: this builder validates only the SHAPE of the rule
(watchBlock present, count XOR, threshold shape, switchToBlock shape). It
deliberately does NOT check cross-references -- that `watchBlock` /
`switchToBlock` name real blocks, that the jump is forward-only, or that the
watched block has `blockIterator.N == 1`. The QCEP engine enforces all of
those in `validateSessionSwitchRules` at session start, where it has the full
trial structure in hand. This matches the set-level division of labor
(`buildQCEswitchRule` likewise defers `switchToSet` existence to the engine).

**Returns.** A list of the form list(watchBlock, threshold, countResponse|countWhen, switchToBlock) with only the supplied fields included (NULL defaults omitted).

### `buildQCEshowIfCompound`

Build a compound showIf condition (all / any) for QCEP dynamic experiments

```r
buildQCEshowIfCompound(kind, conditions)
```

Wraps a list of conditions (each from buildQCEshowIfCondition or another
buildQCEshowIfCompound for nesting) under an `all` or `any` aggregator,
matching the compound shape consumed by evaluateCondition in
customScripts/v9/dynamicEngine.js.

- `kind` — Single string: "all" (every child must be true) or "any" (at least one child must be true).
- `conditions` — A list() of conditions or compounds. Each element must have the right shape (output of buildQCEshowIfCondition or buildQCEshowIfCompound). At least one element required.

**Returns.** A list of the form list(all = list(c1, c2, ...)) or list(any = list(c1, c2, ...)).

### `buildQCEshowIfCondition`

Build a single showIf condition for QCEP dynamic experiments

```r
buildQCEshowIfCondition(stimRef, operator, value = NULL, field = NULL)
```

Creates a condition list in the shape consumed by the QCEP engine's
evaluateCondition (see customScripts/v9/dynamicEngine.js). The returned
list is passed to the `showIf` parameter of builder functions
(addScenarioToQCEscenarioList, addSetToQCEsetInfoList,
addBlockToQCETrialStructureList) or wrapped by buildQCEshowIfCompound
inside an `all`/`any` group.

- `stimRef` — Single string. The identifier of the scenario whose data the condition reads. Must match a `stimRef` declared on some scenario via addScenarioToQCEscenarioList.
- `operator` — Single string. One of: equals, notEquals, greaterThan, lessThan, greaterThanOrEqual, lessThanOrEqual, contains, wasShown, wasNotShown.
- `value` — The value to compare against. Required for value-comparison operators; ignored for wasShown/wasNotShown. DEFAULT = NULL.
- `field` — Single string -- the name of the data field on the referenced stimRef's trial data to read for comparison (e.g., "Response", "RT"). Required for value-comparison operators; ignored for wasShown/wasNotShown. DEFAULT = NULL.

**Details.** 
Operators fall into two groups:
- Value-comparison (require both `value` and `field`):
equals, notEquals, greaterThan, lessThan,
greaterThanOrEqual, lessThanOrEqual, contains
- Stimulus-presence (only `stimRef` and `operator`):
wasShown, wasNotShown

**Returns.** A list of the form list(stimRef, operator, value, field) for value-comparison operators, or list(stimRef, operator) for wasShown/wasNotShown.

### `buildQCEstateCondition`

Build a hook-state showIf condition (stateRef leaf) for QCEP

```r
buildQCEstateCondition(stateRef, operator, value = NULL)
```

Phase 5 (Custom Hooks): builds a showIf leaf condition that reads from the
engine's `qceState.custom[stateRef]` scratchpad -- the channel by which a
custom hook influences declarative flow. A hook (onTrialFinish, onBlockEnd,
etc.) writes a value into `ctx.qceState.custom[key]`; a downstream scenario,
set, or block gates itself on that value via this condition. This is how
"skip / branch based on something a hook computed" is expressed -- the
decision stays declarative and is captured in the engine's showIf log,
rather than a hook imperatively vanishing a trial.

- `stateRef` — Single non-empty string. The key the hook writes to `qceState.custom`. Should match a name in the dbfile's `customHooksStateKeys` when that list is declared.
- `operator` — Single string. One of: equals, notEquals, greaterThan, lessThan, greaterThanOrEqual, lessThanOrEqual, contains, isSet, isNotSet.
- `value` — The value to compare against. Required for value-comparison operators; ignored for isSet/isNotSet. DEFAULT = NULL.

**Details.** 
Operators fall into two groups:
- Value-comparison (require `value`):
equals, notEquals, greaterThan, lessThan,
greaterThanOrEqual, lessThanOrEqual, contains
- Presence (only `stateRef` and `operator`, no `value`):
isSet, isNotSet

Unlike buildQCEshowIfCondition there is no `field`: a stateRef reads the
value the hook stored at `qceState.custom[stateRef]` directly. A key the
hook has not set yet is "not set" -- `isSet` is false, `isNotSet` is true,
and value operators evaluate false (no error). A key set to 0 or FALSE
counts as set.

Returned condition is interchangeable with buildQCEshowIfCondition's and
buildQCEblockSwitchedCondition's output -- you can mix stimRef, blockRef,
and stateRef leaves inside the same buildQCEshowIfCompound all/any group.

Static validation: if the dbfile declares `customHooksStateKeys` (see
addHooksToQCEgroupDbFile), the engine errors at session start on a stateRef
not in that list (typo guard); if no keys are declared, it warns instead.

**Returns.** A list of the form list(stateRef, operator, value) for value-comparison operators, or list(stateRef, operator) for isSet/isNotSet.

### `buildQCEswitchRule`

Build a single switch rule for QCEP dynamic experiments

```r
buildQCEswitchRule(
  threshold,
  countResponse = NULL,
  countWhen = NULL,
  switchToSet = NULL,
  switchInstruction = NULL
)
```

Creates a switch-rule list in the shape consumed by the QCEP engine's
executeSwitch (see customScripts/v9/dynamicEngine.js). The returned list
is passed alongside other rules to the `switchRules` parameter of
addBlockToQCETrialStructureList.

- `threshold` — A threshold spec, typically from buildQCEswitchThreshold. Required.
- `countResponse` — Single non-empty string. Sugar for matching the trial's Key field. Mutually exclusive with countWhen. DEFAULT = NULL.
- `countWhen` — A list `list(field, operator, value)` describing a more general count condition. Mutually exclusive with countResponse. DEFAULT = NULL.
- `switchToSet` — Optional single non-empty string -- the destination set name. Must reference a setName declared in the same block's setInfo (the engine validates this at session start). NULL means "early-stop without redirect" (Decision 3). DEFAULT = NULL.
- `switchInstruction` — DEPRECATED 2026-05-24 (Phase 3.5 Chunk F / Decision 7 close-out). The QCEP engine no longer reads rule.switchInstruction; the field has been removed from the runtime path (rule.fire no longer pushes an instruction trial). The replacement is to declare entryInstruction on the DESTINATION SET via addSetToQCEsetInfoList(..., entryInstruction = c("file.html")). That declaration covers both rule-fire and natural-fallthrough paths uniformly via buildSetNode. If passed, this argument now emits a .Deprecated() warning and is dropped from the output rule (not serialized to JSON). DEFAULT = NULL.

**Details.** 
Switch rules govern intra-block flow: when a configurable count condition
has been met `threshold` times within the currently-watching set, the
rule fires -- it ends the current set early and (optionally) jumps to a
destination set. See `customScripts/v9/dynamicEngine.js` for runtime
semantics; the locked design decisions are summarized in
`DYNAMIC_EXPERIMENTS_PLAN.md` "JSON Schema Additions".

Count condition: pick exactly one of two forms (Decision 1 / hybrid):
- `countResponse` (sugar): a single string. Sugar for
`countWhen = list(field="Key", operator="equals", value=<x>)`.
Use this for the common 2AFC pattern ("count when participant
pressed Yes").
- `countWhen`: a list `list(field, operator, value)` with operator in
the showIf value-comparison vocabulary (equals, notEquals,
greaterThan, lessThan, greaterThanOrEqual, lessThanOrEqual,
contains). Use this for non-Key-based or non-equality counting
(e.g., RT < 500ms, NumberLine value > 5).

Decision 3 — terminal behavior:
- `switchToSet` present: rule fires, current set ends, destination set
is built + pushed. Block continues with the new set.
- `switchToSet` absent (NULL): rule fires, current set ends early; no
redirect. Block falls through to the next set in natural order
(early-stop). Useful for "run training until criterion, then proceed
normally."

Decision 6 (Semantic C) — `excludePreviouslyPresented` is NOT a switch-rule
parameter; it is a property of the destination set's pool, declared on
the set via `addSetToQCEsetInfoList(..., excludePreviouslyPresented = TRUE)`.

Rule sequencing (Decision 3 amendment, Bug Fix #20): Rules are sequential.
The order rules appear in `switchRules` is the order in which they watch
the natural set sequence: rule[1] watches the first set, rule[2] watches
the second, etc. A rule expires when its watched set ends -- either
because the rule fired (and switched), or because the set ended naturally
without the count threshold being reached. Once expired, a rule cannot
fire again, and counting in subsequent sets is governed by the next rule
(if any). If there are fewer rules than sets, sets past the last rule
run with no switching. Counting is per-rule -- each rule's counter
starts at 0 when it becomes active and is reset on rule expiration.

**Returns.** A list of the form list(threshold, countResponse, switchToSet, ...) with only the supplied fields included (NULL defaults are omitted).

### `buildQCEswitchThreshold`

Build a switch-rule threshold specification for QCEP dynamic experiments

```r
buildQCEswitchThreshold(values, rule = "fixed")
```

Creates a threshold spec in the shape consumed by the QCEP engine's
resolveThreshold (see customScripts/v9/dynamicEngine.js). The returned
list is passed to the `threshold` parameter of buildQCEswitchRule.

- `values` — Numeric vector. Length depends on rule: 1+ for "fixed" (only first element is used) and "randomFromList"; exactly 2 for "randomIntBetween" (interpreted as [lo, hi]).
- `rule` — Single string. One of "fixed", "randomFromList", "randomIntBetween". DEFAULT = "fixed".

**Details.** 
Three resolution rules are supported:
- "fixed": threshold = values[1]. Provide a single value.
- "randomFromList": threshold = sample(values, 1). Provide >= 1 value.
- "randomIntBetween": threshold = random int in [values[1], values[2]]
inclusive. Provide exactly 2 values.

Threshold is resolved per-rule when the manager initializes that rule,
so each session can see a different draw under randomFromList /
randomIntBetween (variability across participants).

**Returns.** A list of the form list(values, rule), shape-validated.

## Key maps and response keys

### `addKeyMapToDbfile`

Register a named keyMap on a QCEdbfile (incremental)

```r
addKeyMapToDbfile(QCEdbfile, name, entry)
```

Phase 3.5: appends one named keyMap entry to a QCEdbfile's `keyMaps`
dictionary. Mirrors the incremental pattern of addKeyToKeyMap +
addBlockToQCETrialStructureList + addSetToQCEsetInfoList -- pass NULL
or a partially-built dbfile and chain calls.

- `QCEdbfile` — A QCEdbfile (output of buildQCEgroupDbFile). Required.
- `name` — Single non-empty string -- the name blocks will use to reference this keyMap (via the new `keyMapName` argument on addBlockToQCETrialStructureList).
- `entry` — A keyMap entry list (output of buildQCEkeyMapEntry).

**Details.** 
Pair this with buildQCEkeyMapEntry to construct each entry. For one-shot
construction (declare all keyMaps at dbfile-build time), pass the same
entries as the `keyMaps` argument to buildQCEgroupDbFile; that path
delegates internally to this function in a loop.

Initialization: if the dbfile has no `keyMaps` field yet (the typical
case the first time this is called), the field is created. Subsequent
calls append.

Duplicate names: if a keyMap with the same name is already registered
on the dbfile, this function warns and overwrites. The warning catches
the researcher footgun of accidentally redefining a keyMap (typo in the
name, copy-pasted block that wasn't fully edited). To suppress the
warning intentionally, remove the existing entry first
(`QCEdbfile$keyMaps[[name]] <- NULL`).

**Returns.** The updated QCEdbfile, with the new keyMap registered under `$keyMaps[[name]]`.

### `addKeyToKeyMap`

This function adds a key definition to a QCEkeyMap

```r
addKeyToKeyMap(QCEkeyMap = NULL, keyMeaning, keys)
```

Function that creates or modifys an QCEkeyMap by adding a key Definition.

- `QCEkeyMap` — A list that specifies the maping of the keys to their meaning for the experiment. If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEkeyMap that you are adding an key definition to. DEFAULT = NULL
- `keyMeaning` — A single string that specifies the meaning of the keys. For example, in a target present/target absent experiment, the keyMeaning for one set of keys may be "targetPresent". DEFAULT = NULL
- `keys` — A vector of characters that specifies the keys that map to the keyMeaning. Often these are a single key and it's capital. For example, c("d", "D"). DEFAULT = NULL

**Returns.** the updated QCEkeyMap

### `buildKeyMap`

This function builds a QCEkeyMap from a dataframe

```r
buildKeyMap(dfKeys)
```

Function builds a QCEkeyMap from a dataframe.

- `dfKeys` — A dataframe in which each column specifies a key definition. The column name is the keyMeaning and the column contents contains caracters representing the keys that map to that meaning. The number of columns indicates the number of defined keys.

**Returns.** the QCEkeyMap

### `buildQCEkeyMapEntry`

Build a single named-keyMap entry for use on a QCEdbfile

```r
buildQCEkeyMapEntry(
  map,
  randomize = FALSE,
  presentAfterTrials = NULL,
  instructionFile = NULL
)
```

Phase 3.5: assembles one entry in `mydB.keyMaps` -- the dictionary the
engine reads at session init to populate the named-keyMap registry. Each
entry pairs a label-to-keys mapping with optional shuffle / reminder /
instruction-file settings. Blocks reference the entry by name via the
new `keyMapName` argument on addBlockToQCETrialStructureList.

- `map` — Required QCEkeyMap (output of buildKeyMap or addKeyToKeyMap). The label-to-keys dictionary, e.g., list(Yes = c('y','Y'), No = c('n','N')).
- `randomize` — Single boolean. When TRUE, the engine shuffles the label-to-keys mapping ONCE at session init -- every block that references this name sees the same shuffled instance. DEFAULT = FALSE.
- `presentAfterTrials` — Optional numeric vector. Block-relative scenario counts at which a keymap reminder fires (e.g., c(20, 50) fires after scenario 20 and scenario 50 of the block). Counter is per-keyMap, resets at block boundary (not iteration boundary), and increments only on scenarios containing a key-response frame. NULL means no reminders. DEFAULT = NULL.
- `instructionFile` — Optional single string -- filename of an HTML page describing this keyMap to the participant. The engine pushes this as a jsPsychExternalHtml trial at the first block that uses this keyMap (and at any subsequent block that switches TO this keyMap from a different one). The HTML file must contain a button with id="continue" (legacy convention; Phase 5.5 forms refactor will harmonize). When NULL, the engine generates a basic table from the map -- usable but limited. DEFAULT = NULL.

**Details.** 
Why named keyMaps exist: the locked Phase 3.5 design (Decision A) keeps
the shuffle of randomized keyMaps at the SESSION scope so multiple blocks
pointing at the same name share the same shuffled instance (within-subject
consistency). Declaring keyMaps on individual blocks would mean each block
shuffles independently -- two blocks with the same "yesNo" map randomized
independently would show Yes=y in one and Yes=n in the other. Named
keyMaps on the dbfile solve that.

Pair this with addKeyMapToDbfile to register the entry, or pass entries
as the `keyMaps` argument to buildQCEgroupDbFile for one-shot construction.

**Returns.** A named-keyMap entry list of the shape consumed by addKeyMapToDbfile / buildQCEgroupDbFile's keyMaps argument.

### `getKeyChoicesFromKeyMap`

This function returns a vector of all the allowable keys in a QCEkeyMap

```r
getKeyChoicesFromKeyMap(QCEkeyMap)
```

Function that returns a vector of all the allowable keys in a QCEkeyMap.

- `QCEkeyMap` — A list that specifies the maping of the keys to their meaning for the experiment.

**Returns.** the vector of all the allowable keys in the QCEkeyMap

### `reverseTwoChoiceFeedbackKey`

This function reverses keys assocaited with the display and outtext for a feedback_key

```r
reverseTwoChoiceFeedbackKey(feedback_key)
```

Function that reverses keys assocaited with the display and outtext for a feedback_key. This is useful to create a feedback_key for trials in which the opposite feedback is needed for the keys.

- `feedback_key` — A list that specifies the maping of the keys to their feedback text to display to the subject and the feedback text to print in the data file. This is the list that will be returned with the keys and output reversed

**Returns.** the reversed feedback_key

## Feedback

### `addKeyToFeedbackKeyList`

This function adds feedback information for a key in QCEkeyMap to a feedback_key list

```r
addKeyToFeedbackKeyList(
  feedback_key = NULL,
  QCEkeyMapKey = NULL,
  displayText = NULL,
  outputText = NULL
)
```

Function that creates or modifys an feedback_key list by adding feedback information for a key in QCEkeyMap.

- `feedback_key` — A list that specifies the maping of the keys to their feedback text to display to the subject and the feedback text to print in the data file. If you are building a new list, then this should be NULL. If you are adding a new key to an old list, then this should be the feedback_key that you are adding the feedback key information to. DEFAULT = NULL
- `QCEkeyMapKey` — A single key definition from the QCEkeyMap. This is the key that the feedback information will be defined for. For example, QCEkeyMap[[1]] will define the feedback information for the first key in the keyMap. This option must be input.
- `displayText` — A string that specifies the text to be displayed to the participant if they press this key in a trial containing this feedback_key list. The stimulus must be in html format. You can use any html codes. DEFAULT = NULL
- `outputText` — A single string that specifies the text to be printed in the datafile to code for the pressing of this key and it's meaning in relation to the trial. For example, in a target present/target absent experiment, the output for pressing this key might be, "correct" whereas the other key might be "incorrect" DEFAULT = NULL. NULL will result in the keyMeaning being output in the datafile.

**Returns.** the updated feedback_key

### `buildSpeedFeedbackList`

This function is used to build a speedFeedbackParamsList

```r
buildSpeedFeedbackList(
  showFeedback = TRUE,
  speedThresholdUp = TRUE,
  frameName = "test",
  thresholdWindowSize = 20,
  thresholdArrayPosition = 17,
  thresholdBoundary = NULL,
  currentRTWindowSize = 1,
  initialWindowRtThreshold = NULL,
  initialWindowMessageDisplay = "&nbsp",
  initialWindowMessageOutput = "initial",
  initialStimulusDuration = 1,
  initialPostTrialGap = 1,
  aboveThresholdMessageDisplay = NULL,
  aboveThresholdMessageOutput = "above",
  aboveStimulusDuration = NULL,
  abovePostTrialGap = NULL,
  belowThresholdMessageDisplay = NULL,
  belowThresholdMessageOutput = "below",
  belowStimulusDuration = NULL,
  belowPostTrialGap = NULL
)
```

Function that builds a speedFeedbackParamsList.

- `showFeedback` — A boolean that specifies whether to show the speedFeedback. This is not a trial-by-trial variable. It applies for the entire experiment. So, set it to true if you want to give speedFeedback. Set it to false if you do not want to give speedFeedback. Default = TRUE.
- `speedThresholdUp` — A boolean that specifies whether the speedFeedback is intended to speed the participant's responses up (make them faster) or slow the particpant down (make them slower). Default = TRUE (make the participant faster).
- `frameName` — A string specifying the name of the frame that is want to give speed feedback on. This frameName should be unique and different from the frames that you do not want to provide speed feedback on. DEFAULT = "test"
- `thresholdWindowSize` — An integer specifying the number of frames (i.e., trials) on which you want to base your speedFeedback threshold. The program will take the most recent "thresholdWindowSize" frames to create the RTwindowArray. The RTwindowArray will be used, in part, to determine the RT threshold of the next trial. DEFAULT = 20.
- `thresholdArrayPosition` — An integer specifying the array position in the RTwindowArray, sorted from fastest to slowest, to use as the new potential RT threshold. So, if the thresholdArrayPosition = 18 and thresholdWindowSize = 20, then the 18th slowest RT would be the new potential RT threshold. DEFAULT = 17.
- `thresholdBoundary` — An integer specifying a hard boundary that the RT threshold cannot cross. If speedThresholdUp = TRUE, then the RT threshold cannot go below this boundary. If speedThresholdUp = FALSE, then the RT threshold cannot go above this boundary. DEFAULT = NUll. If NULL, then: thresholdBoundary = 250 if speedThresholdUp = TRUE and thresholdBoundary = 40000 if speedThresholdUp = FALSE.
- `currentRTWindowSize` — An integer specifying the number of frames (i.e., trials) on which you want to base your current trial RT. The program will take the average RT of the most recent "currentRTWindowSize" frames to create the current trial RT. The current trial RT will be compared to the RT threshold. If currentRTWindowSize = 1, then the current trial alone is compared to the RT threshold. If it is greater than 1, then the moving average of the currentRTWindowSize is compared to the RT threshold. currentRTWindowSize must be less than or equal to thresholdWindowSize. DEFAULT = 1 (the current trial alone).
- `initialWindowRtThreshold` — An integer specifying the initial value of the RT threshold while the first "thresholdWindowSize" trials are being accumulated. DEFAULT = NULL. If NULL then: if speedThresholdUp = TRUE, then the initialWindowRtThreshold = 999999 (very high so it is not activated). If speedThresholdUp = FALSE, then initialWindowRtThreshold = -1 (very low so it is not activated).
- `initialWindowMessageDisplay` — A string that specifies the message to be shown to the participant after every trial during the initial period while the first "thresholdWindowSize" trials are being accumulated. The string must be in html format. You can use any html codes. DEFAULT = "&nbsp" (blank).
- `initialWindowMessageOutput` — A string that will be output in the datafile every trial during the initial period while the first "thresholdWindowSize" trials are being accumulated. DEFAULT = "initial".
- `initialStimulusDuration` — An integer specifying the time, in ms, that the feedback message should be presented during the initial period while the first "thresholdWindowSize" trials are being accumulated. DEFAULT = 1 (not noticable)
- `initialPostTrialGap` — An integer specifying the time, in ms, between the feedback message and the next frame during the initial period while the first "thresholdWindowSize" trials are being accumulated. DEFAULT = 1 (not noticable)
- `aboveThresholdMessageDisplay` — A string that specifies the message to be shown to the participant when the current trial's RT is above the RT threshold. The string must be in html format. You can use any html codes. DEFAULT = NUll. If NULL, then the message will be "Too Slow" if speedThresholdUp = TRUE and "&nbsp" if speedThresholdUp = FALSE.
- `aboveThresholdMessageOutput` — A string that will be output in the datafile when the current trial's RT is above the RT threshold. DEFAULT = "above".
- `aboveStimulusDuration` — An integer specifying the time, in ms, that the feedback message should be presented when the current trial's RT is above the RT threshold. DEFAULT = NUll. If NULL, then aboveStimulusDuration = 500 if speedThresholdUp = TRUE and aboveStimulusDuration = 1 if speedThresholdUp = FALSE.
- `abovePostTrialGap` — An integer specifying the time, in ms, between the feedback message and the next frame when the current trial's RT is above the RT threshold. DEFAULT = NUll. If NULL, then abovePostTrialGap = 500 if speedThresholdUp = TRUE and abovePostTrialGap = 1 if speedThresholdUp = FALSE.
- `belowThresholdMessageDisplay` — A string that specifies the message to be shown to the participant when the current trial's RT is below the RT threshold. The string must be in html format. You can use any html codes. DEFAULT = NUll. If NULL, then the message will be "Too Fast" if speedThresholdUp = FALSE and "&nbsp" if speedThresholdUp = TRUE.
- `belowThresholdMessageOutput` — A string that will be output in the datafile when the current trial's RT is below the RT threshold. DEFAULT = "below".
- `belowStimulusDuration` — An integer specifying the time, in ms, that the feedback message should be presented when the current trial's RT is below the RT threshold. DEFAULT = NUll. If NULL, then belowStimulusDuration = 500 if speedThresholdUp = FALSE and belowStimulusDuration = 1 if speedThresholdUp = TRUE.
- `belowPostTrialGap` — An integer specifying the time, in ms, between the feedback message and the next frame when the current trial's RT is above the RT threshold. DEFAULT = NUll. If NULL, then belowPostTrialGap = 500 if speedThresholdUp = FALSE and belowPostTrialGap = 1 if speedThresholdUp = TRUE.

**Returns.** the speedFeedbackList

### `createFeedbackList`

This function creates a QCEfeebackList

```r
createFeedbackList(
  feedback_key = NULL,
  showFeedback = FALSE,
  stimulus_duration = 500,
  post_trial_gap = 500
)
```

Function that creates a QCEfeebackList .

- `feedback_key` — A list that specifies the maping of the keys to their feedback text to display to the subject and the feedback text to print in the data file. If feedback_key = NULL, then showFeedback will be set to FALSE. DEFAULT = NULL
- `showFeedback` — A boolean that specifies whether to show feedback to the participant after each trial. DEFAULT = FALSE.
- `stimulus_duration` — An integer that specifies how long to present the feedback in milliseconds. A NULL will present the stimulus until their is a user input. DEFAULT = 500
- `post_trial_gap` — An integer that specifies how long to present a blank frame after this frame in milliseconds. DEFAULT = 500

**Returns.** the QCEfeebackList

## Dbfiles — experiment- and group-level settings

### `addHooksToQCEgroupDbFile`

Register a custom-hooks file on a QCEdbfile (Phase 5)

```r
addHooksToQCEgroupDbFile(
  QCEdbfile,
  customHooksFile,
  customHooksStateKeys = NULL,
  customHooksColumns = NULL
)
```

Phase 5 (Custom Hooks): declares the researcher-authored JavaScript hooks
file for a group (session), plus the optional list of state keys those
hooks will write to `qceState.custom`. Mirrors the incremental pattern of
addKeyMapToDbfile -- pass a partially-built dbfile (output of
buildQCEgroupDbFile) and chain the call, or declare the same values at
dbfile-build time via the `customHooksFile` / `customHooksStateKeys`
arguments to buildQCEgroupDbFile (that path delegates here).

- `QCEdbfile` — A QCEdbfile (output of buildQCEgroupDbFile). Required.
- `customHooksFile` — Single non-empty string ending in `.js` -- the filename of the hooks file (relative to the experiment directory).
- `customHooksStateKeys` — Optional character vector of state-key names the hooks will write to `qceState.custom`, used for static validation of `stateRef` showIf conditions. NULL means no declared keys (the engine warns rather than errors on unknown stateRefs). DEFAULT = NULL.
- `customHooksColumns` — Optional character vector of the data columns the hooks will write via `dataAnnotations`. Declaring them lets buildQCEoutputFieldManifest report them and missingQCEoutputFields fail a build when `fields.txt` does not carry one -- without this the columns are invisible to every check, because they exist only inside JavaScript. NULL means undeclared. DEFAULT = NULL.

**Details.** 
The hooks file is a `.js` file that, when loaded by the engine, defines a
global `QCEPHooks` object with any of the recognized hook functions
(onTrialStart, onTrialFinish, onSetEnd, onBlockEnd, onSessionEnd). Use
saveCustomHooksTemplate() to scaffold a starter file. The file must be
copied into the experiment directory alongside the other preload assets.

`customHooksColumns` is the companion for data: the set of column names the
hooks promise to write through `dataAnnotations`. A hook can write any column
it likes at run time and nothing in the config files can see it, so a column
left out of `fields.txt` is dropped when the data is saved and the run still
looks clean. Declaring them here is what lets the build catch that.

`customHooksStateKeys` is the set of keys the hooks promise to write into
`qceState.custom`. The engine uses this list for static validation: any
`stateRef` in a showIf condition (built with buildQCEstateCondition) that
is NOT in this list is flagged as a likely typo at session start. Omit it
(NULL) to skip that check -- the engine then only warns rather than errors
on unknown stateRefs.

Backwards compatibility: a dbfile with no `customHooksFile` declared
produces byte-identical JSON to a pre-Phase-5 dbfile (no `customHooksFile`
/ `customHooksStateKeys` keys in the output), and the engine takes the
legacy no-hooks code path.

**Returns.** The updated QCEdbfile, with `$customHooksFile` set (and `$customHooksStateKeys` / `$customHooksColumns` set when supplied).

### `buildQCEexpDbFile`

This function is used to create a QCEB dbfile for the entire experiment

```r
buildQCEexpDbFile(
  expName = "defaultExpName",
  addQualtricsCode = FALSE,
  defaultBackgroundColor = "#000000",
  restAfterEveryNTrials = -1,
  instructionFile = NULL,
  getUserNameFile = NULL,
  getConsentFile = NULL,
  getDemographicsFile = NULL,
  getGenderFile = NULL,
  welcomeMsg = NULL,
  restMsg = NULL,
  endOfSessionMsg = NULL,
  endOfExpMsg = NULL,
  saveMsg = NULL,
  closeBrowserMsg = NULL,
  fullscreenMsg = NULL,
  fullscreenBtn = "Continue",
  completionRedirect = NULL,
  saveDataEveryNTrials = 50,
  completionGate = NULL,
  maxExperimentMinutes = NULL,
  saveTimeoutMs = NULL,
  saveCanary = NULL,
  saveUnavailableMsg = NULL,
  warnOnLeave = NULL,
  strictGroupAssignment = NULL,
  creditClaimTimeoutMs = NULL
)
```

Function that create a QCEB dbfile.

- `expName` — A string specifying the name of the experiment. It will be output in a column in the datafile. DEFAULT = "defaultExpName"
- `addQualtricsCode` — a Boolean that specifies whether to present a time code at the end of the experiment with a message that states asks the user to input the code in a Qualtrics window. This is useful if you want to run the experiment using Qualtrics to randomize conditions and/or assign automatic credits. DEFAULT = FALSE.
- `defaultBackgroundColor` — an rgb color that specifies the default background color of the experiment pages. DEFAULT = "#000000" (black). INERT: the value is accepted and written to the config for backward compatibility, but current engines do not act on it -- the background color of every screen comes from that frame's own `background`.
- `restAfterEveryNTrials` — An integer or vector of integers that specify the trial numbers that you want a break to occur after (e.g., 50, 100, 150). DEFAULT = -1. If -1, then no break will be shown. INERT: the value is accepted and written to the config for backward compatibility, but current engines do not act on it -- rest breaks are configured at the block level.
- `instructionFile` — A string or vector of strings that specifies the name of the html file(s) that contains the instructions. It will be shown at the begining of the experiment. If you have multiple instruction files, they should be entered in the order you would like them presented. If this is NULL, then no instructions will be shown. DEFAULT = NULL.
- `getUserNameFile` — A string that specifies the name of the html file that collects the user's identifying information (e.g., a random number). DEFAULT = NULL. INERT: the value is accepted and written to the config for backward compatibility, but current engines do not act on it -- no such screen is presented.
- `getConsentFile` — A string that specifies the name of the html file that collects the users consent for participating. It will be shown at the begining of the experiment. If this is NULL, then this information will not be collected. DEFAULT = NULL.
- `getDemographicsFile` — A string that specifies the name of the html file that collects the users demongraphic information (e.g., age). It will be shown at the begining of the experiment. If this is NULL, then this information will not be collected. DEFAULT = NULL.
- `getGenderFile` — A string that specifies the name of the html file that collects the users gender information. It will be shown at the begining of the experiment. If this is NULL, then this information will not be collected. DEFAULT = NULL.
- `welcomeMsg` — A string that specifies the welcome message to be shown at the beginning of the experiment. The string must be in html format. You can use any html codes. DEFAULT = NULL. If NULL, then the following message will be presented, "Welcome to the experiment. Press any key to begin."
- `restMsg` — A string that specifies the rest message to be shown at the beginning of a break. The string must be in html format. You can use any html codes. DEFAULT = NULL. INERT at this level: the value is accepted and written to the config for backward compatibility, but current engines do not act on it -- rests, and their message, are configured at the block level.
- `endOfSessionMsg` — A string that specifies the end of a session when a new session is comming up message. The string must be in html format. You can use any html codes. DEFAULT = NULL. If NULL, then the following message will be presented, "You have just completed the block. Please press any key to start to the next block."
- `endOfExpMsg` — A string that specifies the end of experiment message to be shown at the end of the experiment. The string must be in html format. You can use any html codes. DEFAULT = NULL. If NULL, then the following message will be presented, "Thank you for taking part in the experiment."
- `saveMsg` — A string that specifies the data is saving message to be shown at the end of the experiment. The string must be in html format. You can use any html codes. DEFAULT = NULL. If NULL, then the following message will be presented, "Your data is being saved. Please do not close this window until you are told to. Please press any key to continue."
- `closeBrowserMsg` — A string presented on its own final screen, after the end-of-experiment message, telling the user that they may close the browser. The screen accepts no keypress and remains until the window is closed, so this message should not instruct the user to press a key. It is shown only when no completion redirect will fire, so it never appears on a run that navigates the user elsewhere. The string must be in html format. You can use any html codes. DEFAULT = NULL. If NULL, then the following message will be presented, "You may now close this browser window."
- `fullscreenMsg` — A string that specifies a message that clicking the button will put the experiment into full screen mode. The string must be in html format. You can use any html codes. DEFAULT = NULL. If NULL, then the following message will be presented, "The experiment will switch to full screen mode when you press the button below."
- `fullscreenBtn` — A string that specifies a the text to put on the button in full screen mode. DEFAULT = "Continue".
- `completionRedirect` — A string that specifies the return URL that redirects the participant to another site - usually for credit participating (e.g., Prolific). It must be a proper URL. For example, "https://app.prolific.co/submissions/complete?cc=XXXXXXX" If the redirect is for SONA systems, the redirect must take the sona ID as an argument. The program will work if you change the "survey_code" equal to SONA_ID. For example, "https://www.sona-systems.com/webstudy_credit.aspx?experiment_id=769&credit_token=e05ef9d2f821414180dbb0b3f4ae3e59&survey_code=SONA_ID" If it is not appropriate to redirect, omit the argument (or pass NULL), which leaves the key out of the emitted JSON entirely. DEFAULT = NULL.
- `saveDataEveryNTrials` — A single positive integer specifying how often (in trials) the data is incrementally saved to the server during the experiment. The final save always occurs at the end regardless of this value. DEFAULT = 50.
- `completionGate` — Optional named list gating the end-of-experiment completion redirect (e.g. a SONA credit URL) on an EXPERIMENT-WIDE criterion computed over the whole-run data (every trial from every session). Use exactly ONE of two forms. (1) Formula: `formula` = a list of flat formulas, each a list with `fn` (one of "mean", "median", "proportion", "count", "sum", "min", "max", "sd"), `column` (a data column name, e.g. "correct" or "rt"), `op` (one of ">=", "<=", ">", "<", "==", "!="), `value` (a single finite number; for `proportion` in [0,1]), and optional `where` (a named list of column filters, each a scalar for equality or a list(op=, value=) for a range; an ordering op requires a finite numeric value. Equality normalizes booleans to 1/0, so TRUE/1 and FALSE/0 are interchangeable and a boolean column filters the same way it aggregates; strings are NOT coerced, so "1" does not match TRUE); plus optional `combinator` = "all" (default) or "any". proportion(column) = mean of the column coerced to 0/1; count(column) = number of non-missing rows. (2) Escape hatch: `gateFn` = a single string naming a global JS function called with (custom, experimentData) that returns a boolean. `experimentData` is a PLAIN JavaScript ARRAY of trial row objects (every trial from every session), not a jsPsych DataCollection -- so use standard array methods (filter/map/reduce), not DataCollection query methods such as select(). A gateFn needing the DataCollection API can obtain it directly via myJsPsych.data.get(), which is global. Both forms accept an optional `noCreditMsg` (HTML shown on a fail). THE DENIAL SCREEN IS COMPOSED: `noCreditMsg` is your whole message, and the engine adds only the way off the screen -- either "Press Enter to finish." or, where another attempt is available, the retry question. So `noCreditMsg` must NOT end by telling the participant to press a key: the engine appends that line itself when it is true, and the retry question is answered with the mouse, so a message naming the key either prints it twice or names a key that does nothing. Two TOKENS may appear anywhere in `noCreditMsg` or in `retryPrompt$text` and are filled in by the engine: `{score}` (the aggregate this run produced) and `{threshold}` (the requirement, worded from the operator -- "more than 80%", "80% or less", "exactly 4"). They are the only route by which a number reaches a denied participant, because the engine writes no sentence of its own about why. They resolve only when exactly ONE criterion failed -- with several failing at once there is no single score to report -- and an UNRESOLVED token is never left on screen: the engine removes the entire HTML paragraph (the <p> block) that the token sits in, deletes any stray token that sits outside such a block, and, if that leaves nothing at all, substitutes a generic completion message. So any text the participant must ALWAYS see belongs in a block that contains no token. Both numbers are displayed at a fixed precision (a whole percent for `proportion`, two decimal places otherwise), and for the ATTAINMENT operators -- ">=" and ">" -- a run passes if EITHER the rounded or the raw comparison passes, so rounding there can only ever work in the participant's favour. The remaining operators ("<=", "<", "==", "!=") compare the RAW value alone; no rounded second chance applies to them. Where the displayed score and threshold would print identically despite actually differing, the engine widens the displayed precision, or removes that sentence from the message, rather than showing the participant a contradiction. The engine evaluates the gate ONCE after all sessions; on failure it suppresses the redirect and shows the no-credit message. AN UNEVALUABLE CRITERION IS EXCLUDED, NOT FAILED: a formula that cannot be evaluated (unknown column, wrong-type value) is dropped from the decision and logged as a degrade warning, and the criteria that DID evaluate soundly decide on their own, under either `combinator`. If NO criterion evaluates soundly, nothing is left to decide with and the gate GRANTS, with a warning -- it fails OPEN, not closed. A `gateFn` that is missing, throws, or returns a non-boolean grants the same way. Two situations are failed criteria rather than degrades and therefore DENY: a `where` filter whose named columns all exist but which matches no rows, and a run cut short by the whole-experiment time limit. One case never fails open: where scoring is delegated to the server, an unevaluable gate WITHHOLDS the verdict instead of granting it. NULL means no gate (the redirect fires unconditionally, as before). Four further entries are accepted by BOTH forms. Three govern repeat attempts. `attemptsAllowed` = a whole number of 1 or more, with NO upper limit: how many times one participant may be admitted to this material. ABSENT MEANS ONE, which is how every gated study behaved before attempts existed, so an older config is unchanged. `retryPrompt` = a named list with `text`, `yesLabel` and `noLabel`, the question a participant is asked after a failing run to offer the next attempt. The two are only meaningful together and are cross-checked: allowing more than one attempt without a prompt makes the extra attempts unreachable, and offering a prompt while allowing one attempt promises something that cannot be granted. `supersededMsg` = HTML shown when the claim finds the record already settled by a later run. The fourth, `duplicateMsg`, is HTML shown when the credit ledger refuses a run it has already credited. Neither of those two may contain `{score}` or `{threshold}`: both are shown when the LEDGER refused rather than the gate, so the gate's numbers are not part of that decision and nothing can fill them in. Like `noCreditMsg`, neither should tell the participant to press a key. DEFAULT = NULL.
- `maxExperimentMinutes` — Optional single positive number: a generous WHOLE-EXPERIMENT wall-clock cap in minutes, stamped once at experiment start. Once exceeded, the engine skips remaining stimuli at stimulus boundaries and ends the run gracefully (into the completion gate + save/end nodes). A backstop against leaving the tab open indefinitely; set well above the task's expected length. NULL means uncapped. DEFAULT = NULL.
- `saveTimeoutMs` — Optional single positive number: the per-request timeout in milliseconds applied to every data-save POST. A save that neither succeeds nor fails within this window is treated as a failure so the serialized save chain proceeds instead of hanging behind it. Set below any final-save watchdog. NULL uses the engine default (20000). DEFAULT = NULL.
- `saveCanary` — Optional single Boolean gating the start-of-run save health check. When enabled (the engine default), the experiment probes that the save path is writable BEFORE building the timeline and halts the participant before any work if it is not -- bounding a save outage on an unattended run to the cohort already in flight rather than crediting empty runs. Set FALSE to opt a run out. NULL uses the engine default (enabled). DEFAULT = NULL.
- `saveUnavailableMsg` — A string shown on the terminal halt screen when the start-of-run save canary fails. The string must be in html format. You can use any html codes. NULL uses the engine default, which asks the participant to close the window and try again in about 24 hours. DEFAULT = NULL.
- `warnOnLeave` — Optional single Boolean gating the browser's leave-the-page confirmation during a run. When enabled (the engine default), closing the tab or navigating away raises the browser's own "leave site?" dialog, so a participant does not discard an in-progress run with one stray click. The guard is armed only once the experiment itself begins -- the preliminary screens and the file loading are free to leave, and guarding them is noise that teaches participants to dismiss the dialog -- and it is released when the run ends, so it never fires on the final screens. The dialog's wording is fixed by the browser and cannot be set from configuration; this option only turns it on or off. Set FALSE to opt a run out. NULL uses the engine default (enabled). DEFAULT = NULL.
- `strictGroupAssignment` — Optional single Boolean controlling what a multi-group experiment does when it cannot obtain a group assignment from the server. Server-side assignment is what makes the chosen group durable across a reload and what lets the server withhold groups a participant has already completed. When strict, a run that cannot obtain one refuses to start and tells the participant that nothing has been recorded and they may try again; when not strict (the engine default), it falls back to drawing a group in the browser, which is how multi-group experiments behaved before assignment existed but leaves the choice recorded nowhere. Has no effect on a single-group experiment, which never asks the server. Strict is forced on regardless of this setting for repeat-session links, where the recorded group is part of the credit key. Set TRUE to opt in. NULL uses the engine default (not strict). DEFAULT = NULL.
- `creditClaimTimeoutMs` — Optional single number, at least 1000: the timeout in milliseconds on the credit claim, the one request that writes the credit record and returns the grant-or-deny verdict at the end of a gated run. NULL uses the engine default (10000), which is the right choice unless a deployment is known to be slow. ⚠ A value the browser cannot use does not relax the timeout, it REMOVES it -- the underlying field treats zero as "no limit" -- and an unbounded claim against a server that accepts the connection and never answers leaves the participant on a blank screen with the final save unrun. A very small value fails the other way: every claim times out, and the claim fails open, so credit is granted with no record written. Both are refused here. DEFAULT = NULL.

**Returns.** the QCEBdbfileList

### `buildQCEgroupDbFile`

This function is used to create a QCEB dbfile

```r
buildQCEgroupDbFile(
  condName = "defaultCond",
  keyMap = NULL,
  randomizeKeyMap = FALSE,
  presentKeyMapAfterTrialNumbers = -1,
  defaultBackgroundColor = "#000000",
  restTrials = -1,
  speedFeedbackParams = NULL,
  instructionFile = NULL,
  keyMapInstructionFile = NULL,
  restMsg = NULL,
  friendlyReminderMsg = NULL,
  remindMsg = NULL,
  proceedMsg = NULL,
  enableTriggers = FALSE,
  triggerRelayPort = 5678,
  restEveryNMinutes = NULL,
  restMaxTrial = NULL,
  keyMaps = NULL,
  customHooksFile = NULL,
  customHooksStateKeys = NULL,
  customHooksColumns = NULL
)
```

Function that create a QCEB dbfile.

- `condName` — A string specifying the condition of that this dbfile represents. It is really just a placeholder that you can use to code anything that you want. It will be output in a column in the datafile. DEFAULT = "defaultCond"
- `keyMap` — A list that specifies the mapping of the keys to their meaning for the experiment. Create this list using the buildKeyMap() and addKeyToKeyMap() functions. DEFAULT = NULL
- `randomizeKeyMap` — a Boolean that specifies whether the maping of the keys to their meaning should be randomized everytime the experiment is run. This is useful if you want to randomize the key to meaning mapping for every subject on a single session experiment. DEFAULT = FALSE.
- `presentKeyMapAfterTrialNumbers` — An integer or vector of integers that specify when the participant will be reminded of the keyMap. The keyMap reminder message will show up after each trial number specified in the option. So, if you want the keyMap reminder to show up after the first and fifth trial, the option should equal c(1,5). DEFAULT = -1. If -1, then no reminder will be shown.
- `defaultBackgroundColor` — an rgb color that specifies the default background color of the experiment pages. DEFAULT = "#000000" (black).
- `restTrials` — An integer or vector of integers that that specify the trial numbers that you want a break to occur after (e.g., 50, 100, 150). DEFAULT = -1. If -1, then no break will be shown.
- `speedFeedbackParams` — A speedFeedbackList that specifies the parameters of the speed Feedback. Create this list using the buildSpeedFeedbackList() function. DEFAULT = NULL. If NULL, no speed feedback will be provided.
- `instructionFile` — A string or vector of strings that specifies the name of the html file(s) that contains the instructions. It will be shown at the begining of the experiment. If you have multiple instruction files, they should be entered in the order you would like them presented. If this is NULL, then no instructions will be shown. DEFAULT = NULL.
- `keyMapInstructionFile` — A string that specifies the name of the html file that contains the mapping between the keys and their meaning (e.g., "Press the "d" key to indicate YES). It will be shown at the begining of the experiment. DEFAULT = NULL. If NULL, the engine generates the key map screen itself from the keyMap you supplied, so most experiments should leave this alone. Supply a filename only when you want to replace that generated screen with your own html. NOTE: the string "default" is NOT accepted -- it was a sentinel in engine versions before 9.1, and the current engine reads this field as a literal filename, so "default" would send it looking for a file of that name. Use NULL to mean "generate it for me".
- `restMsg` — A string that specifies the rest message to be shown at the beginning of a break. The string must be in html format. You can use any html codes. DEFAULT = NULL. If NULL, then the following message will be presented, "Please take a self-timed break. Press any key to resume the experiment."
- `friendlyReminderMsg` — A string that specifies the "this is a friendly reminder" message to be shown when presenting the keymap reminder. The string must be in html format. You can use any html codes. DEFAULT = NULL. If NULL, then the following message will be presented, "This is a friendly reminder."
- `remindMsg` — A string that specifies a message that the keymap reminder might be shown again. The string must be in html format. You can use any html codes. DEFAULT = NULL. If NULL, then the following message will be presented, "We may present this screen again during the experiment to remind you of the keys."
- `proceedMsg` — A string that specifies a message to hit any key to proceed. The string must be in html format. You can use any html codes. DEFAULT = NULL. If NULL, then the following message will be presented, "Please hit any key to proceed."
- `enableTriggers` — A boolean that enables fNIRS trigger support for this group (session). When TRUE, the engine connects to a local Python relay (tools/fnirsRelay.py) at session start, forwards trigger codes declared at block/set/trial/frame levels to an LSL stream, and writes a separate `_triggers.finalDat` event log alongside the main behavioral data. Silent fallback if the relay is not running (experiment proceeds normally with no triggers sent). DEFAULT = FALSE -- no overhead and identical behavior to pre-Phase-1.5 experiments.
- `triggerRelayPort` — An integer specifying the port the local Python relay listens on. Only meaningful when enableTriggers = TRUE. Must match the `--port` the relay was started with. DEFAULT = 5678.
- `restEveryNMinutes` — Optional positive numeric -- fire a rest break every N minutes of elapsed task time, in addition to any thresholds in `restTrials`. The engine measures elapsed time from the most recent rest (or first trial completion). NULL means no time-based rests. DEFAULT = NULL.
- `restMaxTrial` — Optional positive integer -- suppress all rest breaks once `trialsShown` reaches this number. Useful for experiments where late-stage rests would disrupt a flow state. NULL means no trial-count cap. DEFAULT = NULL.
- `keyMaps` — Optional named list of keyMap entries (each from buildQCEkeyMapEntry). Phase 3.5 named-keyMap registry -- blocks reference these entries by name via the `keyMapName` arg of addBlockToQCETrialStructureList. NULL means no named keyMaps (legacy single-keyMap experiments work unchanged via the `keyMap` arg). For incremental construction, use addKeyMapToDbfile after this function returns. DEFAULT = NULL.
- `customHooksFile` — Optional single filename ending in `.js` -- the Phase 5 researcher-authored custom-hooks file (defines the global `QCEPHooks` object). NULL means no hooks (legacy experiments take the byte-identical no-hooks code path). Use saveCustomHooksTemplate() to scaffold the file, and copy it into the experiment directory. For incremental construction, use addHooksToQCEgroupDbFile after this function returns. DEFAULT = NULL.
- `customHooksStateKeys` — Optional character vector of state-key names the hooks will write to `qceState.custom`, used by the engine to statically validate `stateRef` showIf conditions (built with buildQCEstateCondition) -- an undeclared stateRef errors at session start as a likely typo. Only meaningful when customHooksFile is set. NULL means no declared keys (engine warns rather than errors on unknown stateRefs). DEFAULT = NULL.
- `customHooksColumns` — Optional character vector of the data columns the hooks will write via `dataAnnotations`. Declaring them lets buildQCEoutputFieldManifest report them and missingQCEoutputFields fail a build when fields.txt does not carry one; they are invisible to every check otherwise, existing only inside JavaScript. Only meaningful when customHooksFile is set. DEFAULT = NULL.

**Returns.** the QCEBdbfileList

### `buildQCETriggerList`

Build a trigger-object list for fNIRS event markers

```r
buildQCETriggerList(onset = NULL, offset = NULL, ...)
```

Creates a list in the unified shape used by QCEP engine v9 for fNIRS trigger
codes at any level (block, set, trial, or frame). The returned list is passed
to the `trigger` parameter of builder functions (addBlockToQCETrialStructureList,
addSetToQCEsetInfoList, addScenarioToQCEscenarioList, addFrameToQCEframeList).

- `onset` — Integer — the trigger code sent to LSL when this event begins. NULL for no onset marker.
- `offset` — Integer — the trigger code sent to LSL when this event ends. NULL for no offset marker.
- `...` — Additional named trigger types for extensibility (e.g., `response = 1234`). Any named integer is passed through.

**Details.** 
Returns NULL when all arguments are NULL, which signals "no trigger at this
level" to builder functions and results in no `trigger` key being emitted to
the JSON.

Code range convention (by digit count, for LSL analysis readability):
Block: 1-99 (1-2 digits)
Set: 100-999 (3 digits)
Trial: 1000-9999 (4 digits)
Frame: 10000-99999 (5 digits)

See tools/FNIRS_SETUP.md in the QCEP repo for full documentation.

**Returns.** A list of the form `list(onset=X, offset=Y, ...)`, or NULL if all args are NULL.

## Groups, sessions, and expInfo

### `addSessionListToQCEGroupList`

This function is used to create or modify a QCEGroupList by adding a QCEsessionList to a QCEGroupList

```r
addSessionListToQCEGroupList(
  QCEGroupList = NULL,
  QCEsessionList,
  groupName = "groupName",
  pages = NULL,
  cards = NULL
)
```

Function that creates or modifys a QCEGroupList by adding QCEsessionList to the list one at a time.

- `QCEGroupList` — A list that specifies all the session that participants will see for a single, betweeen subjects group. A session is, essentially, a group of trials that use the same scenario list and have the same instructions and response types. If you are building a new list, then the QCEGroupList should be NULL. If you are adding a new session to an old list, then QCEGroupList should be the QCEGroupList that you are adding an effect to. DEFAULT = NULL
- `QCEsessionList` — A list that specifies the session name, order number, dbfileName, tsFilename, and scenarioFilename.
- `groupName` — A string that specifies the name of the name of the between subjects group that contains these sessions. This will be output in the datafile.
- `pages` — A single string naming this group's page placement file (e.g. "pagesA.json", written by `saveQCEpageFiles`). Positionable HTML pages play at event anchors -- consent, demographics, a debrief. Each group may point at a different file, so groups can differ in the pages they show. NULL means this group shows no pages. DEFAULT = NULL.
- `cards` — A single string naming this group's card placement file (e.g. "cards1.json", written by `saveQCEcardFiles`). Cards are persistent panels that stay on screen across trials. NULL means this group shows no cards. DEFAULT = NULL.

**Returns.** the updated QCEGroupList

### `addSessionToSessionList`

This function is used to create or modify a QCEsessionList

```r
addSessionToSessionList(
  QCEsessionList = NULL,
  sessionOrder = -1,
  sessionName = "unspecified",
  dbFile = "dbfile.txt",
  tsFile = "tsFile.txt",
  stimFile = "stimulus.txt",
  plugins = NULL
)
```

Function that creates or modifys an QCEsessionList by adding sessions to the list one at a time.

- `QCEsessionList` — A list that specifies all session parameters: see next 5 parameters in this function. If you are building a new list, then QCEsessionList should be NULL. If you are adding a new effect to an old list, then QCEsessionList should be the QCEsessionList that you are adding an effect to. DEFAULT = NULL
- `sessionOrder` — An integer specifying the ordinal position that this session should be presented relative to other sessions in the group. If sessionOrder = -1, then the session order will be randomized. DEFAULT = -1
- `sessionName` — A string used to label this session. It is output in the dataFile. DEFAULT = "unspecified".
- `dbFile` — A string that specifies the name of the dbfile that contains necessary information for this session. DEFAULT = "dbfile.txt".
- `tsFile` — A string that specifies the name of the trial structure file for this session. DEFAULT = "tsFile.txt".
- `stimFile` — A string that specifies the name of the stimFile (that contains the scenarios) for this session. DEFAULT = "stimulus.txt".
- `plugins` — Optional character vector of custom plugin names to load for this session (e.g. c("survey")). Each name must correspond to a plugin registered in the deployment's pluginManifest.json; the engine's validateTrialTypes confirms at session start that every non-core trialType used by the session is covered by a loaded plugin. Emitted as the session's "plugins" array only when provided, so legacy sessions produce byte-identical JSON. DEFAULT = NULL (no custom plugins).

**Returns.** the updated QCEsessionList

## Pages and cards

### `addCardToQCEcardPlacement`

Place a persistent card between two event anchors

```r
addCardToQCEcardPlacement(
  QCEcardPlacement = NULL,
  card,
  mount = QCEanchor("sessionStart"),
  unmount = QCEanchor("sessionEnd"),
  position = NULL
)
```

Builds the per-group placement list (`cardsX.json`) one card at a time.
Unlike a page, which plays once and ends, a card MOUNTS at one anchor and
stays on screen -- surviving every trial transition -- until it UNMOUNTS at
another. Point a group at the finished list with the `cards` argument of
`addSessionListToQCEGroupList`.

- `QCEcardPlacement` — An existing placement list to add to, or NULL to start a new one. DEFAULT = NULL.
- `card` — A single string: the card's base name, with no extension. The engine loads `<card>.card.json` and, if present, an optional `<card>.html` shell.
- `mount` — A `QCEanchor` naming the moment the card appears. DEFAULT = `QCEanchor("sessionStart")`.
- `unmount` — A `QCEanchor` naming the moment it disappears. DEFAULT = `QCEanchor("sessionEnd")`.
- `position` — A list describing where the card sits, e.g. `list(region = "top-right")`. Overrides any position on the sidecar. DEFAULT = NULL.

**Details.** 
Several cards may be on screen at once; each is placed independently.

Anchors use the same vocabulary as pages and are built by
`QCEanchor`: `QCEanchor("sessionStart")`,
`QCEanchor("sessionEnd")`, or entry/exit of a named block or set, e.g.
`QCEanchor("entry", session = "1", block = "test")`.

**Returns.** The updated placement list.

### `addPageToQCEpagePlacement`

Place a positionable HTML page at an event anchor

```r
addPageToQCEpagePlacement(
  QCEpagePlacement = NULL,
  anchor,
  file,
  playOnce = FALSE
)
```

Builds the per-group placement map (`pagesX.json`) one page at a time.
The map answers WHERE and WHEN each page plays; what a page IS lives in its
sidecar (`buildQCEpageSidecar`). Point a group at the finished map
with the `pages` argument of
`addSessionListToQCEGroupList`.

- `QCEpagePlacement` — An existing placement map to add to, or NULL to start a new one. DEFAULT = NULL.
- `anchor` — An anchor built by `QCEanchor`, naming the event at which the page plays and which occurrence of it is meant.
- `file` — A single string: the page's base filename, with no extension. The engine loads `<file>.html` and `<file>.page.json`, so a value of "consent" means consent.html plus consent.page.json.
- `playOnce` — A boolean. TRUE shows the page only the first time its anchor fires, which matters at anchors that recur -- a block entered repeatedly, or a set that iterates. Use it for a one-time instruction that should not reappear on every pass. DEFAULT = FALSE.

**Details.** 
Pages at the same anchor play in the order you add them.

Build the anchor with `QCEanchor`, which documents the five events
and which qualifiers each one takes. Cards use the same vocabulary.

The SAME page file may be placed more than once -- once per session, or in
several blocks -- by adding it with a different anchor each time. Each
placement carries its own `playOnce`, so a page may repeat in one
position and appear only once in another.

**Returns.** The updated placement map.

### `buildQCEcardField`

Describe one value shown on a persistent card

```r
buildQCEcardField(
  formula = NULL,
  bind = NULL,
  digits = NULL,
  emptyValue = NULL
)
```

A card is an always-visible panel that lives outside the trial display, so it
survives trial transitions and re-renders from live experiment state. Each
named field it shows is either computed from the data so far, or read from a
state key a custom hook writes. Give exactly one of `formula` or
`bind`.

- `formula` — A list with `fn` (one of "mean", "median", "proportion", "count", "sum", "min", "max", "sd"), `column` (the data column to aggregate), and optionally `where` (a named list of row filters, each entry either a bare value for equality or `list(op=, value=)`). `count` counts rows with a value present; `proportion` is the mean of a 0/1 column. Give this OR `bind`. DEFAULT = NULL.
- `bind` — A single string naming a key in the card view object that a custom hook writes. The escape hatch for values the dataset cannot express. Give this OR `formula`. DEFAULT = NULL.
- `digits` — An integer: decimal places for display rounding. DEFAULT = NULL (no rounding).
- `emptyValue` — What to display before there is anything to compute -- at the start of a run an aggregate over zero rows has no honest value. Without this the field renders blank. DEFAULT = NULL.

**Details.** 
A `formula` is aggregated over the whole run by the same evaluator the
completion gate uses, so a card can show the very number the gate will judge.
Note it takes no `op` or `value`: a gate compares a number, a card
merely displays one.

Nothing a card shows is the source of truth for anything. The gate re-derives
its verdict from the dataset independently, so a card that degrades or fails
to render cannot affect a participant's outcome.

**Returns.** A list describing one card field.

### `buildQCEcardSidecar`

Build the sidecar that describes one persistent card

```r
buildQCEcardSidecar(
  template = NULL,
  fields = NULL,
  refreshMs = NULL,
  position = NULL,
  widgets = NULL,
  renderFn = NULL
)
```

A card's sidecar (`<card>.card.json`) says what the card IS -- its markup,
the values it shows, how often it re-renders -- while the placement list built
by `addCardToQCEcardPlacement` says where it sits and between which
anchors it lives.

- `template` — A single string of HTML with `{fieldName}` placeholders. DEFAULT = NULL (the card renders whatever `renderFn` produces).
- `fields` — A NAMED list of `buildQCEcardField` results. The names are what the template's placeholders refer to. NULL for a card that shows only a countdown or hook-written state. DEFAULT = NULL.
- `refreshMs` — How often the card re-renders, in milliseconds. A countdown needs 1000 to tick once a second; a card showing only running totals can refresh far less often, or rely on the re-render each trial triggers. DEFAULT = NULL (engine default).
- `position` — A list describing where the card sits, e.g. `list(region = "top-right")`. A placement may override this. DEFAULT = NULL.
- `widgets` — Optional list of extra display widgets, passed through to the card engine. DEFAULT = NULL.
- `renderFn` — A single string naming a global JS function that renders the card, for cards whose display is too dynamic for a template. DEFAULT = NULL.

**Details.** 
The `template` is HTML with `{fieldName}` placeholders. Each
placeholder is replaced at every tick by the matching entry in `fields`.
Two names resolve without being declared: `{deadlineRemaining}`, the
whole seconds left on a per-trial deadline, published by any plugin that runs
one; and any key a custom hook writes to the card view.

Templates are treated as trusted researcher-authored content and are not
HTML-escaped, so a placeholder can carry markup.

**Returns.** A list: the card sidecar, ready for `saveQCEcardFiles`.

### `buildQCEpageField`

Describe one form field on a positionable HTML page

```r
buildQCEpageField(
  input,
  type = "text",
  as = NULL,
  required = FALSE,
  emptyValue = NULL,
  requiredMessage = NULL
)
```

A page is a researcher-authored HTML file played at an event anchor. Its
sidecar (`<file>.page.json`) declares which form controls the engine
should read when the participant clicks continue. This function builds one
such declaration; pass a list of them to `buildQCEpageSidecar`.

- `input` — A single string: the HTML `name` attribute of the control to read. Required.
- `type` — The kind of control. One of `"text"`, `"number"`, `"hidden"`, `"radio"`, `"checkbox"`, `"select"`. A checkbox group captures every checked value, joined with underscores. DEFAULT = "text".
- `as` — A single string naming the output data column. NULL uses `input`. DEFAULT = NULL.
- `required` — A boolean. TRUE blocks the continue button while the field is empty. DEFAULT = FALSE.
- `emptyValue` — A single value that should count as "still blank" alongside NULL and "". Use it when your HTML carries a placeholder default -- an age box pre-set to 0, a select whose first option is "Choose..." -- so the required check treats the placeholder as unanswered without you editing the HTML. DEFAULT = NULL.
- `requiredMessage` — A single string shown on the page when this field blocks the continue button. Use it where the generic wording would not tell the participant what to do -- a slider whose starting position is also its placeholder looks answered, so "Please set your age" is worth saying. NULL uses the generic message. DEFAULT = NULL.

**Details.** 
The engine reads the control by its HTML `name` attribute, so
`input` must match the name in your HTML exactly. Whatever it captures is
written to a data column named by `as` (or by `input` when `as`
is omitted). Remember that a column is saved only if it also appears in the
experiment's `fields.txt` whitelist --
`buildQCEoutputFieldManifest` lists the columns a config is
expected to produce, so you can copy the ones you want.

**Returns.** A list describing one page field.

### `buildQCEpageSidecar`

Build the sidecar that describes one positionable HTML page

```r
buildQCEpageSidecar(fields = NULL, contBtn = NULL, dataScope = "global")
```

A page's sidecar (`<file>.page.json`) sits beside the HTML file and says
what the page IS -- which button ends it and which form fields to read --
while the placement file built by `addPageToQCEpagePlacement` says
WHERE and WHEN it plays. Keeping the two apart is what lets a page be copied
between experiments without carrying one study's timeline with it.

- `fields` — A list of field declarations from `buildQCEpageField`, in the order you want them checked. NULL for a display-only page. DEFAULT = NULL.
- `contBtn` — A single string: the HTML `id` attribute of the button that ends the page -- NOT the words printed on it. The engine binds its click handler by looking this id up in the loaded page, so a page whose button carries a different id never advances and errors instead. The visible label lives in your HTML and nothing here changes it. NULL uses the engine default id. DEFAULT = NULL.
- `dataScope` — Where captured values are written. `"global"` stamps them onto every row of the dataset, which is how intake pages behave -- demographics belong to the whole session. `"row"` writes them only onto this page's own data row, which suits a page asked repeatedly whose answers differ each time. DEFAULT = "global".

**Details.** 
Every part is optional. A page with no `fields` is a display-only screen
(instructions, a debrief); the sidecar may then be omitted entirely, in which
case the engine looks for its default button ID.

**Returns.** A list: the page sidecar, ready for `saveQCEpageFiles`.

### `saveQCEcardFiles`

Write a card placement list and its sidecars

```r
saveQCEcardFiles(QCEcardPlacement, cardsFile, sidecars, dir = ".")
```

Writes the placement list to `cardsFile` and each sidecar to
`<name>.card.json` in the same directory. An optional `<name>.html`
shell is yours to author if you want one; a card renders from its template
without it.

- `QCEcardPlacement` — A placement list from `addCardToQCEcardPlacement`.
- `cardsFile` — Path to write the placement list to, e.g. "cards1.json". Name it whatever you referenced from the group's `cards` argument.
- `sidecars` — A NAMED list of `buildQCEcardSidecar` results, the names being card base names.
- `dir` — Directory to write into. DEFAULT = "." (the working directory).

**Details.** 
Unlike a page, a card's sidecar is not optional -- it carries the template and
fields that are the card's entire content.

**Returns.** Invisibly, a character vector of the paths written.

### `saveQCEpageFiles`

Write a page placement map and its sidecars

```r
saveQCEpageFiles(QCEpagePlacement, pagesFile, sidecars = NULL, dir = ".")
```

Writes the placement map to `pagesFile` and each sidecar to
`<name>.page.json` in the same directory. The HTML files themselves are
yours to author -- this writes only the JSON that describes them.

- `QCEpagePlacement` — A placement map from `addPageToQCEpagePlacement`.
- `pagesFile` — Path to write the placement map to, e.g. "pagesA.json". Name it whatever you referenced from the group's `pages` argument.
- `sidecars` — A NAMED list of `buildQCEpageSidecar` results, the names being page base names. NULL writes the placement map only. DEFAULT = NULL.
- `dir` — Directory to write into. DEFAULT = "." (the working directory).

**Details.** 
The sidecar name must match the `file` you used when placing the page, so
that `addPageToQCEpagePlacement(..., file = "consent")` pairs with
`sidecars = list(consent = ...)`.

A page needs no sidecar if it collects nothing and its continue button already
carries the engine's default id; the engine falls back cleanly when one is
absent.

**Returns.** Invisibly, a character vector of the paths written.

## Trial-type registry

### `getRegisteredQCEBtrialTypes`

List all trialTypes registered with QCEB

```r
getRegisteredQCEBtrialTypes()
```

**Returns.** A character vector of registered trialType names (core + survey + any custom types registered via `registerQCEBtrialType`).

### `isRegisteredQCEBtrialType`

Test whether a trialType is registered with QCEB

```r
isRegisteredQCEBtrialType(name)
```

- `name` — A single string trialType name.

**Returns.** TRUE if the trialType is registered (core, survey, or custom), else FALSE.

### `registerQCEBtrialType`

Register a trialType so QCEB frame builders will accept it

```r
registerQCEBtrialType(name, ...)
```

QCEB mirrors, on the R side, the engine's trialType registry
(trialTypeRegistry.js). `addFrameToQCEframeList` validates its
`trialType` argument against this registry instead of a hard-coded list,
so adding support for a new jsPsych plugin in your experiment is a matter of
registering its trialType name here -- no edit to the QCEB core is needed.

- `name` — A non-empty single string -- the trialType name, matching the name the plugin uses in its `registerTrialType()` call on the engine side (e.g. "survey").
- `...` — Optional named metadata describing the type (e.g. `stimulusParam`, `requiresKeymap`, `forceResp`). Stored verbatim for introspection. The registry entry is an OPEN object, exactly like the engine's, so plugins can carry extra metadata without a QCEB change. Two fields are acted on. `usesPointer = TRUE` declares that the participant answers this type with the mouse, so a frame that does not state `cursorVisible` gets the pointer shown, and one that sets it to FALSE is warned about. Omit it for a keyboard- or text-driven plugin, whose pointer is only a distractor. `forceResp = TRUE` declares that the plugin always gives the participant a way to respond -- a mouse, a text field, a submit button -- independently of the frame's `choices`. Such a frame is exempt from the check that a frame with no time limit still has some way to end. Omit it (the default) for keyboard-driven types, whose only exit is a key listed in `choices`.

**Details.** 
The CORE types (`"key"`, `"textbox"`, `"numberline"`,
`"angleline"`) and the bundled `"survey"` and `"mcKeys"` plugins
are pre-registered automatically, so you only call this for additional
third-party / custom plugins (e.g. a Cyberball plugin).

This is a friendly, R-side typo guard only. The authoritative gate is the
engine's `validateTrialTypes` at session start, which checks that the
plugin is actually loaded (listed in a session's `plugins` array and
present in pluginManifest.json). Registering a type in QCEB does NOT load the
plugin -- you still pass `plugins = c("yourPlugin")` to
`addSessionToSessionList`.

**Returns.** Invisibly, the registry entry list that was stored.

## Output fields and data manifest

### `buildQCEhookRowManifest`

Report scenarios that would gain a row to hold trial-hook data

```r
buildQCEhookRowManifest(dir, outFile = "hook_row_manifest.txt", quiet = FALSE)
```

Scans the JSON config files in an experiment directory for scenarios that keep
no row of their own, and writes a plain-text report naming them. It is
ADVISORY: it changes no config, and nothing it lists is a defect.

- `dir` — The experiment directory holding the JSON config files.
- `outFile` — Filename to write the report to, inside `dir`. Pass NULL to return the report without writing it. DEFAULT = "hook_row_manifest.txt".
- `quiet` — When FALSE, also prints a one-line summary via `message()`, so a build that scrolls past still shows the finding. DEFAULT = FALSE.

**Details.** 
Why it matters: `onTrialFinish` fires once per scenario and its return
value is written onto that scenario's last SAVED row. A scenario whose frames
are all `output = FALSE` has no such row, so the engine keeps its last
frame to carry the values rather than discard them. Those scenarios therefore
record one row each that the config does not otherwise ask for -- which
changes the row count of the saved data, and is worth knowing before it turns
up in an analysis.

A scenario that ends on a discarded frame but keeps an earlier one is NOT
reported: the engine routes annotations to the last kept row, so a trailing
fixation costs nothing.

What it CANNOT see: whether your hook returns `dataAnnotations` at all.
That is decided in JavaScript at run time, and no row is added unless it does.
A hook that returns only `feedback`, or that skips these scenarios, leaves
the data exactly as the config describes it.

**Returns.** Invisibly, a character vector: the lines of the report.

### `buildQCEoutputFieldManifest`

Report the data columns an experiment's config is expected to produce

```r
buildQCEoutputFieldManifest(
  dir,
  outFile = "output_fields_manifest.txt",
  fieldsFile = NULL,
  engineVersion = "9.1"
)
```

Scans the JSON config files in an experiment directory and writes a plain-text
manifest of the columns those files imply, grouped by where each one comes
from. It is ADVISORY: it does not write `fields.txt`, it tells you what
could go in one.

- `dir` — The experiment directory holding the JSON config files.
- `outFile` — Filename to write the manifest to, inside `dir`. Pass NULL to return the report without writing it. DEFAULT = "output_fields_manifest.txt".
- `fieldsFile` — Optional path to an existing `fields.txt` to compare against. When given, the manifest flags every expected column missing from it -- the check that catches a silently dropped column. DEFAULT = NULL.
- `engineVersion` — A string recording which engine's row-stamp list this was built against. Stamped into the header so a stale manifest is visible rather than silent. DEFAULT = "9.1".

**Details.** 
Why it matters: `fields.txt` is a WHITELIST. The save path keeps only the
columns listed there, so a column you forget to add is silently dropped -- the
run looks fine and the data simply has a hole in it. Diffing this manifest
against your `fields.txt` is the cheapest way to catch that.

It reads the files rather than watching the builders, so it reports what would
actually ship. That matters when a config is hand-edited after generation, and
it means the manifest cannot quietly disagree with the files on disk.

What it CANNOT see: any column a custom hook writes at run time. When the
config declares a hooks file, the manifest says so explicitly rather than
implying the list is complete.

Alongside each trial type's columns, the report notes registry metadata worth
seeing at build time: a type registered as mouse-driven (`usesPointer`)
is flagged, since its frames must leave the cursor visible (see
`cursorVisible` on `addFrameToQCEframeList`). These notes are
comment lines only and never enter the `fields.txt` comparison.

**Returns.** Invisibly, a character vector: the lines of the manifest.

### `createQCEoutputVariableList`

This function builds a QCEoutvariableList from a dataframe

```r
createQCEoutputVariableList(dfVars)
```

Function builds a QCEoutvariableList from a dataframe.

- `dfVars` — A dataframe in which each column specifies an column and value to output into the datafile for this trial. The column name is the column name used in the datafile. The column contents is the value inserted in the datafile. These output variables are convenient ways to code your trials and stimuli because the stimuli are not output in the datafile.

**Returns.** the QCEoutputVariableList

### `missingQCEoutputFields`

Which expected columns are missing from a fields.txt

```r
missingQCEoutputFields(dir, fieldsFile = file.path(dir, "fields.txt"))
```

The machine-readable half of `buildQCEoutputFieldManifest`: same
scan, but it returns the column names instead of a report to read. Use it to
make a build script FAIL when the config produces a column the whitelist does
not carry, rather than leaving the gap to be noticed in the data later.

- `dir` — The experiment directory holding the JSON config files.
- `fieldsFile` — Path to the `fields.txt` to check. DEFAULT = `file.path(dir, "fields.txt")`.

**Returns.** A character vector of expected columns absent from the file; empty when nothing is missing.

### `promotedQCEhookRows`

Which scenarios would gain a row for their trial-hook data

```r
promotedQCEhookRows(dir, requireHooksFile = TRUE)
```

The machine-readable half of `buildQCEhookRowManifest`: the same
scan, returning scenario names instead of a report to read. Use it when a
build needs to assert its own row count -- for instance to confirm that a
change to which frames are kept has not quietly altered how many rows a
participant produces.

- `dir` — The experiment directory holding the JSON config files.
- `requireHooksFile` — When TRUE, only report scenarios if a hooks file is declared. Set FALSE to check the scenario shape on its own, for a config whose hooks are added later. DEFAULT = TRUE.

**Details.** 
This is informational, not a defect check. Nothing is lost either way: the
engine keeps the last frame of such a scenario precisely so the hook's values
survive. Failing a build on it is usually the wrong response.

Returns nothing when the directory declares no hooks file, since no trial hook
runs and no row can be added.

**Returns.** A character vector of scenario names that keep no row of their own; empty when there are none.

## Writing and reading the config files

### `readQCEBjsonFileToList`

This function is used to read a QCEB json file and returns it in the form of a list

```r
readQCEBjsonFileToList(filename)
```

Function that reads a QCEB json file and returns it in the form of a list

- `filename` — A string specifying the name of the QCEB json file.

**Returns.** the json data in a list structure

### `readQCEjsonFile`

Read a QCEB-written JSON config back into a list you can save again

```r
readQCEjsonFile(filename)
```

Reads a JSON file that `saveJsonFile()` wrote and returns it in the same
shape the package's builders produce, so it can be extended and saved again
without changing the structure of anything that was already in it.

- `filename` — A string giving the path of the JSON file to read.

**Details.** 
This exists because reading and writing are not symmetric by default.
`saveJsonFile()` deliberately writes every scalar as a one-element array,
because that is what the experiment engine's reader expects. A plain
`jsonlite::fromJSON(simplifyVector = FALSE)` hands those back as
one-element *lists*, and saving again wraps them a second time:
`"set": ["someSet"]` becomes `"set": [["someSet"]]`. Every scalar in
the file is affected. Nothing reports it -- the save succeeds, the file is
valid JSON, and the build prints its usual summary -- but a set name nested one
level too deep matches no setInfo entry, so the first symptom is a session that
runs no trials.

Reading with this function instead makes the round trip lossless:
`saveJsonFile(readQCEjsonFile(f), f)` leaves the file unchanged.

Values the package writes *unboxed* on purpose -- a bare
`"choices": "NO_KEYS"` rather than `["NO_KEYS"]` -- are read back
still unboxed, so re-saving does not turn a scalar the engine expects into a
one-element array. That distinction is only visible before the JSON is
simplified, which is why this reads the file unsimplified and restores the
shapes itself.

Arrays of objects are kept as lists of lists rather than collapsed into data
frames, and equal-length nested arrays are kept as nested lists rather than
collapsed into matrices. Both of those simplifications would lose the config's
structure and are switched off.

**Returns.** A list in the same shape the package's builders produce.

### `saveCustomHooksTemplate`

Write a starter custom-hooks JavaScript file (Phase 5)

```r
saveCustomHooksTemplate(filename = "customHooks.js")
```

Scaffolds a heavily-commented `customHooks.js` template into the working
directory for the researcher to edit. The file defines the global
`QCEPHooks` object the QCEP engine looks for when a dbfile declares a
`customHooksFile` (see addHooksToQCEgroupDbFile). Every hook in the
template is optional -- delete the ones you do not need; an absent hook is
simply never called.

- `filename` — Single string ending in `.js` -- the output filename. DEFAULT = "customHooks.js". Must match the name passed to addHooksToQCEgroupDbFile.

**Details.** 
The template documents, against the engine as built, each hook's exact
arguments, the shared `ctx` object (qceState, scenarios, dbConfig), the
recognized return values, and the hook->state->showIf channel (write
`ctx.qceState.custom[key]`, then gate a scenario/set/block with
buildQCEstateCondition). It also includes the stimulus-summary onBlockEnd
example (the clean replacement for the compound-scenario workaround).

After editing, copy the file into the experiment directory alongside the
other preload assets, and point the dbfile at it with
addHooksToQCEgroupDbFile(dbfile, "customHooks.js").

**Returns.** (invisibly) the filename written.

### `saveDbFile`

This function is used to save the QCEdbFileList to dbfile.json

```r
saveDbFile(QCEdbFileList)
```

Function that save the QCEdbFileList to dbfile.json. LEGACY FORMAT WRITER: the output is wrapped in a JavaScript assignment (`var dbInfo =`) rather than being plain JSON, and the output filename is fixed, so an engine that requires plain JSON cannot load what this writes. Use saveJsonFile(data, filename) for current engines.

- `QCEdbFileList` — A list that specifies the experiment- or group-level options of QCEB.

**Returns.** the json data

### `saveJsonFile`

This function writes any QCEB configuration list to a JSON file

```r
saveJsonFile(data, filename)
```

Function that serializes a QCEB configuration list to plain JSON and writes it to a named file. This is the writer for every configuration file a build produces -- Stimfile, Tsfile, group dbfile, experiment dbfile and expInfo alike.

- `data` — The list to serialize. Any QCEB configuration list.
- `filename` — A string giving the path of the file to write. The file is overwritten.

**Returns.** the json data

### `savePreloadFiles`

This function is used to write the preload manifest to preloadFile.json

```r
savePreloadFiles(
  imageFileArray = NULL,
  videoFileArray = NULL,
  audioFileArray = NULL
)
```

Function that writes the list of image, video and audio files the experiment should preload to preloadFile.json in the working directory.

- `imageFileArray` — An array of the image filenames (plus paths) that need to be preloaded.
- `videoFileArray` — An array of the video filenames (plus paths) that need to be preloaded.
- `audioFileArray` — An array of the audio filenames (plus paths) that need to be preloaded.

**Returns.** the json data

### `savePreloadImages`

This function is used to write an image-only preload manifest to preloadFile.json

```r
savePreloadImages(imageFileArray)
```

Function that writes the list of image files the experiment should preload to preloadFile.json in the working directory. LEGACY FORMAT WRITER: the output is wrapped in a JavaScript assignment (`var preloadImages =`) rather than being plain JSON, so an engine that requires plain JSON cannot load it. Use savePreloadFiles() for current engines.

- `imageFileArray` — An array of the image filenames (plus paths) that need to be preloaded.

**Returns.** the json data

### `saveStimFile`

This function is used to save the QCEScenarioList to stimFile.json

```r
saveStimFile(QCEScenarioList)
```

Function that save the QCEScenarioList to stimFile.json. LEGACY FORMAT WRITER: the output is wrapped in a JavaScript assignment (`var scenarios =`) rather than being plain JSON, and the output filename is fixed, so an engine that requires plain JSON cannot load what this writes. Use saveJsonFile(data, filename) for current engines.

- `QCEScenarioList` — A list that specifies all the possible scenarios that participants might see. A scenario is, essentially, a trial. It is composed of a series of frames, some potential response, and maybe feedback. Included in each scenario are an output variable list to code in the datafile and a setName that is used for presentation rules (see trialStructure.json).

**Returns.** the json data

### `saveTSFile`

This function is used to save the QCETrialStructureList to trialStructure.json

```r
saveTSFile(QCETrialStructureList)
```

Function that save the QCETrialStructureList to trialStructure.json. LEGACY FORMAT WRITER: the output is wrapped in a JavaScript assignment (`var trialStructure =`) rather than being plain JSON, and the output filename is fixed, so an engine that requires plain JSON cannot load what this writes. Use saveJsonFile(data, filename) for current engines.

- `QCETrialStructureList` — A list that specifies how the trials will be presented in the experiment. This list specifies the selection of stimuli from stimFile.json, the ordering of stimuli, the blocking structure, etc.

**Returns.** the json data

## Utilities

### `isColor`

This function tests whether a variable contains a valid color

```r
isColor(input)
```

Function tests whether a variable contains a valid color.

- `input` — A variable to be tested.

**Returns.** a boolean (TRUE or FALSE) identifying whether the input contains a valid color (TRUE) or not (FALSE)

### `isSingleNumeric`

This function tests whether a variable contains a single numeric

```r
isSingleNumeric(input)
```

Function tests whether a variable contains a single numeric.

- `input` — A variable to be tested.

**Returns.** a boolean (TRUE or FALSE) identifying whether the input contains a single numeric (TRUE) or not (FALSE)

### `isSingleString`

This function tests whether a variable contains a single string

```r
isSingleString(input)
```

Function tests whether a variable contains a single string.

- `input` — A variable to be tested.

**Returns.** a boolean (TRUE or FALSE) identifying whether the input contains a single string (TRUE) or not (FALSE)

### `isValidFilename`

This function tests whether a variable is a valid filename

```r
isValidFilename(filename, extension)
```

Function tests whether a variable is a valid filename.

- `filename` — A variable to be tested.
- `extension` — A string that represents the file extension you are testing against (e.g. "html").

**Returns.** a boolean (TRUE or FALSE) identifying whether the input is a valid filename (TRUE) or not (FALSE)

### `QCEanchor`

Build a placement anchor for a page or a card

```r
QCEanchor(at, session = NULL, block = NULL, set = NULL)
```

An anchor says WHERE in the run a positionable page plays, or where a
persistent card is mounted or unmounted. Pass the result to
`addPageToQCEpagePlacement` or
`addCardToQCEcardPlacement`; pages and cards use the same
vocabulary.

- `at` — A single string: one of `"experimentStart"`, `"sessionStart"`, `"sessionEnd"`, `"entry"`, `"exit"`.
- `session` — A single string naming the session, as keyed in `expInfo.json` -- that is, the order in which the session was added to its session list, "1" for the first. Required for `"entry"` and `"exit"`; optional for `"sessionStart"`/`"sessionEnd"`, where omitting it means every session; forbidden for `"experimentStart"`. DEFAULT = NULL.
- `block` — A single string naming the block, matching the name used when building the trial structure. Required for `"entry"` and `"exit"`; forbidden otherwise. DEFAULT = NULL.
- `set` — A single string naming a set within that block, matching the name used when building the trial structure. Optional, and only with `"entry"`/`"exit"`: present addresses the set's boundary, absent addresses the block's. DEFAULT = NULL.

**Details.** 
There are five events. Which qualifiers each one accepts differs, because
they attach to different things:

- `"experimentStart"` — Plays ONCE for the whole run, before the
experiment instructions -- the position the built-in intake screens
occupy, and the right home for consent or demographics. No session exists
yet, so it takes no qualifiers at all.

- `"sessionStart"` / `"sessionEnd"` — The top and tail of a
session, after that session's instructions. `session` is OPTIONAL:
omit it and the anchor fires in EVERY session, which is usually what is
wanted.

- `"entry"` / `"exit"` — A block boundary, or a set boundary
inside a block. `session` and `block` are BOTH REQUIRED.
`set` is the only optional qualifier: give it to address that set's
boundary, omit it to address the block's own.

**Why entry/exit demand a session and a block.** Set names are not
unique across blocks -- reusing one is a deliberate authoring pattern -- so a
set named on its own addresses every block that happens to contain a set of
that name, and a page written once plays in all of them. Naming the block is
what makes the address an address. Block names, in turn, are unique only
WITHIN a session, so the session is part of a block's identity rather than a
tie-breaker: an anchor without it is incomplete even when no collision
happens to exist yet, and would change meaning the day a session is added
that reuses the name.

Anchors are compared field by field, so block and set names may contain any
characters -- nothing here has to be escaped or avoided.

**Returns.** A validated anchor list, to be passed to a placement function.

## Deprecated — do not use in new code

These remain exported for backward compatibility with existing build
scripts. New code must not call them; each one's documentation names its
replacement.

- `addBlockToQCETrialStructureListDep` — This function is used to create or modify a QCETrialStructureList. This version is valid for the first incantation of the QCE.
- `addFixationToQCEframeList_7` — DEPRECATED — use addFixationToQCEframeList() instead
- `addFixationToQCEframeListOldDep` — This function is used to add a fixation frameto a QCEframeList
- `addFrameToQCEframeList_7` — DEPRECATED — use addFrameToQCEframeList() instead
- `addFrameToQCEframeListOldDep` — This function is used to create or modify a QCEframeList
- `addSetToQCEsetInfoListOldDep` — DEPRECATED multi-set form of addSetToQCEsetInfoList
- `buildQCEdbFile` — DEPRECATED — use buildQCEgroupDbFile() instead

<!-- END GENERATED API -->

## Worked examples

### Example 1 — a complete minimal experiment

A two-group speeded yes/no judgment task: one block, one set, a key map,
fixation + judgment frames, per-scenario output variables, and the
`fields.txt` save whitelist with the manifest self-check. This script has
been built and run through the QCEP preflight validator as written (all
checks pass for both groups).

```r
# Minimal complete QCEB build: a two-group speeded yes/no judgment task.
#
# Produces a deployable config directory: expInfo.json, expDBfile.json, one
# group dbfile per group, one Tsfile, one Stimfile, instruction HTML, and
# fields.txt, then self-checks with the output-field manifest.

suppressPackageStartupMessages({
  library(QCEB)
  library(jsonlite)
})

OUT_DIR <- file.path(getwd(), "prog")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## ---- 1. Key map -------------------------------------------------------------
# Labels are meanings, not keys: the label is printed on the key-map screen,
# written to the data, and matched by switch rules, so name the MEANING.
# Include both cases of each key.

km <- buildKeyMap(data.frame(Yes = c("d", "D"),
                             No  = c("k", "K"),
                             stringsAsFactors = FALSE))
choices <- getKeyChoicesFromKeyMap(km)

## ---- 2. Stimuli (Stimfile): frames -> scenarios -----------------------------

fixationHTML <- "<div style='font-size:48px;'>+</div>"
stimHTML <- function(word) {
  sprintf("<div style='font-size:36px;'>Is <b>%s</b> a pleasant word?</div>", word)
}

words    <- c("sunrise", "meadow", "laughter", "storm", "gravel", "burden")
pleasant <- c("Yes",     "Yes",    "Yes",      "No",    "No",     "No")

scenarios <- NULL
for (i in seq_along(words)) {
  fr <- addFrameToQCEframeList(NULL, trialType = "key", frameName = "fixation",
          stimulus = fixationHTML, stimulus_duration = 500, post_trial_gap = 250,
          choices = NULL, background = "#000000", output = FALSE)
  fr <- addFrameToQCEframeList(fr, trialType = "key", frameName = "judgment",
          stimulus = stimHTML(words[i]), stimulus_duration = NULL,
          post_trial_gap = 500, choices = choices,
          background = "#000000", output = TRUE)
  ov <- createQCEoutputVariableList(data.frame(Word = words[i],
                                               ExpectedAnswer = pleasant[i],
                                               stringsAsFactors = FALSE))
  scenarios <- addScenarioToQCEscenarioList(scenarios, fr,
                 createFeedbackList(), ov, "judgmentSet")
}

saveJsonFile(scenarios, file.path(OUT_DIR, "task_Stimfile.json"))

## ---- 3. Trial structure (Tsfile): sets -> blocks ----------------------------

si <- addSetToQCEsetInfoList(NULL, scenarios, setName = "judgmentSet",
        numberOfTrialsPerSet = 6, selectionType = "randomWithoutReplacement")

iter <- createBlockIteratorList(numberOfIterations = 1,
          randomizeTrialInSetOrder = TRUE,
          randomizeSetOrder = "fixed",
          randomizeAllTrials = FALSE)

# addBlockToQCETrialStructureList keys its result sequentially, so build each
# block alone, extract with [[1]], and place blocks by hand into a named list
# whose key == blockNumber == timeline position.
b1 <- addBlockToQCETrialStructureList(NULL, si, iter, blockNumber = 1,
        blockName = "judgmentBlock")[[1]]
tsList <- list("1" = b1)

saveJsonFile(tsList, file.path(OUT_DIR, "task_Tsfile.json"))

## ---- 4. Group dbfiles: one per between-subjects condition -------------------

for (g in c("groupA", "groupB")) {
  dbf <- buildQCEgroupDbFile(condName = g,
           keyMap = km, randomizeKeyMap = (g == "groupB"),
           presentKeyMapAfterTrialNumbers = -1,
           defaultBackgroundColor = "#000000",
           instructionFile = "instructions.html")
  saveJsonFile(dbf, file.path(OUT_DIR, paste0(g, "_Dbfile.json")))
}

## ---- 5. Experiment dbfile ---------------------------------------------------

expDb <- buildQCEexpDbFile(expName = "wordJudgment",
           defaultBackgroundColor = "#000000",
           welcomeMsg = "<p>Welcome! Press any key to begin.</p>",
           endOfExpMsg = "<p>That is the end of the study. Thank you!</p>",
           saveDataEveryNTrials = 25)
saveJsonFile(expDb, file.path(OUT_DIR, "expDBfile.json"))

## ---- 6. Sessions -> groups (expInfo) ----------------------------------------

expInfo <- NULL
for (g in c("groupA", "groupB")) {
  sess <- addSessionToSessionList(NULL, sessionOrder = 1,
            sessionName = paste0("wordJudgment_", g),
            dbFile = paste0(g, "_Dbfile.json"),
            tsFile = "task_Tsfile.json",
            stimFile = "task_Stimfile.json")
  expInfo <- addSessionListToQCEGroupList(expInfo, sess, groupName = g)
}
saveJsonFile(expInfo, file.path(OUT_DIR, "expInfo.json"))

## ---- 7. Hand assets + save whitelist ----------------------------------------

writeLines(c("<div style='color:white;'><p>Press the key for your judgment of",
             "each word as quickly and accurately as you can.</p>",
             "<p>Press any key to continue.</p></div>"),
           file.path(OUT_DIR, "instructions.html"))

# fields.txt is the save whitelist: a column not listed here is silently
# dropped at write time. List the engine columns you keep plus every column
# your outputVariables introduce.
fields <- c("sn", "Exp_Name", "Group", "Trial", "TrialInSession", "BlockNum",
            "BlockName", "BlockIt", "TrialinBlock", "StimNum", "FrameNum",
            "FrameName", "Set", "stimRef", "respType", "trial_index",
            "trial_type", "posttgap", "stim_dur", "ShowFeedBack", "FeedBack",
            "rt", "Key", "Response", "Cond_Name", "Sess_Name",
            "SessionKey", "BlockKey",
            "Word", "ExpectedAnswer")
writeLines(fields, file.path(OUT_DIR, "fields.txt"))

## ---- 8. Self-check ----------------------------------------------------------

buildQCEoutputFieldManifest(OUT_DIR, fieldsFile = file.path(OUT_DIR, "fields.txt"))
gaps <- missingQCEoutputFields(OUT_DIR)
if (length(gaps) > 0) stop("fields.txt is missing: ", paste(gaps, collapse = ", "))
cat("Build complete: ", OUT_DIR, "\n")
```

After the build, validate with the QCEP preflight tool before deploying
(see the engine specification for its rule inventory):

```
node <QCEP>/tools/qcep_preflight.js prog
```

### Patterns for larger builds

Two proven structures for real studies, both of which keep the invariants
above:

- **One script per output artifact**, run by a driver script in dependency
  order, with each later stage reading back the JSON an earlier stage wrote
  (via `readQCEjsonFile`, never a generic JSON reader). Suits multi-task
  experiments with hand-authored assets living permanently in the deploy
  directory.
- **One single build script** holding a growing `scenarios` accumulator and
  an `expInfo` accumulator, generating everything (including instruction
  HTML) into a fully regenerated output directory. Suits heavily
  counterbalanced designs; a per-cell loop emits one group dbfile and one
  Tsfile per design cell.

Rules that keep either structure safe:

1. **One writer per JSON file.** Every file has exactly one script that writes
   it, which is what keeps the build deterministic and reviewable: a file
   assembled by several script sections cannot be regenerated by running any
   one of them, so there is no way to reproduce it short of re-running the
   whole build in the right order. (Reading a file back to extend it is safe
   in itself — `readQCEjsonFile()` round-trips losslessly, which is why the
   hard rules require it over a generic JSON reader — the objection is to the
   file having no single point of authorship.)
2. **Group keys in `expInfo.json` are positional and recorded server-side**,
   so any dimension you may later extend must be the outermost loop —
   inserting keys mid-list silently repoints participants already assigned.
3. **Build blocks one at a time** and place them into a named list whose key
   equals `blockNumber` equals timeline position (`[[1]]`-extract pattern in
   Example 1).
4. **Let the build fail loudly:** end every script with the manifest
   self-check, and add `stop()` guards for any invariant your design depends
   on (matched set sizes, referenced HTML files existing, serialization
   round-trips).
