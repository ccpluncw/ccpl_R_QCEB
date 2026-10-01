# QCEP Tutorial: building a study from start to finish

QCEP runs psychology studies in a participant's web browser. A study is a
folder of files on a web server. A program called the **engine** (JavaScript,
built on the jsPsych library) reads those files and shows the participant one
screen after another, recording every answer. **QCEB** is the R package that
writes the files. You describe the study in an R script, run it, and the script
writes the folder.

This tutorial builds one invented study, chapter by chapter. By the last
chapter you will have written the script, built the study, checked it, and know
how it goes onto a server. It assumes you know some R and have never seen QCEP.

Two other documents sit beside this one. `BUILDER_REFERENCE.md` lists every
QCEB function and its arguments. `QCEP_SPEC.md`, in the QCEP repository, says
exactly what the engine does with each file. This tutorial shows how the parts
fit together; when you need the detail of one function, look it up there.

**How to read the code.** Every R block in this tutorial, taken in order, is one
build script. Paste them into one file and it builds the whole study. A block
whose first line is `#not run` is an illustration only and is not part of the
script. The package's tests run every block in order to keep this true. You need
R 4.0 or later (the hooks in chapter 8 use R's raw strings) and the QCEB
package.

## 1. What a study is, and a participant's path through it

### The parts of a study

QCEP has its own words for the parts of a study. Each is defined here and used
in that sense from now on.

- A **frame** is one screen: a fixation cross, a picture waiting for a key
  press, a feedback message. A frame shows a **stimulus**, which is a piece of
  HTML (the language web pages are written in).
- A **scenario** is one trial: an ordered list of frames that run one after
  another, plus the trial's **output variables**. In QCEP a trial is a
  stimulus. It has one stimulus ID, its key in the stimulus file, which the
  data file records as `StimNum`.
- **Output variables** are the columns the trial writes into the data file:
  descriptors of what was shown, such as the path of each picture file and the
  shape and colour in it. You decide what they are. They are the record of
  what the trial was.
- A **set** is a named pool of scenarios. Usually a set is one cell of the
  design: every trial of one condition.
- A **block** deals trials from its sets. For each set it says how many to draw
  for each participant and in what order to run them.
- A **session** is a sequence of blocks with its own three configuration files:
  the stimulus file (all the scenarios), the trial-structure file (the blocks)
  and the group settings file (key maps, instructions, other settings).
- A **group** is one between-subjects arm of the study: a list of sessions. The
  server assigns each participant to a group.
- A **page** is a whole HTML page played at a named point of the run, such as
  an age question before the study starts or a debriefing at the end.
- A **hook** is a JavaScript function of your own that the engine calls at a
  fixed moment, such as the start of each trial or the end of a block.

The files are JSON, a plain-text format for lists and values. The QCEB script
writes them; you never edit them by hand.

### The study this tutorial builds

The study is called `shapeMatch`. On each trial the participant sees two
pictures side by side. Each picture is a coloured shape: a circle, square,
triangle or diamond, in blue, orange or green. R draws the twelve pictures.

There are two groups. The **shape** group decides whether the two pictures
have the same shape. The **colour** group decides whether they have the same
colour. Everyone presses D for "same" and K for "different".

Every ordered pair of the twelve pictures is a possible trial: 144 pairs. Each
pair falls into one of four design cells: same or different shape, crossed with
same or different colour. Each cell is a set, and each participant gets the
same number of trials from every cell.

The run has a practice block with feedback after each trial, a second practice
block for anyone who made two or more mistakes, a main block with a summary at
the end, and the offer of a short extra round.

### What the participant goes through

This is the order in which one participant meets the study, taken from a test
run of the finished build.

1. The participant opens their link. The server checks it and shows the
   study's consent text. The engine has not started yet.
2. The engine's page loads and asks to switch to full screen.
3. A loading screen, while the browser fetches every file in the study's
   preload list (here, the twelve pictures).
4. The pages placed at the start of the experiment: an About-you page, then an
   age page.
5. The welcome message ("Press any key to begin").
6. The session's instructions. Each group has its own.
7. The key-map screen ("D = Same, K = Different"), which the engine shows by
   itself before the first block that takes key presses.
8. The blocks: four practice trials, each followed by "Correct" or "Not
   quite"; four more practice trials if needed; a short page starting the main
   block; 24 main trials; a screen with the score; the offer of an extra round;
   and, for those who accept, 8 more trials and their score.
9. The page placed at the end of the session: a debriefing.
10. A saving screen, the end message, and a last screen saying the window may
    be closed.

### The finished folder

When the script has run, the study is one folder:

```text
shapeMatch/
  shapeMatch.php              starts the engine (chapter 2)
  pictures/                   the twelve drawn pictures (chapter 4)
  preloadFile.json            the files to load before the run (chapter 4)
  shapeMatch_Stimfile.json    every scenario (chapters 3 to 5, 9)
  shapeMatch_Tsfile.json      the blocks (chapters 6, 7, 9)
  shape_Dbfile.json           the shape group's settings (chapters 7 to 9)
  colour_Dbfile.json          the colour group's settings
  expDBfile.json              settings for the whole experiment (chapter 7)
  expInfo.json                the groups and their sessions (chapter 7)
  instructions_shape.html     each group's instructions (chapter 7)
  instructions_colour.html
  customHooks.js              the hooks (chapter 8)
  practice_again.html         pages shown when a block starts (chapter 9)
  main_start.html
  aboutYou.html, age.html,    the pages, their descriptions and where they
  debrief.html, *.page.json,  play (chapter 10)
  pages.json
  consent.txt                 the consent text (chapter 10)
  fields.txt                  the columns the server saves (chapter 11)
  output_fields_manifest.txt  QCEB's list of expected columns (chapter 11)
```

## 2. The build script: one R script writes every file

The whole study comes from one R script. You run it from the folder where the
study should appear, for example with `Rscript build.R`, and it writes the
study's folder from nothing. Nothing in the folder is edited by hand. When you
want to change the study, you change the script and run it again. That way the
script is always a complete, readable record of the study, and a rebuild can
never lose a change.

The script starts by loading QCEB and naming a few things it uses throughout.

```r
library(QCEB)

EXP_NAME  <- "shapeMatch"
TYPE_NAME <- "TestType"
OUT_DIR   <- EXP_NAME
STUDY_URL <- paste0(TYPE_NAME, "/", EXP_NAME, "/")

dir.create(file.path(OUT_DIR, "pictures"), recursive = TRUE,
           showWarnings = FALSE)
```

`EXP_NAME` is the study's name. It is the name of the folder (`OUT_DIR`), and on
the server it is the name of the folder the study is served from.

`TYPE_NAME` and `STUDY_URL` are about addresses. On the server, studies are
grouped by **experiment type**: a study lives in the folder
`wwwFiles/<type>/<name>/`. The participant's browser, however, is never
pointed at that folder. Every participant runs the study through one page,
`experiment.php`, which sits at the top of `wwwFiles/`. A browser reads every
address in a page relative to the page itself. So when a stimulus names a file
of the study, such as one of the pictures, the address must start from the top
of `wwwFiles/`: `TestType/shapeMatch/pictures/circle_blue.png`. `STUDY_URL` holds
that start. An address written relative to the study's own folder
(`pictures/circle_blue.png`) does not work: the browser looks for it at the top
of `wwwFiles/`, and the run stops at the loading screen. `TestType` is the type
the local test run in chapter 12 uses; before you build for a real server, set
`TYPE_NAME` to the type your study is registered under.

The script writes each configuration file with `saveJsonFile()`. QCEB writes
every single value as a list of one (`"expName": ["shapeMatch"]`), because that
is the form the engine reads. Leave the files as QCEB writes them. If a later
step of a script ever has to read one back, use `readQCEjsonFile()`, which keeps
that form; a general JSON reader does not, and the file it writes back looks
right and no longer works.

The first file is the smallest. The server starts a study by running a PHP file
named after the study's folder, here `shapeMatch.php`. Its one line names the
engine version the study runs on. This tutorial uses engine 10.0.

```r
writeLines('<?php require "../../bin/QCEB.10.0.php"; ?>',
           file.path(OUT_DIR, paste0(EXP_NAME, ".php")))
```

The path `../../bin/` is fixed by the server's layout: the engine lives in
`wwwFiles/bin/`, two folders above the study.

## 3. A trial: frames, the stimulus as HTML, responses and key maps

### Responses and key maps

A **key map** says which keys the participant may press and what each one
means. In this study D means "same" and K means "different".

```r
km <- buildKeyMap(data.frame(Same = c("d", "D"), Different = c("k", "K"),
                             stringsAsFactors = FALSE))
choices <- getKeyChoicesFromKeyMap(km)
```

Each column name is a **label**: the meaning of the keys under it. The label
does three jobs. It is what the data file records in the `Key` column when one
of those keys is pressed (the key itself goes in `Response`, and the response
time in milliseconds in `rt`). It is the text of the key-map screen the engine
shows before the first block that takes key presses. And it is the word a
switch rule counts (chapter 9). So name the meaning, not the key, and spell it
the same way everywhere. Both cases of each letter are listed because the
engine looks up the pressed character exactly: a D pressed with Caps Lock on
must be in the map to be recorded as "Same". `choices` is the flat list of
every key in the map, which a frame needs to know which keys end it.

### Frames

A trial in this study has two frames: a fixation cross for half a second, then
the screen with the two pictures, which stays until the participant presses D
or K. Each frame is added with `addFrameToQCEframeList()`:

```r
fixationHTML <- "<div style='font-size:48px'>+</div>"

trialFrames <- function(screenHTML) {
  fr <- addFrameToQCEframeList(NULL, trialType = "key",
          frameName = "fixation", stimulus = fixationHTML,
          stimulus_duration = 500, post_trial_gap = 0, choices = NULL,
          background = "#FFFFFF", output = FALSE)
  addFrameToQCEframeList(fr, trialType = "key", frameName = "pair",
    stimulus = screenHTML, stimulus_duration = NULL, post_trial_gap = 400,
    choices = choices, background = "#FFFFFF", output = TRUE)
}

example <- trialFrames("<p>Two pictures will go here.</p>")
length(example)
```

What the arguments mean:

- `trialType` is how the frame takes its response. `"key"` shows the stimulus
  and waits for a key; the engine hands the HTML to jsPsych's keyboard-response
  plugin. Other types take typed text (`"textbox"`), a mouse drag along a line
  (`"numberline"`) or around a circle (`"angleline"`), or a whole
  questionnaire (`"survey"`).
- `frameName` names the frame in the data (`FrameName`).
- `stimulus` is the HTML to show. It can be any HTML: text, a table, pictures,
  drawings, styled with CSS. Chapter 4 builds the real one.
- `stimulus_duration` is how long the stimulus stays, in milliseconds. `NULL`
  means until the participant responds.
- `post_trial_gap` is a blank pause after the frame, in milliseconds. It has no
  default; you must give it, even as 0.
- `choices` is the keys that end the frame. `NULL` means no key ends it, so the
  fixation frame ends on its timer. A frame with neither a timer nor a key would
  never end, and the engine refuses it.
- `background` is the colour of the page while the frame is shown. QCEB's
  default is black, so a study on white says so.
- `output` says whether the frame's row is kept in the data file. The engine
  records a row for every frame and drops the rows of frames marked `FALSE`
  when it saves. The fixation has nothing worth keeping.

`trialFrames()` is an ordinary R function of this script, not part of QCEB. It
returns a frame list of two frames; chapter 5 calls it once for every pair.

A frame list becomes a trial, a **scenario**, when it is added to the stimulus
file's list of scenarios with `addScenarioToQCEscenarioList()`. A scenario
holds its frames, a feedback list, its output variables and the name of its
set. The feedback list is QCEB's built-in way to show a message after a
response, chosen by which key was pressed; this study gives feedback with a hook
instead (chapter 8), so it passes `createFeedbackList()` with no keys, which
means no feedback.

## 4. Stimuli: pictures and preload, several items on a screen, drawn stimuli

### Pictures

A picture in a stimulus is an ordinary image file, shown by an ordinary HTML
`<img>` tag. This study draws its own twelve pictures with R's `png()` device,
one coloured shape per file:

```r
shapes  <- c("circle", "square", "triangle", "diamond")
colours <- c(blue = "#2F6DB5", orange = "#E08A1E", green = "#3A9E5C")

outline <- function(shape) {
  a <- seq(0, 2 * pi, length.out = 100)
  switch(shape,
    circle   = list(x = 0.5 + 0.4 * cos(a), y = 0.5 + 0.4 * sin(a)),
    square   = list(x = c(0.15, 0.85, 0.85, 0.15), y = c(0.15, 0.15, 0.85, 0.85)),
    triangle = list(x = c(0.1, 0.9, 0.5), y = c(0.15, 0.15, 0.85)),
    diamond  = list(x = c(0.5, 0.9, 0.5, 0.1), y = c(0.1, 0.5, 0.9, 0.5)))
}

drawPicture <- function(shape, fill, file) {
  png(file, width = 200, height = 200, bg = "white")
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  p <- outline(shape)
  polygon(p$x, p$y, col = fill, border = NA)
  invisible(dev.off())
}

pics <- expand.grid(shape = shapes, colour = names(colours),
                    stringsAsFactors = FALSE)
pics$file <- file.path("pictures", paste0(pics$shape, "_", pics$colour, ".png"))
for (i in seq_len(nrow(pics))) {
  drawPicture(pics$shape[i], colours[[pics$colour[i]]],
              file.path(OUT_DIR, pics$file[i]))
}
```

`pics` is a table of the twelve pictures: shape, colour and the file's path
inside the study folder. The folder name `pictures/` is this script's choice.
QCEP has no fixed place for a study's files; a script may put them in any
folder of the study, or in a folder the server shares between studies, as long
as every address it writes points there. Pictures you did not draw, such as a
set of photographs, arrive the same way: the script copies or lists the files
and reads a table that describes them.

### Several items on one screen

Everything one frame shows is one HTML string, so a screen holding several
items is simply HTML that holds several items. This function writes the pair
screen: a question, the two pictures side by side, and a reminder of the keys.

```r
pairScreen <- function(leftFile, rightFile) {
  img <- function(f) {
    paste0("<img src='", STUDY_URL, f,
           "' width='160' height='160' style='margin:0 30px'>")
  }
  paste0(
    "<div style='text-align:center; font-family:sans-serif'>",
    "<p style='font-size:22px'>Do these two pictures have ",
    "the same {{property}}?</p>",
    "<div>", img(leftFile), img(rightFile), "</div>",
    "<p style='font-size:16px; color:#555'>",
    "D = same &nbsp;&nbsp;&nbsp; K = different</p>",
    "</div>")
}
```

Each `<img>` address is `STUDY_URL` followed by the file's path, as chapter 2
explained. `{{property}}` is a **token**: a name in double braces that a hook
replaces with a word when the trial starts (chapter 8). Until then it is just
text in the string.

There is no layout function in QCEB and no list of allowed designs. Whatever a
web page can show, a study may show: words and pictures in a table or a grid,
pictures behind one another, a sentence with one word in colour, sound or video
elements, all of it styled with CSS. The engine places the HTML on the screen
and records which trial it was; it does not inspect how the trial looks.

### Drawn stimuli

A stimulus does not need a file at all. HTML can draw shapes itself with SVG
(a drawing language that browsers read inside HTML) or with CSS. The text `+`
used as a fixation in chapter 3 becomes a drawn cross:

```r
fixationHTML <- paste0(
  "<svg width='60' height='60' viewBox='0 0 60 60'>",
  "<line x1='30' y1='8' x2='30' y2='52' stroke='#222' stroke-width='4'/>",
  "<line x1='8' y1='30' x2='52' y2='30' stroke='#222' stroke-width='4'/>",
  "</svg>")
```

`trialFrames()` reads `fixationHTML` each time it is called, so every trial
built from now on gets the drawn cross. Other ways to draw: a CSS `transform`
rotates or mirrors a letter or a picture; a hook can draw into the page while
the study runs (the score bar in chapter 8 is drawn that way); and the
`"numberline"` and `"angleline"` trial types take a list of drawing settings as
their stimulus instead of HTML and draw the line themselves.

### Preload

Before the first trial, the engine loads every file named in
`preloadFile.json`, so that a picture appears the moment its frame starts
instead of when the network delivers it. The script writes that list with
`savePreloadFiles()`, which writes into the current working directory:

```r
local({
  old <- setwd(OUT_DIR)
  on.exit(setwd(old))
  savePreloadFiles(imageFileArray = paste0(STUDY_URL, pics$file))
})
```

The addresses are the same ones the `<img>` tags use. Sounds and videos go in
`audioFileArray` and `videoFileArray`. Every study writes this file, an empty
list included.

The order of events matters here. The engine preloads first. Only then, when
each block starts, does it shuffle the block's pools and choose the trials this
participant will see. So the list cannot be cut down to one participant's
trials: every stimulus file in every pool a participant's trials can be drawn
from must be on it. For this study that is all twelve pictures. If a listed
file cannot be fetched, the run stops at the loading screen with "The
experiment failed to load."

## 5. The design table: crossing and balancing in R; a row is a trial

### Crossing lists

The design is made in R, before the study ever runs, as an ordinary data frame
with one row per possible trial. This study crosses the list of pictures with
itself, so every ordered pair is a row, and describes each row:

```r
pairs <- expand.grid(left = seq_len(nrow(pics)), right = seq_len(nrow(pics)))
design <- data.frame(
  leftFile    = pics$file[pairs$left],
  rightFile   = pics$file[pairs$right],
  leftShape   = pics$shape[pairs$left],
  rightShape  = pics$shape[pairs$right],
  leftColour  = pics$colour[pairs$left],
  rightColour = pics$colour[pairs$right],
  stringsAsFactors = FALSE)
design$shapeMatch  <- ifelse(design$leftShape == design$rightShape,
                             "same", "different")
design$colourMatch <- ifelse(design$leftColour == design$rightColour,
                             "same", "different")
design$cell <- paste0("shape_", design$shapeMatch, "_colour_",
                      design$colourMatch)
table(design$cell)
```

That gives 144 rows in four cells: 12 pairs with the same shape and the same
colour (a picture beside itself), 24 with the same shape in different colours,
36 with different shapes in the same colour, and 72 that differ in both.

Anything a design needs is ordinary R at this point. To forbid a cell, drop its
rows. To keep a picture from appearing beside itself, drop the rows where the
two files are equal. To match two lists on some value, sort and split them. To
add catch trials, copy some rows and change them. The engine never sees these
steps; it only sees the trials the table turns into.

The cells are unequal in size, which does not matter: chapter 6 draws the same
number of trials from each cell for every participant.

### A row is a trial

Each row of the table becomes one scenario:

```r
scenarios <- NULL
for (i in seq_len(nrow(design))) {
  row <- design[i, ]
  ov <- createQCEoutputVariableList(row[, setdiff(names(row), "cell")])
  scenarios <- addScenarioToQCEscenarioList(scenarios,
                 trialFrames(pairScreen(row$leftFile, row$rightFile)),
                 createFeedbackList(), ov, row$cell)
}
length(scenarios)
```

The arguments of `addScenarioToQCEscenarioList()` are, in order: the list so
far (`NULL` to start one), the trial's frames, its feedback list, its output
variables, and the name of its set. The set name here is the row's design cell,
so the four cells become four sets.

The scenario's key in the list is its stimulus ID: QCEB numbers scenarios
`"1"`, `"2"`, ... in the order they are added, and the data file records the
number as `StimNum`. Because the number follows the order of the build, a
rebuild that adds trials earlier in the list renumbers everything after them.

### Output variables

The output variables are how the data says which stimulus a trial showed.
`createQCEoutputVariableList()` takes a one-row data frame and turns each column
into a data column, written on every saved row of that trial. Here they are the
row's own columns, without `cell` (the engine records the set name in its own
`Set` column).

Two kinds of descriptor are worth having. The path of each file
(`leftFile`, `rightFile`) lets anyone go and look at the exact stimulus the
participant saw. General descriptors (`leftShape`, `colourMatch`, and so on) are
what an analysis groups and compares by. You decide which descriptors a trial
carries; for pictures of faces they might be gender, race and the rating each
face received. Every value is written as text.

## 6. Sets and the trial structure: pools, drawing n, randomising

### Sets are pools

A set is every scenario whose set name is the same; here, every pair in one
design cell. The **trial structure** file says, block by block, how many trials
to draw from each set for each participant and in what order to run them. The
engine makes the draw itself, separately for every participant, each time a
block starts.

A set's entry in a block is made with `addSetToQCEsetInfoList()`. This
function makes the four entries, one per cell, with `n` trials each:

```r
cells <- sort(unique(design$cell))

cellSets <- function(n) {
  si <- NULL
  for (cl in cells) {
    si <- addSetToQCEsetInfoList(si, scenarios, setName = cl,
            numberOfTrialsPerSet = n,
            selectionType = "randomWithoutReplacement")
  }
  si
}
```

`numberOfTrialsPerSet` is the number drawn. `selectionType` is how:

- `"randomWithoutReplacement"` draws `n` different scenarios at random. `n`
  may not be larger than the set.
- `"randomWithReplacement"` draws at random and may draw one scenario twice.
- `"fixed"` takes the scenarios in the order they were added to the stimulus
  file.

Drawing the same number from every cell is how this study balances its design:
each participant gets equal numbers of each kind of pair, whatever the sizes of
the cells.

### Order within a block

The **block iterator** says how the drawn trials are ordered, and how many
times the block runs:

```r
mixed <- createBlockIteratorList(numberOfIterations = 1,
           randomizeTrialInSetOrder = TRUE, randomizeSetOrder = "randomFirst",
           randomizeAllTrials = TRUE)
```

- `numberOfIterations`: how many times the block runs.
- `randomizeAllTrials = TRUE` puts the trials drawn from all the sets into one
  list and shuffles it, so the cells are mixed together. This is what the
  study wants.
- With `randomizeAllTrials = FALSE`, the sets run one after another instead:
  `randomizeSetOrder` says in what order (`"fixed"`, the order they were added;
  `"randomFirst"`, shuffled on the first iteration and then kept;
  `"randomAll"`, shuffled on every iteration), and `randomizeTrialInSetOrder`
  says whether trials are shuffled within each set.

When trials are shuffled across sets, a fixed set order contradicts the
shuffle, and the engine refuses a block that asks for both. So a mixed block
names `"randomFirst"` or `"randomAll"`; with one iteration the two do the same.

### The first two blocks

A **block** puts the sets and the iterator together. The practice block draws
one pair from each cell; the main block draws six from each, 24 in all:

```r
bPractice <- addBlockToQCETrialStructureList(NULL, cellSets(1), mixed,
               blockNumber = 1, blockName = "practice")[[1]]
bMain <- addBlockToQCETrialStructureList(NULL, cellSets(6), mixed,
           blockNumber = 2, blockName = "main",
           entryInstruction = "main_start.html")[[1]]
```

Both blocks draw from the same four pools. Nothing yet stops the main block from
drawing a pair the practice block already showed; chapter 9 adds that.
`entryInstruction` names a page shown when the block starts (written in chapter
7). Chapter 7 explains `blockNumber`, `blockName` and the `[[1]]`.

## 7. Blocks, sessions, groups and group settings

### Placing blocks

The trial-structure file is a list of blocks. Each block's key in the list is
its position in the run, and it must agree with the block's `blockNumber`.
`addBlockToQCETrialStructureList()` numbers the blocks of a list it is given by
counting them, not by `blockNumber`, so the clear way is the one chapter 6
used: build each block alone from `NULL`, take it out with `[[1]]`, and place
the blocks in a list yourself:

```r
ts <- list("1" = bPractice, "2" = bMain)
```

Block names must be unique: switch rules, conditions and hooks find blocks by
name, and the data records the name in `BlockName`. Chapter 9 adds three more
blocks and builds this list again.

### Groups and their settings

The two groups differ in one thing: which property they judge. That difference
lives in each group's **settings file** (QCEP calls it the group's dbfile),
made with `buildQCEgroupDbFile()`:

```r
groups <- c("shape", "colour")
dbfiles <- list()
for (g in groups) {
  dbf <- buildQCEgroupDbFile(condName = g, keyMap = km,
           instructionFile = paste0("instructions_", g, ".html"))
  dbf$judge <- g
  dbfiles[[g]] <- dbf
}
```

- `condName` is written to every row of the data as `Cond_Name`.
- `keyMap` is the session's key map. Both groups use D and K the same way. A
  study that wanted to counterbalance the keys would give the groups different
  maps here, or set `randomizeKeyMap = TRUE` to shuffle the meanings for each
  participant.
- `instructionFile` names the page shown when the session starts. Each group
  gets its own, because each judges a different property.
- `judge` is the study's own setting. QCEB has no argument for it, so the script
  adds it to the list. The engine passes the whole settings file to the hooks,
  where chapter 8 reads it. Any value a group needs at run time can be carried
  this way.

The settings files are kept in `dbfiles` for now; chapters 8 and 9 add to them,
and chapter 11 writes them.

### Pages the settings name

The instruction pages are plain HTML files that the script writes. A page
shown by the engine between trials (an instruction page, a page when a block
starts) ends when its button with the `id` `Go` is clicked.

```r
page <- function(body) {
  paste0("<div style='max-width:640px; margin:40px auto; ",
         "font-family:sans-serif; font-size:18px'>", body, "</div>")
}
for (g in groups) {
  writeLines(page(paste0(
    "<p>On each screen you will see two pictures.</p>",
    "<p>Press <b>D</b> if they have the same ", g,
    " and <b>K</b> if they do not.</p>",
    "<p>We start with a few practice pairs.</p>",
    "<button id='Go' type='button'>Start</button>")),
    file.path(OUT_DIR, paste0("instructions_", g, ".html")))
}
writeLines(page(paste0(
  "<p>Now the real pairs begin. There is no feedback until the end.</p>",
  "<button id='Go' type='button'>Continue</button>")),
  file.path(OUT_DIR, "main_start.html"))
```

### Sessions and groups

A **session** joins one settings file, one trial-structure file and one
stimulus file. A **group** is a list of sessions. Here each group has one
session; the groups share the stimulus file and the trial structure and differ
only in their settings file.

```r
expInfo <- NULL
for (g in groups) {
  sess <- addSessionToSessionList(NULL, sessionOrder = 1,
            sessionName = EXP_NAME,
            dbFile = paste0(g, "_Dbfile.json"),
            tsFile = "shapeMatch_Tsfile.json",
            stimFile = "shapeMatch_Stimfile.json")
  expInfo <- addSessionListToQCEGroupList(expInfo, sess, groupName = g,
               pages = "pages.json", nPerBlock = 1)
}
```

- `sessionOrder` places a session among its group's sessions; `-1` puts the
  sessions in a random order for each participant. A study with several tasks
  usually makes each task a session.
- `groupName` is written to the data as `Group`.
- `pages` names the file saying which pages play where (chapter 10).
- `nPerBlock` asks the server to assign participants in balance. Each group's
  number is its share; equal numbers keep the groups the same size. Every group
  must declare it, or none.

The server records each participant's group by its position in this list. Once
a study is running, add new groups at the end and never insert one in the
middle, or participants already assigned would point at the wrong group.

### Settings for the whole experiment

The **experiment settings file** holds what is the same for everyone: the
study's name in the data, the messages the engine shows, and some policies.

```r
expDb <- buildQCEexpDbFile(expName = EXP_NAME,
           welcomeMsg = "<p>Welcome. Press any key to begin.</p>",
           endOfExpMsg = "<p>Thank you for taking part.</p>",
           saveDataEveryNTrials = 20, strictGroupAssignment = TRUE)
```

- `expName` is written to every row as `Exp_Name`.
- `welcomeMsg` and `endOfExpMsg` are the first and last messages.
- `saveDataEveryNTrials` sends the data to the server every 20 trials as well
  as at the end, so a run that is abandoned midway still leaves its trials.
- `strictGroupAssignment = TRUE` makes a run refuse to start when the server
  cannot assign a group, instead of drawing one in the browser where nothing
  records it. A study with balanced groups should set it.
