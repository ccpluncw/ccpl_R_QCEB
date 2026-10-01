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
