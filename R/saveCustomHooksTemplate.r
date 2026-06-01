#' Write a starter custom-hooks JavaScript file (Phase 5)
#'
#' Scaffolds a heavily-commented `customHooks.js` template into the working
#' directory for the researcher to edit. The file defines the global
#' `QCEPHooks` object the QCEP engine looks for when a dbfile declares a
#' `customHooksFile` (see addHooksToQCEgroupDbFile). Every hook in the
#' template is optional -- delete the ones you do not need; an absent hook is
#' simply never called.
#'
#' The template documents, against the engine as built, each hook's exact
#' arguments, the shared `ctx` object ({qceState, scenarios, dbConfig}), the
#' recognized return values, and the hook->state->showIf channel (write
#' `ctx.qceState.custom[key]`, then gate a scenario/set/block with
#' buildQCEstateCondition). It also includes the stimulus-summary onBlockEnd
#' example (the clean replacement for the compound-scenario workaround).
#'
#' After editing, copy the file into the experiment directory alongside the
#' other preload assets, and point the dbfile at it with
#' addHooksToQCEgroupDbFile(dbfile, "customHooks.js").
#'
#' @param filename Single string ending in `.js` -- the output filename.
#'   DEFAULT = "customHooks.js". Must match the name passed to
#'   addHooksToQCEgroupDbFile.
#''
#' @return (invisibly) the filename written.
#' @keywords QCE hooks template dynamic
#' @export
#' @examples
#' # Scaffold the default customHooks.js, then edit it
#' saveCustomHooksTemplate()
#'
#' # Custom name (remember to match it in addHooksToQCEgroupDbFile)
#' saveCustomHooksTemplate("rubyT4Hooks.js")

saveCustomHooksTemplate <- function(filename = "customHooks.js") {

  if (!isSingleString(filename) || nchar(filename) == 0 ||
      !isValidFilename(filename, "js")) {
    stop("filename option must be a single non-empty filename that ends in '.js'.")
  }

  template <- r"---(/*
 * customHooks.js -- QCEP custom hooks (Phase 5).
 *
 * This file defines a single global object, QCEPHooks, holding any of the
 * five recognized hook functions below. EVERY HOOK IS OPTIONAL -- delete the
 * ones you do not use; an absent hook is simply never called. A hook that
 * throws is caught by the engine, logged, and skipped: the session continues.
 *
 * Declare this file on the group dbfile in R:
 *   dbfile <- addHooksToQCEgroupDbFile(dbfile, "customHooks.js",
 *               customHooksStateKeys = c("reachedCriterion", "blockScore"))
 *
 * ---- The shared ctx object (2nd argument to every hook) -------------------
 *   ctx.qceState   per-session state. Notable fields:
 *                    ctx.qceState.custom            <- your scratchpad (see below)
 *                    ctx.qceState.presentedScenarios { scenarioID: true } for
 *                                                    scenarios the participant has played
 *   ctx.scenarios  the full stimulus dictionary: scenarioID -> scenario object
 *                  (image paths, words, choices, ...). Read-only.
 *   ctx.dbConfig   the resolved group dbfile (condName, keyMap, ...). Read-only.
 *
 *   NOTE on jsonlite wrapping: values that came from R may be 1-element arrays
 *   (e.g. scenario.word1 is ["cat"], not "cat"). Index [0] when reading them,
 *   as the examples below do.
 *
 * ---- The hook->state->showIf channel --------------------------------------
 *   To make a downstream scenario / set / block depend on something a hook
 *   computed, WRITE a value into ctx.qceState.custom[key] from the hook, then
 *   gate the downstream scope in R with buildQCEstateCondition(key, op, value).
 *   This keeps branching declarative and logged -- hooks do NOT skip trials
 *   imperatively. Declare every key you write in customHooksStateKeys so the
 *   engine can catch typos at session start.
 */

var QCEPHooks = {

  /*
   * onTrialStart(trial, ctx) -- fires before a scenario renders (its first
   * frame). Use it to adapt the upcoming trial or stash context.
   *   trial.entry    the trial-structure entry (trial.entry.val = scenarioID)
   *   trial.scenario the resolved scenario object from the stimfile
   *   trial.data     the data row being built (Trial, FrameNum, ...)
   * Optional return keys (all may be combined):
   *   { stimulusReplacements: { token: value } }  // {{token}} substitution in the stimulus HTML
   *   { choicesOverride: ["y", "n"] }             // replace the allowed response keys
   *   { dataAnnotations: { myField: 1 } }         // merge extra fields into the data row
   * Return nothing to leave the trial unchanged.
   */
  onTrialStart: function (trial, ctx) {
    // Example: substitute a running score into the stimulus text
    //   stimulus HTML contains "You have {{score}} points"
    // var score = ctx.qceState.custom.score || 0;
    // return { stimulusReplacements: { score: score } };
    return null;
  },

  /*
   * onTrialFinish(data, ctx) -- fires once per scenario, after its last frame
   * (the response is recorded in `data`). Use it to update state, annotate the
   * data row, or give immediate feedback.
   *   data  the just-recorded trial data row (data.Trial, data.Response, ...)
   * Optional return keys:
   *   { feedback: "<html>" }            // shown as a press-any-key screen after this trial
   *   { dataAnnotations: { ok: true } } // merged into the just-recorded data row
   */
  onTrialFinish: function (data, ctx) {
    // Example: accumulate a score in the scratchpad (read later via showIf)
    // ctx.qceState.custom.score = (ctx.qceState.custom.score || 0) +
    //                             (data.Correct ? 1 : 0);
    return null;
  },

  /*
   * onSetEnd(setName, ctx) -- fires once when a set completes (natural
   * completion only; a set abandoned by a switchRule does not fire it).
   * Optional return: { feedback: "<html>" }.
   */
  onSetEnd: function (setName, ctx) {
    return null;
  },

  /*
   * onBlockEnd(blockName, ctx) -- fires once when a block completes (its final
   * iteration; natural completion only). The clean home for set/block-aggregate
   * feedback -- this is what replaces the compound-scenario workaround.
   * Optional return: { feedback: "<html>" }.
   */
  onBlockEnd: function (blockName, ctx) {
    // Example: stimulus-summary table of everything the participant just saw
    // if (blockName !== "fctMain") return null;
    // var ids = Object.keys(ctx.qceState.presentedScenarios);
    // var rows = ids.map(function (id) {
    //   var s = ctx.scenarios[id];
    //   return "<tr><td><img src='" + s.imagePath[0] + "'></td>" +
    //          "<td>" + s.word1[0] + "</td><td>" + s.word2[0] + "</td></tr>";
    // });
    // return { feedback: "<table>" + rows.join("") + "</table>" };
    return null;
  },

  /*
   * onSessionEnd(ctx) -- fires once at session end, before the final save
   * chain. Note: takes ONLY ctx (no scope name).
   * Optional return: { feedback: "<html>" }.
   */
  onSessionEnd: function (ctx) {
    return null;
  }

};
)---"

  write(template, filename)

  return(invisible(filename))
}
