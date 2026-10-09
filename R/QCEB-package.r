#' QCEB: build the files of a QCEP study
#'
#' QCEB writes the configuration files a QCEP study runs from: the group list, the experiment settings, each group's settings, the trial structure, the stimulus file, the preload list, pages, cards and their sidecars. QCEB 0.1.0 and above build studies for QCEP engine 10 and above; a study for engine 9.x or earlier is built with QCEB 0.0.5.
#'
#' Every file is written by \code{saveJsonFile()} and the other save functions in the form the QCEP settings description gives each key: one value bare, a list always a list, a setting that is not set left out, numbers at full precision. A setting that takes one value but is given several stops the build with a message naming the function and the argument.
#'
#' @keywords internal
"_PACKAGE"
