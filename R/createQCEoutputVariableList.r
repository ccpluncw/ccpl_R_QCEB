#' This  function builds a QCEoutvariableList from a dataframe
#'
#' Function  builds a QCEoutvariableList from a dataframe.
#' @param dfKeys A dataframe in which each column specifies an column and value to output into the datafile for this trial.  The column name is the column name used in the datafile.  The column contents is the value inserted in the datafile.  These output variables are convenient ways to code your trials and stimuli because the stimuli are not output in the datafile.
#''
#' @return the  QCEoutputVariableList
#' @keywords QCE QCEoutputVariableList output variables
#' @export
#' @examples createQCEoutputVariableList ( dataframe(stim = c("dog"), cond = (3)) )


createQCEoutputVariableList <- function (dfVars) {

  outList <- NULL

  outList <- lapply(dfVars, as.character)

  return(outList)
}
