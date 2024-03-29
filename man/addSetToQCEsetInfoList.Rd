% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addSetToQCEsetInfoList.r
\name{addSetToQCEsetInfoList}
\alias{addSetToQCEsetInfoList}
\title{This  function is used to create or modify a QCEframeList}
\usage{
addSetToQCEsetInfoList(
  QCEsetInfoList = NULL,
  QCEScenarioList = NULL,
  setName = NULL,
  numberOfTrialsPerSet = 1,
  selectionType = "randomWithoutReplacement"
)
}
\arguments{
\item{QCEsetInfoList}{A list that specifies the the setInfo information.  This includes, for each setName, : N (the number of trials per set) and selection (the method of selecting the scenarios from the QCEScenarioList).  If you are building a new list, then this should be NULL. If you are adding a new effect to an old list, then this should be the QCEsetInfoList that you are adding an effect to. DEFAULT = NULL}

\item{QCEScenarioList}{A list that specifies all the possible scenarios that participants might see. A scenario is, essentially, a trial.  It is composed of a series of frames, some potential response, and maybe feedback.  Included in each scenario are an output variable list to code in the datafile and a setName that is used for presentation rules.}

\item{setName}{A vector of strings that specify the name of the sets that you will be adding to the setInfo. The order of the strings is the order that they will be entered into the QCEsetInfoList.  It is that order that the sets will be presented in if randomizeSetOrder is set to "fixed" in the trialStructure file. DEFAULT = NULL.  If setName is NULL, then the function will create a setInfo list that is equivelent for all sets in order of validSetNames.}

\item{numberOfTrialsPerSet}{A vector of integers (one for each setName) to indicate the number of stimuli to select for each set. DEFAULT = 1.}

\item{selectionType}{A string to indicate the selection method: "randomWithReplacement" indicates the stimuli  will be selected from all the stimuli with that setName in the stimFile randomly with replacement. "randomWithoutReplacement" indicates the stimuli  will be selected from all the stimuli with that setName in the stimFile randomly without replacement. "fixed indicates the stimuli  will be selected from all the stimuli with that setName in the stimFile starting with the first stimulus of that type and moving successively until the total needed is selected. The nect block will start where the previous block left off.  DEFAULT = "randomWithoutReplacement"
'}
}
\value{
the updated QCEsetInfoList
}
\description{
Function that creates or modifys an QCEframeList by adding frames to the list one at a time.
}
\examples{
addSetToQCEsetInfoList (QCEsetInfoList, QCEScenarioList = "key", set = c("type1", "type2"), numberOfTrialsPerSet = c(10, 20),	selectionType = "randomWithReplacement")
}
\keyword{QCE}
\keyword{QCEsetInfoList}
\keyword{add}
\keyword{effect}
\keyword{update}
