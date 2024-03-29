% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QCEButils.r
\name{isValidFilename}
\alias{isValidFilename}
\title{This function tests whether a variable is a valid filename}
\usage{
isValidFilename(filename, extension)
}
\arguments{
\item{filename}{A variable to be tested.}

\item{extension}{A string that represents the file extension you are testing against (e.g. "html").
'}
}
\value{
a boolean (TRUE or FALSE) identifying whether the input is a valid filename (TRUE) or not (FALSE)
}
\description{
Function tests whether a variable is a valid filename.
}
\examples{
isValidFilename ('myfile.html', "html")
}
\keyword{filename}
\keyword{is}
\keyword{valid}
