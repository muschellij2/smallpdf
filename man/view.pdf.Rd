\name{view.pdf}
\alias{view.pdf}
\title{View pdf from R}
\usage{
view.pdf(filename, viewer = getOption("pdfviewer"), bg = FALSE)
}
\arguments{
  \item{filename}{filename of pdf}

  \item{viewer}{Name for the system command for viewer.
  Defaults to pdfviewer option}

  \item{bg}{run as background process}
}
\value{
Result from \code{\link{system}} command
}
\description{
View a pdf in an external viewer
}
\keyword{viewer}

