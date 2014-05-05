\name{view}
\alias{view}
\title{General viewer for plots}
\usage{
view(filename, viewer = NULL, bg = TRUE)
}
\arguments{
  \item{filename}{file to be opened}

  \item{viewer}{program to view plot, attempts to use
  getOption or display to find it}

  \item{bg}{run as background process}
}
\value{
Result from system
}
\description{
Views plots from devices or pdfs
}
\keyword{viewer}

