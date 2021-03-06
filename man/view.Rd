% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/pngpdf.R
\name{view}
\alias{view}
\title{General viewer for plots}
\usage{
view(filename, viewer = NULL, bg = FALSE)
}
\arguments{
\item{filename}{file to be opened}

\item{viewer}{program to view plot, attempts to use getOption or
display to find it}

\item{bg}{run as background process}
}
\value{
Result from system command
}
\description{
Views plots from devices or PDFs.  This function will
extract the extension from a filename and then uses the viewer
specified, or a default option to view the image
}
\keyword{viewer}

