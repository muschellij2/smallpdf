\name{smallpdf}
\alias{smallpdf}
\title{Open smallpdf device}
\usage{
smallpdf(dev = "png", mypattern = "MYTEMPPNG", maxn = 1e+06,
  type = "cairo", res = 600, width = 7, height = 7, units = "in", ...)
}
\arguments{
  \item{dev}{Device to be used, implemented for most
  types.}

  \item{mypattern}{pattern to create for temporary pngs.
  Does not need to be changed unless want to recover pngs
  for failed run.}

  \item{maxn}{Number of plots to be created (needed higher
  if over 99999)}

  \item{type}{character string of type of device (see
  \code{\link{png}})}

  \item{res}{The nominal resolution in ppi which will be
  recorded in the bitmap.}

  \item{width}{width of device (see \code{\link{png}})}

  \item{height}{height of device (see \code{\link{png}})}

  \item{units}{units of device (see \code{\link{png}})}

  \item{...}{additional options sent to device}
}
\value{
List of pndfname and mypattern (so it can be passed into
smallpdf.off()) and device
}
\description{
Opens a smallpdf device
}
\seealso{
qpdf,png
}

