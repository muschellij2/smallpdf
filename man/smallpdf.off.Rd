\name{smallpdf.off}
\alias{smallpdf.off}
\title{Close smallpdf device}
\usage{
smallpdf.off(pdfname, mypattern, dev, copts = "")
}
\arguments{
  \item{pdfname}{filename of pdf to be created}

  \item{mypattern}{regular expression pattern for device
  files (from \code{\link{smallpdf}})}

  \item{dev}{device used to open (from
  \code{\link{smallpdf}})}

  \item{copts}{Additional options sent to convert}
}
\value{
Result from \code{\link{system}} command for convert
}
\description{
Closes smallpdf device and converts the files into.  This
is only worthwhile when the pdf has a lot of information,
such as when using \code{\link{image}}.
}
\examples{
## not good for small plots
x = smallpdf(dev="jpeg")
dat = matrix(rnorm(1e3*4), ncol=4)
plot(dat[,1], dat[,2])
plot(dat[,3], dat[,4])
fname = file.path(tempdir(), "smallpdf.pdf")
smallpdf.off(pdfname = fname, mypattern=x$mypattern, dev=x$dev)
print(file.info(fname)$size)
# view.pdf(fname)
\dontrun{
fname2 = file.path(tempdir(), "normalpdf.pdf")
pdf(fname2)
plot(dat[,1], dat[,2])
plot(dat[,3], dat[,4])
dev.off()
file.info(fname2)$size
print(file.info(fname2)$size / file.info(fname)$size)
# [1] 0.01808703
x = smallpdf(dev="jpeg")
dat = matrix(rnorm(1e6*4), ncol=4)
plot(dat[,1], dat[,2])
plot(dat[,3], dat[,4])
fname = file.path(tempdir(), "smallpdf.pdf")
smallpdf.off(pdfname = fname, mypattern=x$mypattern, dev=x$dev)
file.info(fname)$size
# [1] 3339399
# view.pdf(fname)
fname2 = file.path(tempdir(), "normalpdf.pdf")
pdf(fname2)
plot(dat[,1], dat[,2])
plot(dat[,3], dat[,4])
dev.off()
file.info(fname2)$size
# [1] 13648866
file.info(fname2)$size / file.info(fname)$size
# [1] 4.087222
}
}
\seealso{
dev.off
}

