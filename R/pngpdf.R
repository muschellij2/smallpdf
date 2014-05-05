#' @title Open smallpdf device
#'
#' @description Opens a smallpdf device
#' @param dev Device to be used, implemented for most types.
#' @param mypattern pattern to create for temporary pngs.  Does not 
#' need to be changed unless want to recover pngs for failed run.
#' @param maxn Number of plots to be created (needed higher if over 99999)
#' @param type character string of type of device (see \code{\link{png}})
#' @param res The nominal resolution in ppi which will be recorded in the bitmap.
#' @param width width of device (see \code{\link{png}})
#' @param height height of device (see \code{\link{png}})
#' @param units units of device (see \code{\link{png}})
#' @param ... additional options sent to device
#' @export
#' @seealso qpdf,png
#' @return List of pndfname and mypattern (so it can be passed into
#' smallpdf.off()) and device
#' @alias pngpdf
smallpdf = function(dev= "png", 
  mypattern= "MYTEMPPNG", maxn = 1e6, type= "cairo",  res=600,  
  width = 7, height = 7, units = "in",...){
  max.d = max(log10(maxn)-1, 5)
  fname = paste0(mypattern, "%0", max.d, "d.", dev)
  gpat = paste0(mypattern, ".*\\.", dev)
  ### remove existing trash files
  takeout = list.files(path=tempdir(), pattern=gpat, full.names=TRUE)
  if (length(takeout) > 0) file.remove(takeout)
  pngname = file.path(tempdir(), fname)
  
  stopifnot(dev %in% c("bmp", "jpeg", "png", "tiff", "svg"))
  do.call(dev, list(filename=pngname, type= type, 
                    height=height, width=width, units = units, 
                    res=res, ...))
  return(list(mypattern=mypattern, dev=dev))
}

#' @title Close smallpdf device
#'
#' @description Closes smallpdf device and converts the files into.  This is only worthwhile
#' when the pdf has a lot of information, such as when using \code{\link{image}}.  
#' 
#' @param pdfname filename of pdf to be created
#' @param mypattern regular expression pattern for device files 
#' (from \code{\link{smallpdf}})
#' @param dev device used to open (from \code{\link{smallpdf}})
#' @param copts Additional options sent to convert
#' @export
#' @seealso dev.off
#' @return Result from \code{\link{system}} command for convert
#' @alias pngpdf.off
#' @examples ## not good for small plots
#' x = smallpdf(dev="jpeg")
#' dat = matrix(rnorm(1e3*4), ncol=4)
#' plot(dat[,1], dat[,2])
#' plot(dat[,3], dat[,4])
#' fname = file.path(tempdir(), "smallpdf.pdf")
#' smallpdf.off(pdfname = fname, mypattern=x$mypattern, dev=x$dev)
#' print(file.info(fname)$size)
#' # view.pdf(fname)
#' \dontrun{ 
#' fname2 = file.path(tempdir(), "normalpdf.pdf") 
#' pdf(fname2)
#' plot(dat[,1], dat[,2])
#' plot(dat[,3], dat[,4])
#' dev.off()
#' file.info(fname2)$size
#' print(file.info(fname2)$size / file.info(fname)$size)
#' # [1] 0.01808703
#' x = smallpdf(dev="jpeg")
#' dat = matrix(rnorm(1e6*4), ncol=4)
#' plot(dat[,1], dat[,2])
#' plot(dat[,3], dat[,4])
#' fname = file.path(tempdir(), "smallpdf.pdf")
#' smallpdf.off(pdfname = fname, mypattern=x$mypattern, dev=x$dev)
#' file.info(fname)$size
#' # [1] 3339399
#' # view.pdf(fname)
#' fname2 = file.path(tempdir(), "normalpdf.pdf") 
#' pdf(fname2)
#' plot(dat[,1], dat[,2])
#' plot(dat[,3], dat[,4])
#' dev.off()
#' file.info(fname2)$size
#' # [1] 13648866
#' file.info(fname2)$size / file.info(fname)$size 
#' # [1] 4.087222
#'}
smallpdf.off = function(pdfname, mypattern, dev, copts = ""){
  dev.off()
  gpat = paste0(mypattern, ".*\\.", dev)  
  pngs = list.files(path=tempdir(), pattern=gpat, full.names=TRUE)
  mystr = paste(pngs, collapse=" ", sep="")
  system(sprintf("convert %s -quality 100 %s %s", mystr, pdfname, copts))
}

#' @title View pdf from R
#'
#' @description View a pdf in an external viewer
#' @param filename filename of pdf
#' @param viewer <what param does>
#' @param bg run as background process
#' @export
#' @keywords viewer
#' @return Result from system
view.pdf = function(filename, 
  viewer=getOption("pdfviewer"), 
  bg = FALSE){
    stopifnot(length(filename) == 1)
  if (is.null(viewer)){
    viewer = getOption("pdfviewer")
  }
  if (bg) {
    filename = paste0(filename, " &")
  }
  system(sprintf("%s %s", viewer, filename))
}

#' @title View png in R
#'
#' @description View a png in external viewer from R
#' @param filename filename of png 
#' @param viewer name of viewer in PATH
#' @param bg run as background process
#' @export
#' @keywords viewer
#' @return Result from system
view.png = function(filename, 
  viewer= "display", 
  bg = FALSE){
  stopifnot(length(filename) == 1)
  if (bg) {
    filename = paste0(filename, " &")
  }  
  system(sprintf("%s %s", viewer, filename))
}

#' @title General viewer for plots
#'
#' @description Views plots from devices or pdfs
#' @param filename file to be opened
#' @param viewer program to view plot, attempts to use getOption or 
#' display to find it
#' @param bg run as background process
#' @export
#' @keywords viewer
#' @return Result from system

view = function(filename, viewer= NULL, bg=TRUE){
  stopifnot(length(filename) == 1)
  get.ext = gsub("(.*)\\.(.*)$", "\\2", file)
  stopifnot( get.ext %in% c("pdf", "bmp", "svg", "png", 
    "jpg", "jpeg", "tiff"))
  if (get.ext == "pdf") {
    if (is.null(viewer)){
      viewer = getOption("pdfviewer")
    }
  }
  if (is.null(viewer)){
    warning("No viewer given, trying display")
    viewer = "display"
  }
  if (bg){
    filename = paste0(filename, " &")
  }
  system(sprintf("%s %s", viewer, filename))
}

#' @title Open a device from the filename extension
#'
#' @description Open a device from the filename extension
#' @param filename filename to open
#' @param type Type of device to open 
#' @param ... arguments to be passed to device
#' @export
#' @keywords viewer
#' @return NULL, with deviced opened
open_dev = function(filename, type= "cairo", ...){
  get.ext = gsub("(.*)\\.(.*)$", "\\2", file)
  stopifnot( get.ext %in% c("pdf", "bmp", "svg", "png", 
    "jpg", "jpeg", "tiff"))

  ## device is jpeg
  if (get.ext == "jpg") get.ext = "jpeg"
  ### difff arguments for diff devices
  if (get.ext %in% c("pdf")) {
    do.call(get.ext, list(file=filename, ...))
  } else if (get.ext %in% 
    c("bmp", "jpeg", "png", "tiff", "svg")) {
    do.call(get.ext, list(filename=filename, type= type,...))
  }
  return(invisible(NULL))
}
