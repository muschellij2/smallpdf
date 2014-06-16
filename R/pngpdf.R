#' @title Open smallpdf device
#'
#' @description Opens a smallpdf device
#' @param dev Device to be used, implemented for most types, 
#' such as \code{png}.
#' @param mypattern pattern to create for temporary PNGs.  Does not 
#' need to be changed unless want to recover PNGs for failed run.
#' @param maxn Number of plots to be created 
#' (need to be raised if over 99999, but you may rethinkt that).
#' @param type character string of type of device (see \code{\link{png}}),
#' such as "cairo"
#' @param res The nominal resolution in ppi which will be recorded in the 
#' bitmap.
#' @param width width of device (see \code{\link{png}})
#' @param height height of device (see \code{\link{png}})
#' @param units units of device (see \code{\link{png}})
#' @param outdir Directory of intermediate bitmap files (defaults to
#' tempdir()) 
#' @param ... additional options sent to device
#' @export
#' @seealso qpdf,png
#' @return List of pndfname and mypattern (so it can be passed into
#' smallpdf.off()) and device
#' @aliases pngpdf
smallpdf = function(dev= "png", 
  mypattern= "MYTEMPPNG", maxn = 1e6, type= "cairo",  res=600,  
  width = 7, height = 7, units = "in", outdir = tempdir(), ...){
  max.d = max(log10(maxn)-1, 5)
  fname = paste0(mypattern, "%0", max.d, "d.", dev)
  gpat = paste0(mypattern, ".*\\.", dev)
  ### remove existing trash files
  takeout = list.files(path=outdir, pattern=gpat, full.names=TRUE)
  if (length(takeout) > 0) file.remove(takeout)
  pngname = file.path(outdir, fname)
  
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
#' @param pdfname filename of PDF to be created
#' @param pdfobj List with elements \code{mypattern} and 
#' \code{dev}, returned from \code{\link{smallpdf}}.  If this is not 
#' \code{NULL}, overrides the \code{mypattern} and 
#' \code{dev} options.
#' @param mypattern regular expression pattern for device files 
#' (from \code{\link{smallpdf}})
#' @param dev device used to open (from \code{\link{smallpdf}})
#' @param extra.opts Additional options sent to convert
#' @param outdir Directory of intermediate bitmap files (defaults to
#' tempdir())
#' @param clean Delete temporary bitmap files or not?
#' @export
#' @seealso dev.off
#' @return The command for the conversion
#' @aliases pngpdf.off
#' @examples 
#' \dontrun{ 
#' ## not good for small plots
#' x = smallpdf(dev="jpeg")
#' dat = matrix(rnorm(1e3*4), ncol=4)
#' plot(dat[,1], dat[,2])
#' plot(dat[,3], dat[,4])
#' fname = file.path(tempdir(), "smallpdf.pdf")
#' smallpdf.off(pdfname = fname, mypattern=x$mypattern, dev=x$dev)
#' print(file.info(fname)$size)
#' # view.pdf(fname)
#' fname2 = file.path(tempdir(), "normalpdf.pdf") 
#' pdf(fname2)
#' plot(dat[,1], dat[,2])
#' plot(dat[,3], dat[,4])
#' dev.off()
#' file.info(fname2)$size
#' print(file.info(fname2)$size / file.info(fname)$size)
#'  
#' ## better for large plots
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
smallpdf.off = function(pdfname, pdfobj = NULL, 
                        mypattern, dev, 
                        extra.opts = "-quality 100", 
                        outdir = tempdir(), clean = FALSE){
  dev.off()
  if (!is.null(pdfobj)){
    mypattern = pdfobj$mypattern
    dev = pdfobj$dev
  }
#   aniopts = ani.options()
  gpat = paste0(mypattern, ".*\\.", dev)  
  pngs = list.files(path=outdir, pattern=gpat, full.names=TRUE)
  mystr = paste(pngs, collapse=" ", sep="")
#   ani.options(autobrowse=FALSE)
#   im.convert(pngs, output = pdfname, convert="convert", 
#              extra.opts = extra.opts, 
#              clean = clean, animation=FALSE)
#   ani.options(aniopts)
  res = system(sprintf("convert %s -quality 100 %s %s", mystr, 
                 extra.opts, pdfname))
  if (res != 0){
    stop("Error in Conversion to PDF!")
  }
  return(invisible(NULL))
}

#' @title View PDF from R
#'
#' @description View a PDF in an external viewer
#' @param filename filename of PDF
#' @param viewer Name for the system command for viewer.  Defaults
#' to \code{pdfviewer} option
#' @param bg run as background process
#' @export
#' @keywords viewer
#' @return Result from \code{\link{system}} command
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

#' @title View PNG in R
#'
#' @description View a PNG in external viewer from R
#' @param filename filename of PNG 
#' @param viewer name of viewer in PATH.  Defaults
#' to "display" for ImageMagick
#' @param bg run as background process
#' @export
#' @keywords viewer
#' @return Result from \code{\link{system}} command
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
#' @description Views plots from devices or PDFs.  This function will
#' extract the extension from a filename and then uses the viewer 
#' specified, or a default option to view the image
#' @param filename file to be opened
#' @param viewer program to view plot, attempts to use getOption or 
#' display to find it
#' @param bg run as background process
#' 
#' @export
#' @keywords viewer
#' @return Result from system command
view = function(filename, viewer= NULL, bg=FALSE){
  stopifnot(length(filename) == 1)
  get.ext = gsub("(.*)\\.(.*)$", "\\2", filename)
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
#' @description Open a device from the filename extension. 
#' This function allows you to simply open a device by using 
#' the filename.  So that if you have .PNG as the extension it opens
#' a PNG, etc.
#' @param filename filename to open
#' @param type Type of device to open, i.e. "cairo",
#' (see \code{\link{png}})
#' @param ... arguments to be passed to device
#' @export
#' @keywords viewer
#' @return NULL, with deviced opened
#' @examples
#' fname = tempfile(fileext= ".png")
#' open_dev(fname)
#' plot(0, 0)
#' dev.off()
#' # view(fname)
#' # view(fname, viewer="open") 
#' fname = tempfile(fileext= ".pdf")
#' open_dev(fname,height=7, width=7)
#' plot(0, 0)
#' dev.off()
#' # view(fname)  
open_dev = function(filename, type= "cairo", ...){
  get.ext = gsub("(.*)\\.(.*)$", "\\2", filename)
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
