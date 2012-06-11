\name{zie}
\alias{ZIE}
\alias{print.ZIE}
\alias{zieMake}
\alias{zieCompile}
\alias{rawConvert}

\title{ ZIE (ZooImage Extension) manipulate .zie files to add custom import, export, etc. routines to ZooImage  }
\description{
  A .zie file describes sequentially images and tells how to convert and rename
  them and automates creation of associated metadata (.zim files). At the end
  of the process, one ends with complete and fully usable information (both
  the images with correct format/name and the metadata), so that you can proceed
  in ZooImage with imported data.
}
\usage{
zieMake(path = ".", Filemap = "Import_Table.zie", check = TRUE, replace = FALSE,
	move.to.raw = TRUE, zip.images = "[.][tT][iI][fF]$",show.log = TRUE,
	bell = FALSE)
zieCompile(path = ".", Tablefile = "Table.txt", Template = "ImportTemplate.zie",
    Filemap = paste("Import_", noExt(Tablefile), ".zie", sep = ""),
	Nrange = c(1, 1000), replace = TRUE, make.it = FALSE, show.log = make.it)
ZIE(title, filter, description, pattern, command, author, version, date,
    license, url, depends = "R (>= 2.4.0), zooimage (>= 1.0-0)",
    type = c("import", "export"))
\method{print}{ZIE}(x, \dots)
rawConvert(RawFile, OutputFile = "fileconv.pgm",
	DcRawArgs = "-v -c -4 -q 3 -t 0 -k 0", fake = FALSE, replace = FALSE,
	check = TRUE)
}

\arguments{
  \item{path}{ The path where the images and the .zie file are located }
  \item{Filemap}{ The name of the .zie file mapping images to convert }
  \item{check}{ Do we check if conversion programs (dc_raw and ppmtopgm) are
  available? }
  \item{replace}{ Do we replace existing files (not used yet)? }
  \item{move.to.raw}{ Do we move processed files to the '_raw' subdirectory
  (currently, it is always the case)? }
  \item{zip.images}{ The pattern to use for images extension in the zipped file }
  \item{show.log}{ Do we display the log at the end of the process? }
  \item{bell}{ Do we ring the bell when the process is done? }
  \item{Tablefile}{ A tab-delimited ASCII file containing a table defining
    samples and samples characteristics used to compile .zim files and to
	convert/rename images }
  \item{Template}{ A template to use for creating the .zie file }
  \item{Nrange}{ A range (two integers) giving min and max number of images for
  each sample/fraction }
  \item{make.it}{ Do we make the .zie (run its commands) after compiling it? }
  \item{title}{ The title of the ZooImage Extension }
  \item{filter}{ A filter to apply to select concerned files }
  \item{description}{ A short description of the ZooImage Extension }
  \item{pattern}{ A regular expression to use to select concerned files }
  \item{command}{ A string holding the R command to run to convert images }
  \item{author}{ Who wrote and maintains the ZooImage Extension? Please, provide
  also an email address }
  \item{version}{ Version of the ZooImage Extension as x.y-z, in a string }
  \item{date}{ Date at which the extension was compiled as yyyy-mm-dd }
  \item{license}{ Which are the license terms for this ZooImage Extension? }
  \item{url}{ A link to a web page dedicated to this ZooImage Extension }
  \item{depends}{ Which R packages does this ZooImage Extension requires? }
  \item{type}{ Type of ZooImage extension; currently, only "import" or "export" }
  \item{x}{ A ZIE object }
  \item{\dots}{ further arguments passes to the function (currently not in use) }
  \item{RawFile}{ A RAW file from a digital camera to convert }
  \item{OutputFile}{ The name of the output file after conversion }
  \item{DcRawArgs}{ Arguments to use for the RAW conversion file (dc_raw) }
  \item{fake}{ Do we create a fake file (test only)? }
}

\value{
  \code{zieMake()} and \code{zieCompile()} anre invoked for their side-effects of
  importing images and metadata into a format that ZooImage can use. The first
  function returns \code{TRUE} or \code{FALSE}, depending if the importation was
  done without errors, or not. More information is written in the log file. The second
  function returns the name of the .zie file resulting from the compilation, ar it
  returns \code{FALSE} invisibly in case it failed to compile the .zie file.

  \code{ZIE()} creates a 'ZIE' object that describes a ZooImage Extension and allows
  to plug it in transparently inside the ZooImage GUI. \code{print.ZIE()} nicely
  formats the content of this objects when using the generic function \code{print()}.

}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }

\seealso{ \code{\link{logProcess}}, \code{\link{zimMake}} }

\examples{
##TODO...

### For programmers ###
# Adding a new import filter is just a mather of writing the importation function
# and making it available to users by means of a ZIEimport object.
# See the example ./etc/FlowCam.r
# Simply sourcing this file makes it available in the ZooImage environment:
###TODO: add this file!
###source(file.path(getTemp("ZIetc"), "FlowCam.r"))
# Now look at the supported types in the 'Import' dialog box
}
\keyword{ utilities }
