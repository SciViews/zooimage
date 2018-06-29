\name{zie}
\alias{ZIE}
\alias{print.ZIE}
\alias{zieMake}
\alias{zieCompile}
\alias{zieCompileFlowCAM}

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
    move.to.raw = TRUE, zip.images = "[.]tif$")

zieCompile(path = ".", Tablefile = "Table.txt", Template = "ImportTemplate.zie",
    Filemap = paste("Import_", noExtension(Tablefile), ".zie", sep = ""),
    Nrange = c(1, 1000), replace = TRUE, make.it = FALSE,
    zip.images = "[.]tif$")
zieCompileFlowCAM(path = ".", Tablefile, Template = "ImportTemplate.zie",
    check.names = TRUE)

ZIE(title, filter, description, pattern, command, author, version, date,
    license, url, depends = "R (>= 2.4.0), zooimage (>= 1.0-0)",
    type = c("import", "export"))
\method{print}{ZIE}(x, \dots)
}

\arguments{
  \item{path}{ path where the images and the .zie file are located. }
  \item{check.names}{ do wr check names in the .zie and .txt files? }
  \item{Filemap}{ name of the .zie file mapping images to convert. }
  \item{check}{ do we check if conversion programs (if any needed) are available? }
  \item{replace}{ do we replace existing files (not used yet)? }
  \item{move.to.raw}{ do we move processed files to the '_raw' subdirectory
  (currently, it is always the case)? }
  \item{zip.images}{ the pattern to use for images extension in the zipped file. }
  \item{Tablefile}{ a tab-delimited ASCII file containing a table defining
    samples and samples characteristics used to compile .zim files and to
    convert/rename images. }
  \item{Template}{ a template to use for creating the .zie file. }
  \item{Nrange}{ A range (two integers) giving min and max number of images for
  each sample/fraction. }
  \item{make.it}{ do we make the .zie (run its commands) after compiling it? }
  \item{title}{ the title of the ZooImage Extension. }
  \item{filter}{ a filter to apply to select concerned files. }
  \item{description}{ a short description of the ZooImage Extension. }
  \item{pattern}{ a regular expression to use to select concerned files. }
  \item{command}{ a string holding the R command to run to convert images. }
  \item{author}{ who wrote and maintains the ZooImage Extension? Please, provide
  also an email address. }
  \item{version}{ version of the ZooImage Extension as x.y-z, in a string. }
  \item{date}{ date at which the extension was compiled as yyyy-mm-dd. }
  \item{license}{ which are the license terms for this ZooImage Extension? }
  \item{url}{ a link to a web page dedicated to this ZooImage Extension. }
  \item{depends}{ which R packages does this ZooImage Extension requires? }
  \item{type}{ type of ZooImage extension; currently, only "import" or "export". }
  \item{x}{ a ZIE object. }
  \item{\dots}{ further arguments passes to the function (currently not in use). }
}

\value{
  \code{zieMake()}, \code{zieCompile()} and \code{zieCompileFlowCAM()} are invoked
  for their side-effects of importing images and metadata into a format that
  ZooImage can use. The first two functions return \code{TRUE} or \code{FALSE},
  depending if the importation was done without errors, or not. More information
  is issued as warnings. The last function returns the name of the .zie file
  resulting from the compilation. It returns \code{NULL} in case it failed to
  compile the .zie file.

  \code{ZIE()} creates a 'ZIE' object that describes a ZooImage Extension and
  allows to plug it in transparently inside the ZooImage GUI. \code{print.ZIE()}
  nicely formats the content of this objects when using the generic function
  \code{print()}.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{ \code{\link{zimMake}} }

\examples{
##TODO...

### For programmers ###
# Adding a new import filter is just a mather of writing the importation function
# and making it available to users by means of a ZIEimport object.
}

\keyword{ utilities }
