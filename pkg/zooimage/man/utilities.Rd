\name{utilities}
\alias{sampleInfo}
\alias{underscoreToSpace}
\alias{trimString}
\alias{listSamples}
\alias{makeId}
\alias{calcVars}
\alias{ecd}
\alias{parseIni}
\alias{calibrate}
\alias{getDec}
\alias{zipNoteAdd}
\alias{zipNoteGet}

\title{ Various utility functions used by ZooImage }
\description{
  These functions are usually not called directly by the user, but they are
  interesting for developers.
}

\usage{
sampleInfo(filename, type = c("sample", "fraction", "image", "scs",
    "date", "id", "frac", "imgnbr"), ext = "_dat1[.]zim$")

underscoreToSpace(string)
trimString(string)

listSamples(ZIobj)
makeId(ZIDat)
calcVars(ZIDat, keep.vars = getOption("ZI.keep.vars"))

ecd(area)

parseIni(data, label = "1")

calibrate(ODfile)

getDec()

zipNoteAdd(zipfile, zimfile)
zipNoteGet(zipfile, zimfile = NULL)
}

\arguments{
  \item{filename}{ name of a file from which to extract information. It is
    supposed to be spelled as: SCS.xxxx-xx-xx.SS+Fnn.ext where 'SCS' is the
    series-cruise-station code, 'xxxx-xx-xx' is the date of collection
    (year-month-day), 'SS' is the unique sample identifier, 'F' is the fraction,
    'nn' is the image number (when there are several images per fraction) or
    'nn.mmm' when there are 'mmm' pictures taken to cover a bigger area of cell
    'nn', and 'ext' is the file extension. }
  \item{type}{ the type of data to extract (see examples) for
    \code{sampleInfo()}, or the type of file to select in \code{selectFile()}. }
  \item{ext}{ the pattern to use (regular expression) to eliminate file
    extension from the 'filename'. }
  \item{string}{ a character string to rework, or a vector of character strings. }
  \item{ZIobj}{ a ZooImage object (here, a 'ZIDat', 'ZIDesc' or 'ZITrain' object). }
  \item{ZIDat}{ a 'ZIDat' object, or a data frame with correct column labels. }
  \item{keep.vars}{ a list of the variables to keep in the classification, if
    the argument is not of length 0 (or \code{NULL}). }
  \item{area}{ a numerical vector with areas from which ECDs are calculated
    (Equivalent Circular Diameter, a more suitable term for 2D images than ESD,
	Equivalent Spherical Diameter). }
  \item{data}{ a vector containing the data to parse. }
  \item{label}{ a label to include for the parsed data. }
  \item{ODfile}{ an image file of O.D. calibrated items that can be used to
    calibrate grayscales. }
  \item{zipfile}{ a zip archive. }
  \item{zimfile}{ a .zim file to use, or to create. If \code{NULL} (default)
    in \code{zipNoteGet()}, the data are not written in a file, but returned. }
}

\value{
  A string or vector of strings for \code{sampleInfo()}, \code{listSamples()}
  and \code{makeId()}. For those functions, \code{character(0)} is returned to
  indicate a problem (usually with a warning issued to explain it), while
  an empty string (\code{""}) is returned in case there is no corresponding
  element found.

  The data.frame with additional columns for calculated variables with
  \code{calcVars()}.

  A vector of numerical values for \code{ecd()}.

  Transformed strings for \code{trimstring()} and \code{underscoreToSpace()}

  \code{parseIni()} reads the data and creates a list of data frames. Each
  entry in the list maps one section in the ini file (with the same name). For
  'key=value' pairs, a one line data frame containing values and with keys as
  column names. The first column of these data frames is named label and get the
  corresponding value passed by the 'label' argument. That way, one can easily
  keep track of entries when data frames originated from various different ini
  files are merged together.
  
  \code{calibrate()} returns a vector of two numbers with white and black point
  calibration (gray levels corresponding, respectively to O.D. = 0 and
  O.D. = 1.024), plus a "msg" attribute with some explanation in case of problem.
  
  \code{zipNoteAdd()} returns \code{TRUE} or \code{FALSE} depending if the data
  from the zimfile was successfully added to the zip archive or not. Problem is
  returned in a warning.
  \code{zipNoteGet()} returns the comment included in the zip archive
  (invisibly if 'zimfile' is not \code{NULL}), \code{character(0)} if no comment
  if found, or \code{NULL} in case of a problem. The problem is detailled in a
  warning.
}

\details{
  As these functions are not made to be directly used by end-users, We don't
  give more details here. Developers interested to use these functions are
  encouraged to look at their code in the zooimage package source!
}

\author{Philippe Grosjean <Philippe.Grosjean@umons.ac.be>}

\seealso{\code{\link{noExtension}}, \code{\link{selectObject}}}

\examples{
# Given a correct ZooImage name for a sample, return parts of it
smp__ <- "MTLG.2010-03-15.H1+A1.03_dat1.zim"
sampleInfo(smp__, "sample")
sampleInfo(smp__, "fraction")
sampleInfo(smp__, "image")
sampleInfo(smp__, "scs")
sampleInfo(smp__, "date")
sampleInfo(smp__, "id")
sampleInfo(smp__, "frac")
sampleInfo(smp__, "imgnbr")
rm(smp__)
sampleInfo(c("ScanG16.2004-10-20+A1.tif", "ScanG16.2004-10-20+B1.tif"),
    type = "sample", ext = extensionPattern("tif"))

# Character strings manipulation functions
underscoreToSpace("Some_string_to_convert")
trimString("    \tString with\textra spaces  \t")

# Variables calculation utilities
df__ <- data.frame(Label = c("Alabel", "AnotherLabel"), Item = c("01", "02"))
makeId(df__)
rm(df__)
ecd(1:10)

### TODO: calibrate(), calcVars(), parseIni(), zipNoteAdd() and zipNoteGet() examples
}

\keyword{ utilities }
