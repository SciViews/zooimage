\name{utilities}
\alias{sampleInfo}
\alias{underscoreToSpace}
\alias{trimString}
\alias{listSamples}
\alias{makeId}
\alias{calcVars}
\alias{ecd}
\alias{parseIni}


\alias{clearProgress}
\alias{getList}
\alias{getVar}
\alias{getDec}
\alias{Progress}
\alias{selectFile}


\alias{ZIpgm}
\alias{ZIpgmHelp}

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
calcVars(ZIDat)

ecd(area)

parseIni(data, label = "1")


getVar(class = "data.frame", default = "", multi = FALSE,
    title = paste("Choose a ", class, ":", sep = ""), warn.only = TRUE)
getList(class = "data.frame", default = "", multi = FALSE,
    title = paste("Choose a list (of ", class, "s):", sep = ""), warn.only = TRUE)
selectFile(type = c("ZipZid", "ZimZis", "LstZid", "ZidZidb", "Zip", "Zid", "Zidb",
    "Zim", "Zis", "Zie", "Zic", "Img", "TifPgm", "RData"), multi = FALSE,
    quote = TRUE, title = NULL)

getDec()

Progress(value, max.value = NULL)
clearProgress()

ZIpgm(pgm, subdir = "misc", ext = "exe")
ZIpgmHelp(pgm, subdir = "misc")
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
    \code{sampleInfo()}, or the type of file to select in \code{selectFile()} }
  \item{ext}{ the pattern to use (regular expression) to eliminate file
    extension from the 'filename' }
  \item{string}{ a character string to rework, or a vector of character strings }
  \item{ZIobj}{ a ZooImage object (here, a 'ZIDat', 'ZIDesc' or 'ZITrain' object) }
  \item{ZIDat}{ a 'ZIDat' object, or a data frame with correct column labels }
  \item{area}{ a numerical vector with areas from which ECDs are calculated
    (Equivalent Circular Diameter, a more suitable term for 2D images than ESD,
	Equivalent Spherical Diameter) }
  
  
  \item{class}{ the class of objects to retrieve (or class of list components
    for \code{getList()}) }
  \item{default}{ the default item selected in the list }
  \item{multi}{ are multiple selections allowed? }
  \item{title}{ the title of the dialog box }
  \item{warn.only}{ issue a warning if there is a problem, otherwise, the
    function stops execution. This is used in the ZooImage GUI, in order to
	avoid interrupting abruptly code execution and to report the error in a more
	"civilized" way to the user }
  \item{quote}{ do we add quotes (\") around file names? }
  \item{data}{ a vector containing the data to parse }
  \item{label}{ a label to include for the parsed data }
  \item{value}{ a value for the progression }
  \item{max.value}{ the maximum value, when task is done. If \code{NULL},
    assumes that value is in percent }
  \item{pgm}{ the name of an executable located in the zooimage package (under
    Windows), or on the system (elsewhere) }
  \item{subdir}{ the subdirectory (to the zooimage install directory) where the
    executable should be located under Windows (this has no effect on other
	platforms) }
}

\value{
  A string or vector of strings for \code{sampleInfo()}, \code{makeId()}, 
  \code{listSamples()}, \code{getVar()}, \code{getList()} and
  \code{selectFile()}. For those functions that use a dialog box,
  \code{character(0)} is returned to indicate the user clicked 'Cancel', while
  an empty string (\code{""}) is returned in case there is no corresponding
  element found.
  
  The combined object is returned for \code{list.add()}, \code{list.reduce()}
  and \code{list.merge}. \code{list.add()} is just a shortcut for
  \code{list.reduce(FUN = "+")}.

  The data.frame with additional columns for calculated variables with
  \code{calcVars}

  A vector of numerical values for \code{ecd}.

  Transformed strings for \code{noext()}, \code{trimstring()} and
  \code{underscore2space()}

  \code{parseIni()} reads the data and creates a list of data frames. Each
  entry in the list maps one section in the ini file (with the same name). For
  'key=value' pairs, a one line data frame containing values and with keys as
  column names. The first column of these data frames is named label and get the
  corresponding value passed by the 'label' argument. That way, one can easily
  keep track of entries when data frames originated from various different ini
  files are merged together (typically using \code{list.add()} or
  \code{list.merge()}.

  Nothing for \code{Progress()} and \code{clearProgress()} (they
  are used in the GUI for their side-effect there).

  \code{ZIpgm()} returns a string usable to start the given program at the
  command line (or using \code{system()}). Under Windows, it returns the full
  path, but in short (DOS) form. So, you don't have to worry about putting this
  string between quotes, in case the path would content blank characters. If the
  program is not found, the function returns "". \code{ZIpgmHelp()} displays the
  help file (man page) of the corresponding executable.
}

\details{
  As these functions are not made to be directly used by end-users, We don't
  give more details here. Developers interested to use these functions are
  encouraged to look at their code in the zooimage package source!
}

\author{Philippe Grosjean <Philippe.Grosjean@umons.ac.be>}

\seealso{\code{\link{noExtension}}}

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

### TODO: a listSamples() example

# Variables calculation utilities
df__ <- data.frame(Label = c("Alabel", "AnotherLabel"), Item = c("01", "02"))
makeId(df__)
rm(df__)
ecd(1:10)

### TODO: a calcVars() and parseIni() examples

\dontrun{
# Create two datasets in R and ask for selecting one:
df1__ <- data.frame(x = 1:3, y = 4:6)
df2__ <- data.frame(z = 1:10)
getVar() # Try also to click 'Cancel'
getVar(multi = TRUE) # Can select both too
getVar("nonexistingclass") # Returns an empty string!
getVar("nonexistingclass", warn.only = FALSE)

# Create lists containing only data frames as components
lst1__ <- list(A = df1__, B = df2__)
lst2__ <- list(C = df1__)
getList() # Try also to click 'Cancel'
getList(multi = TRUE) # Can select both
getList("nonexistingclass")
rm(df1__, df2__, lst1__, lst2__)

# Select one or more ZooImage files
selectFile() # One Zip or Zid file

# A couple of command line executables used by zooimage
ZIpgm("zip")
ZIpgm("pgmhist", "netpbm")
ZIpgm("pnm2biff", "xite")

# ...and their help pages
ZIpgmHelp("zip")
ZIpgmHelp("pgmhist", "netpbm")
ZIpgmHelp("pnm2biff", "xite")
}
}

\keyword{ utilities }
