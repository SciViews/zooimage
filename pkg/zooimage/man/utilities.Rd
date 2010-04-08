\name{utilities}
\alias{calc.vars}
 \alias{ecd}
 \alias{get.sampleinfo}
\alias{getKey}
 \alias{getList}
\alias{gettextZI}
 \alias{getVar}
\alias{getDec}
\alias{list.add}
\alias{list.merge}
\alias{list.samples}
 \alias{make.Id}
 \alias{noext}
\alias{parse.ini}
\alias{Progress}
 \alias{selectFile}
\alias{setKey}
\alias{setwd}
 \alias{trim}
 \alias{underscore2space}
\alias{ZIpgm}
\alias{ZIpgmhelp}

\title{ Various utility functions used by ZooImage }
\description{
  These functions are usually not called directly by the user, but they are
  interesting for developers.
}

\usage{
list.add(x, y)
list.merge(x, y)

calc.vars(x)
ecd(area)
make.Id(df)
get.sampleinfo(filename, type = c("sample", "fraction", "image", "scs",
    "date", "id", "frac", "imgnbr"), ext = "_dat1[.]zim$")
list.samples(obj)

 getVar(class = "data.frame", default = "", multi = FALSE,
	title = paste("Choose a ", class, ":", sep = ""), warn.only = TRUE)
 getList(class = "data.frame", default = "", multi = FALSE,
	title = paste("Choose a ", class, ":", sep=""), warn.only = TRUE)
 selectFile(type = c("ZipZid", "ZimZis", "Zip", "Zid", "Zim", "Zis", "Zie"),
    multi = FALSE, quote = TRUE)

getDec()

 noext(file)
 trim(char)
 underscore2space(char)
parse.ini(data, label = "1")

Progress(value, max.value = NULL)
setwd(dir)

getKey(key, default.value = NULL)
setKey(key, value, type = "sz")

gettextZI(...)

ZIpgm((pgm, subdir = "misc", ext = "exe")
ZIpgmhelp((pgm, subdir = "misc")
}

\arguments{
  \item{x}{ A 'ZIDat' object, or a data frame with correct column labels }
  \item{y}{ Another 'ZIDat' object or data frame }
  \item{area}{ A numerical vector with areas from which ECDs are calculated
    (Equivalent Circular Diameter, a more suitable term for 2D images than ESD,
	Equivalent Spherical Diameter) }
  \item{df}{ A data frame }
  \item{filename}{ Name of a file from which to extract information. It is
    supposed to be spelled as: SCS.xxxx-xx-xx.SS+Fnn.ext where 'SCS' is the
	series-cruise-station code, 'xxxx-xx-xx' is the date of collection
	(year-month-day), 'SS' is the unique sample identifier, 'F' is the fraction,
	'nn' is the image number (when there are several images per fraction) or
	'nn.mmm' when there are 'mmm' pictures taken to cover a bigger area of cell
	'nn', and 'ext' is the file extension. }
  \item{type}{ The type of data to extract (see examples) for
    \code{get.sampleinfo()}, the format to use for a key in \code{setKey()}, or
	the type of file to select in \code{selectFile()} }
  \item{ext}{ The pattern to use (regular expression) to eliminate file
    extension from the 'filename' }
  \item{obj}{ Usually a 'ZIDat' object from which all samples are listed }
  \item{class}{ The class of objects to retrieve (or class of list components
    for \code{getList()}) }
  \item{default}{ The default item selected in the list }
  \item{multi}{ Are multiple selections allowed? }
  \item{title}{ The title of the dialog box }
  \item{warn.only}{ Issue a warning if there is a problem, otherwise, the
    function stops execution. This is used in the ZooImage GUI, in order to
	avoid interrupting abruptly code execution and to report the error in a more
	"civilized" way to the user }
  \item{quote}{ Do we add quotes (\") around file names? }
  \item{file}{ A filename }
  \item{char}{ A character string to rework, or a vector of character strings }
  \item{data}{ A vector containing the data to parse }
  \item{label}{ A label to include for the parsed data }
  \item{value}{ A value for the progression }
  \item{max.value}{ The maximum value, when task is done. If \code{NULL},
    assumes that value is in percent }
  \item{dir}{ The directory to switch to }
  \item{key}{ The registry key to manipulate }
  \item{default.value}{ Value to use if the key is absent in the registry }
  \item{value}{ Data to place in the key }
  \item{\dots}{ Some strings to be translated in the current language }
  \item{pgm}{ The name of an executable located in the zooimage package (under
    Windows), or on the system (elsewhere) }
  \item{subdir}{ The subdirectory (to the zooimage install directory) where the
    executable should be located under Windows (this has no effect on other
	platforms) }
  \item{ext}{ The extension for the executable under Windows (usually, exe).
    This has no effect on other platforms) }
}

\value{
  The combined object is returned for \code{list.add()} and \code{list.merge}.

  The data.frame with additional columns for calculated variables with
  \code{calc.vars}

  A vector of numerical values for \code{ecd}.

  A string or vector of strings for \code{make.Id()}, \code{get.sampleinfo()},
  \code{list.samples()}, \code{getVar()}, \code{getList()} and
  \code{selectFile()}. For those functions that use a dialog box,
  \code{character(0)} is returned to indicate the user clicked 'Cancel', while
  an empty string (\code{""}) is returned in case there is no corresponding
  element found.

  Transformed strings for \code{noext()}, \code{trim()} and
  \code{underscore2space()}

  \code{parse.ini()} reads the data and creates a list of data frames. Each
  entry in the list maps one section in the ini file (with the same name). For
  'key=value' pairs, a one line data frame containing values and with keys as
  column names. The first column of these data frames is named label and get the
  corresponding value passed by the 'label' argument. That way, one can easily
  keep track of entries when data frames originated from various different ini
  files are merged together (typically using \code{list.add()} or
  \code{list.merge()}.

  Nothing for \code{Progress()} and \code{setwd()}.

  For \code{getKey()}, either the content of the key, or the default.value is
  returned. \code{setKey()} returns \code{TRUE} or \code{FALSE}, depending if
  the key=value was successfully entered in the registry or not.

  The translated string is returned for \code{gettextZI()}.

  \code{ZIpgm()} returns a string usable to start the given program at the
  command line (or using \code{system()}). Under Windows, it returns the full
  path, but in short (DOS) form. So, you don't have to worry about putting this
  string between quotes, in case the path would content blank characters. If the
  program is not found, the function returns "". \code{ZIpgmhelp()} displays the
  help file (man page) of the corresponding executable.
}

\details{
  As these functions are not made to be directly used by end-users, We don't
  give more details here. Developers interested to use these functions are
  encouraged to look at their code in the zooimage package source: it is fully
  documented there with plenty of comments!
}

\author{Philippe Grosjean <Philippe.Grosjean@umons.ac.be>}

\seealso{\code{\link{logProcess}}}

\examples{
# Character strings and filenames manipulation functions
underscore2space("Some_string_to_convert")
trim("    \tString with\textra spaces  \t")
noext(paste(tempfile(), ".ext", sep = ""))

# Given a correct ZooImage name for a sample, return parts of it
smp__ <- "MTLG.2010-03-15.H1+A1.03_dat1.zim"
get.sampleinfo(smp__, "sample")
get.sampleinfo(smp__, "fraction")
get.sampleinfo(smp__, "image")
get.sampleinfo(smp__, "scs")
get.sampleinfo(smp__, "date")
get.sampleinfo(smp__, "id")
get.sampleinfo(smp__, "frac")
get.sampleinfo(smp__, "imgnbr")
rm(smp__)

# Variables calculation utilities
df__ <- data.frame(Label = c("Alabel", "AnotherLabel"), Item = c("01", "02"))
make.Id(df__)
rm(df__)
ecd(1:10)

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
ZIpgmhelp("zip")
ZIpgmhelp("pgmhist", "netpbm")
ZIpgmhelp("pnm2biff", "xite")
}
}

\keyword{ utilities }
