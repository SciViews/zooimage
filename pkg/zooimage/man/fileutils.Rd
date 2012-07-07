\name{file-utilities}
\alias{extensionPattern}
\alias{hasExtension}
\alias{noExtension}
\alias{listFilesExt}
\alias{zimList}
\alias{zimDatList}
\alias{zipList}
\alias{zidList}
\alias{zidbList}
\alias{jpgList}
\alias{pngList}
\alias{checkFileExists}
\alias{checkDirExists}
\alias{checkEmptyDir}
\alias{forceDirCreate}
\alias{checkFirstLine}


\title{ Various file utility functions used by ZooImage }
\description{
  These functions are usually not called directly by the user, but they are
  interesting for developers. They help to manage files in the context of
  ZooImage processes.
}

\usage{
extensionPattern(extension = "r", add.dot = !grepl("[.]", extension))
hasExtension(file, extension = "r", pattern = extensionPattern(extension))
noExtension(file)

listFilesExt(dir, extension = "r", pattern = extensionPattern(extension), ... )
zimList(dir, ...)
zimDatList(dir, ...)
zipList(dir, ...)
zidList(dir, ...)
zidbList(dir, ...)
jpgList(dir, ...)
pngList(dir, ...)

checkFileExists(file, extension, message = "file not found: \%s", force.file = FALSE)
checkDirExists(dir, message = 'Path "\%s" does not exist or is not a directory')
checkEmptyDir(dir, message = 'dir "\%s" is not empty')
forceDirCreate(dir)

checkFirstLine(file, expected = c("ZI1", "ZI2", "ZI3"), 
    message = 'file "\%s" is not a valid ZooImage version <= 3 file')
}

\arguments{
  \item{extension}{ lowercase version of the extension (the pattern will be
    constructed to be case-insensitive). }
  \item{add.dot}{ if a dot is not provided, it is added by default in front of
    the pattern. }
  \item{file}{ one or more file names or file paths to check. }
  \item{pattern}{ a pattern matching a given file extension. }
  \item{\dots}{ further arguments passed to the function. Currently, not in use. }
  \item{dir}{ the directory to work with. }
  \item{message}{ a warning message to provide (file/dirname replacement using \%s). }
  \item{force.file}{ make sure the item is a file, not a directory. }
  \item{expected}{ the expected content of the first line of the file. }
}

\value{
  A string with suitable pattern to match a file extension for
  \code{extensionPattern()}.
  
  The function \code{noExtension()} return base filenames without extensions.

  A list of files with given extension for \code{listFilesExt()}, and
  \code{xxxList()} functions.
  
  The other functions return \code{TRUE} or \code{FALSE}, depending if the
  tested condition is met or not.
}

\details{
  All these function issue only warnings, no errors. Those functions that
  return \code{TRUE} or \code{FALSE} are designed to be used in batch mode.
}

\author{Philippe Grosjean <Philippe.Grosjean@umons.ac.be>}

\seealso{\code{\link{sampleInfo}}}

\examples{
# Construct a suitable pattern to match extensions of TIFF image files
extensionPattern("tif")
# Test if file names match given extensions (first 2 items only)
hasExtension(c("test1.tif", "test2.TIF", "test3.R"), "tif")
noExtension(c("test1.tif", "test2.TIF", "test3.R"))

# List all files with a given extension in a directory
ziDir <- system.file("examples", package = "zooimage")
listFilesExt(ziDir, "zid")
zidList(ziDir) # Idem

# Check that a file or a directory exists
checkDirExists(ziDir)
zisFile <- file.path(ziDir, "Description.zis")
checkFileExists(zisFile)

# Is this directory empty? (no)
checkEmptyDir(ziDir)

# force (re)creation of a directory
tmpDir <- file.path(tempdir(), "testdir")
forceDirCreate(tmpDir)
file.info(tmpDir)$isdir # yes
checkEmptyDir(tmpDir)   # yes
file.remove(tmpDir)
file.exists(tmpDir)

# Every .zis file must start with ZI1-3 => check this...
checkFirstLine(zisFile)

# Clean up
rm(ziDir, zisFile, tmpDir)
}

\keyword{ utilities }
