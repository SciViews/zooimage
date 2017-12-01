\name{zip}
\alias{zipImg}
\alias{zipImgAll}
\alias{unzipImg}
\alias{unzipImgAll}

\title{ Zip and unzip raw .tif images for ZooImage and embed metadata }
\description{
  Perform simple zip/unzip operations on images. Corresponding metadata
  from .zim files are embedded as zip comments.
}
\usage{
zipImg(imagefile, zimfile = NULL, check.zim = TRUE, replace = FALSE,
    delete.source = FALSE)
zipImgAll(path = ".", images = NULL, check.zim = TRUE, replace = FALSE,
    delete.source = FALSE)
unzipImg(zipfile, replace = FALSE, delete.source = FALSE)
unzipImgAll(path = ".", zipfiles = NULL, replace = FALSE,
    delete.source = FALSE)
}

\arguments{
  \item{imagefile}{ file path of the .tif image to compress. }
  \item{zimfile}{ file path of the corresponding .zim file (calculated
    automatically if \code{NULL}, by default). }
  \item{check.zim}{ do we verify the .zim file before zipping data? }
  \item{replace}{ do we replace existing .zip files? }
  \item{delete.source}{ should the original .tif file be deleted (the .zim file
    is never deleted)? }
  \item{path}{ directory where .tif images to be zipped are located. }
  \item{images}{ a list of .tif images to zip. If \code{NULL}, the list of all
    .tif images present in \code{path} is computed. }
  \item{zipfile}{ a zipfile to unzip. }
  \item{zipfiles}{ a list of zipfiles to unzip. If \code{NULL}, a list of all
    .zip files found is \code{path} is calculated. }
}

\value{
  All these functions are designed to be run in batch mode. Problems are
  reported as warnings, and the function always returns \code{TRUE} or
  \code{FALSE}, depending if the process succeeds or fails. The \code{xxxAll()}
  functions are convenient wrapper around \code{batch()} to process several
  items in a row. Take care that, despite the functions possibly use internal
  R code to zip or unzip files, they still need the zip and unzip programs
  for injecting and extracting .zim files metadata in the .zip archive!
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{\code{\link{zipNoteAdd}}, \code{\link{zipNoteGet}}}

\examples{
## Create a fake example of two .tif images and their associated .zim files
testdir <- file.path(tempdir(), "ziptest")
dir.create(testdir)
file.copy(system.file("examples", "BIO.2000-05-05.p72.zid",
    package = "zooimage"), testdir)
curdir <-setwd(testdir)
unzip("BIO.2000-05-05.p72.zid", junkpaths = TRUE)
## Keep only first 3 image plus the .zim file
unlink("BIO.2000-05-05.p72.zid")
unlink("BIO.2000-05-05.p72_dat1.RData")
unlink(dir(pattern = "[.]jpg$")[-(1:3)])
## Rename .jpg images, pretending they are .tif images
jpgFiles <- dir(pattern = "[.]jpg$")
tifFiles <- sub("[.]jpg$", ".tif", jpgFiles)
file.rename(jpgFiles, tifFiles)
## Recreate the .zim file
zimData <- readLines("BIO.2000-05-05.p72+A_dat1.zim", n = 32)
zimFile <- "BIO.2000-05-05.p72+A_.zim"
writeLines(zimData, zimFile)
unlink("BIO.2000-05-05.p72+A_dat1.zim")
## Here is what we got...
dir()

## Zip first image...
(zipImg(tifFiles[1]))
## It is added in the _raw subdirectory
dir()
zipDir <- file.path(".", "_raw")
dir(zipDir)

## Now, zip all images in batch and delete sources
(zipImgAll(".", delete.source = TRUE))
dir() # .zim files are never deleted!
(zipFiles <- dir(zipDir, full.names = TRUE)) # All three are there
## Force delete of the .zim file
unlink(zimFile)

## Unzip first zip file...
#####(unzipImg(zipFiles[1]))
## The image and .zim file are recreated
dir()
## Check extracted .zim file
#####all(readLines(zimFile) == zimData)

## Unzip all images...
(unzipImgAll(zipDir, replace = TRUE, delete.source = TRUE))
## All original files are there...
dir()
## and the _raw subdir is now empty
dir(zipDir)

## Reset and clean up
setwd(curdir)
unlink(testdir, recursive = TRUE)
rm(testdir, curdir, jpgFiles, tifFiles, zimFile, zimData, zipDir, zipFiles)
}

\keyword{ utilities }

