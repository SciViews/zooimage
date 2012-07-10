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
zipImg(imagefile, zimfile = NULL, verify.zimfile = TRUE, replace = FALSE,
    delete.source = FALSE)
zipImgAll(path = ".", images = NULL, check = TRUE, replace = FALSE,
    delete.source = FALSE)
unzipImg(zipfile, replace = FALSE, delete.source = FALSE)
unzipImgAll(path = ".", zipfiles = NULL, replace = FALSE,
    delete.source = FALSE)
}

\arguments{
  \item{imagefile}{ file path of the .tif image to compress. }
  \item{zimfile}{ file path of the corresponding .zim file (calculated
    automatically if \code{NULL}, by default). }
  \item{verify.zimfile}{ do we verify the .zim file before zipping data? }
  \item{replace}{ do we replace existing .zip files? }
  \item{delete.source}{ should the original .tif file be deleted (the .zim file
    is never deleted)? }
  \item{path}{ directory where .tif images to be zipped are located. }
  \item{images}{ a list of .tif images to zip. If \code{NULL}, the list of all
    .tif images present in \code{path} is computed. }
  \item{check}{ do we check the validity of associated .zim files before
    starting batch compression of all images? }
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
##TODO...
}
\keyword{ utilities }

