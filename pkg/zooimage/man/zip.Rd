\name{zip}
\alias{zip.img}
\alias{zip.img.all}
\alias{unzip.img}
\alias{unzip.img.all}

\title{ Zip and unzip raw .tif images for ZooImage and embed metadata }
\description{
  Perform simple zip/unzip operations on images. Corresponding metadata
  from .zim files are embedded as zip comments.
}
\usage{
zip.img(imagefile, verify.zimfile = TRUE, replace = FALSE, delete.source = TRUE,
    check.zip = TRUE, show.log = TRUE)
zip.img.all(path = ".", images = NULL, check = TRUE, replace = FALSE,
    delete.source = replace, show.log = TRUE, bell = FALSE)
unzip.img(zipfile)
unzip.img.all(path = ".", zipfiles = NULL)
}

\arguments{
  \item{imagefile}{ The filename of the image }
  \item{verify.zimfile}{ Do we verify the .zim file before zipping data? }
  \item{replace}{ Do we replace existing .zip files? }
  \item{delete.source}{ Should the original file be deleted? }
  \item{check.zip}{ Do we verify first that the Info-Zip 'zip' program is available? }
  \item{show.log}{ Do we display the log at the end of the process? }
  \item{path}{ The path where image to zip are located }
  \item{images}{ A list of images to zip. If \code{NULL}, the list of all .tif images
    present in \code{path} is computed }
  \item{check}{ Do we check the validity of associated .zim files before compression? }
  \item{bell}{ Do we ring the bell after a long process? }
  \item{zipfile}{ A zipfile to unzip }
  \item{zipfiles}{ A list of zipfiles to unzip }
}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }

\examples{
##TODO...
}
\keyword{ utilities }

