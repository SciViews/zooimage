\name{zis}
\alias{createZis}
\alias{editZis}
\alias{read.description}

\title{ Manipulate .zis files (ZooImage Samples data) }
\description{
  Additional data concerning the samples are collected together in .zis files. These functions
  manipulate such .zis files.
}
\usage{
createZis(zisfile = NULL, template = NULL, editor = getOption("ZIEditor"),
    wait = FALSE)
editZis(zisfile, editor = getOption("ZIEditor"), wait = FALSE)
read.description(zisfile = "Description.zis")
}

\arguments{
  \item{zisfile}{ The name of the .zis file to manipulate (usually, "Description.zis") }
  \item{template}{ A .zis template to start with, if the .zis file does not exist yet }
  \item{editor}{ The program to use to edit the .zis file }
  \item{wait}{ Do we wait that the file is edited? }
}

\value{
  \code{read.description()} returns 'ZIDesc' object containing all the data in the .zis file.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }

\seealso{ \code{\link{compress.zid}} }
\examples{
##TODO...
}
\keyword{ utilities }

