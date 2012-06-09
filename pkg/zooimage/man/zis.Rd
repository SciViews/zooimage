\name{zis}
\alias{zisCreate}
\alias{zisEdit}
\alias{readDescription}

\title{ Manipulate .zis files (ZooImage Samples data) }
\description{
  Additional data concerning the samples are collected together in .zis files. These functions
  manipulate such .zis files.
}
\usage{
zisCreate(zisfile = NULL, template = NULL, editor = getOption("ZIEditor"),
    wait = FALSE)
zisEdit(zisfile, editor = getOption("ZIEditor"), wait = FALSE)
readDescription(zisfile = "Description.zis" , expected.sections =
    c("Description", "Series", "Cruises", "Stations", "Samples"))
}

\arguments{
  \item{zisfile}{ The name of the .zis file to manipulate (usually, "Description.zis") }
  \item{template}{ A .zis template to start with, if the .zis file does not exist yet }
  \item{editor}{ The program to use to edit the .zis file }
  \item{wait}{ Do we wait that the file is edited? }
  \item{expected.sections}{ The list of the sections that must be present in the
    .zis file }
}

\value{
  \code{readDescription()} returns 'ZIDesc' object containing all the data in the .zis file.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }

\seealso{ \code{\link{zidCompress}} }
\examples{
##TODO...
}
\keyword{ utilities }

