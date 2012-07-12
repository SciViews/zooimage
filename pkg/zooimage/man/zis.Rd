\name{zis}
\alias{zisCreate}
\alias{zisEdit}
\alias{zisRead}

\title{ Manipulate .zis files (ZooImage Samples description) }
\description{
  Additional data concerning the samples are collected together in .zis files.
  These functions manipulate such .zis files.
}
\usage{
zisCreate(zisfile, template = NULL, edit = TRUE, editor =
    getOption("fileEditor"), wait = FALSE)
zisEdit(zisfile, editor = getOption("fileEditor"), wait = FALSE, ...)
zisRead(zisfile = "Description.zis", expected.sections =
    c("Description", "Series", "Cruises", "Stations", "Samples"))
}

\arguments{
  \item{zisfile}{ the name of the .zis file to manipulate (usually,
    "Description.zis"). }
  \item{template}{ a .zis template to start with, if the .zis file does not
    exist yet. }
  \item{edit}{ do we edit the .zis file after its creation? }
  \item{editor}{ the program to use to edit the .zis file. }
  \item{wait}{ do we wait that edition of file is done? }
  \item{expected.sections}{ list of the sections that must be present in the
    .zis file. }
  \item{\dots}{ further arguments to pass to \code{fileEdit()}. }
}

\value{
  \code{zisRead()} returns a 'ZIDesc' object containing all the data in the .zis
  file, or, in case of fealure (detailed in a warning), it returns \code{NULL}.
  The two other functions return \code{TRUE} or \code{FALSE} invisibly,
  depending if the .zis file could be created/edited or not.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{ \code{\link{zidCompress}}, \code{\link[svMisc]{fileEdit}} }

\examples{
## Read content of the example zis file
zisFile <- system.file("examples", "Description.zis", package = "zooimage")
zisData <- zisRead(zisFile)
zisData  # These are data for samples
attr(zisData, "metadata")  # Further data (for the series) here

## Create a new .zis file in tempdir using the example .zis file as template
zisNew <- file.path(tempdir(), "Description.zis")
zisCreate(zisNew, template = zisFile, edit = FALSE)  # One can edit it here too

## Edit the new file, and wait edition is completed
zisEdit(zisNew, wait = TRUE)
## It contains:
if (interactive()) file.show(zisNew, title = basename(zisNew), delete.file = TRUE)

## Clean up
rm(zisFile, zisData, zisNew)
}

\keyword{ utilities }
