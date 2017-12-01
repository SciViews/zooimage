\name{zid}
\alias{zidClean}
\alias{zidCompress}
\alias{zidCompressAll}
\alias{zidDatMake}
\alias{zidDatRead}
\alias{zidUncompress}
\alias{zidUncompressAll}
\alias{zidVerify}
\alias{zidVerifyAll}

\title{ Manage .zid files (ZooImage Data) }
\description{
  Compress, uncompress and verify ZooImage Data files.
}

\usage{
zidCompress(zidir, type = c("ZI1", "ZI2", "ZI3", "ZI4", "ZI5"), check = TRUE,
    check.vignettes = TRUE, replace = FALSE, delete.source = replace)
zidCompressAll(path = ".", samples = NULL, type = c("ZI1", "ZI2", "ZI3", "ZI4", "ZI5"),
    check = TRUE, check.vignettes = TRUE, replace = FALSE,
    delete.source = replace)

zidClean(path = ".", samples = NULL)
zidDatMake(zidir, type = "ZI5", replace = FALSE)
zidDatRead(zidfile)

zidUncompress(zidfile, path = dirname(zidfile), delete.source = FALSE)
zidUncompressAll(path = ".", zidfiles = zidList(path, full.names = TRUE),
    path.extract = path, skip.existing.dirs = TRUE, delete.source = FALSE)

zidVerify(zidir, type = c("ZI1", "ZI2", "ZI3", "ZI4", "ZI5"), check.vignettes = TRUE)
zidVerifyAll(path = ".", samples = NULL, type = c("ZI1", "ZI2", "ZI3", "ZI4", "ZI5"),
    check.vignettes = TRUE)
}

\arguments{
  \item{zidir}{ a directory containing data to put in a .zid files. }
  \item{type}{ the ZI file format, currently 'ZI1', 'ZI2', 'ZI3', 'ZI4', or 'ZI5' types are
    supported (ZooImage1-5). }
  \item{check}{ do we check the files in this directory before/after compression? }
  \item{check.vignettes}{ do we check if the future .zid file contains all vignettes? }
  \item{replace}{ do we replace existing files? }
  \item{delete.source}{ do we delete source files after compression? }
  \item{path}{ (un)compress or verify all subdirectories (except those starting with '\_')
    in respective .zid files. Also, a place where to put uncompressed files (in the 'sample' subdirectory). }
  \item{samples}{ a list of 'samples', i.e., subdirectories to process. }
  \item{zidfile}{ a .zid file to uncompress or read from. }
  \item{zidfiles}{ a series of .zid files to uncompress. }
  \item{path.extract}{ a place where to extract data from .zid files. }
  \item{skip.existing.dirs}{ do not unzip if the subdir already exists. }
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{ \code{\link{zimMake}} }

\examples{
##TODO...
}

\keyword{ utilities }
