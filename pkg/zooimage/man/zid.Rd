\name{zid}
\alias{makeRData}
\alias{zidClean}
\alias{zidCompress}
\alias{zidCompressAll}
\alias{zidRead}
\alias{zidUncompress}
\alias{zidUncompressAll}
\alias{zidVerify}
\alias{zidVerifyAll}
\alias{zidExtract}


\title{ Manage .zid files (ZooImage Data) }
\description{
  Compress, uncompress and verify ZooImage Data files.
}
\usage{
zidCompress(zidir, type = "ZI1", check = TRUE, check.vignettes = TRUE,
    replace = FALSE, delete.source = replace, check.zip = TRUE, show.log = TRUE)
zidCompressAll(path = ".", samples = NULL, type = "ZI1", check = TRUE,
    check.vignettes = TRUE, replace = FALSE, delete.source = replace,
    show.log = TRUE, bell = FALSE)
makeRData(zidir, type = "ZI1", replace = FALSE, show.log = TRUE)
zidClean(path = ".", samples = NULL)
zidRead(zidfile)
zidUncompress(zidfile, path = dirname(zidfile), delete.source = FALSE,
    show.log = TRUE)
zidUncompressAll(path = ".", zidfiles = zidList(path), path.extract = path,
    skip.existing.dirs = TRUE, delete.source = FALSE, show.log = TRUE,
    bell = FALSE)
zidVerify(zidir, type = "ZI1", check.vignettes = TRUE, show.log = TRUE)
zidVerifyAll(path = ".", samples = NULL, type = "ZI1", check.vignettes = TRUE,
    show.log = TRUE, bell = FALSE)
zidExtract(file, zidfile)
}

\arguments{
  \item{zidir}{ A directory containing data to put in a .zid files }
  \item{type}{ Currently, only 'ZI1' type is supported (ZooImage1) }
  \item{check}{ Do we check the files in this directory before/after compression? }
  \item{check.vignettes}{ Do we check if the future .zid file contains all vignettes? }
  \item{replace}{ Do we replace existingfiles? }
  \item{delete.source}{ Do we delete source files after compression? }
  \item{check.zip}{ Do we verify that the 'zip' program is availalble? }
  \item{show.log}{ Do we show the log file at the end? }
  \item{path}{ (Un)compress or verify all subdirectories (except those starting with '\_')
    in respective .zid files. Also, a place where to put uncompressed files (in the 'sample' subdirectory) }
  \item{samples}{ A list of 'samples', i.e., subdirectories to process }
  \item{bell}{ Do we ring the bell at the end of the process? }
  \item{zidfile}{ A .zid file to uncompress or read from }
  \item{zidfiles}{ A series of .zid files to uncompress }
  \item{path.extract}{ A place where to extract data from .zid files }
  \item{skip.existing.dirs}{ Do not unzip if the subdir already exists }
  \item{file}{ A file to extract form a .zid file (you must give the directory too,
    for instance "mysample/mysample+A1\_dat1.zim") }
}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }

\seealso{ \code{\link{zimMake}} }
\examples{
##TODO...
}
\keyword{ utilities }

