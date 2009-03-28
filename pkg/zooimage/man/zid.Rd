\name{zid}
\alias{make.RData}
\alias{clean.after.zid}
\alias{compress.zid}
\alias{compress.zid.all}
\alias{read.zid}
\alias{uncompress.zid}
\alias{uncompress.zid.all}
\alias{verify.zid}
\alias{verify.zid.all}
\alias{zid.extract}


\title{ Manage .zid files (ZooImage Data) }
\description{
  Compress, uncompress and verify ZooImage Data files.
}
\usage{
compress.zid(zidir, type = "ZI1", check = TRUE, check.vignettes = TRUE,
    replace = FALSE, delete.source = replace, check.zip = TRUE, show.log = TRUE)
compress.zid.all(path = ".", samples = NULL, type = "ZI1", check = TRUE,
    check.vignettes = TRUE, replace = FALSE, delete.source = replace,
    show.log = TRUE, bell = FALSE)
make.RData(zidir, type = "ZI1", replace = FALSE, show.log = TRUE)
clean.after.zid(path = ".", samples = NULL)
read.zid(zidfile)
uncompress.zid(zidfile, path = dirname(zidfile), delete.source = FALSE,
    show.log = TRUE)
uncompress.zid.all(path = ".", zidfiles = list.files(path = path, pattern =
    "^.*[.][zZ][iI][dD]"), path.extract = path, skip.existing.dirs = TRUE,
    delete.source = FALSE, show.log = TRUE, bell = FALSE)
verify.zid(zidir, type = "ZI1", check.vignettes = TRUE, show.log = TRUE)
verify.zid.all(path = ".", samples = NULL, type = "ZI1", check.vignettes = TRUE,
    show.log = TRUE, bell = FALSE)
zid.extract(file, zidfile)
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

\seealso{ \code{\link{make.zim}} }
\examples{
##TODO...
}
\keyword{ utilities }

