\name{ZITrain}
\alias{get.ZITrain}
\alias{expand.ZITrain}
\alias{ZIRecodeLevels}
\alias{read.ZITrain}
\alias{recode.ZITrain}
\alias{unzip.ZITrain}
\alias{write.ZITrain}
\alias{zip.ZITrain}

\title{ Manipulate 'ZITrain' objects (containing manual training sets) }
\description{
  'ZITrain' contain a hierarchy of groups (taxonomic or not) and a link to a
  series of items belonging to these groups. It can be obtained after manual or
  automatic classification of various objects from .zid files.
}
\usage{
get.ZITrain(dir, creator = NULL, desc = NULL, keep_ = FALSE, na.rm = FALSE)
expand.ZITrain(ZITrain, ZIDdir, destination)
ZIRecodeLevels(ZITrain, level = 1)
read.ZITrain(file)
recode.ZITrain(ZITrain, ZIRecode, warn.only = FALSE)
unzip.ZITrain(zipfile, dir, overwrite = FALSE)
write.ZITrain(ZITrain, file)
zip.ZITrain(dir, zipfile, overwrite = FALSE)
}

\arguments{
  \item{dir}{ Where to read data for making a ZITrain object? }
  \item{creator}{ The name of the author of this classification (or the method, if done automatically) }
  \item{desc}{ A short description of this classification }
  \item{keep_}{ Do we keep items in the '\_' subdirectory (corresponding to unclassified ones)? }
  \item{na.rm}{ Do we remove item with missing values? By default, not. }
  \item{ZITrain}{ A 'ZITrain' object }
  \item{ZIDdir}{ A directory where corresponding .zid files are located (to get the vignettes) }
  \item{destination}{ A directory where to expand a classification contained in a 'ZITrain' object }
  \item{level}{ A level for the recoding }
  \item{file}{ Read or write a 'ZITrain' object from/to a text file }
  \item{ZIRecode}{ A 'ZIRecode' object (for instance, one obtained by \code{ZIRecodeLevels} }
  \item{warn.only}{ Do not interrupt execution if there is a mismatch between levels in the 'ZITrain' and 'ZIRecode' objects }
  \item{zipfile}{ A zip archive file to process }
  \item{overwrite}{ Do we overwrite existing files? }
}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }

\seealso{ \code{\link{ZIClass}} }
\examples{
##TODO...
}
\keyword{ utilities }

