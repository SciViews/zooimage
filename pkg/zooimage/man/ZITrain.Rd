\name{ZITrain}
\alias{getTrain}
\alias{prepareTrain}
\alias{addToTrain}
\alias{ZIRecodeLevels}
\alias{recode.ZITrain}

\title{ Manipulate training sets and 'ZITrain' objects }

\description{
  'ZITrain' contain a hierarchy of groups (taxonomic or not) and a link to a
  series of items belonging to these groups. It can be obtained after manual or
  automatic classification of various objects from .zid or .zidb files.
}
\usage{
prepareTrain(rootdir, subdir = "_train", zidfiles, zidbfiles = NULL,
    groups.template = c("[Basic]", "[Detailed]", "[Very detailed]"),
    ident = NULL, start.viewer = FALSE)
addToTrain(traindir, zidbfiles)
getTrain(traindir, creator = NULL, desc = NULL, keep_ = FALSE, na.rm = FALSE,
    numvars = NULL)

recode.ZITrain(ZITrain, ZIRecode, warn.only = FALSE)
ZIRecodeLevels(ZITrain, level = 1)
}

\arguments{
  \item{rootdir}{ root directory where to create a training set. }
  \item{subdir}{ the subdirectory containing the training set (will be traindir)
    in the other functions. }
  \item{zidfiles}{ .zid files to use for data and vignettes in the training set. }
  \item{zidbfiles}{ .zidb files to use for data and vignettes in the training set. }
  \item{groups.template}{ template to use for creating groups in the training set.
    Either a defaut template between [], or the name of a .zic file. }
  \item{ident}{ if vignettes are already identified in the zid(b)files, should
    they be sorted that way in the created training set? }
  \item{start.viewer}{ do we start the image viewer program at the end? }
  \item{traindir}{ the root directory of the training set. }
  \item{creator}{ name of the author of this classification (or the method,
    if done automatically). }
  \item{desc}{ a short description of this manual classification. }
  \item{keep_}{ do we keep items in the '\_' subdirectory (corresponding to
    unclassified ones)? Usually not! }
  \item{na.rm}{ do we remove item with missing data? By default, not. }
  \item{numvars}{ a list of variables to check and force to be numeric. If
    \code{NULL}, a default list of usual variables is checked. }
  \item{ZITrain}{ a 'ZITrain' object. }
  \item{ZIRecode}{ a 'ZIRecode' object (for instance, one obtained by
    \code{ZIRecodeLevels}. }
  \item{warn.only}{ do not interrupt execution if there is a mismatch between
    levels in the 'ZITrain' and 'ZIRecode' objects. }
  \item{level}{ level to use for the recoding. }
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{ \code{\link{ZIClass}} }

\examples{
##TODO...
}

\keyword{ utilities }
