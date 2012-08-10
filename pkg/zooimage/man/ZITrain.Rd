\name{ZITrain}
\alias{getTrain}
\alias{prepareTrain}
\alias{addToTrain}
\alias{recode}
\alias{recode.ZITrain}

\title{ Manipulate training sets and 'ZITrain' objects }

\description{
  'ZITrain' contain a hierarchy of classes (taxonomic or not) and a link to a
  series of items belonging to these classes. It can be obtained after manual or
  automatic classification of various objects from .zid or .zidb files.
}
\usage{
prepareTrain(traindir, zidbfiles, template = c("[Basic]", "[Detailed]",
    "[Very detailed]"), ident = NULL)
addToTrain(traindir, zidbfiles, ident = NULL)
getTrain(traindir, creator = NULL, desc = NULL, keep_ = FALSE, na.rm = FALSE)

recode(object, \dots)
\method{recode}{ZITrain}(object, new.levels, depth, \dots)
}

\arguments{
  \item{traindir}{ the root directory of the training set. }
  \item{zidbfiles}{ .zidb files or .zid files to use for data and vignettes in
    the training set. }
  \item{template}{ file containing subdirectories template to use for creating
    classes in the training set. Either a defaut template between [], or the
    name of a .zic file. }
  \item{ident}{ if vignettes are already identified in the zid(b) files, should
    they be sorted that way in the created training set? If not \code{NULL},
    indicate the name of the classification column (usually, \code{Class} for
    manual classification or \code{Ident} for automatic predictions). Not uset yet! }  
  \item{creator}{ name of the author of this classification (or the method,
    if done automatically). }
  \item{desc}{ a short description of this manual classification. }
  \item{keep_}{ do we keep items in the '\_' subdirectory (corresponding to
    unclassified ones)? Usually not! }
  \item{na.rm}{ do we remove item with missing data? By default, not. }
  \item{object}{ a 'ZITrain' object. }
  \item{new.levels}{ a character string of same length as the levels of
    \code{object$Class} with the labels of the new levels. }
  \item{depth}{ the depth in the hierachy of the classes as in the \code{"path"}
    attribute of the object to use for recoding classes. If this argument is
    provided, \code{new.levels} is ignored and recomputed (and a warning is
    issued if both arguments are provided). }
  \item{\dots}{ further arguments passed to the method (not used yet). }
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{ \code{\link{ZIClass}} }

\examples{
##TODO...
}

\keyword{ utilities }
