\name{ZITrain}
\alias{prepareTrain}
\alias{addToTrain}
\alias{getTrain}
\alias{prepareTest}
\alias{addToTest}
\alias{getTest}
\alias{template}
\alias{template.default}
\alias{recode}
\alias{recode.ZITrain}
\alias{recode.ZITest}

\title{ Manipulate training and testsets 'ZITrain'/'ZITest' objects }

\description{
  'ZITrain' contain a hierarchy of classes (taxonomic or not) and a link to a
  series of items belonging to these classes. It can be obtained after manual or
  automatic classification of various objects from .zid or .zidb files. 'ZITest'
  objects are almost identical, but with a '+others+' class added.
}
\usage{
prepareTrain(traindir, zidbfiles, template = c("[Basic]", "[Detailed]",
    "[Very detailed]"), ident = NULL)
addToTrain(traindir, zidbfiles, ident = NULL)
getTrain(traindir, creator = NULL, desc = NULL, keep_ = FALSE, na.rm = FALSE)

prepareTest(testdir, zidbfiles, template, ident = NULL)
addToTest(testdir, zidbfiles, ident = NULL)
getTest(testdir, creator = NULL, desc = NULL, keep_ = FALSE, na.rm = FALSE)

template(object, \dots)
\method{template}{default}(object, add.others = TRUE, \dots)

recode(object, \dots)
\method{recode}{ZITrain}(object, new.levels, depth, \dots)
\method{recode}{ZITest}(object, new.levels, depth, \dots)
}

\arguments{
  \item{traindir}{ the root directory of the training set. }
  \item{testdir}{ the root directory of the test set. }
  \item{zidbfiles}{ .zidb files or .zid files to use for data and vignettes in
    the training set. }
  \item{template}{ file containing subdirectories template to use for creating
    classes in the training or test set. Either a defaut template between [], or
    the name of a .zic file, or a template extracted from a 'ZITrain' or 'ZIClass'
    object using \code{template(object)} (with the \code{add.others} argument
    to TRUE for \code{prepareTest()} and to \code{FALSE} for \code{prepareTrain()}) }
  \item{ident}{ if vignettes are already identified in the zid(b) files, should
    they be sorted that way in the created training or test set? If not \code{NULL},
    indicate the name of the classification column (usually, \code{Class} for
    manual classification or \code{Ident} for automatic predictions). Not uset yet! }  
  \item{creator}{ name of the author of this classification (or the method,
    if done automatically). }
  \item{desc}{ a short description of this manual classification. }
  \item{keep_}{ do we keep items in the '\_' subdirectory (corresponding to
    unclassified ones)? Usually not! }
  \item{na.rm}{ do we remove item with missing data? By default, not. }
  \item{object}{ a 'ZITrain' or 'ZITest' object. For \code{prepareTest()}, a
    'ZITrain' or 'ZIClass' object to use as reference to determine the
    classes to make. }
  \item{add.others}{ do we add the '+others+' group if not already in the
    template? \code{TRUE} (by default) is suitable to create a template for
    'ZITest' objects, otherwise, use \code{FALSE} for 'ZITrain' objects. }
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
