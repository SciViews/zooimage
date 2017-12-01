\name{ZITrain}
\alias{prepareTrain}
\alias{addToTrain}
\alias{cellModel}
\alias{cellCompute}
\alias{cellCount}
\alias{getTrain}
\alias{prepareTest}
\alias{addToTest}
\alias{getTest}
\alias{template}
\alias{template.default}
\alias{recode}
\alias{recode.ZITrain}
\alias{recode.ZITest}
\alias{contextSelection}

\title{ Manipulate training and testsets 'ZITrain'/'ZITest' objects }

\description{
  'ZITrain' contain a hierarchy of classes (taxonomic or not) and a link to a
  series of items belonging to these classes. It can be obtained after manual or
  automatic classification of various objects from .zid or .zidb files. 'ZITest'
  objects are almost identical, but with a constraint on the classes that must
  match the ones of the reference 'ZItrain' or 'ZIClass' object (a necessity to
  allow for comparisons)!
}
\usage{
prepareTrain(traindir, zidbfiles, template = c("[Basic]", "[Detailed]",
    "[Very detailed]"), classes = NULL, \dots)
addToTrain(traindir, zidbfiles, classes = NULL, \dots)
getTrain(traindir, creator = NULL, desc = NULL, keep_ = FALSE, na.rm = FALSE)

prepareTest(testdir, zidbfiles, template, classes = NULL, \dots)
addToTest(testdir, zidbfiles, classes = NULL, \dots)
getTest(testdir, creator = NULL, desc = NULL, keep_ = NA, na.rm = FALSE)

cellModel(train, traindir, class, method = "mda")
cellCompute(data, cellModels)
cellCount(traindir, class, reset = FALSE)

template(object, \dots)
\method{template}{default}(object, \dots)

recode(object, \dots)
\method{recode}{ZITrain}(object, new.levels, depth, \dots)
\method{recode}{ZITest}(object, new.levels, depth, \dots)

contextSelection()
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
  \item{classes}{ if vignettes are already classified in the zid(b) files, should
    they be sorted that way in the created training or test set? If not \code{NULL},
    indicate the name of the classification column (usually, \code{Class} for
    manual classification or \code{Predicted} for automatic predictions). This
    can also be a 'ZIClass' or 'mlearning' object that will be used for
    classification of the particles first, ... or it can be a function that does
    the classification. }  
  \item{creator}{ name of the author of this classification (or the method,
    if done automatically). }
  \item{desc}{ a short description of this manual classification. }
  \item{keep_}{ do we keep items in the '\_' subdirectory (corresponding to
    unclassified ones)? Default to \code{FALSE} in \code{getTrain()} and to
    \code{NA} for \code{getTest()}, which transforms all items in the '\_' or
    one of its subdirectories into missing data. }
  \item{na.rm}{ do we remove item with missing data? By default, not. }
  \item{train}{ a ZITrain file to use for building the model. }
  \item{class}{ a character string with the name of the class to process. }
  \item{method}{ a character string with the nazme of the predictive method to
    use: \code{"lm"}, \code{"lda"} or \code{"mda"} (by default). }
  \item{data}{ a sample containing the particles to count. }
  \item{cellModels}{ the file containing the models for cells countings. }
  \item{reset}{ do we reset excisting counts for that class? By default, no. }
  \item{object}{ a 'ZITrain' or 'ZITest' object. For \code{prepareTest()}, a
    'ZITrain' or 'ZIClass' object to use as reference to determine the
    classes to make. }
  \item{new.levels}{ a character string of same length as the levels of
    \code{object$Class} with the labels of the new levels. }
  \item{depth}{ the depth in the hierachy of the classes as in the \code{"path"}
    attribute of the object to use for recoding classes. If this argument is
    provided, \code{new.levels} is ignored and recomputed (and a warning is
    issued if both arguments are provided). }
  \item{\dots}{ further arguments passed to the method. For \code{prepareXXX()}
    and \code{addToXXX()}, it is further arguments passed to the prediction
    function provided in \code{classes}, or to the \code{predict()} method for
    'ZIClass' or 'mlearning' objects. }
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{ \code{\link{ZIClass}} }

\examples{
##TODO...
}

\keyword{ utilities }
