\name{ZIClass}
\alias{ZIClass}
\alias{predict.ZIClass}

\title{ Create and manipulate 'ZIClass' objects }
\description{
  'ZIClass' objects are key items in ZooImage. They contain all what is required
  for automatically classify plancton from .zid files. They can be used as blackboxes
  by all users (but require users trained in machine learning techniques to build them).
  Hence, ZooImage is made very simple for biologists that just want to use classifiers
  but do not want to worry about all the complexities of what is done inside the engine!
}
\usage{
ZIClass(df, algorithm = c("lda", "randomForest"), package = c("MASS",
    "randomForest"), Formula = Class ~ logArea + Mean + StdDev + Mode + Min +
    Max + logPerim. + logMajor + logMinor + Circ. + logFeret + IntDen +
    Elongation + CentBoxD + GrayCentBoxD + CentroidsD + Range + MeanPos +
    SDNorm + CV, calc.vars = "calcVars", k.xval = 10, \dots)
\method{predict}{ZIClass}(object, ZIDat, calc.vars = TRUE, class.only = FALSE,
    type = "class", na.rm = FALSE, \dots)
}

\arguments{
  \item{df}{ A data frame, containing both measurement and identification 'Class' of items }
  \item{algorithm}{ The algorithm to use }
  \item{package}{ The R package where functions performing this algorithm can be found }
  \item{Formula}{ A formula with first memeber being the classification and the second member being measurement separated by a '+' sign }
  \item{calc.vars}{ A function to use to calculate variables from the original data frame }
  \item{k.xval}{ The k time for cross-validation }
  \item{\dots}{ Further arguments to pass to the classification algorithm (see help of that particular function) }
  \item{object}{ A 'ZIClass' object }
  \item{ZIDat}{ A 'ZIDat' object where items should be classified }
  \item{class.only}{ If TRUE, retrun just a vector with classification, otherwise, return a 'ZIDat' object with 'Ident' column appended to it }
  \item{type}{ The type of result to return, \code{"class"} by default }
  \item{na.rm}{ Do we eliminate entries with missing data first? }
}

\value{
  \code{ZIClass} is the constructor that build the 'ZIClass' object.
  \code{predict} is the method used to predict groups for ZooImage samples,
  using one 'ZIClass' object.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umh.ac.be> }
\note{
    Always analyze carefully the properties, performances and limitations of a
    'ZIClass' object before using it to classify objects of one series. For instance,
    you can use \code{confu()} to compare two classifiers, or an automatic classifier
    with a manual classification done by a taxonomists. Always respect the limitations
    in the use of a 'ZIClass' object (for instance, a classifier specific of
    one given series should not be used to classify items in a different series)!
    It is a good practice to make a report, documenting a 'ZIClass' object, together
    with the comments of taxonomists that made the reference training set, and with
    details on the analysis of the performances of the classifier.
}
\seealso{ \code{\link{get.ZITrain}} }
\examples{
##TODO...
}

\keyword{tree}
