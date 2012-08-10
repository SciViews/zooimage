\name{ZIClass}
\alias{ZIClass}
\alias{print.ZIClass}
\alias{summary.ZIClass}
\alias{predict.ZIClass}
\alias{confusion.ZIClass}

\title{ Create and manipulate 'ZIClass' objects }

\description{
  'ZIClass' objects are key items in ZooImage. They contain all what is required
  for automatically classify plancton from .zid files. They can be used as
  blackboxes by all users (but require users trained in machine learning
  techniques to build them). Hence, ZooImage is made very simple for biologists
  that just want to use classifiers but do not want to worry about all the
  complexities of what is done inside the engine!
}

\usage{
ZIClass(formula, data, method = getOption("ZI.mlearning", "mlRforest"),
    calc.vars = getOption("ZI.calcVars", calcVars), cv.k = 10, cv.strat = TRUE,
    \dots, subset, na.action = na.omit)

\method{print}{ZIClass}(x, \dots)
\method{summary}{ZIClass}(object, sort.by = "Fscore", decreasing = TRUE,
    na.rm = FALSE, \dots)
\method{predict}{ZIClass}(object, ZIDat, calc = TRUE, class.only = TRUE,
    type = "class", \dots)
\method{confusion}{ZIClass}(x, y = response(x), labels = c("Actual", "Predicted"),
    prior, use.cv = TRUE, \dots)
}

\arguments{  
  \item{formula}{ a formula with left member being the class variable and the
    right member being a list of predicting variables separated by a '+' sign.
    Since \code{data} is supposed to be previously filtered using
    \code{calc.vars} and the class variable in 'ZITrain' object is always
    named \code{Class}, the formula almost always reduces to \code{Class ~ .} }
  \item{data}{ a data frame (a 'ZITrain' object usually), containing both
    measurement and identification (a factor variables usually named 'Class'). }
  \item{method}{ the machine learning method to use. It should produce
    results compatible with \code{mlearning} objects as returned by the various
    \code{mlXXX()} functions in the \code{mlearning} package. By default, the
    random forest algorithm is used (it is among the ones that give best result
    with plankton). }
  \item{calc.vars}{ a function to use to calculate variables from the original
    data frame. }
  \item{cv.k}{ the k times for cross-validation. }
  \item{cv.strat}{ do we use a stratified sampling for cross-validation?
    (recommended). }
  \item{\dots}{ further arguments to pass to the classification algorithm (see
    help of that particular function). }
  \item{subset}{ an expression for subsetting to original data frame. }
  \item{na.action}{ the function to filter the initial data frame for missing
    values. Althoung the default in R is \code{na.fail}, leading to failure if
    at least one \code{NA} is found in the data frame, the default here is
    \code{na.omit} which leads to elimination of all lines containing at least
    one \code{NA}. Take care about how many items remain, if you encounter
    many \code{NA}s in your dataset! }
  \item{x}{ a 'ZIClass' object. }
  \item{object}{ a 'ZIClass' object. }
  \item{sort.by}{ the statistics to use to sort the table (by default, F-score). }
  \item{decreasing}{ do we sort in increasing or decreasing order? }
  \item{na.rm}{ do we eliminate entries with missing data first (using
    \code{na.omit()})? }
  \item{calc}{ a boolean indicating if variables have to be recalculated
    before running the prediction. }
  \item{ZIDat}{ a 'ZIDat' object where items should be classified. }
  \item{class.only}{ if TRUE, return just a vector with classification,
    otherwise, return the 'ZIDat' object with 'Ident' column appended to it. }
  \item{type}{ the type of result to return, \code{"class"} by default. No other
    value is permitted if class.only is \code{FALSE}. }
  \item{y}{ a factor with reference classes. }
  \item{labels}{ labels to use for, respectively, the reference class and the
    predicted class. }
  \item{prior}{ class frequencies to use for first classifier that
    is tabulated in the rows of the confusion matrix. This is either a single
    positive numeric to set all class frequencies to this value (use 1 for
    relative frequencies and 100 for relative freqs in percent), or a vector of
    positive numbers of the same length as the levels in the object. If the
    vector is named, names must match levels. Alternatively, providing
    \code{NULL} or an object of null length resets row class prefencies into
    their initial values. }
  \item{use.cv}{ the predicted values extracted from the 'ZIClass' object can
    either be the predicted values from the training set, or the cross-validated
    predictions (by default). Most of the time, you want the cross-validated
    predictions, which allows for not (or less) biased evaluation of the
    classifier prediction... So, if you don't know, you are probably better
    leaving the default value. }
}

\value{
  \code{ZIClass()} is the constructor that build the 'ZIClass' object.
  \code{print()}, \code{summary()} and \code{predict())} are the methods to
  print the object, to calculate statistics on this classifier based on the
  confusion matrix and to predict groups for ZooImage samples, using one
  'ZIClass' object.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\note{
    Always analyze carefully the properties, performances and limitations of a
    'ZIClass' object before using it to classify objects of one series. For
    instance, you can use \code{confusion()} to compare two classifiers, or an
    automatic classifier with a manual classification done by a taxonomists.
    Always respect the limitations in the use of a 'ZIClass' object (for
    instance, a classifier specific of one given series should not be used to
    classify items in a different series)! It is a good practice to make a
    report, documenting a 'ZIClass' object, together with the comments of
    taxonomists that made the reference training set, and with details on the
    analysis of the performances of the classifier.
}

\seealso{ \code{\link{getTrain}}, \code{\link[mlearning]{confusion}} }

\examples{
##TODO...
}

\keyword{tree}
