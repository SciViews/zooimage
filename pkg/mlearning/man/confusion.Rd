\name{confusion}
\alias{confusion}
\alias{confusion.default}
\alias{confusion.mlearning}
\alias{print.confusion}
\alias{plot.confusion}
\alias{summary.confusion}
\alias{print.summary.confusion}
\alias{comparisonPlot}

\title{ Construct and analyze confusion matrices }
\description{
  Confusion matrices compare two classifications (usually one done automatically
  using a machine learning algorithm versus the true classication represented by
  a manual classification by a specialist... but one can also compare two
  automatic or two manual classifications against each other).
}

\usage{
confusion(x, \dots)
\method{confusion}{default}(x, y = NULL, vars = c("Actual", "Predicted"),
    labels = vars, merge.by = "Id", \dots)
\method{confusion}{mlearning}(x, y = response(x),
    labels = c("Actual", "Predicted"), \dots)

\method{print}{confusion}(x, error.col = TRUE, \dots)
\method{plot}{confusion}(x, y, type = c("image", "image2", "tree_image",
    "precision_recall", "precision_recall2", "dendrogram"), \dots)
\method{summary}{confusion}(object, sort.by = NULL, decreasing = FALSE,
    na.rm = FALSE, \dots)
\method{print}{summary.confusion}(x, \dots)

comparisonPlot(x, y, stat1 = "Recall", stat2 = "Precision", barplot = TRUE)
}

\arguments{
  \item{x}{ an object (a \code{summary.confusion} object for \code{comparisonPlot()}). }
  \item{y}{ another object, from which to extract the second classification
    (a \code{summary.confusion} second object for \code{comparisonPlot()}). }
  \item{vars}{ the variables of interest in the first and second classification
    in the case the objects are lists or data frames. Otherwise, this argument
    is ignored and \code{x} and \code{y} must be factors with same length and
    same levels. }
  \item{labels}{ labels to use for the two classifications. By default, it is
    the same as \code{vars}. }
  \item{merge.by}{ a character string with the name of variables to use to merge
    the two data frames, or \code{NULL}. }
  \item{error.col}{ is a column with class error added (yes by default)? }
  \item{type}{ the type of graph to plot. }
  \item{object}{ a 'confusion' object. }
  \item{sort.by}{ the statistics to use to sort the table (by default, FN, the
    false negative rate). }
  \item{decreasing}{ do we sort in increasing or decreasing order? }
  \item{na.rm}{ do we eliminate entries with missing data first (using
    \code{na.omit()})? }
  \item{\dots}{ further arguments passed to the function (not used yet). }
  \item{stat1}{ first statistic to compare. }
  \item{stat2}{ second statistic to compare. }
  \item{barplot}{ make a barplot or scatterplot? }
}

\value{
  A confusion matrix in a 'confusion' object.
}

\author{Philippe Grosjean <Philippe.Grosjean@umons.ac.be>}

\seealso{\code{\link{mlNnet}}}

\examples{
data("iris", package = "datasets")

## Use learning vector quantization to classify the iris species
## (using default parameters)
irisLvq <- mlLvq(Species ~ ., data = iris)
summary(irisLvq)

## TODO: use a better example and show how to use cross-validation!
## Calculate confusion matrix and plot it in different ways
## Note that it is just for demonstration...
## In real world, you should use cross-validation in order
## to get interpretable results!
irisConf <- confusion(irisLvq) # Self-consistency
irisConf
plot(irisConf)
plot(irisConf, type = "image2")
plot(irisConf, type = "tree_image")
plot(irisConf, type = "precision_recall")
plot(irisConf, type = "precision_recall2")
plot(irisConf, type = "dendrogram")

summary(irisConf)

## Build another classifier and make a comparison
irisNaiveBayes <- mlNaiveBayes(Species ~ ., data = iris)
summary(irisNaiveBayes)

irisConf2 <- confusion(irisNaiveBayes) # Self-consistency
irisConf2

comparisonPlot(summary(irisConf), summary(irisConf2))
}

\keyword{ tree }
