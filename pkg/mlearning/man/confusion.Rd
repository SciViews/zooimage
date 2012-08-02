\name{confusion}
\alias{confusion}
\alias{confusion.default}
\alias{confusion.mlearning}
\alias{print.confusion}
\alias{plot.confusion}
\alias{summary.confusion}
\alias{print.summary.confusion}

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
\method{plot}{confusion}(x, y = NULL, type = c("image", "barplot", "stars",
    "dendrogram"), stat1 = "Recall", stat2 = "Precision", \dots)
\method{summary}{confusion}(object, sort.by = NULL, decreasing = FALSE,
    na.rm = FALSE, \dots)
\method{print}{summary.confusion}(x, \dots)
}

\arguments{
  \item{x}{ an object. }
  \item{y}{ another object, from which to extract the second classification. }
  \item{vars}{ the variables of interest in the first and second classification
    in the case the objects are lists or data frames. Otherwise, this argument
    is ignored and \code{x} and \code{y} must be factors with same length and
    same levels. }
  \item{labels}{ labels to use for the two classifications. By default, it is
    the same as \code{vars}. }
  \item{merge.by}{ a character string with the name of variables to use to merge
    the two data frames, or \code{NULL}. }
  \item{error.col}{ is a column with class error added (yes by default)? }
  \item{type}{ the type of graph to plot (only \code{"stars"} if two confusion
    matrices are to be compared). }
  \item{stat1}{ first statistic to compare in the stars plot. }
  \item{stat2}{ second statistic to compare in the stars plot. }
  \item{object}{ a 'confusion' object. }
  \item{sort.by}{ the statistics to use to sort the table (by default, FN, the
    false negative rate). }
  \item{decreasing}{ do we sort in increasing or decreasing order? }
  \item{na.rm}{ do we eliminate entries with missing data first (using
    \code{na.omit()})? }
  \item{\dots}{ further arguments passed to the function. }
}

\value{
  A confusion matrix in a 'confusion' object.
}

\author{Philippe Grosjean <Philippe.Grosjean@umons.ac.be> and
  Kevin Denis <Kevin.Denis@umons.ac.be>}

\seealso{\code{\link{mlearning}}}

\examples{
data("Glass", package = "mlbench")
## Use a little bit more informative labels for Type
Glass$Type <- as.factor(paste("Glass", Glass$Type))

## Use learning vector quantization to classify the glass types
## (using default parameters)
summary(glassLvq <- mlLvq(Type ~ ., data = Glass))

## Calculate cross-validated confusion matrix and plot it in different ways
(glassConf <- confusion(cvpredict(glassLvq), Glass$Type))
plot(glassConf) # Image by default, with attempt to sort cols and rows
plot(glassConf, sort = NULL) # No sorting
plot(glassConf, type = "barplot")
plot(glassConf, type = "stars")
plot(glassConf, type = "dendrogram")

summary(glassConf)

## Build another classifier and make a comparison
summary(glassNaiveBayes <- mlNaiveBayes(Type ~ ., data = Glass))
(glassConf2 <- confusion(cvpredict(glassNaiveBayes), Glass$Type))

## Comparison plot for two items
plot(glassConf, glassConf2)
}

\keyword{ tree }
