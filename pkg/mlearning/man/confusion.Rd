\name{confusion}
\alias{confusion}
\alias{confusion.default}
\alias{confusion.mlearning}
\alias{print.confusion}
\alias{summary.confusion}
\alias{print.summary.confusion}
\alias{plot.confusion}
\alias{confusionImage}
\alias{confusionBarplot}
\alias{confusionStars}
\alias{confusionDendrogram}
\alias{prior}
\alias{prior.confusion}
\alias{prior<-}
\alias{prior<-.confusion}

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
    labels = vars, merge.by = "Id", useNA = "ifany", prior, \dots)
\method{confusion}{mlearning}(x, y = response(x),
    labels = c("Actual", "Predicted"), useNA = "ifany", prior, \dots)

\method{print}{confusion}(x, sums = TRUE, error.col = sums, digits = 0,
    sort = "ward", \dots)
\method{summary}{confusion}(object, type = "all", sort.by = "Fscore",
    decreasing = TRUE, \dots)
\method{print}{summary.confusion}(x, \dots)
\method{plot}{confusion}(x, y = NULL, type = c("image", "barplot", "stars",
    "dendrogram"), stat1 = "Recall", stat2 = "Precision", names, \dots)

confusionImage(x, y = NULL, labels = names(dimnames(x)), sort = "ward",
    numbers = TRUE, digits = 0, mar = c(3.1, 10.1, 3.1, 3.1), cex = 1, asp = 1,
    colfun, ncols = 41, col0 = FALSE, grid.col = "gray", \dots)
confusionBarplot(x, y = NULL, col = c("PeachPuff2", "green3", "lemonChiffon2"),
    mar = c(1.1, 8.1, 4.1, 2.1), cex = 1, cex.axis = cex, cex.legend = cex,
    main = "F-score (precision versus recall)", numbers = TRUE, min.width = 17,
    \dots)
confusionStars(x, y = NULL, stat1 = "Recall", stat2 = "Precision", names, main,
    col = c("green2", "blue2", "green4", "blue4"), \dots)
confusionDendrogram(x, y = NULL, labels = rownames(x), sort = "ward",
    main = "Groups clustering", \dots)

prior(object, \dots)
\method{prior}{confusion}(object, \dots)
prior(object, \dots) <- value
\method{prior}{confusion}(object, \dots) <- value
}

\arguments{
  \item{x}{ an object. }
  \item{y}{ another object, from which to extract the second classification, or
    \code{NULL} if not used. }
  \item{vars}{ the variables of interest in the first and second classification
    in the case the objects are lists or data frames. Otherwise, this argument
    is ignored and \code{x} and \code{y} must be factors with same length and
    same levels. }
  \item{labels}{ labels to use for the two classifications. By default, it is
    the same as \code{vars} or the one in the confusion matrix. }
  \item{merge.by}{ a character string with the name of variables to use to merge
    the two data frames, or \code{NULL}. }
  \item{useNA}{ do we keep NAs as a separate category? The default \code{"ifany"}
    creates this category only if there are missing values. Other possibilities
    are \code{"no"}, or \code{"always"}. }
  \item{prior}{ class frequencies to use for first classifier that
    is tabulated in the rows of the confusion matrix. For its value, see here
    under, the \code{value} argument. }
  \item{sums}{ is the confusion matrix printed with rows and columns sums? }
  \item{error.col}{ is a column with class error for first classifier added
    (equivalent to flase negative rate of FNR)? }
  \item{digits}{ the number of digits after the decimal point to print in the
    confusion matrix. The default or zero leads to most compact presentation
    and is suitable for frequencies, but not for relative frequencies. }
  \item{sort}{ are rows and columns of the confusion matrix sorted so that
    classes with larger confusion are closer together? Sorting is done
    using a hierachical clustering with \code{hclust()}. The clustering method
    is provided is the one provides (\code{"ward"}, by default, but see the
    \code{hclust()} help for other options). If \code{FALSE} or \code{NULL}, no
    sorting is done. }
  \item{object}{ a 'confusion' object. }
  \item{sort.by}{ the statistics to use to sort the table (by default, Fmeasure,
    the F1 score for each class = 2 * recall * precision / (recall + precision)). }
  \item{decreasing}{ do we sort in increasing or decreasing order? }
  \item{type}{ the type of graph to plot (only \code{"stars"} if two confusion
    matrices are to be compared). }
  \item{stat1}{ first statistic to compare in the stars plot. }
  \item{stat2}{ second statistic to compare in the stars plot. }
  \item{\dots}{ further arguments passed to the function. In particular for
    \code{plot()}, it can be all arguments for the corresponding plot. }
  \item{numbers}{ are actual numbers indicated in the confusion matrix image? }
  \item{mar}{ graph margins. }
  \item{cex}{ text magnification factor. }
  \item{cex.axis}{ idem for axes. If \code{NULL}, the axis is not drawn. }
  \item{cex.legend}{ idem for legend text. If \code{NULL}, no legend is added. }
  \item{asp}{ graph aspect ration. There is little reasons to cvhange the
    default value of 1. }
  \item{col}{ color(s) to use fir the graph. }
  \item{colfun}{ a function that calculates a series of colors, like e.g.,
    \code{cm.colors()} and that accepts one argument being the number of colors
    to be generated. }
  \item{ncols}{ the number of colors to generate. It should preferrably be
    2 * number of levels + 1, where levels is the number of frequencies you
    want to evidence in the plot. Default to 41. }
  \item{col0}{ should null values be colored or not (no, by default)? }
  \item{grid.col}{ color to use for grid lines, or \code{NULL} for not drawing
    grid lines. }
  \item{names}{ names of the two classifiers to compare. }
  \item{main}{ main title of the graph. }
  \item{min.width}{ minimum bar width required to add numbers. }
  \item{value}{ a single positive numeric to set all class frequencies to this
    value (use 1 for relative frequencies and 100 for relative freqs in percent),
    or a vector of positive numbers of the same length as the levels in the
    object. If the vector is named, names must match levels. Alternatively,
    providing \code{NULL} or an object of null length resets row class
    frequencies into their initial values. }
}

\value{
  A confusion matrix in a 'confusion' object. \code{prior()} returns the
  current class frequencies associated with first classification tabulated, i.e.,
  for rows in the confusion matrix.
}

\author{Philippe Grosjean <Philippe.Grosjean@umons.ac.be> and
  Kevin Denis <Kevin.Denis@umons.ac.be>}

\seealso{\code{\link{mlearning}}, \code{\link[stats]{hclust}},
  \code{\link[grDevices]{cm.colors}}}

\examples{
data("Glass", package = "mlbench")
## Use a little bit more informative labels for Type
Glass$Type <- as.factor(paste("Glass", Glass$Type))

## Use learning vector quantization to classify the glass types
## (using default parameters)
summary(glassLvq <- mlLvq(Type ~ ., data = Glass))

## Calculate cross-validated confusion matrix and plot it in different ways
(glassConf <- confusion(cvpredict(glassLvq), Glass$Type))
## Raw confusion matrix: no sort and no margins
print(glassConf, sums = FALSE, sort = FALSE)
## Graphs
plot(glassConf) # Image by default
plot(glassConf, sort = FALSE) # No sorting
plot(glassConf, type = "barplot")
plot(glassConf, type = "stars")
plot(glassConf, type = "dendrogram")

summary(glassConf)
summary(glassConf, type = "Fscore")

## Build another classifier and make a comparison
summary(glassNaiveBayes <- mlNaiveBayes(Type ~ ., data = Glass))
(glassConf2 <- confusion(cvpredict(glassNaiveBayes), Glass$Type))

## Comparison plot for two classifiers
plot(glassConf, glassConf2)

## When the probabilities in each class do not match the proportions in the
## training set, all these calculations are useless. Having an idea of
## the real proportions (so-called, priors), one should first reweight the
## confusion matrix before calculating statistics, for instance:
prior1 <- c(10, 10, 10, 100, 100, 100) # Glass types 1-3 are rare
prior(glassConf) <- prior1
glassConf
summary(glassConf, type = c("Fscore", "Recall", "Precision"))
plot(glassConf)

## This is very different than if glass types 1-3 are abundants!
prior2 <- c(100, 100, 100, 10, 10, 10) # Glass types 1-3 are abundants
prior(glassConf) <- prior2
glassConf
summary(glassConf, type = c("Fscore", "Recall", "Precision"))
plot(glassConf)

## Weight can also be used to construct a matrix of relative frequencies
## In this case, all rows sum to one
prior(glassConf) <- 1
print(glassConf, digits = 2)
## However, it is easier to work with relative frequencies in percent
## and one gets a more compact presentation
prior(glassConf) <- 100
glassConf

## To reset row class frequencies to original propotions, just assign NULL
prior(glassConf) <- NULL
glassConf
prior(glassConf)
}

\keyword{ tree }
