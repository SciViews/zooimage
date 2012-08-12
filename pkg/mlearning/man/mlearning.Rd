\name{mlearning}
\alias{mlearning}
\alias{print.mlearning}
\alias{summary.mlearning}
\alias{print.summary.mlearning}
\alias{summary.lvq}
\alias{print.summary.lvq}
\alias{plot.mlearning}
\alias{predict.mlearning}
\alias{cvpredict}
\alias{cvpredict.mlearning}
\alias{mlLda}
\alias{mlLda.default}
\alias{mlLda.formula}
\alias{predict.mlLda}
\alias{mlQda}
\alias{mlQda.default}
\alias{mlQda.formula}
\alias{predict.mlQda}
\alias{mlRforest}
\alias{mlRforest.default}
\alias{mlRforest.formula}
\alias{predict.mlRforest}
\alias{mlNnet}
\alias{mlNnet.default}
\alias{mlNnet.formula}
\alias{mlLvq}
\alias{mlLvq.default}
\alias{mlLvq.formula}
\alias{predict.mlLvq}
\alias{mlSvm}
\alias{mlSvm.default}
\alias{mlSvm.formula}
\alias{predict.mlSvm}
\alias{mlNaiveBayes}
\alias{mlNaiveBayes.default}
\alias{mlNaiveBayes.formula}
\alias{response}
\alias{response.default}
\alias{train}
\alias{train.default}

\title{ Alternate interface to various machine learning algorithms }
\description{
  In order to provide a unified (formula-based) interface to various machine
  learning algorithms, these function wrap a common UI around a couple of
  existing code.
}

\usage{
mlearning(formula, data, method, model.args, call = match.call(), ...,
    subset, na.action = na.fail)
\method{print}{mlearning}(x, ...)
\method{summary}{mlearning}(object, ...)
\method{print}{summary.mlearning}(x, ...)
\method{plot}{mlearning}(x, y, ...)
\method{predict}{mlearning}(object, newdata, type = c("class", "membership", "both"),
    method = c("direct", "cv"), na.action = na.exclude, ...)
    
cvpredict(object, ...)
\method{cvpredict}{mlearning}(object, type = c("class", "membership", "both"),
    cv.k = 10, cv.strat = TRUE, ...)

mlLda(...)
\method{mlLda}{default}(train, response, ...)
\method{mlLda}{formula}(formula, data, ..., subset, na.action)
\method{predict}{mlLda}(object, newdata, type = c("class", "membership", "both",
    "projection"), prior = object$prior, dimension,
    method = c("plug-in", "predictive", "debiased", "cv"), ...)

mlQda(...)
\method{mlQda}{default}(train, response, ...)
\method{mlQda}{formula}(formula, data, ..., subset, na.action)
\method{predict}{mlQda}(object, newdata, type = c("class", "membership", "both"),
    prior = object$prior, method = c("plug-in", "predictive", "debiased",
    "looCV", "cv"), ...)

mlRforest(...)
\method{mlRforest}{default}(train, response, ntree = 500, mtry, replace = TRUE, classwt = NULL, ...)
\method{mlRforest}{formula}(formula, data, ntree = 500, mtry, replace = TRUE, classwt = NULL, ...,
    subset, na.action)
\method{predict}{mlRforest}(object, newdata, type = c("class", "membership", "both",
    "vote"), method = c("direct", "oob", "cv"), ...)

mlNnet(...)
\method{mlNnet}{default}(train, response, size = NULL, rang = NULL, decay = 0, maxit = 1000, ...)
\method{mlNnet}{formula}(formula, data, size = NULL, rang = NULL, decay = 0, maxit = 1000, ...,
    subset, na.action)

mlLvq(...)
\method{mlLvq}{default}(train, response, k.nn = 5, size, prior, algorithm = "olvq1", ...)
\method{mlLvq}{formula}(formula, data, k.nn = 5, size, prior, algorithm = "olvq1", ...,
    subset, na.action)
\method{summary}{lvq}(object, ...)
\method{print}{summary.lvq}(x, ...)
\method{predict}{mlLvq}(object, newdata, type = "class", method = c("direct", "cv"), 
    na.action = na.exclude,...)

mlSvm(...)
\method{mlSvm}{default}(train, response, scale = TRUE, type = NULL, kernel = "radial",
    classwt = NULL, ...)
\method{mlSvm}{formula}(formula, data, scale = TRUE, type = NULL, kernel = "radial",
    classwt = NULL, ..., subset, na.action)
\method{predict}{mlSvm}(object, newdata, type = c("class", "membership", "both"),
    method = c("direct", "cv"), na.action = na.exclude,...)

mlNaiveBayes(...)
\method{mlNaiveBayes}{default}(train, response, laplace = 0, ...)
\method{mlNaiveBayes}{formula}(formula, data, laplace = 0, ..., subset, na.action)

response(object, ...)
\method{response}{default}(object, ...)
train(object, ...)
\method{train}{default}(object, ...)
}

\arguments{
  \item{formula}{ a formula with left term being the factor variable to predict
    (for supervised classification), a vector of numbers (for regression) or
    nothing (for unsupervised classification) and the right term with the list
    of independent, predictive variables, separated with a plus sign. If the
    data frame provided contains only the dependent and independent variables,
    one can use the \code{class ~ .} short version (and that one is strongly
    encouraged). Variables with minus sign are eliminated and calculations on
    variables are possible according to usual formula convention (possibly
    protected by \code{I()}). }
  \item{data}{ a data.frame to use as a training set. }
  \item{method}{ a machine learning method to use. For \code{predict()}, it is
    the prediction method. According to \code{predict.lda} in package MASS:
    determines how the parameter estimation is handled. With \code{"plug-in"}
    (the default) the usual unbiased parameter estimates are used and assumed to
    be correct. With \code{"debiased"} an unbiased estimator of the log posterior
    probabilities is used, and with \code{"predictive"} the parameter estimates
    are integrated out using a vague prior. With \code{"looCV"} the leave-one-out
    cross-validation fits to the original dataset are computed and returned. If
    you indicate \code{method = "cv"} then \code{cvpredict()} is used and you
    cannot provide \code{newdata} in that case. }
  \item{model.args}{ arguments for formula modeling with substituted data and
    subset... Not to be used by the end-user. }
  \item{call}{ the function call. Not to be used by the end-user. }
  \item{\dots}{ further arguments passed to the machine learning algorithm or
    the \code{predict()} method. See original algorithm. }
  \item{subset}{ index vector with the cases to define the training set in use
    (this argument must be named, if provided). }
  \item{na.action}{ function to specify the action to be taken if NAs are found
    \code{na.fail}, by default. Another option is \code{na.omit}, and cases with
    missing values on any required variable are dropped (this argument must be
    named, if provided). The default, and most suitable option for
    \code{predict()} methods is \code{na.exclude} and rows with NAs in newdata
    are then, excluded from prediction, but reinjected in final results. }  
  \item{cv.k}{ k for k-fold cross validation, cf \code{errorest()}. }
  \item{cv.strat}{ is the subsampling stratified or not in cross validation,
    cf \code{errorest()}. }
  \item{x}{ a mlearning object. }
  \item{y}{ another object (depending on the machine learning algorithm, but it
    is usually not used). }
  \item{object}{ one of the mlearning objects. }
  \item{newdata}{ a data.frame with same variables as \code{data} to use for new
    predictions. }
  \item{type}{ the type of result to get. Usually, \code{"class"}, which is the
    default. Depending on the algorithm, other types are also available.
    \code{membership} and \code{both} are almost always available too.
    \code{membership} corresponds to posterior probability, raw results,
    normalized votes, etc., depending on the machine learning algorithm. With
    \code{both}, class and membership are both returned at once in a list. For
    \code{mlSvm()}, it is the type of algorithm to use (see \code{?svm}). }
  \item{train}{ a matrix or data frame with predictors. }
  \item{response}{ a vector of factor (classification) or numeric (regression),
    or \code{NULL} (unsupervised classification). }
  \item{prior}{ prior probabilities of the classes (the proportions in the
    training set are used by default). For \code{mlLvq()}, probabilities to
    represent classes in the codebook (default values are the proportions in
    the training set). }
  \item{dimension}{ the dimension of the space to be used for prediction. }
  \item{ntree}{ the number of trees to generate (use a value large enough to get
    at least a few predictions for each input row). }
  \item{mtry}{ number of variables randomly sampled as candidates at each split. }
  \item{replace}{ sample cases with or without replacement? }
  \item{classwt}{ priors of the classes. Need not add up to one. }
  \item{size}{ number of units in the hidden layer for \code{mlNnet()}. Can be
    zero if there are skip-layer units. If \code{NULL}, a reasonalbe default is
    computed. for \code{mlLvq()}, the size of the codebook. Defaults to
    min(round(0.4*ng*(ng-1 + p/2),0), n) where ng is the number of classes.}
  \item{rang}{ initial random weights on [-rang, rang]. Value about 0.5 unless
    the inputs are large, in which case it should be chosen so that
    rang * max(|x|) is about 1. If \code{NULL}, a reasonalbe default is
    computed. }
  \item{decay}{ parameter for weight decay. Default 0. }
  \item{maxit}{ maximum number of iterations. Default 1000. }
  \item{k.nn}{ k used for k-NN test of correct classification. Default is 5. }
  \item{algorithm}{ an algorithm among 'olvq1' (default, the optimized lvq1),
    'lvq1', 'lvq2', or 'lvq3'. }
  \item{scale}{ are all the variables scaled? If a vector is provided, it is
    applied to variables with recycling. }
  \item{kernel}{ the kernel used by svm, see \code{?svm}. Can be "radial",
    "linear", "polynomial" or "sigmoid". }
  \item{laplace}{ positive double controlling Laplace smoothing for the naive
    Bayes classifier. The default (0) disables Laplace smoothing. }
}

\value{
  A machine learning object where the \code{predict()} method can be applied
  to classify new items.
  
  For \code{response()} and \code{train()}, the respective resmonse vector and
  training matrix (the matrix with all predicting terms).
}

\details{
  TODO: explain here the mechanism used to provide a common interface on top
  of various existing algorithms, and how one can add new items.
}

\author{ All these functions are just wrapper around existing R code written by
  Philippe Grosjean <Philippe.Grosjean@umons.ac.be> in order to get similar
  interface and objects. All credits to original authors (click here under). }

\seealso{\code{\link{confusion}}, \code{\link[ipred]{errorest}},
  \code{\link[MASS]{lda}}, \code{\link[MASS]{qda}},
  \code{\link[randomForest]{randomForest}}, \code{\link[class]{olvq1}},
  \code{\link[nnet]{nnet}}, \code{\link[e1071]{naiveBayes}},
  \code{\link[e1071]{svm}}}

\examples{
## Prepare data: split into training set (2/3) and test set (1/3)
data("iris", package = "datasets")
train <- c(1:34, 51:83, 101:133)
irisTrain <- iris[train, ]
irisTest <- iris[-train, ]
## One case with missing data in train set, and another case in test set
irisTrain[1, 1] <- NA
irisTest[25, 2] <- NA

data("HouseVotes84", package = "mlbench")

data(airquality, package = "datasets")

## Supervised classification using linear discriminant analysis
irLda <- mlLda(Species ~ ., data = irisTrain)
irLda
summary(irLda)
plot(irLda, col = as.numeric(response(irLda)) + 1)
predict(irLda, newdata = irisTest) # class (default type)
predict(irLda, type = "membership") # posterior probability
predict(irLda, type = "both") # both class and membership in a list
## Sometimes, other types are allowed, like for lda:
predict(irLda, type = "projection") # Projection on the LD axes
## Add test set items to the previous plot
points(predict(irLda, newdata = irisTest, type = "projection"),
    col = as.numeric(predict(irLda, newdata = irisTest)) + 1, pch = 19)

## predict() and confusion() should be used on a separate test set
## for unbiased estimation (or using cross-validation, bootstrap, ...)
confusion(irLda) # Wrong, cf. biased estimation (so-called, self-consistency)
## Estimation using a separate test set
confusion(predict(irLda, newdata = irisTest), irisTest$Species)

## Another dataset (binary predictor... not optimal for lda, just for test)
summary(res <- mlLda(Class ~ ., data = HouseVotes84, na.action = na.omit))
confusion(res) # Self-consistency
print(confusion(res), error.col = FALSE) # Without error column

## More complex formulas
summary(mlLda(Species ~ . - Sepal.Width, data = iris)) # Exclude variable
summary(mlLda(Species ~ log(Petal.Length) + log(Petal.Width) +
    I(Petal.Length/Sepal.Length), data = iris)) # With calculations

## Factor levels with missing items are allowed
ir2 <- iris[-(51:100), ] # No Iris versicolor in the training set
summary(res <- mlLda(Species ~ ., data = ir2)) # virginica is NOT there
## Missing levels are reinjected in class or membership by predict()
predict(res, type = "both")
## ... but, of course, the classifier is wrong for Iris versicolor
confusion(predict(res, newdata = iris), iris$Species)

## Simpler interface, but more memory-effective
summary(mlLda(train = iris[, -5], response = iris$Species))


## Supervised classification using quadratic discriminant analysis
summary(res <- mlQda(Species ~ ., data = irisTrain))
confusion(res) # Self-consistency
confusion(predict(res, newdata = irisTest), irisTest$Species) # Performances

## Another dataset (binary predictor... not optimal for qda, just for test)
summary(res <- mlQda(Class ~ ., data = HouseVotes84, na.action = na.omit))
confusion(res) # Self-consistency


## Supervised classification using random forest
summary(res <- mlRforest(Species ~ ., data = irisTrain))
plot(res)
## For such a relatively simple case, 50 trees are enough
summary(res <- mlRforest(Species ~ ., data = irisTrain, ntree = 50))
predict(res) # Default type is class
predict(res, type = "membership")
predict(res, type = "both")
predict(res, type = "vote")
## Out-of-bag prediction
predict(res, method = "oob")
confusion(res) # Self-consistency
confusion(res, method = "oob") # Out-of-bag performances
## Cross-validation prediction is a good choice when there is no test set:
predict(res, method = "cv")  # Idem: cvpredict(res)
confusion(res, method = "cv") # Cross-validation for performances estimation
## Evaluation of performances using a separate test set
confusion(predict(res, newdata = irisTest), irisTest$Species) # Test set perfs

## Regression using random forest (from ?randomForest)
set.seed(131)
summary(ozone.rf <- mlRforest(Ozone ~ ., data = airquality, mtry = 3,
    importance = TRUE, na.action = na.omit))
## Show "importance" of variables: higher value mean more important:
round(randomForest::importance(ozone.rf), 2)
plot(na.omit(airquality)$Ozone, predict(ozone.rf))
abline(a = 0, b = 1)

## Unsupervised classification using random forest (from ?randomForest)
set.seed(17)
summary(iris.urf <- mlRforest(~ ., iris[, -5]))
randomForest::MDSplot(iris.urf, iris$Species)
plot(hclust(as.dist(1 - iris.urf$proximity), method = "average"),
    labels = iris$Species)


## Supervised classification using neural network
set.seed(689)
summary(res <- mlNnet(Species ~ ., data = irisTrain))
predict(res) # Default type is class
predict(res, type = "membership")
predict(res, type = "both")
confusion(res) # Self-consistency
confusion(predict(res, newdata = irisTest), irisTest$Species) # Test set perfs

## Idem, but two classes prediction using factor predictors
set.seed(325)
summary(res <- mlNnet(Class ~ ., data = HouseVotes84, na.action = na.omit))
confusion(res) # Self-consistency

## Regression using neural network
set.seed(34)
summary(ozone.nnet <- mlNnet(Ozone ~ ., data = airquality, na.action = na.omit,
    skip = TRUE, decay = 1e-3, size = 20, linout = TRUE))
plot(na.omit(airquality)$Ozone, predict(ozone.nnet))
abline(a = 0, b = 1)


## Supervised classification using learning vector quantization
summary(res <- mlLvq(Species ~ ., data = irisTrain))
predict(res) # This object only returns class
confusion(res) # Self-consistency
confusion(predict(res, newdata = irisTest), irisTest$Species) # Test set perfs


## Supervised classification using support vector machine
summary(res <- mlSvm(Species ~ ., data = irisTrain))
predict(res) # Default type is class
predict(res, type = "membership")
predict(res, type = "both")
confusion(res) # Self-consistency
confusion(predict(res, newdata = irisTest), irisTest$Species) # Test set perfs

## Another dataset
summary(res <- mlSvm(Class ~ ., data = HouseVotes84, na.action = na.omit))
confusion(res) # Self-consistency

## Regression using support vector machine
summary(ozone.svm <- mlSvm(Ozone ~ ., data = airquality, na.action = na.omit))
plot(na.omit(airquality)$Ozone, predict(ozone.svm))
abline(a = 0, b = 1)


## Supervised classification using naive Bayes
summary(res <- mlNaiveBayes(Species ~ ., data = irisTrain))
predict(res) # Default type is class
predict(res, type = "membership")
predict(res, type = "both")
confusion(res) # Self-consistency
confusion(predict(res, newdata = irisTest), irisTest$Species) # Test set perfs

## Another dataset
summary(res <- mlNaiveBayes(Class ~ ., data = HouseVotes84, na.action = na.omit))
confusion(res) # Self-consistency
}

\keyword{ tree }
