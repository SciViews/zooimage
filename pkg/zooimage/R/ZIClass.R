## Copyright (c) 2004-2012, Ph. Grosjean <phgrosjean@sciviews.org>
##
## This file is part of ZooImage
##
## ZooImage is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## ZooImage is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with ZooImage.  If not, see <http://www.gnu.org/licenses/>.

ZIClass <- function (formula, data, mlearning = getOption("ZI.mlearning",
mlRforest), calc.vars = getOption("ZI.calcVars", calcVars), cv.k = 10, cv.strat,
..., subset, na.action = getOption("ZI.naAction", na.omit))
{	
	## Check calc.vars and use it on data
	if (length(calc.vars))
		if (!is.function(calc.vars)) {
			stop("'calc.vars' must be a function or NULL")
		} else data <- calc.vars(data)

	## Machine learning function
	mlearning <- match.fun(mlearning)
	if (!is.function(mlearning))
		stop("'mlearning' must be a function that produce a 'mlearning' object or a compatible one")
	
	## Train the machine learning algorithm
	if (missing(subset) || !length(subset)) {
		ZI.class <- mlearning(formula, data = data, ..., na.action = na.action)
	} else {
		ZI.class <- mlearning(formula, data = data, ..., subset,
			na.action = na.action)
	}
	
	## Add ZIClass as class of the object
	class(ZI.class) <- c("ZIClass", class(ZI.class))
	attr(ZI.class, "calc.vars") <- calc.vars

	## Calculate predictions with full training set
    attr(ZI.class, "predict") <- predict(ZI.class, data, calc.vars = FALSE)

	## Possibly make a k-fold cross-validation and check results
	if (length(cv.k)) {
		attr(ZI.class, "cvpredict") <- cvpredict(ZI.class, type = "both",
			cv.k = cv.k, cv.strat = cv.strat)
		attr(ZI.class, "k") <- cv.k
		attr(ZI.class, "strat") <- cv.strat
	}
	ZI.class
}

print.ZIClass <- function (x, ...)
{
	algorithm <- attr(x, "algorithm")
	classes <- attr(x, "classes")
	lclasses <- levels(classes)
    predicted <- attr(x, "predict")
	k <- attr(x, "k")
	cat("A 'ZIClass' object predicting for", length(lclasses), "classes:\n")
	print(lclasses)
	Confu <- confusion(classes, predicted)
	mism <- 100 * (1 - (sum(diag(Confu)) / sum(Confu)))

	## Change the number of digits to display
	oldDigits <- options(digits = 4)
	on.exit(options(oldDigits))
	cat("\nAlgorithm used:", algorithm, "\n")
	cat("Mismatch in classification: ", mism, "%\n", sep = "")
	if (!is.null(k)) {
    	cat("k-fold cross validation error estimation (k = ", k, "):\n")
		kfold.predict <- attr(x, "kfold.predict")
		prior <- table(classes)
		ok <- diag(table(classes, kfold.predict))
		err <- 100 * (1 - (sum(ok) / sum(prior)))
		cat(err, "%\n", sep = "")
		cat("\nError per class:\n")
		`Error (%)` <- sort(1 - (ok / prior)) * 100
		print(as.data.frame(`Error (%)`))
	}
	invisible(x)
}

summary.ZIClass <- function(object, sort.by = "Fscore", decreasing = TRUE,
na.rm = FALSE, ...)
{
	## Get the confusion object out of a ZIClass object and calc stats from there
	summary(confusion(object), sort.by = sort.by, decreasing = decreasing,
		na.rm = na.rm)
}

predict.ZIClass <- function (object, ZIDat, calc.vars = TRUE,
class.only = FALSE, type = "class", na.rm = FALSE, ...)
{
	## Make sure we have correct objects
	if (!inherits(object, "ZIClass"))
		stop("'object' must be a 'ZIClass' object")
	if (!inherits(ZIDat, c("ZIDat", "data.frame")))
		stop("'ZIDat' must be a 'ZIDat' or 'data.frame' object")
	
    class(object) <- class(object)[-1]
	data <- as.data.frame(ZIDat)
	
	if (isTRUE(as.logical(calc.vars)))
		data <- attr(object, "calc.vars")(data)
	if (isTRUE(as.logical(na.rm))) na.omit(data)
	
	algorithm <- attr(object, "algorithm")
	if (type != "prob") {
	   # modification to accept algoritms from party package
	   if (algorithm %in% c("ctree", "cforest")) {
            Ident <- predict(object, newdata = data, type = "response",
				OOB = FALSE)
		} else {
            Ident <- predict(object, newdata = data, type = type)
		}
	} else {
		if (inherits(object, "randomForest")) {
			Ident <- predict(object, newdata = data, type = type)
		} else if (inherits(object, "lda")) {
			Ident <- predict(object, newdata = data)$posterior
		} else stop("Cannot calculate yet for other algorithms than Random Forest or LDA")
	}

	## Special case for prediction from an LDA (list with $class item)
	if (inherits(Ident, "list") && "class" %in% names(Ident))
		Ident <- Ident$class
	if (!isTRUE(as.logical(class.only))) {
		res <- cbind(ZIDat, Ident)
		class(res) <- class(ZIDat)
	} else res <- Ident
	
	## New metadata attribute
	attr(res, "metadata") <- attr(ZIDat, "metadata")
	res
}

#confusion.ZIClass <- function (x, ...)
#{
#	## If the object is ZIClass, calculate 'confusion'
#	## from attributes 'classes' and 'kfold.predict' 
#	if (!inherits(x, "ZIClass"))
#		stop("'x' must be a 'ZIClass' object")
#	
#	x <- attr(x, "classes")
#	y <- attr(x, "kfold.predict")
#	labels <- c("Class", "Predict")
#	clCompa <- data.frame(Class = x, Predict = y)
#	## How many common objects by level?
#	NbrPerClass1 <- table(clCompa[, 1])
#	## How many predicted objects
#	NbrPerClass2 <- table(clCompa[, 2])
#	## Confusion matrix
#	Conf <- table(clCompa)
#	## Further stats: total, true positives, accuracy
#	Total <- sum(Conf)
#	TruePos <- sum(diag(Conf))
#	Stats <- c(total = Total, truepos = TruePos, accuracy = TruePos / Total * 100)
#
#	## Change labels to get a more compact presentation
#	colnames(Conf) <- formatC(1:ncol(Conf), digits = 1, flag = "0")
#	rownames(Conf) <- paste(colnames(Conf), rownames(Conf))
#
#	## Additional data as attributes
#	attr(Conf, "stats") <- Stats
#	attr(Conf, "nbr.rows") <- NbrPerClass1
#	attr(Conf, "nbr.cols") <- NbrPerClass2
#	
#	## This is a confusion object
#	class(Conf) <- c("confusion", "table")
#	Conf
#}
