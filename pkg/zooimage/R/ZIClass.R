# Copyright (c) 2004-2010, Ph. Grosjean <phgrosjean@sciviews.org>
#
# This file is part of ZooImage
#
# ZooImage is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# ZooImage is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ZooImage.  If not, see <http://www.gnu.org/licenses/>.

# Version 1.2.0: check package loading, and add a 'package' attribute to ZIClass
### TODO: allow for defining parameters and use a plugin mechanism

# Modifications in calculation of probabilities to accept variables selection v1.2-2
"ZIClass" <- function (df, algorithm = c("lda", "randomForest"),
package = c("MASS", "randomForest"), Formula = Class ~ logArea + Mean + StdDev +
Mode + Min + Max + logPerim. + logMajor + logMinor + Circ. + logFeret + IntDen +
Elongation + CentBoxD + GrayCentBoxD + CentroidsD + Range + MeanPos + SDNorm + CV,
calc.vars = "calc.vars", k.xval = 10, ...)
{
	# Check package availability
	# Note: this is supposed to be managed in the NAMESPACE
	# package <- package[1]
	# if (!is.null(package)) require( package, character.only = TRUE)

	# Check calc.vars
	calc.vars <- calc.vars[1]
	if (!is.null(calc.vars)) {
		CV <- match.fun(calc.vars)
		df <- CV(df)
	}

	# Algorithm
	algorithm <- algorithm[1]
	algo.fun  <- match.fun(algorithm)
	ZI.class <- algo.fun(Formula, data = df, ...)
	ZI.class <- structure(ZI.class,
		class = c("ZIClass", class(ZI.class)),
		algorithm = algorithm,
		package = package,
		calc.vars = CV,
		classes = df[[as.character(Formula)[2]]]
	)

	# Calculate predictions with full training set
    attr(ZI.class, "predict") <- predict(ZI.class, df, calc.vars = FALSE,
		class.only = TRUE)

	# Calculation of probabilities
  	if (algorithm == "randomForest") {
  		# Use Formula for the probabilities v1.2-2
  		rf <- randomForest(formula = Formula, data = df)
  		attr(ZI.class, "proba") <- predict(object = rf, newdata = df,
			type = "prob")
	}

	# Possibly make a k-fold cross-validation and check results
	if (!is.null(k.xval)) {
		mypredict <- if (algorithm == "lda") {
			function (object, newdata)
				predict(object, newdata = newdata)$class
		} else {
			function (object, newdata)
				predict(object, newdata = newdata, type = "class")
		}
    	res <- cv(attr(ZI.class, "classes"), Formula, data = df,
			model = get(algorithm), predict = mypredict, k = k.xval,
			predictions = TRUE, ...)$predictions
		attr(ZI.class, "kfold.predict") <- res
		attr(ZI.class, "k") <- k.xval
		attr(ZI.class, "formula") <- Formula
		attr(ZI.class, "path") <- attr(df, "path")
	}
	return(ZI.class)
}

"print.ZIClass" <- function (x, ...)
{
	algorithm <- attr(x, "algorithm")
	classes <- attr(x, "classes")
	lclasses <- levels(classes)
    predict <- attr(x, "predict")
	k <- attr(x, "k")
	cat("A ZIClass object predicting for", length(lclasses), "classes:\n")
	print(lclasses)
	Confu <- confu(classes, predict)
	mism <- 100 * (1 - (sum(diag(Confu)) / sum(Confu)))

	# Change the number of digits to display
	oldDigits <- options(digits = 4); on.exit(options(oldDigits))
	cat("\nAlgorithm used:", algorithm, "\n")
	cat("Mismatch in classification: ", mism, "%\n", sep = "")
	if (!is.null(k)) {
    	cat("k-fold cross validation error estimation (k = ", k, "):\n",
			sep = "")
		kfold.predict <- attr(x, "kfold.predict")
		prior <- table(classes)
		ok <- diag(table(classes, kfold.predict))
		err <- 100 * (1 - (sum(ok) / sum(prior)))
		cat(err, "%\n", sep = "")
		cat("\nError per class:\n")
		`Error (%)` <- sort(1 - (ok / prior)) * 100
		print(as.data.frame(`Error (%)`))
	}
	return(invisible(x))
}

"predict.ZIClass" <- function (object, ZIDat, calc.vars = TRUE,
class.only = FALSE, type = "class", na.rm = FALSE, ...)
{

	# Make sure we have correct objects
	mustbe(object, "ZIClass")
	mustbe(ZIDat , c("ZIDat", "data.frame"))
	
	# Possibly load a specific package for prediction
	package <- attr(object, "package")
	if (!is.null(package)) {
        # Make sure that the specific required package is loaded
        eval(parse(text = paste("require(", package, ")", sep = "")))
    }

    class(object) <- class(object)[-1]
	data <- as.data.frame(ZIDat)
	
	if (calc.vars) data <- attr(object, "calc.vars")(data)
	if (isTRUE(na.rm)) na.omit(data)
	
	if (type != "prob") {
		Ident <- predict(object, newdata = data, type = type)
	} else {
		if (inherits(object, "randomForest")) {
			Ident <- predict(object, newdata = data, type = type)
		} else if (inherits(object, "lda")) {
			Ident <- predict(object, newdata = data)$posterior
		} else stop("Cannot calculate yet for other algorithms than Random Forest or LDA")
	}

	# Special case for prediction from an LDA (list with $class item)
	if (inherits(Ident, "list") && "class" %in% names(Ident))
		Ident <- Ident$class
	if (!class.only) {
		res <- cbind(ZIDat, Ident)
		class(res) <- class(ZIDat)
	} else res <- Ident
	
	# New metadata attribute
	attr(res, "metadata") <- attr(ZIDat, "metadata")
	return(res)
}

"confu" <- function (classes1, classes2, classes.predicted = FALSE)
{
	if (is.factor(classes1) || is.factor(classes2)) {
		if (NROW(classes1) != NROW(classes2))
			stop("Not same number of items in classes1 and classes2")

		# Check that levels match
		mustmatch(levels(classes1), levels(classes2),
			msg = "'Class' levels in the two objects do not match")
		clCompa <- data.frame(Class.x = classes1, Class.y = classes2)
	} else { # Merge two data frame according to common objects in "Id" column

		# Check levels match
		mustmatch(levels(classes1$Class), levels(classes2$Class),
			msg = "Levels for 'Class' in the two objects do not match")

		# Are there common objects left?
		clCompa <- merge(classes1, classes2, by = "Id")
		if (nrow(clCompa) == 0)
			stop("No common objects between the two 'classes' objects")
	}

	# How many common objects by level?
	NbPerClass <- table(clCompa$Class.x)

	# Confusion matrix
	if (classes.predicted) {
		Conf <- table(classes = clCompa$Class.x, predicted = clCompa$Class.y)
	} else {
		Conf <- table(Class1 = clCompa$Class.x, Class2 = clCompa$Class.y)
	}

	# Pourcent of common objects
	Acc <- sum(diag(Conf)) / sum(Conf) * 100

	# Change labels to get a more compact presentation
	colnames(Conf) <- formatC(1:ncol(Conf), digits = 1, flag = "0")
	rownames(Conf) <- paste(colnames(Conf), rownames(Conf))

	# Results
	attr(Conf, "accuracy") <- Acc
	attr(Conf, "nbr.per.class") <- NbPerClass
	return(Conf)
}

"confu.map" <- function (set1, set2, level = 1)
{
	opar <- par(no.readonly = TRUE)
	on.exit(par(opar))
    par(mar = c(5, 12, 4, 2) + 0.1)

	n <- length(levels(set1))
	image(1:n, 1:n, 1 / (t(confu(set1, set2)[n:1, 1:n])), col = heat.colors(10),
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    axis(1, at = 1:n, las = 2)
    axis(2, at = n:1, labels = paste(levels(set1), 1:n), las = 1)
    abline(h = (1:(n + 1)) - 0.5, lty = 2, col = "gray")
    abline(v = (1:(n + 1)) - 0.5, lty = 2, col = "gray")
}

# New function v1.2-2 using library gplots
"confusion.tree" <- function (confmat, maxval, margin = NULL, Rowv = TRUE,
Colv = TRUE)
{
	nX <- nrow(confmat)
	nY <- ncol(confmat)
	nZ <- nX * nY
	confmat <- pmin(confmat, maxval)

	# Note: done in NAMESPACE
	# require(RColorBrewer)
	# require(gplots)
	mypalette <- brewer.pal(maxval - 1, "Spectral")
	heatmap.2(confmat, col= c(0, mypalette), symm = TRUE, margin = margin,
		trace = "both", Rowv = Rowv, Colv = Colv, cexRow = 0.2 + 1 / log10(nX),
		cexCol = 0.2 + 1 / log10(nY), tracecol = "Black", linecol = FALSE)
}

# New function v 1.2-2 false positive and negative
"confusion.bar" <- function (confmat, mar = NULL)
{
	if (!inherits(confmat, c("table", "matrix")))
		stop("'confmat' must be a table or a matrix")
	TP <- tp <- diag(confmat)
	fn <- rowSums(confmat) - tp
	fp <- colSums(confmat) - tp
	# Express fn and fp in proportions
	FN <- fn <- fn / (fn + tp)
	FP <- fp <- fp / (tp + fp)
	FP[is.na(FP)] <- 1
	# Rescale values so that:
	# fn/tp ratio and tp/fp ratio are kept, using same tp
	# total fn + tp + fp makes 100
	fp <- tp / (1 - fp) * fp
	# Rescale all so that they sum to 1
	scale <- fn + tp + fp
	fn <- fn / scale * 100
	tp <- tp / scale * 100
	fp <- fp / scale * 100
	# Just in case we have no tp at all:
	fn[is.na(tp)] <- 50
	fp[is.na(tp)] <- 50
	tp[is.na(tp)] <- 0
	res <- matrix(c(fp, tp, fn), ncol = 3)
	colnames(res) <- c( "fp", "tp", "fn")
	# Order items from smallest to largest tp
	pos <- order(res[, 2], decreasing = TRUE)
	res <- res[pos, ]
	FN <- FN[pos]
	FP <- FP[pos]
	TP <- TP[pos]

	# Plot
	if (is.null(mar)) mar <- c(1.1, 8.1, 4.1, 2.1)
	omar  <- par("mar")
	on.exit(par(omar)) # mar = margin size c(bottom, left, top, right)
	par(mar = mar)
	barplot(t(res), horiz = TRUE, col = c("PeachPuff2", "green3", "lemonChiffon2"),
		xaxt = "n", las = 1, space = 0)
	abline(v = (1:9) * 10, lty = 2)
	abline(v = 50, lwd = 2)

	# Print the fraction of fp and fn
	text(rep(4, length(FP)), 1:length(FP) - 0.1,
		paste(round((1 - FP) * 100), "%", sep = ""),
		adj = c(1, 1), cex = 0.7)
	text(rep(99, length(FN)), 1:length(FN) - 0.1,
		paste(round((1 - FN) * 100), "%", sep = ""),
		adj = c(1, 1), cex = 0.7)

	# Print the number of true positives
	xpos <- res[, 1] + res[, 2] / 2 
	text(xpos, 1:length(FN) - 0.1, round(TP),
		adj = c(0.5, 1), cex = 0.7)

	# Add a legend
  	legend(50, length(FN) * 1.05, legend = c("false positive (FP)",
		"true positive (TP)", "false negative (FN)"),
		xjust = 0.5, yjust = 1, fill = c("PeachPuff2", "green3", "lemonChiffon2"),
		bty = "n", horiz = TRUE)
	axis(2, 1:length(FN) - 0.5, tick = FALSE, las = 1, cex.axis = 0.7,
		labels = names(attr(confmat, "nbr.per.class")))
	title(main = "Precision tp/(tp+fp) at left versus recall tp/(tp+fn) at right")
}

"nnet2" <- function (formula, data, size = 7, rang = 0.1, decay = 5e-4,
maxit = 1000, ...)
{
 	# Note: done in NAMESPACE
	# require(nnet)

	structure(
		nnet(formula = formula, data = data, size = size, rang = rang,
			decay = decay, maxit = maxit, ...),
		class = c("nnet2", "nnet.formula", "nnet"))
}

"predict.nnet2" <- function (object, newdata, type = c("raw", "class"), ...)
{
	# Note: done in NAMESPACE
	# require(nnet)
	mustbe(object, "nnet2")
    class(object) <- class(object)[-1]
	res <- predict(object, newdata = newdata, type = type, ...)
	# If type is class, we got a character vector... but should get a factor
	if (type == "class")
    	res <- factor(res, levels = object$lev)
	return(res)
}

# Extract classes and training vars from data, according to formula lhs ~ rhs
# This is a very simplified way of doing it... It does not manage complex
# formula constructions!
"lvq" <- function (formula, data, k = 5, size = NULL)
{
	# Note: done in NAMESPACE
	# require(class)
    vars <- all.vars(formula)
	train <- data[, vars[-1]]
	cl <- data[, vars[1]]
	lev <- levels(cl)
	codebk <- olvq1(train, cl, lvqinit(train, cl, k = k, size = size))
	res <- list(codebook = codebk, data = data, vars = vars, classes = cl,
		lev = lev)
	class(res) <- "lvq"
	return(res)
}

"predict.lvq" <- function (object, newdata, type = "class", ...)
{
   	# Note: done in NAMESPACE
	# require(class)
	mustbe(object, "lvq")
    if (missing(newdata)) newdata <- object$data
	lvqtest(object$codebook, newdata[, object$vars[-1]])
}

# Formula calculation by variables selection for the classifier creation v1.2-2
FormVarsSelect <- function (x)
{
	# x must be a ZItrain object
	mustbe(x, "ZI1Train")

	# Parameters measured on particles and new variables calculated
	mes <- as.vector(colnames(calc.vars(x)))

	# Selection of features for the creation of the classifier
	keep <- select.list(list = mes, preselect = c("ECD", "FIT_Area_ABD",
		"FIT_Diameter_ABD", "FIT_Volume_ABD", "FIT_Diameter_ESD",
		"FIT_Volume_ESD", "FIT_Length", "FIT_Width", "FIT_Aspect_Ratio",
		"FIT_Transparency", "FIT_Intensity", "FIT_Sigma_Intensity",
		"FIT_Sum_Intensity", "FIT_Compactness", "FIT_Elongation",
		"FIT_Perimeter", "FIT_Convex_Perimeter", "FIT_Roughness",
		"FIT_Ch1_Peak", "FIT_Ch1_TOF", "FIT_Ch2_Peak", "FIT_Ch2_TOF",
		"Area", "Mean", "StdDev", "Mode", "Min", "Max", "Perim.", "Width",
		"Height", "Major", "Minor", "Circ.", "Feret", "IntDen", "Median",
		"Skew", "Kurt", "Elongation", "CentBoxD", "GrayCentBoxD", "CentroidsD",
		"Range", "MeanPos", "SDNorm", "CV", "logArea", "logPerim.", "logMajor",
		"logMinor", "logFeret"),
		multiple = TRUE, title = "Select variables to keep")
	# Creation of one formula for classifier calculation
	res <- as.formula(paste("Class ~ ", paste(keep, collapse= "+")))
	return(res)
}
