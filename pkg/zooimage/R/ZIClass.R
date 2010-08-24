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
    predicted <- attr(x, "predict")
	k <- attr(x, "k")
	cat("A ZIClass object predicting for", length(lclasses), "classes:\n")
	print(lclasses)
	Confu <- ZIConf(classes, predicted)
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
	if (!inherits(object, "ZIClass"))
		stop("'object' must be a 'ZIClass' object")
	if (!inherits(ZIDat, c("ZIDat", "data.frame")))
		stop("'ZIDat' must be a 'ZIDat' or 'data.frame' object")
	
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
	if (!inherits(object, "nnet2"))
		stop("'object' must be a 'nnet2' object")
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
	if (!inherits(object, "lvq"))
		stop("'object' must be a 'lvq' object")
    if (missing(newdata)) newdata <- object$data
	lvqtest(object$codebook, newdata[, object$vars[-1]])
}

# Formula calculation by variables selection for the classifier creation v1.2-2
FormVarsSelect <- function (x)
{
	# x must be a ZItrain object
	if (!inherits(x, "ZITrain"))
		stop("'x' must be a 'ZITrain' object")

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
