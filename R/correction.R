## Copyright (c) 2004-2015, Ph. Grosjean <phgrosjean@sciviews.org>
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
## along with ZooImage. If not, see <http://www.gnu.org/licenses/>.

## TODO: disable next button when in last fraction
## TODO: save temporary and final results in the zidb file
## TODO: correct bugs with the back button

## Load libraries
#library(svMisc) #,  lib.loc = "./Libraries/V3")
#library(svDialogs) #, lib.loc = "./Libraries/V3")
#library(zooimage) #, lib.loc = "./Libraries/V3")
##library(RANN) # Only function: nn2()... apparently not used here!
#library(randomForest)
#library(e1071)
#library(RWeka)
#library(C50)
#library(ipred)

#### Functions not used, but kept for possible future use ######################
## Area under the curve
auc <- function (x, y) {
	if (length(x) != length(y) || length(x) < 2)
		stop("'x' and 'y' must have same length >= 2")
	sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
}


#### Functions NOT used by errorCorrection(), but by examples ##################
## TODO: corrHist() and addPoints() are used together... make it one plot() method?
## TODO: make it S3 object with print, summry and plot methods + predict + ...
## Function to plot size spectrum of particles.
## Can be used for error correction of biomass (perspectives)
corrHist <- function (data, subset, sizeParam, interval) {
	data <- data[subset, sizeParam]
	if (!length(data)) return(0)
	hist(data, breaks = seq(min(data) - (2* interval), max(data) +
		(2 * interval), interval), plot = FALSE)
}

## Add points to an histogram
addPoints <- function (x, ...) {
	if (!inherits(x, "histogram"))
		stop("x must be an object of class 'histogram'")
	points(x = x$mids[x$counts > 0], y = x$counts[x$counts > 0], ...)
}

## Calculation of the residual error based on global error estimation
residError <- function (ErrorEstim, Error, Validated, na.rm = FALSE) {
	nObs <- length(Validated)
	if (sum(Validated) == nObs) return(0) # No error since everybody validated
	nErr <- ErrorEstim * nObs
	nErrVal <- sum(Error & Validated, na.rm = na.rm)
	nErrRes <- nErr - nErrVal
	max(nErrRes / nObs, 0) # To avoid potential negative values
}

## Evolution of residual error
residErrorEvol <- function (ErrorEstimEvol, Error, Validated, Step,
na.rm = FALSE) {
	nIter <- length(ErrorEstimEvol)
	if (nIter == 0) return(numeric(0))
		
	res <- numeric(nIter)
	for (i in 1:nIter) {
		Valid <- Validated & Step < i  #Step %in% 0:(i - 1)
		res[i] <- residError(ErrorEstim = ErrorEstimEvol[i], Error = Error,
			Validated = Valid, na.rm = na.rm)
	}
	res
}


#### Functions used both by errorCorrection() and by examples ##################
## Compute the Bray-Curtis dissimilarity index on two vectors
## Note that this is faster than vegan::vegdist for two vectors
## and it remains faster for a vector versus a matrix. But fir all pairs,
## use vegan::vegdist(x, method = "bray") instead
## x must be a reference vector, and y can be a vector of same length,
## or a matrix or a data frame with same number of rows
dissimilarity <- function (x, y, na.rm = FALSE) {
	if (is.data.frame(y)) y <- as.matrix(y)
	if (!is.numeric(x) || !is.numeric(y))
		stop("'x' and 'y' must be numeric")
	if (!is.null(dim(x)))
		stop("'x' must be a vector, not a matrix or an array")
	if (is.matrix(y)) {
		if (length(x) != nrow(y))
			stop("'y' must have same rows as the length of 'x'")
		
				## The matrix calculation version
		colSums(abs(x - y), na.rm = na.rm) /
			colSums(rbind(y, sum(x, na.rm = na.rm)), na.rm  = na.rm)
	
	} else { # x and y must be vectors of same length
		if (!is.null(dim(y)) || length(x) != length(y))
			stop("'x' and 'y' must be two vectors of same length")
		
		## The simpler vector version
		sum(abs(x - y), na.rm = na.rm) / sum(x, y, na.rm = na.rm) 
	}
}

#### Functions used only by errorCorrection() ##################################
## Calculation of the global error based on validated items
## /!\ NOT based on items randomly selected --> Bad approximation
.globalError <- function (suspect, error, subset, na.rm = FALSE) {
	suspTot <- sum(suspect, na.rm = na.rm)
	trustTot <- sum(!suspect, na.rm = na.rm)
	vSusp <- suspect[subset]
	vTrust <- !suspect[subset]
	susp <- sum(vSusp, na.rm = na.rm)
	trust <- sum(vTrust, na.rm = na.rm)
	pSusp <- susp / suspTot
	pTrust <- trust / trustTot
	wTrust <- pSusp / pTrust
	if (is.na(wTrust) || !is.finite(wTrust)) wTrust <- 1
	vErr <- error[subset]
	err <- sum(vErr[vSusp], na.rm = na.rm) + wTrust * sum(vErr[vTrust],
		na.rm = na.rm)
	tot <- susp + wTrust * trust
	err / tot
}

## Error in each of the four fractions
.errorInFractions <- function (suspect, error, validated, iteration = NA,
	na.rm = FALSE) {
  
	## The four fractions
	suspValIdx <- suspect & validated 
	suspNotValIdx <- suspect & !validated
	trustValIdx <- !suspect & validated
	trustNotValIdx <- !suspect & !validated
  
	## Error in each fraction
	errSuspValIdx <- error[suspValIdx]
	errSuspNotValIdx <- error[suspNotValIdx]
	errTrustValIdx <- error[trustValIdx]
	errTrustNotValIdx <- error[trustNotValIdx]
  
	## Number of items in each fraction
	nSuspVal <- sum(suspValIdx, na.rm = na.rm)
	nSuspNotVal <- sum(suspNotValIdx, na.rm = na.rm)
	nSuspTot <- nSuspVal + nSuspNotVal
	nTrustVal <- sum(trustValIdx, na.rm = na.rm)
	nTrustNotVal <- sum(trustNotValIdx, na.rm = na.rm)
	nTrustTot <- nTrustVal + nTrustNotVal
   
	## Number of error in each fraction
	nErrSuspVal <- sum(errSuspValIdx, na.rm = na.rm) 
	nErrSuspNotVal <- sum(errSuspNotValIdx, na.rm = na.rm) 
	nErrSuspTot <- nErrSuspVal + nErrSuspNotVal
	nErrTrustVal <- sum(errTrustValIdx, na.rm = na.rm) 
	nErrTrustNotVal <- sum(errTrustNotValIdx, na.rm = na.rm) 
	nErrTrustTot <- nErrTrustVal + nErrTrustNotVal
  
	## Number of error in validated fraction if random distribution of the error
	nErrSuspValRd <- nErrSuspTot / nSuspTot * nSuspVal  
	nErrTrustValRd <- nErrTrustTot / nTrustTot * nTrustVal
  
	## Error rate in each fraction
	errSuspVal <- nErrSuspVal / nSuspVal
	errSuspNotVal <- nErrSuspNotVal / nSuspNotVal
	errTrustVal <- nErrTrustVal / nTrustVal
	errTrustNotVal <- nErrTrustNotVal / nTrustNotVal
  
	## Weight for trusted items
	probaSusp <- nSuspVal / nSuspTot
	probaTrust <- nTrustVal / nTrustTot
	weightTrust <- probaSusp / probaTrust
  
	## Results: data frame
	if (!all(is.na(iteration))) {
  	## Calculation of error in validated fraction at current iteration
		suspStepIdx <- suspect & iteration 
		trustStepIdx <- !suspect & iteration
		errSuspStepIdx <- error[suspStepIdx]
		errTrustStepIdx <- error[trustStepIdx]
		nSuspStep <- sum(suspStepIdx, na.rm = na.rm)
		nTrustStep <- sum(trustStepIdx, na.rm = na.rm)
		nErrSuspStep <- sum(errSuspStepIdx, na.rm = na.rm)
		nErrTrustStep <- sum(errTrustStepIdx, na.rm = na.rm)
		errSuspStep <- nErrSuspStep / nSuspStep
		errTrustStep <- nErrTrustStep / nTrustStep

		res <- data.frame(errSuspVal = errSuspVal, errTrustVal = errTrustVal,
			errSuspNotVal = errSuspNotVal, errTrustNotVal = errTrustNotVal,
			errSuspStep = errSuspStep, errTrustStep = errTrustStep, 
			nErrSuspTot = nErrSuspTot, nErrTrustTot = nErrTrustTot,
			nErrSuspVal = nErrSuspVal, nErrTrustVal = nErrTrustVal,
			nErrSuspStep = nErrSuspStep, nErrTrustStep = nErrTrustStep,
			nErrSuspValRd = nErrSuspValRd, nErrTrustValRd = nErrTrustValRd,
			nErrSuspNotVal = nErrSuspNotVal, nErrTrustNotVal = nErrTrustNotVal,
			nSuspTot = nSuspTot, nTrustTot = nTrustTot, nSuspVal = nSuspVal,
			nTrustVal = nTrustVal, nSuspStep = nSuspStep,
			nTrustStep = nTrustStep, nSuspNotVal = nSuspNotVal,
			nTrustNotVal = nTrustNotVal, weightTrust = weightTrust)    
	} else {
		## Calculation of error in global sample
		res <- data.frame(errSuspVal = errSuspVal, errTrustVal = errTrustVal,
			errSuspNotVal = errSuspNotVal, errTrustNotVal = errTrustNotVal,
			nErrSuspTot = nErrSuspTot, nErrTrustTot = nErrTrustTot,
			nErrSuspVal = nErrSuspVal, nErrTrustVal = nErrTrustVal,
			nErrSuspValRd = nErrSuspValRd, nErrTrustValRd = nErrTrustValRd,
			nErrSuspNotVal = nErrSuspNotVal, nErrTrustNotVal = nErrTrustNotVal,
			nSuspTot = nSuspTot, nTrustTot = nTrustTot, nSuspVal = nSuspVal,
			nTrustVal = nTrustVal, nSuspNotVal = nSuspNotVal,
			nTrustNotVal = nTrustNotVal, weightTrust = weightTrust)
	}
	res
}

## Compute probabilities that will be used for suspect detection
classProba <- function (data, predicted = data$Predicted, classifier,
diff.max = 0.2, prop.bio = NULL) {
	## Get first and second identification probabilities
	## TODO: only works for randomForest and does not use mlearning!
	if (inherits(classifier, "randomForest")) {
		if (inherits(classifier, "ZIClass")) {
			data <- attr(classifier, "calc.vars")(data)
		}
		class(classifier) <- class(classifier)[-1]
		prob <- predict(classifier, newdata = data, class.only = FALSE,
			type = "membership")
	} else stop("Suspect detection not yet implemented for this algorithm")
	max.ids <- apply(prob, 1, sort, decreasing = TRUE)[1:2, ]
	first <- max.ids[1, ]
	second <- max.ids[2, ]

	## 1) Coefficient quantifying quality of identification
	identCoef <- sqrt(first * pmin((first - second) / (first * diff.max), 1))
	
	## 2) Probability if identification in that group
	#first
	
	## 3) Difference between first and second probabilities	
	probaDiff <- first - second

	## 4) Proportion of the group (rares groups tend to have more FP!)
	predTable <- table(predicted)
	prop <- predTable / sum(predTable)
	gpProp <- prop[as.character(predicted)]
	
	# 5) ProbaBio modulation?
	if (is.null(prop.bio)) {
		proba <- data.frame(IdentCoef = identCoef, TreeProp = first,
		ProbaDiff = probaDiff, GpProp = gpProp)
	} else {
		bioFact <- prop.bio[as.character(predicted)]
		proba <- data.frame(IdentCoef = identCoef, TreeProp = first,
		ProbaDiff = probaDiff, GpProp = gpProp, BioFact = bioFact)
	}
	attr(proba, "IdentParam") <- list(DiffMax = diff.max,
		ProbaBio = prop.bio, ProbaTable = prob)
	proba
}

## Compute the proportion of false positives according to Bayes theorem
corrProba <- function (proba, predicted, confusion, nobs) {
	stats <- summary(confusion, type = c("Recall", "Specificity"))
	table.pred <- table(predicted)
	prop <- table.pred / nobs
	recall <- stats$Recall 
	specif <- stats$Specificity
	fpprop <- numeric(length(table.pred))
	for (name in names(table.pred)) {
		prop.a <- prop[name]
		prop.b <- 1 - prop.a
		recall.a <- stats[name, ]$Recall
		fprate <- 1 - stats[name, ]$Specificity
		b.in.a <- fprate * prop.b
		a.in.a <- recall.a * prop.a
		fpprop[name] <- b.in.a / (b.in.a + a.in.a)
		if (fpprop[name] == "NaN") fpprop[name] <- 1
	}
	proba$FP <- fpprop[as.character(predicted)]
	## Compare this proportion to group proportion
	proba$GpFPDiff <- proba$GpProp - proba$FP
	proba
}


## Detect suspect particles using different algorithms and update corr$Suspect
suspect <- function (data, proba, error, subset, predicted, confusion,
algorithm = "rf", knn.k = 3, svm.kernel = "polynomial", svm.degree = 5, ...) {
	algorithm <- match.arg(algorithm,
		c("rf", "knn", "svm", "bayes", "c50", "glm"))
	
	## In the improbable case we have no error at all, consider everyone as suspect...
	if (all(as.character(error) != "Error"))
		return(factor(rep("Error", nrow(set)), levels = levels(error)))
		
	## Update proba with latest calculations, according to a corrected confusion matrix
	proba <- corrProba(proba, predicted, confusion, nrow(data))
	set <- cbind(data, proba, Error = error)
	
	## TODO: mlearning is there for complete interchangeability of algorithms
	## thus, we should not need this switch() construct!
	res <- switch(algorithm,
		rf = {
			rf.model <- mlearning(formula = Error ~ ., data = set,
				subset = subset, method = "mlRforest", ...)    
			susp <- predict(rf.model, newdata = set, type = "class")
			susp[subset] <- predict(rf.model, type = "class", method = "oob")  
		},
		knn = {
		#	set <- subset(set, select = -c(Error))
		#	warning("Prediction of training set by 'knn' method without cross validation")
		#	susp <- knn(train = set[subset, ], test = set,
		#		cl = corr$Error[subset], k = knn.k, ...)
		stop("Not implemented yet!")
		},
		svm = { 
		#	svm.model <- mlearning(Error ~ ., data = set,
		#		subset = subset, kernel = svm.kernel, degree = svm.degree,
		#		method = "mlSvm", ...)
		#	susp <- predict(svm.model, newdata = set, type = "class")
		#	susp[subset] <- predict(svm.model, type = "class", method = "cv") 
		stop("Not implemented yet!")
		},
		bayes = {
		#	nb.model <- mlearning(Error ~ ., data = set,
		#		subset = subset, method = "mlNaiveBayes", ...) 
		#	susp <- predict(nb.model, newdata = set, type = "class")
		#	susp[subset] <- predict(nb.model, type = "class", method = "cv") 
		stop("Not implemented yet!")
		},
		c50 = {
		#	C50.train <- set[subset, ]
		#	c50.model <- C5.0(formula = Error ~ ., data = C50.train, ...)
		#	susp <- predict(c50.model, newdata = set, type = "class")
		#	susp[subset] <- cv(y = C50.train$Error, formula = Error ~ .,
		#		data = C50.train, model =  C5.0, predict =
		#		function(object, newdata) predict(object, newdata,
		#		type = "class"), k = 10, predictions = TRUE)$predictions  
		stop("Not implemented yet!")
		},
		glm =  {
		#	glm.model <- Logistic(Error ~ ., data = set, subset = subset, ...)
		#	warning("Prediction of training set by 'glm' method without cross-validation")
		#	susp <- predict(glm.model, newdata = set, type = "class")
		stop("Not implemented yet!")
		},
		stop("Unknown algorithm '", algorithm, "'")
	)
	susp
}

## Main function for error correction
## Replace data by ZIobj and put zidb in first position... or group both items
## and how to retrieve class then???
## Group options in a single object too
errorCorrection <- function (data, classifier, mode = "validation",
fraction = 0.05, sample.min = 100, sample.max = 200, grp.min = 2,
random.sample = 0.1, algorithm = "rf", diff.max = 0.2, prop.bio = NULL,
rare = 0.01, zidb = NULL, testdir = NULL, id = NULL,
result = ".last_valid", envir = parent.frame()) {
	#### Parameters explanations
	#### Data and classifier
	## data -- the dataset to study
	if (missing(data) || !inherits(data, "ZIDat"))
		stop("data must be a ZIdat object")
## Temporary hack to eliminate possible unwanted columns!
data$Id.1 <- NULL
data$X.Item.1 <- NULL
	
	## classifier -- the classifier to use to classify particles
	if (missing(classifier) || !inherits(classifier, "ZIClass"))
		stop("classifier must be a ZIClass object")
#######	calc.vars <- attr(classifier, "calc.vars")
	
	#### General parameters of the iterative correction
	## mode -- the mode (validation, stat or demo)
	mode <- match.arg(mode, c("validation", "demo", "stat"))
	if (mode != "validation" & is.null(data$Class))
		 	stop("data requires a 'Class' column in this mode")
	## fraction -- fraction of sample to validate
	fraction <- as.numeric(fraction)[1]
	if (fraction < 0.01 || fraction > 1)
		stop("fraction must be between 0.01 and 1")
	## sample.min -- minimum number of particles to validate ate each step
	sample.min <- as.integer(sample.min)[1]
	if (sample.min < 1)
		stop("sample.min must be a positive integer")
	## sample.max -- maximum number of particles to validate ate each step
	sample.max <- as.integer(sample.max)[1]
	if (sample.max < 1)
		stop("sample.max must be a positive integer")
	if (sample.max < sample.min)
		stop("sample.max must be higher or equal than sample.min")
	## grp.min -- minimum number of particles of each group to validate
	grp.min <- as.integer(grp.min)[1]
	if (grp.min < 1 || grp.min > sample.min)
		stop("grp.min must be a positive integer and cannot be larger than sample.min")
	## random.sample -- fraction of random sample in the validation set
	random.sample <- as.numeric(random.sample)[1]
	if (random.sample < 0 || random.sample > 1)
		stop("random.sample must be a number between 0 and 1")
	
	#### Parameters for the detection of suspects
	## algorithm -- algorithm used to detect suspect particles
	## diff.max -- maximum tolerated difference in probabilities for class identification
	diff.max <- as.numeric(diff.max)[1]
	if (diff.max < 0)
		stop("diff.max must be a positive number or zero")
	## proba.bio -- groups probabilities, using biological information
	if (length(prop.bio) && (!is.numeric(prop.bio) || is.null(names(prop.bio))))
		stop("prop.bio must be a named vector (groups) of numbers")
	## rare -- detection of rare items
    rare <- as.numeric(rare)[1]
    if (rare < 0 || rare > 0.2)
		stop("rare must be between 0 and 0.2")
	
	## zidb -- path to the zidbfile to manually validate
	## testdir -- path of the directory used for manual validation
	
	## Variables
	proba <- NULL			# identification probabilities (additional vars for suspect detection)
	corr <- NULL			# main informations about the correction
	validated <- NULL		# validated class of the particles
	validation.data <- NULL # Data send as validation of a given step
	predicted <- NULL		# predictions at the beginning
	step <- -1 				# current iteration
	sample.size <- NULL		# size of the next subsample to validate
	## TODO: eliminate this and get it from table(corr$Step)
	validated.fracs <- 0 	# fraction of items validated at each step

	## Manual validation variables
	ntrusted <- NULL			# number of trusted particles detected
	nsuspect <- NULL			# number of suspect particles detected
	ntrusted.tovalid <- NULL 	# number of trusted particles to validate
	nsuspect.tovalid <- NULL 	# number of suspect particles to validate
	testset <- NULL				# subsample to validate
	
	## Validation of particles TODO: get rid of these two flags!
	step.manual <- FALSE		# should the user validate particles?
	testset.validated <- FALSE	# is the testset validated?
	
	## Plankton sorter variables
	sorter.title <- NULL 		# title of the plankton sorter page
	sorter.id <- NULL			# identifier of the plankton sorter page

	## Results
	manual.history <- NULL		# history of the manual confusion matrix
	manual2.history <- NULL		# history of manual + 2nd ident confusion matrix
	corr.confusion <- NULL 		# corrected confusion matrix
	classRare <- NULL			# String with the list of classes considered as rare
	cell.confusion <- NULL   	# corrected confusion matrix for cells
    bioweight.confusion <- NULL # corrected confusion matrix for bioweight
	correction.history <- NULL 	# history of the correction confusion matrix
	correctionCell.history <- NULL   # history of the correction confusion matrix for cells
    correctionBio.history <- NULL   # history of the correction confusion matrix for bioweight
	error.estim.data <- NULL 	# data used to estimate the error
	error.estim <- NULL			# history of the error estimation
	error.estim.history <- NULL # evolution of the error estimation
	error.estim.rd <- NULL 		# error using validated, randomly selected items
	error.estim.rd.history <- NULL # evolution of the error estimation
	suspect.history <- list()	# history of suspect detection
	validated.history <- list()	# history of validated particles
	error.valid.history <- list() # history of error among validated items
	error.suspect.history <- list() # history of error among suspect items
	error.trusted.history <- list() # history of error among suspect items
	nsuspect.history <- NULL 	# history of the number of suspect items
	ntrusted.history <- NULL 	# history of the number of trusted items
	nsuspect.tovalid.history <- NULL # history of suspect validation
	ntrusted.tovalid.history <- NULL # history of trusted validation
	errInFract.history <- data.frame() # evolution of the error in each fraction

	## Initialize the data for error correction (data, proba, corr and others)
	initialize <- function () {
		## Check that this directory exists and is empty
		if (mode != "stat") {
			if (is.null(testdir))
				testdir <<- file.path(tempdir(),
					noExtension(zidb))
			if (file.exists(testdir)) {			
				res <- dlgMessage(paste("Temporary validation directory already",
					"exists. Do we erase old data there?"), type = "okcancel")$res
				if (res == "cancel")
					stop("testdir (", testdir, ") must not exist yet!")
				unlink(testdir, recursive = TRUE)
			}	
			dir.create(testdir, recursive = TRUE)
			if (!file.exists(testdir))
				stop("cannot create 'testdir'!")
			testdir <<- normalizePath(testdir)
			## Put required files there: create the planktonSorter directory
			plSort <- file.path(testdir, "planktonSorter")
			dir.create(plSort)
			plFiles <- dir(system.file("planktonSorter", package = "zooimage"),
				full.names = TRUE)
			res <- file.copy(plFiles, file.path(plSort, basename(plFiles)))
			if (any(!res))
				stop("Problem when copying one or more plankton sorter files")
		}
		
		## In the other modes, indicate that we prepare the validation environment
		cat("Preparing a validation environment...\n")
		
		## Be sure that data is correctly ordered
		data <<- data[order(data$Item), ]
		## Be sure 'Id' exists in data
		data$Id <- makeId(data)

		## Make sure items' membership is predicted by the classifier
		predicted <- data$Predicted
		## TODO: shouldn't we do this all the time???
		if (is.null(predicted)) # Predict class if it wasn't done yet
			predicted <- predict(classifier, data, class.only = TRUE,
				type = "class")
		predicted <<- predicted

#		if (mode != "validation") {
			## Store validated items and table of validated items
			validated <<- data$Class 
			## TODO: why this, and Class put in validated?
			data$Class <- NULL
			data <<- data
#		}
		## Keep all data
###		data <<- attr(classifier, "calc.vars")(data)
###		## Do not keep uncomplete attributes
###		data <<- data[, sapply(data, FUN = function(x) !any(is.na(x)))] 
		proba <<- classProba(data, predicted, classifier, diff.max = diff.max,
			prop.bio = prop.bio)
		nobs <- nrow(data)
		error <- factor(rep("Error", nobs), levels = c("Correct", "Error"))
		## Compute the second possible identification
		secondIdent <- function (groups) {
			proba.table <- attr(proba, "IdentParam")$ProbaTable
			proba.order <- apply(proba.table, 1, order, decreasing = TRUE)
		
			fst.ids <- proba.order[1, ]
			scd.ids <- proba.order[2, ]
		
			proba.max <- apply(proba.table, 1, sort, decreasing = TRUE)[1, ]
			max.ids <- proba.max == 1
			scd.ids[max.ids] <- fst.ids[max.ids]
		
			factor(groups[scd.ids], levels = groups)
		}
		predicted2 <- secondIdent(levels(predicted))
		
		predTable <- table(predicted)
		prop <- predTable / sum(predTable)
		classRare <<- names(which(prop < rare))
		
		## Creation of corr object
		corr <<- data.frame(Actual = predicted, Actual2 = predicted2,
			Predicted = predicted, Predicted2 = predicted2, Validated = FALSE,
			Error = error, Step = step, Suspect = rep(TRUE, nobs),
			Rare = predicted %in% classRare, RdValidated = rep(Inf, nobs),
			OtherGp = rep(FALSE, nobs))
    
		## Confusion matrix of original classifier
		train.validated <- attr(classifier, "response")
		train.predicted <- attr(classifier, "cvpredict")
		train.conf <- confusion(x = train.predicted, y = train.validated)
		
		## First error correction: we start with Solow et al. estimation
		## Compute correction of abundances using a method similar to Solow et al.,
		## but that is not stuck with singular matrices problems
		## TODO: this takes a long time => try Solow et al first before using this!
		correctionLab <- function (x, conf) {
			l <- length(x)
			if (l != ncol(conf))
				stop("'x' has not the same length than 'conf'")
			
			toMat <- function(x, byrow = TRUE)
				matrix(rep(x, l), ncol = l, byrow = byrow)
			
			predMat <- toMat(colSums(conf))
			xMat <- toMat(x)
			
			## Remove 0
			notZero <- function (x) {
				if (0 %in% x) x[x==0] <- 1
				x
			}
			
			confB <- conf / notZero(predMat) * xMat
			x2 <- rowSums(confB)
			classMat <- toMat(rowSums(conf), byrow = FALSE)
			while (!isTRUE(all.equal(x, x2, tolerance = 0.00001))) {
				x <- x2
				confA <- conf / notZero(classMat) * toMat(x2, byrow = FALSE)
				xMat2 <- toMat(colSums(confA))
				confB <- confA / notZero(xMat2) * xMat
				x2 <- rowSums(confB)
			}
			round(x2)
		}
		tablePredicted <- table(predicted)
		correction.history <<- correctionLab(x = tablePredicted,
			conf = train.conf)
		manual.history <<- tablePredicted
		manual2.history <<- manual.history
	
		## Increment step (should be 0 now, because initial value is -1)
		step <<- step + 1
		
		## Determine the number of vignettes to manually validate
		setSampleSize()
	}

	## Compute the size of the next subsample: update sample.size
	setSampleSize <- function () {
		## Number of items we want to take
		sample.size <<- ceiling(nrow(data) * fraction)
		## No less than sample.min
		sample.size <<- max(sample.size, sample.min)
		## According to complexity of the training set, take possibly more
		sample.size <<- max(sample.size, grp.min * length(levels(predicted)))
		## ... but no more than sample.max
		sample.size <<- min(sample.size, sample.max)
		## Or how much remains?
		sample.size <<- min(sample.size, nrow(corr[!corr$Validated, ]))
	}	
	
	## Determine the subsample to validate
	## Update Step and RdValidated (used for error estimation)
	## Automatically place vignettes in directories for manual validation
	prepareTest <- function () {
		nobs <- nrow(data)
		if (step < 1) {
			## At first time, take a random subsample
			## Same as considering everything as suspect
#PhG			nsuspect <<- nobs
#PhG			ntrusted <<- 0
			sample.ids <- sample(1:nobs, size = sample.size)
			corr$Step[sample.ids] <<- step
			corr$RdValidated[sample.ids] <<- step
			nsuspect.tovalid.history <<- c(nsuspect.tovalid.history, sample.size)
			ntrusted.tovalid.history <<- c(ntrusted.tovalid.history, 0)
			nsuspect.history <<- c(nsuspect.history, nobs)
			ntrusted.history <<- c(ntrusted.history, 0)
		} else { # Step > 0
			## Mix trusted and suspect particles
			#validated <- corr[corr$Validated, ]
			notvalidated <- corr[!corr$Validated, ]
			nsuspect <<- sum(notvalidated$Suspect)	#nrow(notvalidated[notvalidated$Suspect, ])
			ntrusted <<- nrow(notvalidated) - nsuspect	#nrow(notvalidated[!notvalidated$Suspect, ])
			## Determine the number of random items used in error estimation
			numRandom  <- max(sample.size - nsuspect,
				round(random.sample * sample.size))
			ids <- as.character(1:nobs)
			## All items not included in RdValidated
			tosample.ids <- ids[is.infinite(corr$RdValidated)]
			## Items to validate
			randomsample.ids <- as.numeric(sample(tosample.ids,
				size = min(numRandom, length(tosample.ids))))
			## Used to estimate error at step i
			corr$RdValidated[randomsample.ids] <<- step
			newstep <- corr$Step[randomsample.ids]
			## Except those already validated and used for error estimation
			newstep[newstep == -1] <- step
			corr$Step[randomsample.ids] <<- newstep
			notvalid.ids <- ids[!corr$Validated & corr$RdValidated == step]
			## Number of items to valid in order to achieve sample.size
			numSample <- sample.size - length(notvalid.ids) 
			if (numSample > 0) {
				## Randomly select suspect items not validated
				suspnotval.ids <- ids[!corr$Validated & corr$Suspect &
					is.infinite(corr$RdValidated) & corr$Step == -1]
				if (length(suspnotval.ids)) {
					suspsample.ids <- as.numeric(sample(suspnotval.ids,
						size = min(numSample, length(suspnotval.ids))))
					corr$Step[suspsample.ids] <<- step
					numSample <- numSample - length(suspsample.ids)        
				}
				if (numSample > 0) {
					## Randomly select trusted items not validated
					trustnotval.ids <- ids[!corr$Validated & !corr$Suspect  &
						is.infinite(corr$RdValidated) & corr$Step == -1]
					if (length(trustnotval.ids)) {
						trustsample.ids <- as.numeric(sample(trustnotval.ids,
							size = min(numSample, length(trustnotval.ids))))
						corr$Step[trustsample.ids] <<- step
					}
				}
			}
					            
      ############### stratified random sampling ############### 
#       if (numSample > 0) {
#     		## Select the same number of suspect items not validated in each class
#   			suspnotval.ids <- ids[!corr$Validated & corr$Suspect &
#   			  is.infinite(corr$RdValidated) & corr$Step == -1]
#   		  if (length(suspnotval.ids)) {
#   		    splitGp <- split(suspnotval.ids, list(corr[suspnotval.ids,]$Predicted))
#   		    strat.samples <- lapply(splitGp, function(x) x[sample(1:NROW(x), 
#   		                      min(NROW(x), round(numSample/length(unique(corr$Predicted[as.numeric(suspnotval.ids)])))), 
#   		                      replace = FALSE)])
#   		    suspsample.ids <- as.numeric(do.call(c, strat.samples))
#   			  corr$Step[suspsample.ids] <<- step
#   			  numSample <- numSample - length(suspsample.ids)        
#   			}
#         
#         if (numSample > 0) {
#           ## If not completed, randomly select suspects items not validated
#           suspnotval.ids <- ids[!corr$Validated & corr$Suspect &
#                                   is.infinite(corr$RdValidated) & corr$Step == -1]
#           if (length(suspnotval.ids)) {
#             suspsample.ids <- as.numeric(sample(suspnotval.ids,
#                        size = min(numSample, length(suspnotval.ids))))
#             corr$Step[suspsample.ids] <<- step
#             numSample <- numSample - length(suspsample.ids)        
#           }
#         }
#         
#   			if (numSample > 0) {
#   			  ## If not completed, Select the same number of trusted items not validated in each class
#   			  trustnotval.ids <- ids[!corr$Validated & !corr$Suspect  &
#   			    is.infinite(corr$RdValidated) & corr$Step == -1]
#   			  if (length(trustnotval.ids)) {
#   			    splitGp <- split(trustnotval.ids, list(corr[trustnotval.ids,]$Predicted))
#   			    strat.samples <- lapply(splitGp, function(x) x[sample(1:NROW(x), 
#   			                          min(NROW(x), round(numSample/length(unique(corr$Predicted[as.numeric(trustnotval.ids)])))), 
#   			                          replace = FALSE)])
#   			    trustsample.ids <- as.numeric(do.call(c, strat.samples))
#   			    corr$Step[trustsample.ids] <<- step
#   			    numSample <- numSample - length(trustsample.ids)
#   			  }
#   			}
#         
#   			if (numSample > 0) {
#           ## If not completed, randomly select trusted items not validated
#   			  trustnotval.ids <- ids[!corr$Validated & !corr$Suspect &
#   			                          is.infinite(corr$RdValidated) & corr$Step == -1]
#   			  if (length(trustnotval.ids)) {
#   			    trustsample.ids <- as.numeric(sample(trustnotval.ids,
#   			                                        size = min(numSample, length(trustnotval.ids))))
#   			    corr$Step[trustsample.ids] <<- step
#   			    numSample <- numSample - length(trustsample.ids)        
#   			  }
#   			}
      ############### ############### ###############
			
			nsuspect.tovalid <- length(ids[corr$Step == step & corr$Suspect])
			ntrusted.tovalid <- length(ids[corr$Step == step & !corr$Suspect])
			nsuspect.history <<- c(nsuspect.history, nsuspect)
			ntrusted.history <<- c(ntrusted.history, ntrusted)
			nsuspect.tovalid.history <<- c(nsuspect.tovalid.history, nsuspect.tovalid)
			ntrusted.tovalid.history <<- c(ntrusted.tovalid.history, ntrusted.tovalid)
		}
		
		if (mode != "stat") {
			## Make sure the R Httpd server is started
			tools <- getNamespace("tools")
			if (R.Version()$`svn rev` >= 67550) {
				port <- tools::startDynamicHelp(NA)
			} else {
				port <- tools$httpdPort
			}
			if (port == 0) port <- startDynamicHelp(TRUE)
			if (port == 0) stop("Impossible to start the R httpd server")
			
			subdir <- paste0("step", step + 1) # Because it start at 0,
			## but this is really the beginning of next step now
			stepdir <- file.path(testdir, subdir)
			dir.create(stepdir)			
			Zidb <- zidbLink(zidb)
			## Write data in test directory
			keepRows <- corr$Step == step
			Vigns <- data[keepRows, "Id"]
			imgext <- Zidb[[".ImageType"]]
			## Extract all these images to stepdir
			vigpath <- file.path(stepdir, paste(Vigns, imgext, sep = "."))
			names(vigpath) <- Vigns
			if (length(Vigns))
				for (j in 1:length(Vigns))
					writeBin(Zidb[[Vigns[j]]], vigpath[j])			
			
			## Create all directories of groups
			path <- attr(classifier, "path")
			names(path) <- basename(path)
			## Create planktonSorter.html file
			vigfiles <- basename(vigpath)
			pred <- as.character(corr[keepRows, "Predicted"])
			names(vigfiles) <- pred
			Sample <- as.character(sub("\\+.+_[0-9]+", "", Vigns[1]))
			
			if (step > 0) {
				## Recreate previous page with sorter items
				oldPage <- file.path(testdir, paste0("step", step),
					"planktonSorter.html")
				oldItems <- corr$Step == (step - 1)
				oldVigns <- paste(data[oldItems, "Id"], imgext, sep = ".")
				oldNames <- as.character(corr[oldItems, "Actual"])
				oldNames[is.na(oldNames)] <- "[other]"
				names(oldVigns) <- oldNames
				res <- planktonSorterPage(groups = path, vigns = oldVigns,
					title = sorter.title, id = sorter.id,
					step = step, file = oldPage)
			}
			
			## Create new page
			plSorterPage <- file.path(stepdir, "planktonSorter.html")
			sorter.title <<- paste0(Sample, "/Step", step + 1)
			sorter.id <<- paste(id, step + 1, sep = "/")
			res <- planktonSorterPage(groups = path, vigns = vigfiles,
				title = sorter.title, id = sorter.id,
				step = step + 1, file = plSorterPage)
			cat(paste("You have to validate vignettes in", subdir,
				"directory", "before next iteration...\n"))
			browseURL(paste0("file://", plSorterPage, "?v=", round(runif(1, max = 100000))))
		}
		testset.validated <<- FALSE
	}
  
	## Retrieve test set (validated or not)
	getTest <- function ()
		testset <<- corr[corr$Step == step, ]

	## Automatic validation of the particles (only in demo and stat mode)
	# Read manual validation on hardrive
	# Update corr$Actual column
	validate <- function () {
		if (mode == "stat") {
			corr[corr$Step == step, ]$Actual <<- validated[corr$Step == step]
		} else {
## TODO: change this!
#			## Read Test set with getTest() from zooimage package
#			SubDir <- file.path(testdir, paste0("step", step))
#			TestSetStep <- getTest(SubDir)[, c("Id", "Class")]
			## Id if items validated at step 'i' and included in data
## ????
#			dfId <- data.frame(Id = as.factor(rownames(data))[corr$Step == step])
#			## Reorder TestSetStep to replace values in corr object
#			test2 <- merge(dfId, TestSetStep, by = "Id", sort = FALSE)
#			## Be sure that names correspond
#			if (!isTRUE(all(test2$Id == dfId$Id)))
#				stop("'Id' in manual validation doesn't correspond with 'Id' ",
#					"from dataset used in error correction.")
			if (mode == "validation") {
				if (is.null(validation.data)) {
					return()
				} else {
					stepVal <- as.numeric(validation.data[1, 2]) - 1
					if (is.na(stepVal) || !length(stepVal) || stepVal < 0)
						stop("Wrong validation data (step = ", stepVal, ")")
					grps <- validation.data[-1, 1]
					## Transform [other] into NA
					grps[grps == "[other]"] <- NA
					testStep <- data.frame(Id = validation.data[-1, 2],
						Class = factor(grps, levels = levels(corr$Actual)))
					validation.data <<- NULL
					dfId <- data.frame(Id = makeId(data)[corr$Step == stepVal])
					test2 <- merge(dfId, testStep, by = "Id", sort = FALSE)
					corr[corr$Step == stepVal, ]$Actual <<- test2$Class
					step <<- stepVal
					
					## TODO: save these results also in the zidb file!
					
				}
			} else {
				corr[corr$Step == step, ]$Actual <<- validated[corr$Step == step]
			}
		}
		## Update column for vignettes impossible to identify or that belong to
		## other group than those included in the classifier
		corr$OtherGp <<- is.na(corr$Actual)
		testset.validated <<- TRUE
	}

	## Estimate global error in the sample based on RdValidated colum
	estimateError <- function () {		
		## At first step, use all validated particles to estimate the error 
		if (step == 0) {
			error.estim.data <<- corr[corr$Step == step, ]
			## Manage NAs
			error.estim <<- sum(error.estim.data$Actual[!error.estim.data$OtherGp] !=
				error.estim.data$Predicted[!error.estim.data$OtherGp]) /
				(nrow(error.estim.data) - sum(error.estim.data$OtherGp))
			error.estim.rd <<- error.estim
			error.estim.history <<- error.estim
			error.estim.rd.history <<- error.estim.history
		} else { # data used previously + a portion of the validated test set
			## All error at step i
			Error <- corr$Actual != corr$Predicted
			## Calculation of the global error
			error.estim <<- .globalError(suspect = corr$Suspect[!corr$OtherGp],
				error = Error[!corr$OtherGp],
				subset = corr$Validated[!corr$OtherGp], na.rm = TRUE)
			error.estim.history <<- c(error.estim.history, error.estim)
			error.estim.rd.history <<- c(error.estim.rd.history, error.estim.rd)
		}
		
		## Error in the different fractions
#PhG		if (mode != "validation") {
		if (mode == "stat") {
			error <- validated != corr$Predicted
			errInFract <- .errorInFractions(suspect = corr$Suspect,
				error = error, validated = corr$Validated,
				iteration = (corr$Step == step), na.rm = TRUE)
			errInFract.history <<- rbind(errInFract.history, errInFract)
		}
	}
	
	## Compute the weighted sum of validated suspect and trusted particles
	## Confusion matrix to estimate the abundance
	estimateAbundance <- function () {
		## At the first step all particles are considered suspect
		if (step == 0) {
			error.conf <- confusion(error.estim.data$Predicted,
				error.estim.data$Actual, useNA = "no") # remove NAs
			corr.confusion <<- error.conf / sum(error.conf) *
				(nrow(data) - sum(corr$OtherGp)) # remove NAs
				
			## For cells
			if ("Nb_cells" %in% names(data)) {
				error.conf.cell <- xtabs(data$Nb_cells[corr$Step==step] ~
                    error.estim.data$Actual + error.estim.data$Predicted,
					exclude = c(NA, NaN))
				cell.confusion <<- error.conf.cell /
					sum(error.conf.cell) * (sum(data$Nb_cells) -
					sum(data$Nb_cells[corr$OtherGp])) # remove NAs
			}

			## For biovolumes
			if ("BioWeight" %in% names(data)) {
				error.conf.bioweight <- xtabs(data$BioWeight[corr$Step==step] ~
			        error.estim.data$Actual + error.estim.data$Predicted,
					exclude = c(NA, NaN))
				bioweight.confusion <<- error.conf.bioweight /
					sum(error.conf.bioweight) * (sum(data$BioWeight) -
					sum(data$BioWeight[corr$OtherGp])) # remove NAs
			}	
				
			## Calculate error in valid data and in both suspect and trusted parts
			error.valid.history[[step + 1]] <<-
				error.estim.data$Actual != error.estim.data$Predicted 
			error.suspect.history[[step + 1]] <<-
				error.estim.data$Actual != error.estim.data$Predicted
			error.trusted.history[[step + 1]] <<- rep(FALSE, sum(error.conf))
		} else {
			## Confusion matrix suspect vs trusted
			nSuspTot <- sum(corr$Suspect & !corr$OtherGp)
			valSuspIdx <- corr$Validated & !corr$OtherGp & corr$Suspect
			valTrustIdx <- corr$Validated & !corr$OtherGp & !corr$Suspect
			nSuspVal <- sum(valSuspIdx)
			nTrustVal <- sum(valTrustIdx)
			confSuspVal <- confusion(corr$Predicted[valSuspIdx],
				corr$Actual[valSuspIdx]) 
			confTrustVal <- confusion(corr$Predicted[valTrustIdx],
				corr$Actual[valTrustIdx])
			confSusp.w <- confSuspVal / nSuspVal * nSuspTot  

			notValTrustIdx <- !corr$Validated & !corr$Suspect
			confNotValTrust <- confusion(corr$Predicted[notValTrustIdx],
				corr$Actual[notValTrustIdx]) 
			corr.confusion <<- confSusp.w + confTrustVal + confNotValTrust

			## For cells
			if ("Nb_cells" %in% names(data)) {
				nCellSuspTot <- sum(data$Nb_cells[corr$Suspect &
					!corr$OtherGp])
				nCellSuspVal <- sum(data$Nb_cells[valSuspIdx])
				nCellTrustVal <- sum(data$Nb_cells[valTrustIdx])
				confSuspValCell <- xtabs(data$Nb_cells[valSuspIdx] ~
			        corr$Actual[valSuspIdx] + corr$Predicted[valSuspIdx],
					exclude = c(NA, NaN))
				confTrustValCell <- xtabs(data$Nb_cells[valTrustIdx] ~
			        corr$Actual[valTrustIdx] + corr$Predicted[valTrustIdx],
					exclude = c(NA, NaN))
				confSuspCell.w <- confSuspValCell / nCellSuspVal * nCellSuspTot
				confNotValTrustCell <- xtabs(data$Nb_cells[notValTrustIdx] ~
			        corr$Actual[notValTrustIdx] + corr$Predicted[notValTrustIdx],
					exclude = c(NA, NaN))
				cell.confusion <<-
					confSuspCell.w + confTrustValCell + confNotValTrustCell
			}

			## For biovolumes
			if ("BioWeight" %in% names(data)) {
				nBioSuspTot <- sum(data$BioWeight[corr$Suspect & !corr$OtherGp])
				nBioSuspVal <- sum(data$BioWeight[valSuspIdx])
				nBioTrustVal <- sum(data$BioWeight[valTrustIdx])
				confSuspValBio <- xtabs(data$BioWeight[valSuspIdx] ~
			        corr$Actual[valSuspIdx] + corr$Predicted[valSuspIdx],
					exclude = c(NA, NaN))
				confTrustValBio <- xtabs(data$BioWeight[valTrustIdx] ~
			        corr$Actual[valTrustIdx] + corr$Predicted[valTrustIdx],
					exclude = c(NA, NaN))
				confSuspBio.w <- confSuspValBio / nBioSuspVal * nBioSuspTot
				confNotValTrustBio <- xtabs(data$BioWeight[notValTrustIdx] ~
					corr$Actual[notValTrustIdx] + corr$Predicted[notValTrustIdx],
					exclude = c(NA, NaN))
				bioweight.confusion <<-
					confSuspBio.w + confTrustValBio + confNotValTrustBio
			}
			
			error.valid.history[[step + 1]] <<- testset$Actual != testset$Predicted 
			if  (nsuspect > 0) {
				error.suspect.history[[step + 1]] <<-
					testset[testset$Suspect, ]$Actual !=
					testset[testset$Suspect, ]$Predicted
			}
			if (ntrusted > 0) {
				error.trusted.history[[step + 1]] <<-
					testset[!testset$Suspect, ]$Actual !=
					testset[!testset$Suspect,]$Predicted
			}
		}
	}

	## Compute the corrected coefficients for particles, cells, biovolume
#   estimateCoeffs <- function () {
#     ## For particles (colonies)
#     col.confusion <- table(corr$Predicted[corr$Validated], corr$Actual[corr$Validated], useNA = "no") # remove NAs
#     corr.coeffs <- ifelse(!colSums(col.confusion), rowSums(col.confusion), 
#                           rowSums(col.confusion)/colSums(col.confusion))
#     ## For cells
#     if ("Nb_cells" %in% names(data)) {
#       cell.confusion <- xtabs(data$Nb_cells[corr$Validated] ~ 
#                                 corr$Predicted[corr$Validated] + 
#                                 corr$Actual[corr$Validated], exclude = c(NA, NaN))
#       corr.coeffs <- cbind(corr.coeffs, ifelse(!colSums(cell.confusion), rowSums(cell.confusion), 
#                                              rowSums(cell.confusion)/colSums(cell.confusion)))
#     }
#
#     ## For biovolumes
#     if ("BioWeight" %in% names(data)) {
#       bioweight.confusion <- xtabs(data$BioWeight[corr$Validated] ~ 
#                                      corr$Predicted[corr$Validated] + 
#                                      corr$Actual[corr$Validated], exclude = c(NA, NaN))
#       corr.coeffs <- cbind(corr.coeffs, ifelse(!colSums(bioweight.confusion), rowSums(bioweight.confusion), 
#                                              rowSums(bioweight.confusion)/colSums(bioweight.confusion)))
#     }
#     corr.coeffs
#   }
	
	## Estimate error and abundance
	## Update Validated, training set and histories
	correct <- function () {
		getTest()
		
		curr.validated.ids <- corr$Step == step
		corr$Validated[curr.validated.ids] <<- TRUE
		corr$Error[corr$Validated & (corr$Actual == corr$Predicted)] <<- "Correct"
		corr$Actual2 <<- corr$Actual
		
		if (step > 0) {
			ids <- !corr$Validated & corr$Suspect
			corr$Actual2[ids] <<- corr$Predicted2[ids]
		}

		estimateError()
		estimateAbundance()
		#estimateCoeffs()
		
		validated.fracs <<- c(validated.fracs, sample.size)
		correction.history <<- cbind(correction.history,
			rowSums(corr.confusion))
		if ("Nb_cells" %in% names(data)) {
			correctionCell.history <<- cbind(correctionCell.history,
		        rowSums(cell.confusion))
		}
		if ("BioWeight" %in% names(data)) {
			correctionBio.history <<- cbind(correctionBio.history,
		        rowSums(bioweight.confusion))
		}
		
		manual.history <<- cbind(manual.history, table(corr$Actual))
		manual2.history <<- cbind(manual2.history, table(corr$Actual2))
		setSampleSize() # Set the next subsample size
	}

	process <- function () {
		if (!step.manual) {
			if (step > 0) {
				susp <- suspect(attr(classifier, "calc.vars")(data), proba, error = corr$Error,
					subset = corr$Validated, predicted = predicted,
					confusion = corr.confusion, algorithm = algorithm)
				## Update Suspect column
				corr$Suspect <<- susp == "Error"
				## Keep a track of the detection to evaluate performance
				suspect.history[[step]] <<- susp 
				validated.history[[step]] <<- corr$Validated
			}
			prepareTest()
			step.manual <<- TRUE
		} else if (testset.validated) {
			step.manual <<- FALSE
if (mode == "stat") {
	getTest()
	correct()
}
			cat(paste("Step", step + 1, "finished \n"))
			step <<- step + 1
		} else warning("You have to complete the validation first \n")
		flush.console()
	}

	## TODO: process called twice, and it seems that the step.manual flag is
	##       indeed triggering something else each time... This is *crazy*
	##       Do change this!!!
	processValidation <- function () {
		if (sample.size > 0) {
			#if (!isTRUE(step.manual)) {
			#	process()
			#} else {
			#	#validate()
			#	process()
			#}
			process()
		} else cat("Correction done!\n")
	}

	processDemo <- function () {
		if (sample.size > 0) {
#PhG			process()
#PhG			validate()
			process()
		} else cat("Correction done!\n")
	}

	processStats <- function () {
		repeat {
			if (sample.size > 0) {
				process()
				validate()
				process()
			} else {
				cat("Correction done!\n")
				break
			}
		}
	}

	## Accessors
	iterate <- function () {
		if (step < 0) initialize()
		switch(mode,
			validation = processValidation(),
			demo = processDemo(),
			stat = processStats(),
			stop("'mode' must be 'validation', 'demo', or 'stat'"))
	}
	
	setValidation <- function (validation.data = NULL) {
		validation.data <<- validation.data
		validate()
		correct()
		
		## Recreate the page with current sorting by default			
		Zidb <- zidbLink(zidb)
		imgext <- Zidb[[".ImageType"]]
		## Create all directories of groups
		path <- attr(classifier, "path")
		names(path) <- basename(path)
		## Recreate previous page with sorter items
		oldPage <- file.path(testdir, paste0("step", step + 1),
			"planktonSorter.html")
		oldItems <- corr$Step == step
		oldVigns <- paste(data[oldItems, "Id"], imgext, sep = ".")
		oldNames <- as.character(corr[oldItems, "Actual"])
		oldNames[is.na(oldNames)] <- "[other]"
		names(oldVigns) <- oldNames
		res <- planktonSorterPage(groups = path, vigns = oldVigns,
			title = sorter.title, id = sorter.id,
			step = step + 1, file = oldPage)
		
		## Create the diagnostic graphs
		reportdir <- file.path(testdir, paste0("step", step + 1))
		reportplot <- file.path(reportdir, "ReportError.png")
		unlink(reportplot)
		png(reportplot, width = 800, height = 500)
		plotResult("b")
		dev.off()
		## Create a page displaying the current state of correction process
		reportfile <- file.path(reportdir, "planktonSorterResults.html")
		res <- planktonSorterReport(title = sorter.title, id = sorter.id,
			step = step + 1, file = reportfile)
		## This is a trick to avoid using cached version in memory!
		browseURL(paste0("file://", reportfile, "?v=", round(runif(1, max = 100000))))
		process()
		return(reportfile)
	}
	
	finish <- function () {
		cat("Validation done...\n")
		abd <-  getAbundance()
		print(abd)
		
		## Create an object with these results...
		test <- data.frame(Id = makeId(data), data, Class = corr$Actual, Validated = corr$Validated, Suspect = corr$Suspect)
		#test <- data.frame(Id = makeId(data), data, Class = corr$Actual)
		attr(test, "path") <- attr(classifier, "path")
		class(test) <- unique(c("ZI3Test", "ZITest", class(data)))
		assign(result, test, envir = envir)
		cat("Object `", result, "` created with validation results!\n", sep = "")
		
		## Erase the temporary directory on disk...
		unlink(testdir, recursive = TRUE)
		
		## Return abundances in this file
		abd
	}

	## Return the estimated abundance
	getAbundance <- function ()
		rowSums(corr.confusion)

	## Return the estimated error 
	getErrorEstimation <- function () 
		error.estim

	## Evaluate the quality of the classifier for the suspects
	evaluateDetection <- function () {
		if (!length(suspect.history)) return(NULL)
			
		error <- factor(rep("Error", nrow(data)), levels = c("Correct", "Error"))
		error[validated == corr$Predicted] <- "Correct"
		conf <- list()
		for (i in 1:length(suspect.history)) {
			validated <- validated.history[[i]]
			conf[[paste0("Step", i)]] <-
				summary(confusion(x = error[!validated],
				y = as.factor(suspect.history[[i]][!validated])))
		}
		list(error = error, confusion = conf)
	}

	## Compare graphically the result with manual validation
	## and manual validation + second identification
	plotResult <- function (type = c("dissimilary", "partition", "barplot")) {
		#if (mode == "validation")
		#	stop("This function is not available in validation mode.")
		
		type <- match.arg(type[1], c("dissimilarity", "partition", "barplot"))
		
		if (type == "dissimilarity") {
			if (is.null(validated)) {
				## In case we are in validation mode, we don't have this!...
				## We start from original prediction
				abundances <- as.vector(table(predicted[!corr$OtherGp]))
			} else {
				abundances <- as.vector(table(validated[!corr$OtherGp]))
			}
			error1 <- dissimilarity(abundances, manual.history, na.rm = TRUE) * 100
			error3 <- dissimilarity(abundances, correction.history, na.rm = TRUE) * 100
			par(mar = c(5, 4, 4, 4) + 0.1)
			plot(cumsum(validated.fracs) / nrow(corr) * 100, error1,
				type = "l", xlab = "Validated fraction (%)",
				ylab = "Dissimilarity (%)", col = "green", xlim = c(0, 100),
				ylim = c(0, max(error1, error3) * 1.02),
				main = "Dissimilarity at each iteration")
			## Baseline for manual validation
			lines(c(0, 100), c(error1[1], 0), col = 1, lty = 2)
			## Line for correction
			lines(cumsum(validated.fracs) / nrow(corr) * 100, error3, col = "red")
			grid()
			legend("topright", legend = c("Random validation",
				"Suspects validation", "Error correction"),
				col = c("black", "green","red"), lty = c(2, 1, 1))
		
		} else if (type == "partition") {
			fracs <- cumsum(validated.fracs[-1]) / nrow(corr)
			errByFrac <- sapply(error.valid.history, sum, na.rm = TRUE) /
				validated.fracs[-1]
			suspByFrac <- nsuspect.tovalid.history / validated.fracs[-1]
			suspByFrac[1] <- 0
			par(mar = c(5, 4, 4, 4) + 0.1)
			plot(fracs * 100, errByFrac * 100, type = "l", xlab = "Validated fraction (%)",
				ylab = "Suspect and error (%)", xlim = c(0, 100), ylim = c(0, 100), col = "red",
				main = "Suspects and error at each iteration")
			lines(fracs * 100, suspByFrac * 100, col = "black")
			grid()
			legend("topright", legend = c("Suspect", "Error"),
				col = c("black", "red"), cex = 0.8, lwd = 2)
		
		} else { # Should be type == "barplot"
			thresholdDiffDiss <- 5  # Differential dissimilarity <= 5%
			nbStep <- ceiling(nrow(data) / validated.fracs[-1][1])
			errByFrac <- sapply(error.valid.history, sum, na.rm = TRUE) /
				validated.fracs[-1]
			suspByFrac <- nsuspect.tovalid.history / validated.fracs[-1]
			#suspByFrac[1] <- 0
			## case 1 item => projection, case more => another projection...
			dat <- rbind(suspByFrac * 100, errByFrac * 100)
			
			diffDiss <- sapply(2:ncol(correction.history), function (x)
				dissimilarity(correction.history[, x - 1], correction.history[, x],
				na.rm = TRUE) * 100
			)
			xcoord <-
				seq(0.7, ceiling(nrow(data) / validated.fracs[-1][1]) * 1.2, by = 1.2)
			if (step < 1) {
				suspRemain <- NA
				stepSD <- round((errByFrac*nsuspect.history -
					errByFrac*nsuspect.tovalid.history) / 
                    nsuspect.tovalid.history) + (step+1)
				idxStepSD <- stepSD
				coordStepSD <- mean(c(xcoord[idxStepSD], xcoord[idxStepSD + 1]))
			} else {
				suspRemain <- c(NA, nsuspect.history[2:(step+1)] -
					nsuspect.tovalid.history[2:(step+1)])
				stepSD <- round(suspRemain / nsuspect.tovalid.history) + 1:(step+1)
				if (length(which(suspRemain == 0)) > 0) {
					idxStepSD <- which(suspRemain == 0)[1]
				} else {
					idxStepSD <- tail(stepSD,1)
				}
				coordStepSD <- mean(c(xcoord[idxStepSD], xcoord[idxStepSD + 1]))
			}
      		      
			par(mfrow = c(2, 1), mar = c(4, 4, 1, 4) + 0.1)
			bp1 <- barplot(suspRemain, #xlab = "Validation step",
			    ylab = "Nb remaining suspects", xlim = c(0.2,
				xcoord[ceiling(idxStepSD + (length(xcoord) - idxStepSD) / 3)]),
			    ylim = c(0, max(suspRemain, diffDiss, na.rm = TRUE)), yaxs = "r",
                col = "grey10", cex.axis = .7, cex.main = 1, ann = FALSE,
				yaxt = "n", #main = "Remaining suspects and differential dissimilarity")
			)
			title(expression(bold("Remaining suspects") *
				phantom("\tand\tdifferential dissimilarity")), 
				col.main = "grey10", cex.main = 1)
			title(expression(phantom("Remaining suspects\t") * "and" *
				phantom("\tdifferential dissimilarity")), 
				col.main = "black", cex.main = 1)
			title(expression(phantom("Remaining suspects\tand\t") *
				bold("differential dissimilarity")), 
				col.main = "blue", cex.main = 1)
# 			legend("top", legend = c("Remaining suspects","Diff dissimilarity"),
# 			     fill = c("grey20","blue"), cex = .6, bty = "n", adj = c(0,0))
			axis(side = 1, at = seq(bp1[1], by = 1.2, length.out = nbStep),
				labels = 1:nbStep, cex.axis = .7)
			if (step > 0) axis(side = 2, cex.axis = .7)
      
			par(new = TRUE)
			plot(bp1, diffDiss, type = "o", col = "blue", ylim = c(0, 100), 
				xlim = c(0.2, xcoord[ceiling(idxStepSD + (length(xcoord) -
				## TODO: why '+' at the end of next line???
				idxStepSD) / 3)]), lwd = 3, axes = FALSE, ann = FALSE) +
			## TODO: why '+' at the end of next line???
			axis(side = 4, col = "blue", col.axis = "blue", cex.axis = .7) + 
			mtext("Differential dissimilarity (%)", side = 4, line = 3,
				col = "blue")
			abline(v = coordStepSD, lwd = 2, lty = 2, col = "dimgrey")
			text(x = coordStepSD + .5, y = 90, "SD", srt = -90, pos = 4,
				cex = 0.6, col = "dimgrey")
			if (length(which(diffDiss < thresholdDiffDiss)) > 0) {
				coordStepEC <- mean(c(xcoord[which(diffDiss < thresholdDiffDiss)[1]],
					xcoord[which(diffDiss < thresholdDiffDiss)[1]+1]))
				abline(v = coordStepEC, lwd = 2, lty = 2, col = "darkgoldenrod")
				text(x = coordStepEC + .5, y = 90, "EC", srt = -90, pos = 4,
					cex = 0.6, col = "darkgoldenrod")
			}
			grid()
			box()
			
			bp2 <- barplot(dat, xlab = "Validated step", beside = TRUE,
				ylab = "Suspect and corrected error (%)",
				xlim = c(0.2, xcoord[ceiling(idxStepSD + (length(xcoord) -
					idxStepSD) / 3)]),
				ylim = c(0, 100), col = c("#dddddd", "#dd0000"),
				cex.axis = .7, cex.main = 1, width = .5, space = c(0, .4),
				#main = "Suspects and error corrected at each iteration")
			)
			title(expression(bold("Nbr of suspects") *
				phantom("\tand\tcorrected error")),
				col.main = "gray40", cex.main = 1)
			title(expression(phantom("Nbr of suspects\t") * "and" *
				phantom("\tcorrected error")), 
				col.main = "black", cex.main = 1)
			title(expression(phantom("Nbr of suspects\tand\t") *
				bold("corrected error")), 
				col.main = "#dd0000", cex.main = 1)
# 			legend("topright", legend = c("Nbr of suspect", "Corrected error"),
# 			    fill = c("#dddddd", "#dd0000"), cex = .6, bty = "n", adj = c(0, 0))
			axis(side = 1, at = seq(mean(bp2[, 1]), by = 1.2, length.out = nbStep),
				labels = 1:nbStep, cex.axis = .7)
			axis(side = 4, cex.axis = .7)
			grid()
			box()
		}
	}
	
	getEnv <- function ()
		environment(getEnv)

	list(iterate = iterate, validate = setValidation, done = finish, abundance = getAbundance,
		error = getErrorEstimation, evaluate = evaluateDetection,
		plot = plotResult, environment = getEnv)
}
