# {{{ Copyright (c) 2004, Ph. Grosjean <phgrosjean@sciviews.org>
#
# This file is part of ZooImage .
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
# }}}

# Version 1.2.0: check for package loading, and add a 'package' attribute to ZIClass
### TODO: allow for defining parameters and use a plugin mechanism

# {{{ ZIClass 
#' Modifications in calculation of probabilities to accept variables selection v1.2-2
"ZIClass" <- function(df, algorithm = c("lda", "randomForest"),
		package = c("MASS", "randomForest"),
		Formula = Class ~ logArea + Mean + StdDev + Mode + Min + Max + logPerim. +
		logMajor + logMinor + Circ. + logFeret + IntDen + Elongation + CentBoxD +
		GrayCentBoxD + CentroidsD + Range + MeanPos + SDNorm + CV, calc.vars = "calc.vars", k.xval = 10, ...) {
	
	# check package availability
	### TODO: add error checking in all evals!
	(require(ipred) || stop("Package 'ipred' is required!"))
	package <- package[1]
	if (!is.null(package)){
		require( package, character.only = TRUE )
	}
	
	# check calc.vars
	calc.vars <- calc.vars[1]
	if (!is.null(calc.vars)) {
		#eval(parse(text = paste("df <- ", calc.vars, "(df)", sep = "")))
		if (!exists(calc.vars, mode = "function"))
		   stop("Function ", calc.vars, "() not found!")
		CV <- get(calc.vars, mode = "function")
		df <- CV(df)
	}
	
	# algorithm
	algorithm <- algorithm[1]
	eval(parse(text = paste("ZI.class <- ", algorithm, "(Formula, data = df, ...)", sep = "")))
	#	if (!exists(ZI.class))
	#		stop("Error while training the '", algorithm, "' algorithm!")
	# Return a ZIClass object
	class(ZI.class) <- c("ZIClass", class(ZI.class))
	attr(ZI.class, "algorithm") <- algorithm
	attr(ZI.class, "package") <- package
	attr(ZI.class, "calc.vars") <- get(calc.vars, envir = parent.frame())
	Classes <- df[[as.character(Formula)[2]]]
	attr(ZI.class, "classes") <- Classes
	# Calculate predictions with full training set
    attr(ZI.class, "predict") <- predict(ZI.class, df, calc.vars = FALSE, class.only = TRUE)

	### Calculation of probabilities
  	if (algorithm == "randomForest") {
  		# Use Formula for the probabilities v1.2-2
  		rf <- randomForest(formula = Formula, data = df)
  		attr(ZI.class, "proba") <- predict(object = rf, newdata = df, type = "prob")
	}
  
	# Possibly make a k-fold cross-validation and check results
	if (!is.null(k.xval)) {
		if (algorithm == "lda") {
			mypredict <- function(object, newdata) predict(object, newdata = newdata)$class
#		} else if (algorithm %in% c("rpart", "ipredknn", "nnet2")) {
#            mypredict <- function(object, newdata) predict(object, newdata = newdata, type = "class")
		} else {
			mypredict <- function(object, newdata) predict(object, newdata = newdata, type = "class")
		}
    	res <- cv(Classes, Formula, data = df, model = get(algorithm),
			predict = mypredict, k = k.xval, predictions = TRUE, ...)$predictions
		attr(ZI.class, "kfold.predict") <- res
		attr(ZI.class, "k") <- k.xval	
	}
	return(ZI.class)
}
#}}}

# {{{ print.ZIClass
"print.ZIClass" <- function(x, ...) {
	algorithm <- attr(x, "algorithm")
	classes <- attr(x, "classes")
	lclasses <- levels(classes)
    predict <- attr(x, "predict")
	k <- attr(x, "k")
	cat("A ZIClass object predicting for", length(lclasses), "classes:\n")
	print(lclasses)
	Confu <- confu(classes, predict)
	mism <- (1 - (sum(diag(Confu)) / sum(Confu))) * 100 
	# Change the number of digits to display
	oldDigits <- options(digits = 4)
	on.exit(options(digits = oldDigits))
	cat("\nAlgorithm used:", algorithm, "\n")
	cat("Mismatch in classification: ", mism, "%\n", sep = "")   
	if (!is.null(k)) {
    	cat("k-fold cross validation error estimation (k = ", k, "):\n", sep = "")
		kfold.predict <- attr(x, "kfold.predict")
		prior <- table(classes)
		ok <- diag(table(classes, kfold.predict)) 
		err <- (1 - (sum(ok) / sum(prior))) * 100
		cat(err, "%\n", sep = "")
		cat("\nError per class:\n")
		`Error (%)` <- sort(1 - (ok / prior)) * 100
		print(as.data.frame(`Error (%)`)) 
	}
	return(invisible(x))	
}
# }}}

# {{{ predict.ZIClass
"predict.ZIClass" <- function(object, ZIDat, calc.vars = TRUE, class.only = FALSE, ...) {
	
	# Make sure we have correct objects
	mustbe( object, "ZIClass" )
	mustbe( object, c("ZIDat", "data.frame") )
	
	# Possibly load a specific package for prediction
	package <- attr(object, "package")
	if (is.null(package)) {
        # This is for old version, we make sure to load
        # MASS, RandomForest, class, rpart, e1071, ipred
        # Rem: nnet has a special treatment in nnet2
        (require(MASS) || stop("package 'MASS' is required!"))
        (require(RandomForest) || stop("package 'RandomForest' is required!"))
        (require(class) || stop("package 'class' is required!"))
        (require(rpart) || stop("package 'rpart' is required!"))
        (require(e1071) || stop("package 'e1071' is required!"))
        (require(ipred) || stop("package 'ipred' is required!"))
    } else { 
        # Make sure that the specific required package is loaded
        require( package, character.only = TRUE )
    }
    class(object) <- class(object)[-1]
	data <- as.data.frame(ZIDat)
	if (calc.vars) data <- attr(object, "calc.vars")(data)
	Ident <- predict(object, newdata = data, type = "class")

	# Special case for prediction from an LDA (list with $class item)
	if (inherits(Ident, "list") && "class" %in% names(Ident))
		Ident <- Ident$class
	if (!class.only) {
		res <- cbind(ZIDat, Ident)
		class(res) <- class(ZIDat)
	} else res <- Ident
	return(res)
}
# }}}

# {{{ confu
"confu" <- function(classes1, classes2, classes.predicted = FALSE) {
	
	if (is.factor(classes1) || is.factor(classes2)) {
		if (NROW(classes1) != NROW(classes2))
			stop("Not same number of items in classes1 and classes2")
		
		# Check that levels match
		if (!all(levels(classes1) == levels(classes2)))
			stop("'Class' levels in the two objects do not match")
		clCompa <- data.frame(Class.x = classes1, Class.y = classes2)
	} else { # Merge two data frame according to common objects in "Id" column
		
		# Check levels match
		if (!all(levels(classes1$Class) == levels(classes22$Class)))
			stop("Levels for 'Class' in the two objects do not match")
		
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
	Acc <- sum(diag(Conf)) / sum(Conf)*100
	
	# Change labels to get a more compact presentation
	colnames(Conf) <- formatC(1:ncol(Conf), digits = 1, flag = "0")
	rownames(Conf) <- paste(colnames(Conf), rownames(Conf))
	
	# Results
	res <- Conf
	attr(res, "accuracy") <- Acc
	attr(res, "nbr.per.class") <- NbPerClass
	return(res)
}
# }}}

# {{{ confu.map
"confu.map" <- function(set1, set2, level = 1){
    
	opar <- par(no.readonly = TRUE) ; on.exit(par = opar)
    par(mar = c(5, 12, 4, 2) + 0.1)
	
	n <- length(levels(set1))    
	image(1:n, 1:n, 1/ (t(confu(set1, set2)[n:1, 1:n])), col = heat.colors(10), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    axis(1, at = 1:n, las = 2)
    axis(2, at = n:1, labels = paste(levels(set1), 1:n), las = 1)
    abline(h = (1:(n + 1)) - 0.5, lty = 2, col = "gray")
    abline(v = (1:(n + 1)) - 0.5, lty = 2, col = "gray")
}
# }}}

# {{{ confusion.tree 
# New function v1.2-2 using library gregmisc
confusion.tree <- function (confmat, maxval, margin=NULL, Rowv = TRUE, Colv = TRUE) {
	nX <- nrow(confmat)
	nY <- ncol(confmat)
	nZ <- nX*nY
	
	confmat <- pmin( confmat, maxval )
	
	library(RColorBrewer)
	mypalette <- brewer.pal(maxval-1, "Spectral")
	#hc <- c("#FFFFFF", rev(heat.colors(maxval)))
	library(gregmisc)
	heatmap.2(confmat, col= c(0,mypalette), symm=TRUE, margin=margin, trace="both", Rowv=Rowv,
			Colv=Colv, cexRow=0.2 + 1/log10(nX), cexCol=0.2 + 1/log10(nY),tracecol="Black", linecol=FALSE)
}
# }}}

# New function v 1.2-2 false positive and negative
confusion.bar <- function(confmat, mar=NULL) {
	if (is.matrix(confmat) == FALSE)
	stop("object must be a matrix")
	Nn <- nrow(confmat)
	## percent of correctly predicted objects in the test set
	pred.tok = diag(confmat)/apply(confmat, 2, sum)*100; pred.tok
	# If there are no items good recognize 0/0 = NaN so replace NaN by 0 for calculation
  if (NaN %in% pred.tok){
  pred.tok[pred.tok == "NaN"] <- 0
  }
  # percent of items in the test set predicted in its category
	pred.tfrac = diag(confmat)/apply(confmat, 1, sum)*100; pred.tfrac
	if (NaN %in% pred.tfrac){
  pred.tfrac[pred.tfrac == "NaN"] <- 0
  }
  prediction <- cbind(pred.tok, pred.tfrac)
	prediction.df <- data.frame(prediction)
	CR <- prediction[1:Nn,2] # 
	FN <- 100 - CR # faux n�gatif = objects which exist in the test set but not in the training set;
	# they are wrongly predicted as not to belong to a particular group
	prediction.df$FN <- FN
	#put to scale
	CR2 <- prediction[1:Nn,1]
	FP <- 100-CR2 # Faux positifs
	prediction.df$FP <- FP
	prediction.df <- round(prediction.df,0) # arrondi les valeurs � des dombres entiers
	Failure <- prediction.df[c("FN", "FP")]
	#put all to scale
	allN <- CR+FN # all negative
	allP <- CR2+FP # all positive
	cr <- (CR/allN)*100 #% good identify by pc
	cr2 <- (CR2/allP)*100 #% good identify by pc
	fn <- (FN/allN)*100 # percentage of FN
	fp <- (FP/allP)*100 # percentage of FP
	all <- as.matrix(data.frame(fn=fn, cr=cr, cr2=cr2, fp=fp))
	Order <- order((all[, 2] + all[, 3]), decreasing = TRUE) # trie du mieux reconnu au moin bon
	all2 <- t(all[Order, ]) # transposer la matrice tri�e
	Failure <- Failure[Order,] # grp du moin au plus d'erreur
	Failure.mat <- as.matrix(Failure)
	Nmat <- ncol(all2)
  #### Construction du graphe
	valx <- matrix(c(rep(2, Nmat), rep(198, Nmat)),ncol=2) #matrix for location
	valx2 <- matrix(c(rep(98, Nmat), rep(102, Nmat)),ncol=2) #matrix for location
	omar = par("mar") # mar = margin size c(bottom, left, top, right)
	par(mar=mar)
	barplot(all2[,!is.na(all2[2,])], horiz=TRUE, col=c("PeachPuff2", "green3", "green3", "lemonChiffon2"),
		xaxt="n", las=1, space = 0)
	for (i in 1:Nmat) {
		text(valx[i,],i-0.45, Failure.mat[i,] , cex=0.7)
		text(valx2[i,],i-0.45, 100 - Failure.mat[i,] , cex=0.7)
		}
  #### Ajout des l�gendes
  legend(100, Nmat+(Nmat/15), legend = c("false negative (FN)", "correct ident (CI)", "false positive (FP)"), xjust = 0.5, fill = c("PeachPuff2", "green3", "lemonChiffon2"), bty="n", horiz = TRUE)
	legend(100, Nmat/55, "Percentage", xjust = 0.5, bty = "n")
	segx0 <- rep(c(25, 50, 75, 125, 150, 175),2)
	segy0 <- rep(c(0, Nmat),c(6,6))
	segments(segx0[c(1:6)], segy0[c(1:6)], segx0[c(7:12)], segy0[c(7:12)], col="red", lty=2)
	valx3 <- c(25, 50, 75, 125, 150, 175)
	text(valx3[1:6], -(Nmat/35), labels= segx0[c(1:3, 7:9)], cex=0.7)
	#par(mar=omar)
}

"nnet2" <-
	function(formula, data, size = 7, rang = 0.1, decay = 5e-4, maxit = 1000, ...) {
 	(require(nnet) || stop("package 'nnet' is required!"))
	res <- nnet(formula = formula, data = data, size = size, rang = rang, decay = decay, maxit = maxit, ...)
	class(res) <- c("nnet2", "nnet.formula", "nnet")
	return(res)
}

"predict.nnet2" <-
	function (object, newdata, type = c("raw", "class"), ...) {
	 if (!inherits(object, "nnet2")) 
        stop("object not of class \"nnet2\"")
	(require(nnet) || stop("package 'nnet' is required!"))
    class(object) <-class(object)[-1]
	res <- predict(object, newdata = newdata, type = type, ...)
	# If type is class, we got a character vector... but should get a factor
	if (type == "class")
    	res <- factor(res, levels = object$lev)
	return(res)
}

"lvq" <- 
	function(formula, data, k = 5, size = NULL) {
	# Extract classes and training vars from data, according to formula lhs ~ rhs
	# This is a very simplified way of doing it... It does not manage complex formula constructions!
	(require(class) || stop("package 'class' is required!"))
    vars <- all.vars(formula)
	train <- data[, vars[-1]]
	cl <- data[, vars[1]]
	lev <- levels(cl)
	codebk <- olvq1(train, cl, lvqinit(train, cl, k = k, size = size))
	res <- list(codebook = codebk, data = data, vars = vars, classes = cl, lev = lev)
	class(res) <- "lvq"
	return(res)
}

"predict.lvq" <-
	function(object, newdata, type = "class", ...) {
   	if (!inherits(object, "lvq")) 
        stop("object not of class \"lvq\"")
	(require(class) || stop("package 'class' is required!"))
    if (missing(newdata)) newdata <- object$data
	lvqtest(object$codebook, newdata[, object$vars[-1]])
}

# Formula calculation by variables selection for the classifier creation v1.2-2
FormVarsSelect <- function(x){
  # x must be a ZItrain object
  if (class(x) != "ZI1Train")
    stop("your object must be a ZItrain object")
  # Parameters measured on particles and new variables calculated
  mes <- as.vector(colnames(calc.vars(x)))
  # Selection of features for the creation of the classifier
  keep <-select.list(list = mes,
    preselect = c("ECD", "FIT_Area_ABD", "FIT_Diameter_ABD", "FIT_Volume_ABD", "FIT_Diameter_ESD",
    "FIT_Volume_ESD", "FIT_Length", "FIT_Width", "FIT_Aspect_Ratio", "FIT_Transparency",
    "FIT_Intensity", "FIT_Sigma_Intensity", "FIT_Sum_Intensity", "FIT_Compactness",
    "FIT_Elongation", "FIT_Perimeter", "FIT_Convex_Perimeter", "FIT_Roughness",
    "FIT_Feret_Max_Angle", "FIT_Ch1_Peak", "FIT_Ch1_TOF", "FIT_Ch2_Peak", "FIT_Ch2_TOF",
    "Area", "Mean", "StdDev", "Mode", "Min", "Max", "Perim.", "Width","Height",
    "Major", "Minor", "Circ.", "Feret", "IntDen", "Median", "Skew", "Kurt", "Elongation",
    "CentBoxD", "GrayCentBoxD", "CentroidsD", "Range", "MeanPos", "SDNorm", "CV", "logArea",
    "logPerim.", "logMajor", "logMinor", "logFeret"),
    multiple = TRUE, title = "Select variables to keep")
  # Creation of one formula for classifier calculation
  res <- as.formula(paste("Class ~ ", paste(keep, collapse= "+")))
  return(res)
}


# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:
