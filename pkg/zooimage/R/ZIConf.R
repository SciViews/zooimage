## Copyright (c) 2010-2012, Ph. Grosjean <phgrosjean@sciviews.org>
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

### TODO: define a confusion method that create a ZIConf object
ZIConf <- function (x, y = NULL, vars = c("Class", "Ident"), labels = vars,
merge.by = "Id")
{	
	## If the object is already a ZIConf, return it
	if (inherits(x, "ZIConf")) return(x)
	## Idem if there is a ZIConf attribute
	ziconf <- attr(x, "ZIConf")
	if (!is.null(ziconf)) return(ziconf)
	
	## If the object is ZIClass, calculate ZIConf
	## from attributes 'classes' and 'kfold.predict' 
	if (inherits(x, "ZIClass")) {
		y <- attr(x, "kfold.predict")
		x <- attr(x, "classes")
		labels = c("Class", "Predict")
	}
		
	## Check/Convert vars and labels
	if (missing(vars)) vars <- NULL else {
		vars <- as.character(vars)
		if (length(vars) != 2)
			stop("You must provide exactly 2 strings for 'vars'")
	}
	if (is.null(labels)) labels <- c("Class", "Ident")
	labels <- as.character(labels)
	if (length(labels) != 2)
		stop("You must provide exactly 2 strings for 'labels'")
	merge.by <- as.character(merge.by)
	
	## There are three possibilities:
	## 1) a single data frame => use vars
	if (is.null(y)) {
		## Special case of a data frame or list of two factors: keep as it is
		if (inherits(x, c("list", "data.frame") && ncol(x) == 2 &&
			is.null(vars))) {
			clCompa <- as.data.frame(x)
			labels <- names(clCompa)
		} else {
			x <- as.data.frame(x)
			## Check that levels of two vars do match
			if (!all(sort(levels(x[[vars[1]]]))  == sort(levels(x[[vars[2]]]))))
				stop("Levels of two 'vars' in 'x' do not match")
			if (is.null(names(x)) || !all(vars %in% names(x)))
				stop("'vars' are not among column names of 'x'")
			clCompa <- data.frame(class1 = x[[vars[1]]], class2 = x[[vars[2]]])
		}
	} else { # y is provided
		## 2) two vectors of factors to compare (must have same length/same levels)
		if (is.factor(x) && is.factor(y)) {
			if (length(x) != length(x))
				stop("Not same number of items in 'x' and 'y'")
			if (!all(sort(levels(x))  == sort(levels(y))))
				stop("'x' and 'y' levels do not match")
			clCompa <- data.frame(class1 = x, class2 = y)
		} else {
			## 3) two data frames => merge first, then use vars
			## Check levels match
			## Note: if one is a subset of the other, would it be possible to match them???
			if (!all(sort(levels(x[[vars[1]]]))  == sort(levels(y[[vars[2]]]))))
				stop("Levels of two 'vars' in 'x' and 'y' do not match")
			## Merge data according to merge.by
			clCompa <- merge(x[, c(vars[1], merge.by)], y[, c(vars[2], merge.by)],
				by = merge.by)
			clCompa <- clCompa[, c(ncol(clCompa) - 1, ncol(clCompa))]
			## Are there common objects left?
			if (nrow(clCompa) == 0)
				stop("No common objects between 'x' and 'y'")
		}
	}
	names(clCompa) <- labels
	## How many common objects by level?
	NbrPerClass1 <- table(clCompa[, 1])
	## How many predicted objects
	NbrPerClass2 <- table(clCompa[, 2])
	## Confusion matrix
	Conf <- table(clCompa)
	## Further stats: total, true positives, accuracy
	Total <- sum(Conf)
	TruePos <- sum(diag(Conf))
	Stats <- c(total = Total, truepos = TruePos,
		accuracy = TruePos / Total * 100)

	## Change labels to get a more compact presentation
	colnames(Conf) <- formatC(1:ncol(Conf), digits = 1, flag = "0")
	rownames(Conf) <- paste(colnames(Conf), rownames(Conf))

	## Additional data as attributes
	attr(Conf, "stats") <- Stats
	attr(Conf, "nbr.rows") <- NbrPerClass1
	attr(Conf, "nbr.cols") <- NbrPerClass2
	
	## This is a ZIConf object
	class(Conf) <- c("ZIConf", "table")
	return(Conf)
}

print.ZIConf <- function (x, ...)
{
	X <- x
	class(X) <- "table"
	print(X)
	Stats <- attr(x, "stats")
	cat("\n", Stats["total"], " particles classified with ", Stats["truepos"],
		" true positives (", round(Stats["accuracy"], 1), "% accuracy)\n", sep ="")
	return(invisible(x))
}

plot.ZIConf <- function (x, y,
type = c("image", "tree_image", "precision_recall"), ...)
{
	type <- match.arg(type)
	res <- switch(type,
		image = confusionMap(x, ...),
		tree_image = confusionTree(x, ...),
		precision_recall = confusionBar(x, ...),
		stop("'type' must be 'image', 'tree_image', or 'precision_recall'"))
	return(invisible(res))
}

## These functions do the respective ZIConf graphs and are not exported!
confusionMap <- function (ZIConf, col = heat.colors(10),
mar = c(5.1, 12.1, 4.1, 2.1))
{
	if (!inherits(ZIConf, c("ZIConf")))
		stop("'ZIConf' must be a 'ZIConf' object")
	omar  <- par("mar")
	on.exit(par(omar))
    par(mar = mar)
	n <- ncol(ZIConf)
	image(1:n, 1:n, 1 / (t(ZIConf[n:1, 1:n])), col = col,
		xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    axis(1, at = 1:n, las = 2)
    axis(2, at = n:1, labels = paste(names(attr(ZIConf, "nbr.cols")), 1:n),
		las = 1)
    abline(h = (1:(n + 1)) - 0.5, lty = 2, col = "gray")
    abline(v = (1:(n + 1)) - 0.5, lty = 2, col = "gray")
	return(invisible(ZIConf))
}

## New function v1.2-2 using library gplots
confusionTree <- function (ZIConf, maxval = 10, margins = c(2, 10),
Rowv = TRUE, Colv = TRUE)
{
	if (!inherits(ZIConf, c("ZIConf")))
		stop("'ZIConf' must be a 'ZIConf' object")
	nX <- nrow(ZIConf)
	nY <- ncol(ZIConf)
	nZ <- nX * nY
	confmat <- pmin(ZIConf, maxval)
	mypalette <- brewer.pal(maxval - 1, "Spectral")
	heatmap.2(ZIConf, col= c(0, mypalette), symm = TRUE, margins = margin,
		trace = "both", Rowv = Rowv, Colv = Colv, cexRow = 0.2 + 1 / log10(nX),
		cexCol = 0.2 + 1 / log10(nY), tracecol = "Black", linecol = FALSE)
}

# Confusion bar with recall and precision in green bar and not outside as before
# function modified for publication hclust 
confusionBar <- function (ZIConf, col = c("PeachPuff2", "green3", "lemonChiffon2"),
    mar = c(1.1, 8.1, 4.1, 2.1), cex = 0.7, cex.axis = cex, cex.legend =
        cex, main = "Precision (at left) versus recall (at right)", minPlace = 17)
{
    if (!inherits(ZIConf, c("ZIConf")))
        stop("'ZIConf' must be a 'ZIConf' object")
    TP <- diag(ZIConf)
    fn <- rowSums(ZIConf) - TP
    fp <- colSums(ZIConf) - TP
    FN <- fn <- fn/(fn + TP)
    FP <- fp <- fp/(TP + fp)
    FP[is.na(FP)] <- 1
    tp <- 1 - fn
    fp <- tp/(1 - fp) * fp
    scale <- fn + tp + fp
    fn <- fn/scale * 100
    tp <- tp/scale * 100
    fp <- fp/scale * 100
    fn[is.na(tp)] <- 50
    fp[is.na(tp)] <- 50
    tp[is.na(tp)] <- 0
    res <- matrix(c(fp, tp, fn), ncol = 3)
    colnames(res) <- c("fp", "tp", "fn")
    Labels <- names(attr(ZIConf, "nbr.cols"))
    pos <- order(res[, 2], decreasing = TRUE)
    res <- res[pos, ]
    FN <- FN[pos]
    FP <- FP[pos]
    TP <- TP[pos]
    Labels <- Labels[pos]
    L <- length(FN)
    omar <- par("mar")
    on.exit(par(omar))
    par(mar = mar)
    barplot(t(res), horiz = TRUE, col = col, xaxt = "n", las = 1,
        space = 0)
    lines(c(50, 50), c(0, L), lwd = 1)
    xpos <- res[, 1] + res[, 2]/2
    text(xpos, 1:L - 0.5, round(TP), adj = c(0.5, 0.5), cex = cex)
    # Add recall and precision if enough place to print -->
    NotPlace <- res[,"tp"] <= minPlace
    if(any(NotPlace)){
      # Special case if not engough place to print precision and recall
      # Add Precision
      PrecTxt <- paste(round((1 - FP) * 100), "%", sep = "")
      PrecTxt[NotPlace] <- ""
      text(res[,"fp"] + 1, 1:L - 0.5, PrecTxt, adj = c(0, 0.5), cex = cex)
      # Add FDR
      FDTxt <- paste(round((FP) * 100), "%", sep = "")
      FDTxt[!NotPlace] <- ""
      text(rep(1, length(FP)), 1:L - 0.5, FDTxt, adj = c(0, 0.5), cex = cex)
      # Add Recall
      RecTxt <- paste(round((1 - FN) * 100), "%", sep = "")
      RecTxt[NotPlace] <- ""
      text(res[,"fp"] + res[, "tp"] - 5, 1:L - 0.5, RecTxt, adj = c(0, 0.5), cex = cex)
      # Add FN
      FNTxt <- paste(round((FN) * 100), "%", sep = "")
      FNTxt[!NotPlace] <- ""
      text(rep(99, length(FN)), 1:L - 0.5, FNTxt, adj = c(1, 0.5), cex = cex)
    } else {
      # Add Precision
      text(res[,"fp"] + 1, 1:L - 0.5, paste(round((1 - FP) *
          100), "%", sep = ""), adj = c(0, 0.5), cex = cex)
      # Add Recall
      text(res[,"fp"] + res[, "tp"] - 5, 1:L - 0.5, paste(round((1 - FN) *
          100), "%", sep = ""), adj = c(0, 0.5), cex = cex)
    }
    legend(50, L * 1.05, legend = c("False Discovery : (1 - Prec.)", "True Positive (TP)",
        "False Negative : (1 - Rec.)"), cex = cex.legend, xjust = 0.5,
        yjust = 1, fill = col, bty = "n", horiz = TRUE)
    axis(2, 1:L - 0.5, tick = FALSE, las = 1, cex.axis = cex.axis,
        labels = Labels)
    title(main = main)
    text(50, -0.5, "< higher precision : TP/(TP+FP) - underestimate <=> overestimate - higher recall : TP/(TP+FN) >  ",
        cex = cex)
    return(invisible(res))
}

# Precision vs Recall basic representation
PrecVsRec <- function (ZIConf, col = c("PeachPuff2", "green",  "green3", "lemonChiffon2"),
  mar = c(2.1, 8.1, 4.1, 2.1), cex = 0.7, cex.axis = cex, main = "Precision Vs Recall")
{
  if (!inherits(ZIConf, c("ZIConf")))
    stop("'ZIConf' must be a 'ZIConf' object")
  # Calculation of statistics
  Stats <- ConfMatStats(ZIConf = ZIConf)
  FDR <- Stats$FDR * 100
  Precision <- Stats$Precision * 100
  Recall <- Stats$Recall * 100
  FNR <- Stats$FNR * 100
  # Order statistices according Precision + recall
  pos <- order(Recall + Precision, decreasing = TRUE)
  # Results to plot
  res <- cbind(FDR, Precision, Recall, FNR)
  # plot
  par(mar = mar)
  barplot(t(res[pos,]), horiz = TRUE, col = col, xaxt = "n", las = 1, space = 0)
  # add information
  Ngp <- nrow(Stats)
  Labels <- names(attr(ZIConf, "nbr.cols"))
  axis(2, 1:Ngp - 0.5, tick = FALSE, las = 1, cex.axis = cex.axis, labels = Labels[pos])
  title(main = main)
  text(rep(1, Ngp), 1:Ngp - 0.5, paste(round(FDR[pos]), "%", sep = ""), adj = c(0, 0.5), cex = cex)
  text(FDR[pos] + Precision[pos]/2, 1:Ngp - 0.5, paste(round(Precision[pos]), "%", sep = ""), adj = c(0, 0.5), cex = cex)
  text(FDR[pos] + Precision[pos] + Recall[pos]/2, 1:Ngp - 0.5, paste(round(Recall[pos]), "%", sep = ""), adj = c(0, 0.5), cex = cex)
  text(rep(191, Ngp), 1:Ngp - 0.5, paste(round(FNR[pos]), "%", sep = ""), adj = c(0, 0.5), cex = cex)
  legend("top", legend = c("False Discovery ", "Precision", "Recall", "False Negative"), cex = cex, fill = col, bty = "n", horiz = TRUE)
  text(96, -0.5, "< higher precision - underestimate <=> overestimate - higher recall >  ", cex = cex)
  return(invisible(res))
}



## Graphical representation of the confusion matrix (modif K. Denis)
confusionPlot <- function (manual, automatic, label = "manual \\ auto",
sort = "complete", cex = 1, left.mar = 10, colfun = NULL,
ncols = 41, col0 = FALSE, grid.col = "gray", asp = 1, ...)
{
	## Default color function
	rwb.colors <- function (n, alpha = 1, s = 0.9, v = 0.9) {
		if ((n <- as.integer(n[1L])) <= 0) return(character(0L))
		## Define the initial (red) and final (blue) colors with white in between
		cols <- c(hsv(h = 0, s = s, v = v, alpha = alpha),   # Red
				  hsv(h = 0, s = 0, v = v, alpha = alpha),   # White
				  hsv(h = 2/3, s = s, v = v, alpha = alpha)) # Blue
		## Use a color ramp from red to white to blue
		return(colorRampPalette(cols)(n))
	}
	if (is.null(colfun)) colfun <- rwb.colors
	
	## Calculate margins
	mar <- c(3, left.mar, 3, 3) + 0.1
	## Check manual and automatic
	if (missing(automatic) || is.null(automatic)) {
		## This must be a ZIClass object, or something equivalent
		automatic <- attr(manual, "kfold.predict")
		manual <- attr(manual, "classes")
	}	
	## Get levels
	manuLev <- levels(manual)
	autoLev <- levels(automatic)
	if (!identical(manuLev, autoLev))
		stop("Factor levels for 'manu' and 'auto' must be the same!")
	l <- length(manuLev)
	## Calculate confusion matrix
	confu <- table(manual, automatic)
	## Do we sort items?
	if (!is.null(sort) && !is.na(sort) && sort != FALSE && sort != "") {
		## Grouping of items
		confuSim <- confu + t(confu)
		confuSim <- max(confuSim) - confuSim
		confuDist <- structure(confuSim[lower.tri(confuSim)], Size = l,
			Diag = FALSE, Upper = FALSE, method = "confusion", call = "",
			class = "dist")
		order <- hclust(confuDist, method = sort)$order
		confu <- confu[order, order]
		autoLev <- autoLev[order]
		manuLev <- manuLev[order]
	}
	## Recode levels so that a number is used in front of manu labels
	## and shown in auto
	autoLev <- formatC(1:length(autoLev), width = 2, flag = "0")
	manuLev <- paste(manuLev, autoLev, sep = "-")
	row.names(confu) <- manuLev
	colnames(confu) <- autoLev
	## Calculate colors (use a transfo to get 0, 1, 2, 3, 4, 7, 10, 15, 25+)
	confuCol <- confu
	confuCol <- log(confuCol + .5) * 2.33
	confuCol[confuCol < 0] <- if (isTRUE(col0)) 0 else NA
	confuCol[confuCol > 10] <- 10
	## Negative values (in blue) on the diagonal (correct IDs)
	diag(confuCol) <- -diag(confuCol)	
	## Make an image of this matrix
	omar <- par(no.readonly = TRUE)
	on.exit(par(mar = omar))
	par(mar = mar, cex = cex)
	image(1:l, 1:l, -t(confuCol[nrow(confuCol):1, ]), zlim = c(-10, 10),
		asp = asp, bty = "n", col = colfun(ncols), xaxt = "n", yaxt = "n",
		xlab = "", ylab = "", main = "")
	## Print the actual numbers
	confuTxt <- as.character(confu[l:1, ])
	confuTxt[confuTxt == "0"] <- ""
	text(rep(1:l, each = l), 1:l, labels = confuTxt)
	## The grid
	abline(h = 0:l + 0.5, col = grid.col)
	abline(v = 0:l + 0.5, col = grid.col)
	## The axis labels
	axis(1, 1:l, labels = autoLev, tick =  FALSE, padj = 0)
	axis(2, 1:l, labels = manuLev[l:1], tick =  FALSE, las = 1, hadj = 1)
	axis(3, 1:l, labels = autoLev, tick =  FALSE) #, cex.lab = cex)
	axis(4, 1:l, labels = autoLev[l:1], tick =  FALSE, las = 1, hadj = 0)
	## Legend at top-left
	mar[2] <- 1.1
	par (mar = mar, new = TRUE)
	plot(0, 0, type = "n", xaxt = "n", yaxt = "n", bty = "n")
	mtext(label, adj = 0, line = 1, cex = cex)
	## Return the confusion matrix, as displayed, in text format
	return(invisible(confu))
}

## Table with stats per groupe precision, recall, etc
confusionStat <- function(ZIClass, ZIConf = NULL, sort.by = NULL, decreasing = FALSE, NaN.rm = FALSE)
{
    # Create confusion matrix
    if(is.null(ZIConf)){
        Confu <- ZIConf(ZIClass)
    } else {
	   Confu <- ZIConf
    }
    ##### General parameters
    # Number of groups
    Ngp <- ncol(Confu)
    
    # Total : TP + TN + FP + FN
    Tot <- sum(Confu)
    
    # TP : True positive item : All items on diagonal
    TP <- diag(Confu)
    
    # TP + TN : sum of diagonal = All correct identification
    TP_TN <- sum(TP)
    
    # TP + FP : sum of columns : Automatic classification
    TP_FP <- colSums(Confu)
    
    # TP + FN : sum of rows : Manual classification
    TP_FN <- rowSums(Confu)
    
    # FP : False positive items
    FP <- TP_FP - TP    

    # FN : False negative item
    FN <- TP_FN - TP

    # TN : True Negative = Total - TP - FP - FN
    TN <- rep(Tot, Ngp) - TP - FP - FN
    
    ##### General statistics
    # Accuracy = (TP + TN) / (TP + TN + FP + FN)
    Accuracy <- TP_TN / Tot
    
    # Error = 1 - Accuracy
    Error <- 1 - Accuracy
    
    ##### The 8 basic ratios
    # Recall = TP / (TP + FN) = 1 - FNR
    Recall <- TP / (TP_FN)

    # Specificity = TN / (TN + FP) = 1 - FPR
    Specificity <- TN / (TN + FP)

    # Precision = TP / (TP + FP) = 1 - FDR
    Precision <- TP / (TP_FP)
    
    # NPV : Negative predicted value = TN / (TN + FN) = 1 - FOR
    NPV <- TN / (TN + FN)
    
    # FPR : False positive rate = 1 - Specificity = FP / (FP + TN) 
    FPR <- FP / (FP + TN) #1 - Specificity
    
    # FNR : False negative rate = 1 - Recall = FN / (TP + FN)
    FNR <- FN / (TP + FN) #1 - Recall

    # FDR : False Discovery Rate = 1 - Precision = FP / (TP + FP)
    FDR <- FP / (TP_FP) #1 - Precision
    
    # FOR : False omission rate = 1 - NPV = FN / (FN + TN)
    FOR <- FN / (FN + TN) #1 - NPV

    ##### The 4 ratios of ratios
    # LRPT = Likelihood Ratio for Positive Tests = Recall / FPR = Recall / (1 - Specificity)
    LRPT <- Recall / (FPR)
    
    # LRNT = Likelihood Ratio for Negative Tests = FNR / Specificity = (1 - Recall) / Specificity
    LRNT <- FNR / (Specificity)
    
    # LRPS : Likelihood Ratio for Positive Subjects = Precision / FOR = Precision / (1 - NPV)
    LRPS <- Precision / (FOR)
    
    # LRNS : Likelihood Ratio Negative Subjects = FDR / NPV = (1 - Precision) / (1 - FOR)
    LRNS <- FDR / (NPV)
    
    ##### Additional statistics
    # F-measure = F1 score = Harmonic mean of Precision and recall
    Fmeasure <- 2 * ((Precision * Recall) / (Precision + Recall))
    
    # Balanced accuracy = (Sensitivity + Specificity) / 2
    BalAcc <- (Recall + Specificity) / 2

    # MCC : Matthews correlation coefficient
    Sum1 <- TP + FP
    Sum2 <- TP + FN
    Sum3 <- TN + FP
    Sum4 <- TN + FN
    Denominator <- sqrt(Sum1 * Sum2 * Sum3 * Sum4)
    ZeroIdx <- Sum1 == 0 | Sum2 == 0 | Sum3 == 0 | Sum4 == 0
    if(any(ZeroIdx)){
	   Denominator[ZeroIdx] <- 1
    }
    MCC <- ((TP * TN) - (FP * FN)) / Denominator
    
    # Chisq : Significance
    Chisq <- (((TP * TN) - (FP * FN))^2 * (TP + TN + FP + FN)) / ((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))

    # Automatic classification - Manual calssification
    Auto_Manu <- TP_FP - TP_FN
    
    # Dissimilarity Index of Bray Curtis
    Dissimilarity <- abs(Auto_Manu) / (sum(TP_FP) + sum(TP_FN))
    
    res <- data.frame(
	   Auto = TP_FP, Manu = TP_FN, Auto_Manu = Auto_Manu, Dissimilarity = Dissimilarity,
	   TP = TP, FP = FP, FN = FN, TN = TN,
	   Recall = Recall, Precision = Precision,	Specificity = Specificity, NPV = NPV,
	   FPR = FPR, FNR = FNR, FDR = FDR, FOR = FOR,
	   LRPT = LRPT, LRNT = LRNT, LRPS = LRPS, LRNS = LRNS,
	   Fmeasure = Fmeasure, BalAcc = BalAcc, MCC = MCC, Chisq = Chisq
    )

    rownames(res) <- rownames(Confu)
    # Sort the table in function of one parameter by default FN
    if(!is.null(sort.by)){
	   res <- res[order(res[, sort.by], decreasing = decreasing), ]
    }
    attr(res, "Accuracy") <- Accuracy
    attr(res, "Error") <- Error
    
    # Remove Nan if any 0/0
    if(isTRUE(NaN.rm)){
    	# Cases where it is impossible to calculate some statistics: 0/0 or X/0
    	# Case 1 : Everything is Correct --> FP = 0, FN = 0
    	Case1Idx <- FP == 0 & FN == 0 # no any error!
    	if(any(Case1Idx)){
    	    Recall[Case1Idx] <- 1
    	    FNR[Case1Idx] <- 0
    	    Precision[Case1Idx] <- 1
    	    FDR[Case1Idx] <- 0
    	    Specificity[Case1Idx] <- 1
    	    FPR[Case1Idx] <- 0
    	    NPV[Case1Idx] <- 1
    	    FOR[Case1Idx] <- 0
    	}
    	# Case 2 : Everything is Wrong and only false positive --> Impossible to calculate Recall and NPV
    	Case2Idx <- TP == 0 & TN == 0 & FP > 0 & FN == 0
    	if(any(Case2Idx)){
    #	    Recall[Case2Idx] <- 0
    #	    FNR[Case2Idx] <- 1
    #	    NPV[Case2Idx] <- 0
    #	    FOR[Case2Idx] <- 1
    	    Recall[Case2Idx] <- 1
    	    FNR[Case2Idx] <- 0
    	    NPV[Case2Idx] <- 1
    	    FOR[Case2Idx] <- 0
    	}
    	# Case 3 : Everything is Wrong and only false negative --> Impossible to calculate Precision and Specificity
    	Case3Idx <- TP == 0 & TN == 0 & FP == 0 & FN > 0
    	if(any(Case3Idx)){
    #	    Precision[Case3Idx] <- 0
    #	    FDR[Case3Idx] <- 1
    #	    Specificity[Case3Idx] <- 0
    #	    FPR[Case3Idx] <- 1
    	    Precision[Case3Idx] <- 1
    	    FDR[Case3Idx] <- 0
    	    Specificity[Case3Idx] <- 1
    	    FPR[Case3Idx] <- 0
    	}
    	# Case 4 : No FP and No TN --> Impossible to calculate Specificity
    	Case4Idx <- TP > 0 & TN == 0 & FP == 0 & FN > 0
    	if(any(Case4Idx)){
    #	    Specificity[Case4Idx] <- 0
    #	    FPR[Case4Idx] <- 1
    	    Specificity[Case4Idx] <- 1
    	    FPR[Case4Idx] <- 0
    	}
    	# Case 5 : No TP and No FP --> Impossible to calculate Precision
    	Case5Idx <- TP == 0 & TN > 0 & FP == 0 & FN > 0
    	if(any(Case5Idx)){
    #	    Precision[Case5Idx] <- 0
    #	    FDR[Case5Idx] <- 1
    	    Precision[Case5Idx] <- 1
    	    FDR[Case5Idx] <- 0
    	}
    	# Case 6 : No TP and no FN --> Impossible to calculate Recall
    	Case6Idx <- TP == 0 & TN > 0 & FP > 0 & FN == 0
    	if(any(Case6Idx)){
    #	    Recall[Case6Idx] <- 0
    #	    FNR[Case6Idx] <- 1
    	    Recall[Case6Idx] <- 1
    	    FNR[Case6Idx] <- 0
    	}
    	# Case 7 : No TN and no FN --> Impossible to calculate Negative predicted value
    	Case7Idx <- TP > 0 & TN == 0 & FP > 0 & FN == 0
    	if(any(Case7Idx)){
    #	    NPV[Case7Idx] <- 0
    #	    FOR[Case7Idx] <- 1
    	    NPV[Case7Idx] <- 1
    	    FOR[Case7Idx] <- 0
    	}
    }
    cat(paste("Accuracy:", round(Accuracy * 100, digits = 2), "%", "\n", "Error:", round(Error * 100, digits = 2), "%", "\n"))
    return(res)
}
