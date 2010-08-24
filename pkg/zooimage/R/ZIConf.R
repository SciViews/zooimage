# Copyright (c) 2010, Ph. Grosjean <phgrosjean@sciviews.org>
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

### TODO: define a confusion method that create a ZIConf object
"ZIConf" <- function (x, y = NULL, vars = c("Class", "Ident"), labels = vars,
	merge.by = "Id")
{	
	# If the object is already a ZIConf, return it
	if (inherits(x, "ZIConf")) return(x)
	# Idem if there is a ZIConf attribute
	ziconf <- attr(x, "ZIConf")
	if (!is.null(ziconf)) return(ziconf)
	
	# If the object is ZIClass, calculate ZIConf
	# from attributes 'classes' and 'kfold.predict' 
	if (inherits(x, "ZIClass")) {
		y <- attr(x, "kfold.predict")
		x <- attr(x, "classes")
		labels = c("Class", "Predict")
	}
		
	# Check/Convert vars and labels
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
	
	# There are three possibilities:
	# 1) a single data frame => use vars
	if (is.null(y)) {
		# Special case of a data frame or list of two factors: keep as it is
		if (inherits(x, c("list", "data.frame") && ncol(x) == 2 &&
			is.null(vars))) {
			clCompa <- as.data.frame(x)
			labels <- names(clCompa)
		} else {
			x <- as.data.frame(x)
			# Check that levels of two vars do match
			if (!all(sort(levels(x[[vars[1]]]))  == sort(levels(x[[vars[2]]]))))
				stop("Levels of two 'vars' in 'x' do not match")
			if (is.null(names(x)) || !all(vars %in% names(x)))
				stop("'vars' are not among column names of 'x'")
			clCompa <- data.frame(class1 = x[[vars[1]]], class2 = x[[vars[2]]])
		}
	} else { # y is provided
		# 2) two vectors of factors to compare (must have same length/same levels)
		if (is.factor(x) && is.factor(y)) {
			if (length(x) != length(x))
				stop("Not same number of items in 'x' and 'y'")
			if (!all(sort(levels(x))  == sort(levels(y))))
				stop("'x' and 'y' levels do not match")
			clCompa <- data.frame(class1 = x, class2 = y)
		} else {
			# 3) two data frames => merge first, then use vars
			# Check levels match
			# Note: if one is a subset of the other, would it be possible to match them???
			if (!all(sort(levels(x[[vars[1]]]))  == sort(levels(y[[vars[2]]]))))
				stop("Levels of two 'vars' in 'x' and 'y' do not match")
			# Merge data according to merge.by
			clCompa <- merge(x[, c(vars[1], merge.by)], y[, c(vars[2], merge.by)],
				by = merge.by)
			clCompa <- clCompa[, c(ncol(clCompa) - 1, ncol(clCompa))]
			# Are there common objects left?
			if (nrow(clCompa) == 0)
				stop("No common objects between 'x' and 'y'")
		}
	}
	names(clCompa) <- labels
	# How many common objects by level?
	NbrPerClass1 <- table(clCompa[, 1])
	# How many predicted objects
	NbrPerClass2 <- table(clCompa[, 2])
	# Confusion matrix
	Conf <- table(clCompa)
	# Further stats: total, true positives, accuracy
	Total <- sum(Conf)
	TruePos <- sum(diag(Conf))
	Stats <- c(total = Total, truepos = TruePos,
		accuracy = TruePos / Total * 100)

	# Change labels to get a more compact presentation
	colnames(Conf) <- formatC(1:ncol(Conf), digits = 1, flag = "0")
	rownames(Conf) <- paste(colnames(Conf), rownames(Conf))

	# Additional data as attributes
	attr(Conf, "stats") <- Stats
	attr(Conf, "nbr.rows") <- NbrPerClass1
	attr(Conf, "nbr.cols") <- NbrPerClass2
	
	# This is a ZIConf object
	class(Conf) <- c("ZIConf", "table")
	return(Conf)
}

"print.ZIConf" <- function (x, ...)
{
	X <- x
	class(X) <- "table"
	print(X)
	Stats <- attr(x, "stats")
	cat("\n", Stats["total"], " particles classified with ", Stats["truepos"],
		" true positives (", round(Stats["accuracy"], 1), "% accuracy)\n", sep ="")
	return(invisible(x))
}

"plot.ZIConf" <- function (x, y,
type = c("image", "tree_image", "precision_recall"), ...)
{
	type <- match.arg(type)
	res <- switch(type,
		image = confusion.map(x, ...),
		tree_image = confusion.tree(x, ...),
		precision_recall = confusion.bar(x, ...),
		stop("'type' must be 'image', 'tree_image', or 'precision_recall'"))
	return(invisible(res))
}

# These functions do the respective ZIConf graphs and are not exported!
"confusion.map" <- function (ZIConf, col = heat.colors(10),
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

# New function v1.2-2 using library gplots
"confusion.tree" <- function (ZIConf, maxval = 10, margin = c(2, 10),
Rowv = TRUE, Colv = TRUE)
{
	if (!inherits(ZIConf, c("ZIConf")))
		stop("'ZIConf' must be a 'ZIConf' object")
	nX <- nrow(ZIConf)
	nY <- ncol(ZIConf)
	nZ <- nX * nY
	confmat <- pmin(ZIConf, maxval)

	# Note: done in NAMESPACE
	# require(RColorBrewer)
	# require(gplots)
	mypalette <- brewer.pal(maxval - 1, "Spectral")
	heatmap.2(ZIConf, col= c(0, mypalette), symm = TRUE, margin = margin,
		trace = "both", Rowv = Rowv, Colv = Colv, cexRow = 0.2 + 1 / log10(nX),
		cexCol = 0.2 + 1 / log10(nY), tracecol = "Black", linecol = FALSE)
}

# New function v 1.2-2 false positive and negative
"confusion.bar" <- function (ZIConf, col = c("PeachPuff2", "green3", "lemonChiffon2"),
mar = c(1.1, 8.1, 4.1, 2.1), cex = 0.7, cex.axis = cex, cex.legend = 1.2 * cex,
main = "Precision (at left) versus recall (at right)")
{
	if (!inherits(ZIConf, c("ZIConf")))
		stop("'ZIConf' must be a 'ZIConf' object")
	TP <- diag(ZIConf)
	fn <- rowSums(ZIConf) - TP
	fp <- colSums(ZIConf) - TP
	# Express fn and fp in proportions
	FN <- fn <- fn / (fn + TP)
	FP <- fp <- fp / (TP + fp)
	FP[is.na(FP)] <- 1
	tp <- 1 - fn
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
	Labels <- names(attr(ZIConf, "nbr.cols"))
	# Order items from smallest to largest tp
	pos <- order(res[, 2], decreasing = TRUE)
	res <- res[pos, ]
	FN <- FN[pos]
	FP <- FP[pos]
	TP <- TP[pos]
	Labels <- Labels[pos]
	L <- length(FN)
	
	# Plot
	omar  <- par("mar")
	on.exit(par(omar)) # mar = margin size c(bottom, left, top, right)
	par(mar = mar)
	barplot(t(res), horiz = TRUE, col = col, xaxt = "n", las = 1, space = 0)
	#lines(rep((1:9) * 10, each = 3), rep(c(0, L, NA), 9), lty = 2)
	#abline(v = (1:9) * 10, lty = 2)
	lines(c(50, 50), c(0, L), lwd = 1)
	#abline(v = 50, lwd = 2)

	# Print the fraction of fp and fn
	text(rep(1, length(FP)), 1:L - 0.5,
		paste(round((1 - FP) * 100), "%", sep = ""),
		adj = c(0, 0.5), cex = cex)
	text(rep(99, length(FN)), 1:L - 0.5,
		paste(round((1 - FN) * 100), "%", sep = ""),
		adj = c(1, 0.5), cex = cex)

	# Print the number of true positives
	xpos <- res[, 1] + res[, 2] / 2 
	text(xpos, 1:L - 0.5, round(TP), adj = c(0.5, 0.5), cex = cex)

	# Add a legend
  	legend(50, L * 1.05, legend = c("false positive (FP)",
		"true positive (TP)", "false negative (FN)"), cex = cex.legend,
		xjust = 0.5, yjust = 1, fill = col, bty = "n", horiz = TRUE)
	axis(2, 1:L - 0.5, tick = FALSE, las = 1, cex.axis = cex.axis, labels = Labels)
	title(main = main)
	text(50, -1, "< higher precision TP/(TP+FP) - underestimate <=> overestimate - higher recall (TP/(TP+FN)) >  ",
		cex = cex)
	return(invisible(res))
}
