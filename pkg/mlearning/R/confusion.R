## TODO: add the possibility to droplevels() in confusion object!... or in print()/plot()?
confusion <- function (x, ...)
	UseMethod("confusion")

.confusion <- function (classes, labels, ...)
{
	if (!length(labels)) {
		labels <- c("Predicted", "Actual")
	} else {
		labels <- as.character(labels)
		if (length(labels) != 2)
			stop("You must provide exactly 2 character strings for 'labels'")
	}
	## Make sure both variables are correctly named
	names(classes) <- labels
	## How many objects by level?
	NbrPerClass1 <- table(classes[, 1])
	## How many predicted objects
	NbrPerClass2 <- table(classes[, 2])
	## Confusion matrix
	Conf <- table(classes)
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
	
	## This is a confusion object
	class(Conf) <- c("confusion", "table")
	Conf
}
	
confusion.default <- function (x, y = NULL, vars = c("Actual", "Predicted"),
labels = vars, merge.by = "Id", ...)
{	
	## If the object is already a 'confusion' object, return it
	if (inherits(x, "confusion")) {
		if (!missing(y))
			warning("you cannot provide 'y' when 'x' is already a 'confusion' object")
		return(x)
	}
	
	## Idem if there is a 'confusion' attribute and no y
	conf <- attr(x, "confusion")
	if (!is.null(conf) && missing(y)) return(conf)
	
	## Check/convert vars and labels
	if (!length(vars)) {
		vars <- c("Class", "Ident")
	} else {
		vars <- as.character(vars)
		if (length(vars) != 2)
			stop("You must provide exactly 2 strings for 'vars'")
	}
	merge.by <- as.character(merge.by)
	
	## There are three possibilities:
	## 1) a single data frame => use vars
	if (missing(y)) {
		## Special case of a data frame or list of two factors: keep as it is
		if (is.list(x) && length(x) == 2 && is.null(vars)) {
			clCompa <- as.data.frame(x)
			labels <- names(clCompa)
		} else {
			x <- as.data.frame(x)
			## Check that vars exist and levels of two vars do match
			if (is.null(names(x)) || !all(vars %in% names(x)))
				stop("'vars' are not among column names of 'x'")
			if (!all(sort(levels(x[[vars[1]]])) == sort(levels(x[[vars[2]]]))))
				stop("the levels of the two variables in 'x' do not match")
			clCompa <- data.frame(class1 = x[[vars[1]]], class2 = x[[vars[2]]])
		}
	} else { # y is provided
		## 2) two vectors of factors to compare (must have same length/same levels)
		if (is.factor(x) && is.factor(y)) {
			if (length(x) != length(x))
				stop("not the same number of items in 'x' and 'y'")
			if (!all(sort(levels(x))  == sort(levels(y))))
				stop("'x' and 'y' levels do not match")
			clCompa <- data.frame(class1 = y, class2 = x)
		} else {
			## 3) two data frames => merge first, then use vars
			## Check levels match
			## Note: if one is a subset of the other,
			## would it be possible to match them???
			if (is.null(names(x)) || !(vars[1] %in% names(x)))
				stop("first item of 'vars' is not among names of 'x'")
			if (is.null(names(y)) || !(vars[2] %in% names(y)))
				stop("second item of 'vars' is not among names of 'y'")
			if (!all(sort(levels(x[[vars[1]]]))  == sort(levels(y[[vars[2]]]))))
				stop("levels of the  two variables in 'x' and 'y' do not match")
			## Merge data according to merge.by
			clCompa <- merge(y[, c(vars[2], merge.by)], x[, c(vars[1], merge.by)],
				by = merge.by)
			clCompa <- clCompa[, c(ncol(clCompa) - 1, ncol(clCompa))]
			## Are there common objects left?
			if (nrow(clCompa) == 0)
				stop("no common objects between 'x' and 'y'")
		}
	}
	
	.confusion(clCompa, labels, ...)
}

confusion.mlearning <- function (x, y = response(x),
labels = c("Actual", "Predicted"), ...)
	.confusion(data.frame(class1 = y, class2 = predict(x, ...)),
		labels = labels, ...)

print.confusion <- function (x, error.col = TRUE, ...)
{
	## General stats on the confusion matrix
	Stats <- attr(x, "stats")
	cat(Stats["total"], " items classified with ", Stats["truepos"],
		" true positives (", round(Stats["accuracy"], 1), "% accuracy)\n",
		sep = "")
	
	## Print the confusion matrix itself
	X <- x
	class(X) <- "table"
	if (isTRUE(as.logical(error.col))) {
		print(cbind(X, `Error (FNR)` = round((1 - diag(X) / apply(X, 1, sum)), 3)))
	} else print(X)
	
	## Return the original object invisibly
	invisible(x)
}

plot.confusion <- function (x, y = NULL,
type = c("image", "barplot", "stars", "dendrogram"), stat1 = "Recall",
stat2 = "Precision", ...)
{
	if (is.null(y)) type <- match.arg(type)[1] else type <- "stars"
	res <- switch(type,
		image = .confusionImage(x, y, ...),
		barplot = .confusionBar(x, y, ...),
		stars = .confusionStars(x, y, stat1 = stat1, stat2 = stat2, ...),
		dendrogram = .confusionDendro(x, y, ...),
		stop("'type' must be 'image', 'barplot', 'stars' or 'dendrogram'"))
	invisible(res)
}

## These functions do the respective graphs for confusion objects
## Old (simpler) version
#.confusionImage <- function (x, y = NULL, col = heat.colors(10),
#mar = c(5.1, 12.1, 4.1, 2.1), ...)
#{
#	if (!inherits(x, "confusion"))
#		stop("'x' must be a 'confusion' object")

#	if (!is.null(y))
#		stop("cannot use a second classifier 'y' for this plot")
#	omar  <- par("mar")
#	on.exit(par(omar))
#    par(mar = mar)
#	n <- ncol(x)
#	image(1:n, 1:n, 1 / (t(x[n:1, 1:n])), col = col, xlab = "", ylab = "",
#		xaxt = "n", yaxt = "n", ...)
#    axis(1, at = 1:n, las = 2)
#    axis(2, at = n:1, labels = paste(names(attr(x, "nbr.cols")), 1:n),
#		las = 1)
#    abline(h = (1:(n + 1)) - 0.5, lty = 2, col = "gray")
#    abline(v = (1:(n + 1)) - 0.5, lty = 2, col = "gray")
#	invisible(x)
#}

## Representation of the confusion matrix
.confusionImage <- function (x, y = NULL, mar = c(3.1, 10.1, 3.1, 3.1), asp = 1, 
label = "Actual \\ Predicted", sort = "complete", cex = 1, colfun = NULL,
ncols = 41, col0 = FALSE, grid.col = "gray", ...)
{
	if (!inherits(x, "confusion"))
        stop("'x' must be a 'confusion' object")

	if (!is.null(y))
		stop("cannot use a second classifier 'y' for this plot")
	
	## Default color function
	if (!length(colfun)) colfun <- function (n, alpha = 1, s = 0.9, v = 0.9) {
		if ((n <- as.integer(n[1L])) <= 0) return(character(0L))
		## Define the initial (red) and final (blue) colors with white in between
		cols <- c(hsv(h = 0, s = s, v = v, alpha = alpha),   # Red
				  hsv(h = 0, s = 0, v = v, alpha = alpha),   # White
				  hsv(h = 2/3, s = s, v = v, alpha = alpha)) # Blue
		## Use a color ramp from red to white to blue
		return(colorRampPalette(cols)(n))
	}
	
    manuLev <- sub("...", "", rownames(x))
    autoLev <- manuLev
    n <- ncol(x)

	## Do we sort items?
	if (length(sort) && !is.na(sort) && sort != FALSE && sort != "") {
		## Grouping of items
		confuSim <- x + t(x)
		confuSim <- max(confuSim) - confuSim
		confuDist <- structure(confuSim[lower.tri(confuSim)], Size = n,
			Diag = FALSE, Upper = FALSE, method = "confusion", call = "",
			class = "dist")
		order <- hclust(confuDist, method = sort)$order
		x <- x[order, order]
		autoLev <- autoLev[order]
		manuLev <- manuLev[order]
	}
	## Recode levels so that a number is used in front of manu labels
	## and shown in auto
	autoLev <- formatC(1:length(autoLev), width = 2, flag = "0")
	manuLev <- paste(manuLev, autoLev, sep = "-")
	row.names(x) <- manuLev
	colnames(x) <- autoLev
	## Calculate colors (use a transfo to get 0, 1, 2, 3, 4, 7, 10, 15, 25+)
	confuCol <- x
	confuCol <- log(confuCol + .5) * 2.33
	confuCol[confuCol < 0] <- if (isTRUE(as.logical(col0))) 0 else NA
	confuCol[confuCol > 10] <- 10
	## Negative values (in blue) on the diagonal (correct IDs)
	diag(confuCol) <- -diag(confuCol)	
	## Make an image of this matrix
	opar <- par(no.readonly = TRUE)
	on.exit(par(opar))
	par(mar = mar, cex = cex)
	image(1:n, 1:n, -t(confuCol[nrow(confuCol):1, ]), zlim = c(-10, 10),
		asp = asp, bty = "n", col = colfun(ncols), xaxt = "n", yaxt = "n",
		xlab = "", ylab = "", main = "", ...)
	## Print the actual numbers
	confuTxt <- as.character(x[n:1, ])
	confuTxt[confuTxt == "0"] <- ""
	text(rep(1:n, each = n), 1:n, labels = confuTxt)
	## The grid
	abline(h = 0:n + 0.5, col = grid.col)
	abline(v = 0:n + 0.5, col = grid.col)
	## The axis labels
	axis(1, 1:n, labels = autoLev, tick =  FALSE, padj = 0)
	axis(2, 1:n, labels = manuLev[n:1], tick =  FALSE, las = 1, hadj = 1)
	axis(3, 1:n, labels = autoLev, tick =  FALSE) #, cex.lab = cex)
	axis(4, 1:n, labels = autoLev[n:1], tick =  FALSE, las = 1, hadj = 0)
	## Legend at top-left
	mar[2] <- 1.1
	par (mar = mar, new = TRUE)
	plot(0, 0, type = "n", xaxt = "n", yaxt = "n", bty = "n")
	mtext(label, adj = 0, line = 1, cex = cex)
	## Return the confusion matrix, as displayed, in text format
	invisible(x)
}

## Eliminated to avoid dependency on RColorBrewer and gplots!
#.confusionTree <- function (x, y = NULL, maxval = 10, margins = c(2, 10),
#row.v = TRUE, col.v = TRUE, ...)
#{
#	if (!inherits(x, "confusion"))
#		stop("'x' must be a 'confusion' object")
#	if (!is.null(y))
#		stop("cannot use a second classifier 'y' for this plot")
#
#	nX <- nrow(x)
#	nY <- ncol(x)
#	nZ <- nX * nY
#	confmat <- pmin(x, maxval)
#	mypalette <- brewer.pal(maxval - 1, "Spectral")
#	heatmap.2(x, col= c(0, mypalette), symm = TRUE, margins = margins,
#		trace = "both", Rowv = row.v, Colv = col.v, cexRow = 0.2 + 1 / log10(nX),
#		cexCol = 0.2 + 1 / log10(nY), tracecol = "Black", linecol = FALSE, ...)
#}

# Confusion bar with recall and precision in green bar and not outside as before
# function modified for publication hclust 
.confusionBar <- function (x, y = NULL,
col = c("PeachPuff2", "green3", "lemonChiffon2"), mar = c(1.1, 8.1, 4.1, 2.1),
cex = 0.7, cex.axis = cex, cex.legend = cex, main = "Precision versus Recall",
min.width = 17, ...)
{
    if (!inherits(x, "confusion"))
        stop("'x' must be a 'confusion' object")
		
	if (!is.null(y))
		stop("cannot use a second classifier 'y' for this plot")
	
    TP <- diag(x)
    fn <- rowSums(x) - TP
    fp <- colSums(x) - TP
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
    Labels <- names(attr(x, "nbr.cols"))
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
	## Plot the graph
    barplot(t(res), horiz = TRUE, col = col, xaxt = "n", las = 1, space = 0, ...)
    lines(c(50, 50), c(0, L), lwd = 1)
    xpos <- res[, 1] + res[, 2]/2
    text(xpos, 1:L - 0.5, round(TP), adj = c(0.5, 0.5), cex = cex)
    # Add recall and precision if enough place to print it...
    NotPlace <- res[,"tp"] <= min.width
    if (any(NotPlace)) {
		## Special case if not enough place to print precision and recall
		## Add Precision
		PrecTxt <- paste(round((1 - FP) * 100), "%", sep = "")
		PrecTxt[NotPlace] <- ""
		text(res[,"fp"] + 1, 1:L - 0.5, PrecTxt, adj = c(0, 0.5), cex = cex)
		## Add FDR
		FDTxt <- paste(round((FP) * 100), "%", sep = "")
		FDTxt[!NotPlace] <- ""
		text(rep(1, length(FP)), 1:L - 0.5, FDTxt, adj = c(0, 0.5), cex = cex)
		## Add Recall
		RecTxt <- paste(round((1 - FN) * 100), "%", sep = "")
		RecTxt[NotPlace] <- ""
		text(res[,"fp"] + res[, "tp"] - 5, 1:L - 0.5, RecTxt, adj = c(0, 0.5),
			cex = cex)
		## Add FN
		FNTxt <- paste(round((FN) * 100), "%", sep = "")
		FNTxt[!NotPlace] <- ""
		text(rep(99, length(FN)), 1:L - 0.5, FNTxt, adj = c(1, 0.5), cex = cex)
    } else {
		## Add Precision
		text(res[,"fp"] + 1, 1:L - 0.5, paste(round((1 - FP) *
			100), "%", sep = ""), adj = c(0, 0.5), cex = cex)
		## Add Recall
		text(res[,"fp"] + res[, "tp"] - 5, 1:L - 0.5, paste(round((1 - FN) *
			100), "%", sep = ""), adj = c(0, 0.5), cex = cex)
    }
    legend(50, L * 1.05, legend = c("False Discovery : (1 - Prec.)",
		"True Positive (TP)", "False Negative : (1 - Rec.)"), cex = cex.legend,
		xjust = 0.5, yjust = 1, fill = col, bty = "n", horiz = TRUE)
    axis(2, 1:L - 0.5, tick = FALSE, las = 1, cex.axis = cex.axis,
		labels = Labels)
    title(main = main)
    text(50, -0.5, "< higher precision : TP/(TP+FP) - underestimate <=> overestimate - higher recall : TP/(TP+FN) >  ",
        cex = cex)
    invisible(res)
}

## Precision vs Recall, alternate presentation
## Note used, but saved for now
#.confusionBar <- function (x, y = NULL,
#col = c("PeachPuff2", "green",  "green3", "lemonChiffon2"),
#mar = c(2.1, 8.1, 4.1, 2.1), cex = 0.7, cex.axis = cex, cex.legend = cex,
#main = "Precision versus Recall", ...)
#{
#	if (!inherits(x, "confusion"))
#		stop("'x' must be a 'confusion' object")
#	if (!is.null(y))
#		stop("cannot use a second classifier 'y' for this plot")
#
#	## Calculation of statistics
#	Stats <- summary(x)
#	FDR <- Stats$FDR * 100
#	Precision <- Stats$Precision * 100
#	Recall <- Stats$Recall * 100
#	FNR <- Stats$FNR * 100
#	## Order statistics according to Precision + recall
#	pos <- order(Recall + Precision, decreasing = TRUE)
#	## Results to plot
#	res <- cbind(FDR, Precision, Recall, FNR)
#	## Do the plot
#	omar <- par("mar")
#	on.exit(par(omar))
#	par(mar = mar)
#	barplot(t(res[pos, ]), horiz = TRUE, col = col, xaxt = "n", las = 1,
#		space = 0, ...)
#	## Add information
#	n <- nrow(Stats)
#	Labels <- names(attr(x, "nbr.cols"))
#	axis(2, 1:n - 0.5, tick = FALSE, las = 1, cex.axis = cex.axis,
#		labels = Labels[pos])
#	title(main = main)
#	text(rep(1, n), 1:n - 0.5, paste(round(FDR[pos]), "%", sep = ""),
#		adj = c(0, 0.5), cex = cex)
#	text(FDR[pos] + Precision[pos]/2, 1:n - 0.5, paste(round(Precision[pos]),
#		"%", sep = ""), adj = c(0, 0.5), cex = cex)
#	text(FDR[pos] + Precision[pos] + Recall[pos]/2, 1:n - 0.5,
#		paste(round(Recall[pos]), "%", sep = ""), adj = c(0, 0.5), cex = cex)
#	text(rep(191, n), 1:n - 0.5, paste(round(FNR[pos]), "%", sep = ""),
#		adj = c(0, 0.5), cex = cex)
#	legend("top", legend = c("False Discovery ", "Precision", "Recall",
#		"False Negative"), cex = cex.legend, fill = col, bty = "n", horiz = TRUE)
#	text(96, -0.5, "< higher precision - underestimate <=> overestimate - higher recall >  ",
#		cex = cex)
#	invisible(res)
#}

.confusionStars <- function(x, y = NULL, stat1 = "Recall", stat2 = "Precision",
main = NULL, col = NULL, ...)
{
    if (!inherits(x, "confusion"))
        stop("'x' must be a 'confusion' object")
    if (!is.null(y) && !inherits(x, "confusion"))
        stop("'y' must be NULL or a 'confusion' object")
	
	SupportedStats <- c("Recall", "Precision", "Specificity",
        "NPV", "FPR", "FNR", "FDR", "FOR")
    if (!stat1 %in% SupportedStats)
        stop("stats1 must be one of followed stats: Recall, Precision, Specificity, NPV, FPR, FNR, FDR, FOR")
    if (!stat2 %in% SupportedStats)
        stop("stats2 must be one of followed stats: Recall, Precision, Specificity, NPV, FPR, FNR, FDR, FOR")
    Blue <- topo.colors(16)
    Green <- terrain.colors(16)
    Stat <- summary(x)
    if (!is.null(y)) { # Comparison of two confusion matrices
		Stat2 <- summary(y)
		Data <- data.frame(Stat2[, stat1], Stat[, stat1], Stat[, stat2],
			Stat2[, stat2])
		Data <- rbind(Data, rep(0, 4))
		colnames(Data) <- paste(rep(c(stat1, stat2), each = 2), c(2, 1, 1, 2))
		if (!length(main))
			main <- paste("Groups comparison between classifier 1 and 2\nAccuracy 1 =",
				round(attr(Stat, "Accuracy") * 100), "%, accuracy 2 =",
				round(attr(Stat2, "Accuracy") * 100), "%")
		if (!length(col))
			col <- c("green", Green[1], Blue[2], Blue[6])
	} else { # Single confusion matrix
		Data <- data.frame(Stat[, stat1], Stat[, stat2])
		Data <- rbind(Data, rep(0, 2))
		colnames(Data) <- c(stat1, stat2)
		if (!length(main))
			main <- paste("Groups comparison\nAccuracy =",
				round(attr(Stat, "Accuracy") * 100), "%")
		if (!length(col))
			col <- c(Green[1], Blue[2])
	}
    rownames(Data) <- c(rownames(Stat), " ")
    
	## Calculate key location
		kl <- stars(Data, draw.segments = TRUE, scale = FALSE, # key.loc = c(13, 1.5),
			len = 0.8, main =  main, col.segments = col, plot = FALSE, ...)
		kcoords <- c(max(kl[, 1]), min(kl[, 2]))
		kspan <- apply(kl, 2, min) / 1.95
	## Draw the plot	
	res <- stars(Data, draw.segments = TRUE, scale = FALSE, key.loc = kcoords,
		len = 0.8, main =  main, col.segments = col, ...)
	## Draw a rectangle around key to differentiate it from the rest
	rect(kcoords[1] - kspan[1], kcoords[2] - kspan[2], kcoords[1] + kspan[1],
		kcoords[2] + kspan[2])
	
	res
}

## Representation of the confusion matrix as a dendrogram
.confusionDendro <- function (x, y = NULL, method = "ward")
{
    if (!inherits(x, "confusion"))
        stop("'x' must be a 'confusion' object")
	if (!is.null(y))
		stop("cannot use a second classifier 'y' for this plot")	
	
    ## Transform the confusion matrix into a symmetric matrix by adding its
	## transposed matrix
    ConfuSim <- x + t(x)
    ConfuSim <- max(ConfuSim) - ConfuSim
    ## Create the structure of a "dist" object
    ConfuDist <- structure(ConfuSim[lower.tri(ConfuSim)], Size = nrow(x),
        Diag = FALSE, Upper = FALSE, method = "confusion", call = "",
        class = "dist")
    ## method :"ward", "single", "complete", "average", "mcquitty",
	## "median" or "centroid"
    HC <- hclust(ConfuDist, method = method)
    plot(HC, labels = rownames(x))
    invisible(HC)
}

## Table with stats per groupe precision, recall, etc
summary.confusion <- function(object, sort.by = NULL, decreasing = FALSE,
na.rm = FALSE, ...)
{
    ## General parameters
    ## Number of groups
    Ngp <- ncol(object)
    
    ## Total : TP + TN + FP + FN
    Tot <- sum(object)
    
    ## TP : True positive item : All items on diagonal
    TP <- diag(object)
    
    ## TP + TN : sum of diagonal = All correct identification
    TP_TN <- sum(TP)
    
    ## TP + FP : sum of columns : Automatic classification
    TP_FP <- colSums(object)
    
    ## TP + FN : sum of rows : Manual classification
    TP_FN <- rowSums(object)
    
    ## FP : False positive items
    FP <- TP_FP - TP    

    ## FN : False negative item
    FN <- TP_FN - TP

    ## TN : True Negative = Total - TP - FP - FN
    TN <- rep(Tot, Ngp) - TP - FP - FN
    
    ## General statistics
    ## Accuracy = (TP + TN) / (TP + TN + FP + FN)
    Accuracy <- TP_TN / Tot
    
    ## Error = 1 - Accuracy
    Error <- 1 - Accuracy
    
    ## The 8 basic ratios
    ## Recall = TP / (TP + FN) = 1 - FNR
    Recall <- TP / (TP_FN)

    ## Specificity = TN / (TN + FP) = 1 - FPR
    Specificity <- TN / (TN + FP)

    ## Precision = TP / (TP + FP) = 1 - FDR
    Precision <- TP / (TP_FP)
    
    ## NPV : Negative predicted value = TN / (TN + FN) = 1 - FOR
    NPV <- TN / (TN + FN)
    
    ## FPR : False positive rate = 1 - Specificity = FP / (FP + TN) 
    FPR <- FP / (FP + TN) #1 - Specificity
    
    ## FNR : False negative rate = 1 - Recall = FN / (TP + FN)
    FNR <- FN / (TP + FN) #1 - Recall

    ## FDR : False Discovery Rate = 1 - Precision = FP / (TP + FP)
    FDR <- FP / (TP_FP) #1 - Precision
    
    ## FOR : False omission rate = 1 - NPV = FN / (FN + TN)
    FOR <- FN / (FN + TN) #1 - NPV

    ## The 4 ratios of ratios
    ## LRPT = Likelihood Ratio for Positive Tests = Recall / FPR = Recall /
	## (1 - Specificity)
    LRPT <- Recall / (FPR)
    
    ## LRNT = Likelihood Ratio for Negative Tests = FNR / Specificity =
	## (1 - Recall) / Specificity
    LRNT <- FNR / (Specificity)
    
    ## LRPS : Likelihood Ratio for Positive Subjects = Precision / FOR =
	## Precision / (1 - NPV)
    LRPS <- Precision / (FOR)
    
    ## LRNS : Likelihood Ratio Negative Subjects = FDR / NPV = (1 - Precision) /
	## (1 - FOR)
    LRNS <- FDR / (NPV)
    
    ## Additional statistics
    ## F-measure = F1 score = Harmonic mean of Precision and recall
    Fmeasure <- 2 * ((Precision * Recall) / (Precision + Recall))
    
    ## Balanced accuracy = (Sensitivity + Specificity) / 2
    BalAcc <- (Recall + Specificity) / 2

    ## MCC : Matthews correlation coefficient
    Sum1 <- TP + FP
    Sum2 <- TP + FN
    Sum3 <- TN + FP
    Sum4 <- TN + FN
    Denominator <- sqrt(Sum1 * Sum2 * Sum3 * Sum4)
    ZeroIdx <- Sum1 == 0 | Sum2 == 0 | Sum3 == 0 | Sum4 == 0
    if (any(ZeroIdx)) Denominator[ZeroIdx] <- 1
    MCC <- ((TP * TN) - (FP * FN)) / Denominator
    
    ## Chisq : Significance
    Chisq <- (((TP * TN) - (FP * FN))^2 * (TP + TN + FP + FN)) /
		((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))

    ## Automatic classification - Manual calssification
    Auto_Manu <- TP_FP - TP_FN
    
    ## Bray-Curtis dissimilarity index
    Dissimilarity <- abs(Auto_Manu) / (sum(TP_FP) + sum(TP_FN))
    
    res <- data.frame(
	   Auto = TP_FP, Manu = TP_FN, Auto_Manu = Auto_Manu,
	   Dissimilarity = Dissimilarity, TP = TP, FP = FP, FN = FN, TN = TN,
	   Recall = Recall, Precision = Precision,	Specificity = Specificity,
	   NPV = NPV, FPR = FPR, FNR = FNR, FDR = FDR, FOR = FOR, LRPT = LRPT,
	   LRNT = LRNT, LRPS = LRPS, LRNS = LRNS, Fmeasure = Fmeasure,
	   BalAcc = BalAcc, MCC = MCC, Chisq = Chisq)

    rownames(res) <- rownames(object)
    ## Sort the table in function of one parameter... by default FN
    if (!is.null(sort.by))
	   res <- res[order(res[, sort.by], decreasing = decreasing), ]
    attr(res, "Accuracy") <- Accuracy
    attr(res, "Error") <- Error
    
    ## Kevin, je comprend rien a tout ce code. Tu as deja injecte les resultats
	## dans res. Donc, tout ce que tu fais ci-dessous n'est PAS repercute dans
	## le resultat final renvoye par la fonction!!!
	## Remove NaN if any 0/0
    if (isTRUE(as.logical(na.rm))) {
    	## Cases where it is impossible to calculate some statistics: 0/0 or X/0
    	## Case 1 : Everything is Correct -> FP = 0, FN = 0
    	Case1Idx <- FP == 0 & FN == 0 # not any error!
    	if (any(Case1Idx)) {
    	    Recall[Case1Idx] <- 1
    	    FNR[Case1Idx] <- 0
    	    Precision[Case1Idx] <- 1
    	    FDR[Case1Idx] <- 0
    	    Specificity[Case1Idx] <- 1
    	    FPR[Case1Idx] <- 0
    	    NPV[Case1Idx] <- 1
    	    FOR[Case1Idx] <- 0
    	}
    	## Case 2 : Everything is Wrong and only false positive
		## -> Impossible to calculate Recall and NPV
    	Case2Idx <- TP == 0 & TN == 0 & FP > 0 & FN == 0
    	if (any(Case2Idx)) {
    #	    Recall[Case2Idx] <- 0
    #	    FNR[Case2Idx] <- 1
    #	    NPV[Case2Idx] <- 0
    #	    FOR[Case2Idx] <- 1
    	    Recall[Case2Idx] <- 1
    	    FNR[Case2Idx] <- 0
    	    NPV[Case2Idx] <- 1
    	    FOR[Case2Idx] <- 0
    	}
    	## Case 3 : Everything is Wrong and only false negative
		## -> Impossible to calculate Precision and Specificity
    	Case3Idx <- TP == 0 & TN == 0 & FP == 0 & FN > 0
    	if (any(Case3Idx)) {
    #	    Precision[Case3Idx] <- 0
    #	    FDR[Case3Idx] <- 1
    #	    Specificity[Case3Idx] <- 0
    #	    FPR[Case3Idx] <- 1
    	    Precision[Case3Idx] <- 1
    	    FDR[Case3Idx] <- 0
    	    Specificity[Case3Idx] <- 1
    	    FPR[Case3Idx] <- 0
    	}
    	## Case 4 : No FP and No TN -> Impossible to calculate Specificity
    	Case4Idx <- TP > 0 & TN == 0 & FP == 0 & FN > 0
    	if (any(Case4Idx)) {
    #	    Specificity[Case4Idx] <- 0
    #	    FPR[Case4Idx] <- 1
    	    Specificity[Case4Idx] <- 1
    	    FPR[Case4Idx] <- 0
    	}
    	## Case 5 : No TP and No FP -> Impossible to calculate Precision
    	Case5Idx <- TP == 0 & TN > 0 & FP == 0 & FN > 0
    	if(any(Case5Idx)){
    #	    Precision[Case5Idx] <- 0
    #	    FDR[Case5Idx] <- 1
    	    Precision[Case5Idx] <- 1
    	    FDR[Case5Idx] <- 0
    	}
    	## Case 6 : No TP and no FN -> Impossible to calculate Recall
    	Case6Idx <- TP == 0 & TN > 0 & FP > 0 & FN == 0
    	if (any(Case6Idx)) {
    #	    Recall[Case6Idx] <- 0
    #	    FNR[Case6Idx] <- 1
    	    Recall[Case6Idx] <- 1
    	    FNR[Case6Idx] <- 0
    	}
    	## Case 7 : No TN and no FN
		## -> Impossible to calculate Negative predicted value
    	Case7Idx <- TP > 0 & TN == 0 & FP > 0 & FN == 0
    	if (any(Case7Idx)) {
    #	    NPV[Case7Idx] <- 0
    #	    FOR[Case7Idx] <- 1
    	    NPV[Case7Idx] <- 1
    	    FOR[Case7Idx] <- 0
    	}
    }
    class(res) <- c("summary.confusion", "data.frame")
	res
}

print.summary.confusion <- function (x, ...)
{
	## TODO: be more verbous and indicate more data here!
	cat("Accuracy: ", round(attr(x, "Accuracy") * 100, digits = 2),
		"%\n", "Error: ", round(attr(x, "Error") * 100, digits = 2),
		"%\n\n", sep = "")
	X <- x
	class(X) <- "data.frame"
	print(X)
	return(invisible(x))
}

#comparisonPlot <-
#function (x, y, stat1 = "Recall", stat2 = "Precision", type = c("barplot", "p", "stars"), ...)
#{
#    type <- match.arg(type)
#    res <- switch(type[1], barplot = barplot.comparison(x, y, stat1, stat2, ...),
#        p = plot.comparison(x, y, stat1, stat2, ...),
#        stars = stars.comparison(x, y, stat1, stat2, ...), stop("'type' must be 'barplot', 'p' or 'stars'"))
#    invisible(res)
#}
#
#stars.comparison <- function(x, y, stat1 = "Recall", stat2 = "Precision", ...)
#{
#    if(!inherits(x, "summary.confusion"))
#        stop("x must be a summary.confusion object")
#    if(!inherits(y, "summary.confusion"))
#        stop("y must be a summary.confusion object")
#    SupportedStats <- c("Recall", "Precision", "Specificity",
#        "NPV", "FPR", "FNR", "FDR", "FOR")
#    if (!stat1 %in% SupportedStats)
#        stop("stats1 must be one of followed stats: Recall, Precision, Specificity, NPV, FPR, FNR, FDR, FOR")
#    if (!stat2 %in% SupportedStats)
#        stop("stats2 must be one of followed stats: Recall, Precision, Specificity, NPV, FPR, FNR, FDR, FOR")
#    Blue <- topo.colors(16)
#    Green <- terrain.colors(16)
#    Data <- data.frame(y[, stat1], x[, stat1], x[, stat2], y[, stat2])
#    rownames(Data) <- rownames(x)
#    colnames(Data) <- c(paste(stat1, "_2", sep = ""), paste(stat1, "_1", sep = ""), paste(stat2, "_1", sep = ""), paste(stat2, "_2", sep = ""))
#    stars(Data, draw.segments = TRUE, scale = FALSE, key.loc = c(13,1.5), len = 0.8,
#        main = paste("Groups comparison between classifier 1 and 2", "\n", "Accuracy 1 =", round(attr(Stats, "Accuracy") *100), "%,",  "Accuracy 2 =", round(attr(Stats2, "Accuracy") *100), "%"),
#        col.segments = c("green", Green[1], Blue[2], Blue[6]), ...)
#}

#barplot.comparison <- function(x, y, stat1 = "Recall", stat2 = "Precision", ...){
#    SupportedStats <- c("Recall", "Precision", "Specificity",
#        "NPV", "FPR", "FNR", "FDR", "FOR")
#    if (!stat1 %in% SupportedStats)
#        stop("stats1 must be one of followed stats: Recall, Precision, Specificity, NPV, FPR, FNR, FDR, FOR")
#    if (!stat2 %in% SupportedStats)
#        stop("stats2 must be one of followed stats: Recall, Precision, Specificity, NPV, FPR, FNR, FDR, FOR")
#    n <- nrow(x)
#    xstat1 <- x[, stat1]
#    xstat2 <- x[, stat2]
#    ystat1 <- y[, stat1]
#    ystat2 <- y[, stat2]
#    barplot(xstat1, ylim = c(-1.05, 1.15), axes = FALSE,
#            ylab = paste("<==", stat2, "/", stat1, "==>", sep = " "),
#            xlab = "Groups", main = "Comparison of two statistics for two classifiers", ...)
#    barplot(-xstat2, add = TRUE, axes = FALSE)
#    for (i in 1:n) abline(v = i + i * 0.2 - 0.5, lty = 3,
#        col = "lightgray")
#    abline(h = 0, lty = 1)
#    abline(h = 0.25, lty = 2)
#    abline(h = 0.5, lty = 2)
#    abline(h = 0.75, lty = 2)
#    abline(h = 1, lty = 3)
#    abline(h = -0.25, lty = 2)
#    abline(h = -0.5, lty = 2)
#    abline(h = -0.75, lty = 2)
#    abline(h = -1, lty = 3)
#    X <- 1:n + 1:n * 0.2 - 0.5
#    suppressWarnings(arrows(x0 = X, y0 = xstat1, x1 = X,
#        y1 = ystat1, length = 0.1))
#    suppressWarnings(arrows(x0 = X, y0 = -xstat2, x1 = X,
#        y1 = -ystat2, length = 0.1))
#    axis(1, at = X, labels = 1:n)
#    axis(2, at = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5,
#        0.75, 1), labels = c(1, 0.75, 0.5, 0.25, 0, 0.25,
#        0.5, 0.75, 1))
#    legend("topright", legend = c("Classifier 1", "Classifier 2"),
#        pch = c(15, 4), col = c("darkgray", "black"), horiz = TRUE,
#        bg = "white", cex = 0.75, pt.cex = 1.5, pt.lwd = 2)
#    invisible(list(xstat1 = xstat1, xstat2 = xstat2, ystat1 = ystat1,
#        ystat2 = ystat2))
#}
#
#plot.comparison <- function(x, y, stat1 = "Recall", stat2 = "Precision", ...){
#    SupportedStats <- c("Recall", "Precision", "Specificity",
#        "NPV", "FPR", "FNR", "FDR", "FOR")
#    if (!stat1 %in% SupportedStats)
#        stop("stats1 must be one of followed stats: Recall, Precision, Specificity, NPV, FPR, FNR, FDR, FOR")
#    if (!stat2 %in% SupportedStats)
#        stop("stats2 must be one of followed stats: Recall, Precision, Specificity, NPV, FPR, FNR, FDR, FOR")
#    n <- nrow(x)
#    xstat1 <- x[, stat1]
#    xstat2 <- x[, stat2]
#    ystat1 <- y[, stat1]
#    ystat2 <- y[, stat2]
#    plot(xstat1, ylim = c(-1, 1.1), ylab = paste("<==", stat2,
#        "/", stat1, "==>", sep = " "), xlab = "Groups", axes = FALSE,
#        col = "red", main = "Comparison of two statistics for two classifiers",
#        lwd = 2, cex = 1.5, pch = 3, ...)
#    points(ystat1, pch = 4, col = "blue", lwd = 2, cex = 1.5)
#    points(-xstat2, pch = 3, col = "red", lwd = 2, cex = 1.5)
#    points(-ystat2, pch = 4, col = "blue", lwd = 2, cex = 1.5)
#    for (i in 1:n) abline(v = i, lty = 3, col = "lightgray")
#    abline(h = 0, lty = 1)
#    abline(h = 0.25, lty = 2)
#    abline(h = 0.5, lty = 2)
#    abline(h = 0.75, lty = 2)
#    abline(h = -0.25, lty = 2)
#    abline(h = -0.5, lty = 2)
#    abline(h = -0.75, lty = 2)
#    axis(1, at = 1:n, labels = 1:n)
#    axis(2, at = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5,
#        0.75, 1), labels = c(1, 0.75, 0.5, 0.25, 0, 0.25,
#        0.5, 0.75, 1))
#    legend("topright", legend = c("Classifier 1", "Classifier 2"),
#        pch = c(3, 4), col = c("red", "blue"), horiz = TRUE,
#        bg = "white", cex = 0.75, pt.cex = 1.5, pt.lwd = 2)
#    invisible(list(xstat1 = xstat1, xstat2 = xstat2, ystat1 = ystat1,
#        ystat2 = ystat2))
#}
