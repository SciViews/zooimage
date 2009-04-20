# Copyright (c) 2004, Ph. Grosjean <phgrosjean@sciviews.org>
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


"process.sample" <-
	function(ZidFile, ZIClass, ZIDesc,
		abd.taxa = NULL, abd.groups = NULL, abd.type = "absolute",
		bio.taxa = NULL, bio.groups = NULL, bio.conv = c(1, 0, 1), headers = c("Abd", "Bio"),
		spec.taxa = NULL, spec.groups = NULL, spec.breaks = seq(0.25, 2, by = 0.1), spec.use.Dil = TRUE,
		exportdir = NULL, show.log = TRUE) {
    # Check if the ZidFile exists
	if (!file.exists(ZidFile)) {
		logProcess("file not found!", ZidFile, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Check if ZIClass is of the right class
	mustbe(ZIClass, "ZIClass")
	# Get ZIDat from the ZidFile
	ZIDat <- read.zid(ZidFile)
	Sample <- get.sampleinfo(ZidFile, type = "sample", ext = "[.][zZ][iI][dD]$")
	# Check if one can get sample metadata from ZIDesc
	RES <- ZIDesc[ZIDesc$Label == Sample, ] 
	if (nrow(RES) != 1) {
		logProcess("ZIDesc has no data for that sample!", Sample, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Predict classes (add a new column Ident to the table)
	ZIDat <- predict(ZIClass, ZIDat)
	
	Grp <- levels(ZIDat$Ident)	
	if (is.null(abd.groups)) {
		# Calculate groups (list with levels to consider)
		abd.groups <- as.list(c("", Grp))
		names(abd.groups) <- c("total", Grp)
	}
	# Process abundances
	ABD <- Abd.sample(ZIDat, Sample, taxa = abd.taxa, groups = abd.groups, type = abd.type,
		header = headers[1])
	RES <- cbind(RES, t(ABD))
	# Process biomasses
	if (!is.null(bio.conv)) {
		if (is.null(bio.groups)) {
			# Calculate groups (list with levels to consider)
			bio.groups <- as.list(c("", Grp))
			names(bio.groups) <- c("total", Grp)
		}
        BIO <- Bio.sample(ZIDat, Sample, taxa = bio.taxa, conv = bio.conv,
			groups = bio.groups, header = headers[2], exportdir = exportdir)
		RES <- cbind(RES, t(BIO))
	}
	# Process size spectra
	if (!is.null(spec.breaks)) {
		if (is.null(spec.groups)) {
			# Calculate groups (list with levels to consider)
			spec.groups <- as.list(c("", Grp))
			names(spec.groups) <- c("total", Grp)
		}
		SPC <- Spectrum.sample(ZIDat, Sample, taxa = spec.taxa, groups = spec.groups,
			breaks = spec.breaks, use.Dil = spec.use.Dil)
		SPClist <- list()
		SPClist[[Sample]] <- SPC
		attr(RES, "spectrum") <- SPClist
	}
	attr(RES, "metadata") <- attr(ZIDesc, "metadata")
	class(RES) <- c("ZI1Res", "ZIRes", "data.frame")
	return(RES)
}

"process.samples" <-
	function(path = ".", ZidFiles = NULL, ZIClass, ZIDesc = read.description("Description.zis"),
	abd.taxa = NULL, abd.groups = NULL, abd.type = "absolute",
	bio.taxa = NULL, bio.groups = NULL, bio.conv = c(1, 0, 1), headers = c("Abd", "Bio"),
	spec.taxa = NULL, spec.groups = NULL, spec.breaks = seq(0.25, 2, by = 0.1), spec.use.Dil = TRUE,
	exportdir = NULL, show.log = TRUE, bell = FALSE) {

	# Determine which samples do we have to process...
	if (is.null(ZidFiles)) {
    	# Get the list of files from ZIDesc
		ZidFiles <- paste(ZIDesc$Label, ".zid", sep = "")
		if (path == ".") path = getwd()
		ZidFiles <- file.path(path, ZidFiles)
	} else { # Check that all zid files have entries in ZIDesc
		Samples <- get.sampleinfo(ZidFiles, type = "sample", ext = "[.][zZ][iI][dD]$$")
		if (!all(Samples %in% ZIDesc$Label)) {
			logProcess("One or more samples not in ZIDesc!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	}
	# Start the process
	logClear()
	ok <- TRUE
	restot <- NULL
	imax <- length(ZidFiles)
	cat("Processing",  imax, "samples...\n")
	logProcess(paste("Processing",  imax, "samples..."))
	for (i in 1:imax) {
       	Progress(i, imax)
		res <- process.sample(ZidFiles[i], ZIClass = ZIClass, ZIDesc = ZIDesc,
			abd.taxa = abd.taxa, abd.groups = abd.groups, abd.type = abd.type,
			bio.taxa = bio.taxa, bio.groups = bio.groups, bio.conv = bio.conv, headers = headers,
			spec.taxa = spec.taxa, spec.groups = spec.groups, spec.breaks = spec.breaks, spec.use.Dil = spec.use.Dil,
            exportdir = exportdir, show.log = FALSE)
		if (is.logical(res) && !res) { # Error
           	logProcess("Error!", ZidFiles[i])
			ok <- FALSE
		} else if (is.null(restot)) {
			logProcess("OK", ZidFiles[i])
			restot <- res
		} else {
        	logProcess("OK", ZidFiles[i])
			# Append res to restot
			restot <- rbind(restot, res)
			attr(restot, "spectrum") <- c(attr(restot, "spectrum"), attr(res, "spectrum"))
			attr(restot, "metadata") <- attr(res, "metadata")
			class(restot) <- c("ZI1Res", "ZIRes", "data.frame") 
		}
	}
	Progress (imax + 1, imax)	 # To dismiss the Progress() indication
	
	# {{{ Final report
	finish.loopfunction( ok = ok, show.log = show.log, bell = bell )
	# }}}

	return(restot)
}

# {{{ Spectrum.sample
#' Cut a sample into ECD classes (for size spectra)
"Spectrum.sample" <- function(ZIDat, sample, taxa = NULL, groups = NULL,
	breaks = seq(0.25, 2, by = 0.1), use.Dil = TRUE) {
	
	# Check arguments
	mustbe(ZIDat, "ZIDat")
	if (!is.character(sample) && length(sample) != 1)
		stop("sample must be a character string of length one")
	
	# Extract only data for a given sample
	Smps <- sub("[+].*", "", as.character(ZIDat$Label)) # Sample is everything before a '+' sign
	if (!sample %in% unique(Smps))
		stop("sample '", sample, "' is not in ZIDat")
	Smp <- ZIDat[Smps == sample, ]
	# Determine the number of images in this sample
	imgs <- unique(ZIDat$Label)
	res <- Spectrum(Smp, imgs[1], taxa = taxa, groups = groups, breaks = breaks, use.Dil = use.Dil)
	if (length(imgs) > 1) {
		for (i in 2:length(imgs))
			res <- list.add(res, Spectrum(Smp, imgs[i], taxa = taxa, groups = groups, breaks = breaks, use.Dil = use.Dil))			
	}
	return(res)
}
# }}}

"Spectrum" <-
	function(ZIDat, image,  taxa = NULL, groups = NULL, 
	breaks = seq(0.25, 2, by = 0.1), use.Dil = TRUE) {
	
	# Check arguments
	mustbe(ZIDat, "ZIDat")
	if (!is.character(image) && length(image) != 1)
		stop("image must be a character string of length one")
	dat <- ZIDat[ZIDat$Label == image, ] # Select the image
	if (nrow(dat) == 0)
		warning("ZIDat contains no '", image, "' data!")
	# Remember dilution (in case there are no data)
	if (nrow(dat) > 0) Dil <- dat$Dil[1] else Dil <- 1
	# taxa must correspond to levels in ZIDat$Ident
	if (!is.null(taxa)) {
		if (!all(taxa %in% levels(dat$Ident)))
			stop("taxa not in ZIDat")
		dat <- dat[dat$Ident %in% taxa, ] # Select taxa
	}
	if (is.null(groups)) {
		# Total spectrum only
		groups <- list("")
		names(groups) <- "total"
	}
	if (!is.list(groups))
		stop("groups must be a list")
	res <- list()
	gnames <- names(groups)
	for (i in 1: length(groups)) {
		if (length(groups[[i]]) == 1 && groups[[i]] == "") { # Total abundance
			Dat <- dat$ECD
		} else { # Abundance for given groups
			Dat <- dat$ECD[dat$Ident %in% groups[[i]]]
		}
		spc <- table(cut(Dat, breaks = breaks))
		if (use.Dil) spc <- spc * Dil
		res[[gnames[i]]] <- spc 
	}
	attr(res, "breaks") <- breaks
	attr(res, "unit") <- if(use.Dil) "ind/m^3" else "count"
	return(res)
}

"Bio.sample" <-
	function(ZIDat, sample, taxa = NULL, groups = NULL,
	conv = c(1, 0, 1), header = "Bio", exportdir = NULL) {
	# Convert ECD (biomass calculation, etc.)
	# Check arguments
	mustbe(ZIDat, "ZIDat" )
		
	if (!is.character(sample) && length(sample) != 1)
		stop("sample must be a character string of length one")
	# Extract only data for a given sample
	Smps <- sub("[+].*", "", as.character(ZIDat$Label)) # Sample is everything before a '+' sign
	if (!sample %in% unique(Smps))
		stop("sample '", sample, "' is not in ZIDat")
	Smp <- ZIDat[Smps == sample, ]
	# Subsample, depending on taxa we keep
	if (!is.null(taxa)) {
		if (!all(taxa %in% levels(Smp$Ident)))
			stop("taxa not in the sample")
		Smp <- Smp[Smp$Ident %in% taxa, ] # Select taxa
	}
	if (nrow(Smp) == 0)
		stop("no data for this sample/taxa in ZIDat")
	# Add P1/P2/P3 conversion params to the table
	if (inherits(conv, "data.frame")) {
		if (  ! all(names(conv)[1:4] == c("Group", "P1", "P2", "P3") ) || !all(names(conv)[1:4] == c("Group", "a", "b", "c") ) ){
			stop("conv must have 'Group', 'P1', 'P2', 'P3' or 'a', 'b', 'c' columns!")
		}
		IdSmp <- as.character(Smp$Ident)
		IdSmpU <- unique(IdSmp)
		IdConv <- as.character(conv$Group)
		# Eliminate [other] from the table and the list and keep its values for further use
		IsOther <- (IdConv == "[other]")
		Other <- conv[IsOther, ]
		if (sum(IsOther) > 0) {
		  IdConv <- IdConv[!IsOther]
		  conv <- conv[!IsOther, ]
		  conv$Group <- as.factor(as.character(conv$Group))
		}
        if (!all(IdSmpU %in% IdConv)) {
            if (nrow(Other) > 0) {
                # Fill all the other groups with the formula for other and issue a warning
                NotThere <- IdSmpU[!(IdSmpU %in% IdConv)]
                warning(paste("Applying default [other] biomass conversion for ", paste(NotThere, collapse = ", "), sep = ""))
                N <- length(NotThere)
                conv2 <- data.frame(Group = NotThere, P1 = rep(Other[1, 2], N),
                    P2 = rep(Other[1, 3], N), P3 = rep(Other[1, 4], N))
                conv <- rbind(conv, conv2)
                conv$Group <- as.factor(as.character(conv$Group)) 
            } else {
                # All groups must be there: stop!
                stop("Not all 'Ident' in sample match 'Group' in the conv table")
            }
        }
		# Line number of the corresponding parameter
		# is calculated as the level of a factor whose levels
		# are the same as in the conversion table
		Pos <- as.numeric(factor(IdSmp, levels = as.character(conv$Group)))
		Smp$P1 <- conv[Pos, "P1"]
		Smp$P2 <- conv[Pos, "P2"]
		Smp$P3 <- conv[Pos, "P3"]
	} else { # Use the same three parameters for all
		if (length(conv) != 3)
			stop("You must provide a vector with three numbers")
		Smp$P1 <- conv[1]
		Smp$P2 <- conv[2]
		Smp$P3 <- conv[3]
	}
	# Individual contributions to biomass by m^3
    Smp$Biomass <- (Smp$P1 * Smp$ECD + Smp$P2)^Smp$P3 * Smp$Dil
    # AZTI special treatment
    # introducimos la formula de montagnes y la correccion para ESD(2.61951)
	#Smp$Biomass <- (0.109 * (pi*4/3*((2.61951*Smp$ECD)/2)^3)^0.991) * Smp$Dil
    if (!is.null(exportdir))
        write.table(Smp, file = paste(file.path(exportdir, sample), "_Bio.txt", sep = ""), sep = "\t", row.names = FALSE)

	if (is.null(groups)) {
		# Total biomass only
		res <- sum(Smp$Biomass)
		names(res) <- header
	} else {
		if (!is.list(groups))
		    stop("groups must be a list")
		res <- NULL
		for (i in 1: length(groups)) {
			if (length(groups[[i]]) == 1 && groups[[i]] == "") { # Total biomass
				res[i] <- sum(Smp$Biomass)
			} else { # Biomass for given groups
				res[i] <- sum(Smp$Biomass[Smp$Ident %in% groups[[i]]])
			}
		}
		names(res) <- paste(header, names(groups))
	}
 	return(res)
}

#{{{ Abd.sample
#' Calculate abundances for various taxa in a sample
"Abd.sample" <- function(ZIDat, sample, taxa = NULL, groups = NULL,
	type = c("absolute", "log", "relative"), header = "Abd") {

	# Check arguments
	mustbe( ZIDat, "ZIDat")
	if (!is.character(sample) && length(sample) != 1)
		stop("sample must be a character string of length one")
	type <- match.arg( type, several.ok = FALSE )
	
	# Extract only data for a given sample
	Smps <- sub("[+].*", "", as.character(ZIDat$Label)) # Sample is everything before a '+' sign
	if (!sample %in% unique(Smps)){
		stop("sample '", sample, "' is not in ZIDat")
	}
	Smp <- ZIDat[Smps == sample, ]
	
	# Subsample, depending on taxa we keep
	if (!is.null(taxa)) {
		if (!all(taxa %in% levels(Smp$Ident)))
			stop("taxa not in the sample")
		Smp <- Smp[Smp$Ident %in% taxa, ] # Select taxa
	}
	if (nrow(Smp) == 0){
		stop("no data for this sample/taxa in ZIDat")
	}
	
	# If relative abundance, calculation of fraction for each individual
	if (type == "relative") {
		Table <- table(Smp$Dil)
		Coefs <- 1 / Table / length(Table)
		Dils <- as.numeric(names(Table))
		Pos <- as.numeric(factor(as.character(Smp$Dil), levels = as.character(Dils)))
		Smp$Coef <- Coefs[Pos]
	} else {
		# If absolute or log abundance, calculation in ind/m^3)
		Smp$Coef <- Smp$Dil
	}
	if (is.null(groups)) {
		# Total abundance only
		res <- sum(Smp$Coef)
		names(res) <- header
	} else {
		if (!is.list(groups))
			stop("groups must be a list")
		res <- NULL
		for (i in 1: length(groups)) {
			if (length(groups[[i]]) == 1 && groups[[i]] == "") { # Total abundance
				res[i] <- sum(Smp$Coef)
			} else { # Abundance for given groups
				res[i] <- sum(Smp$Coef[Smp$Ident %in% groups[[i]]])
			}
		}
		names(res) <- paste(header, names(groups))
	}
	if (type == "log")
		res <- log10(res + 1)
	return(res)
}
# }}}

"plot.ZITable" <-
	function(x, y, ...) {
	barplot(x, names.arg = attr(x, "breaks")[-1], ...)
}

"merge.ZITable" <- function(x, y, ...) {
	
	mustbe(x, "ZITable")
	mustbe(y, "ZITable")
	
	breaks.x <- attr(x, "breaks")
	breaks.y <- attr(y, "breaks")
	mustmatch( breaks.x, breaks.y, 
		"breaks of all objects must match")
	
	unit.x <- attr(x, "unit")
	unit.y <- attr(y, "unit")
	mustmatch( unit.x, unit.y, "units of all objects must match")
	res <- x + y

	# If the user provides more tables, merge them all
	moreargs <- list(...)
	if (length(moreargs) > 0) {
		# Merge all provided tables
		for (i in 1:length(moreargs)) {
			tt <- moreargs[[i]]
			mustbe( tt, "ZITable", msg = "all arguments must be 'ZITable' objects")
			breaks.tt <- attr(tt, "breaks")
			mustmatch( breaks.x, breaks.tt, "breaks of all objects must match")
			unit.tt <- attr(tt, "unit")
			mustmatch( unit.x, unit.tt, "units of all objects must match")
			res <- res + tt
		}
	}
	# In case we make the average of several images,
	# coef divides and calculates the mean value
	return(res)
}

"histSpectrum" <-
	function(spect, class = 1:18 * 0.3 / 3 + 0.2, lag = 0.25, log.scale = TRUE,
	width = 0.1, xlab = "classes (mm)",
	ylab = if (log.scale) "log(abundance + 1)/m^3" else "Abundance (ind./m^3",
	main = "", ylim = c(0, 2), plot.exp = FALSE) {
	# Plot of histograms and optionally line for exponential decrease for size spectra
	if (plot.exp) {
		spect.lm <- lm(spect ~ class)
		print(summary(spect.lm))
		slope <- format(coef(spect.lm)[2], digits = 3)
		main <- paste(main, " (slope = ", slope, ")", sep = "")
		class2 <- class - lag
		spect.lm2 <- lm(spect ~ class2)
		if (!log.scale) {
			spect <- 10^Spect - 1
			expdat <- 10^predict(spect.lm2) - 1
		}
	}
	barplot(spect, width = 0.1, space = 0, xlab = xlab, ylab = ylab, main = main, ylim = ylim)
	if (plot.exp) {
		if (log.scale) {
			abline(coef = coef(spect.lm2), col = 2, lwd = 2)
		} else {
			lines(class2, expdat, col = 2, lwd = 2)
		}
		return(invisible(spect.lm2))
	}
}

"plotAbdBio" <-
	function (t, y1, y2, y3, ylim = c(0,3),
	xlab = "Date", ylab = "log(abundance + 1)", main = "",
	cols = c("green", "blue", "red"), pchs = 1:3,
	hgrid = 1:3, vgrid = t, vline = NULL, xleg = min(vgrid), yleg = ylim[2],
	legend = c("series 1", "series 2", "series 3"), type = "o") {
	# Custom plot for abundance and biomass
	plot(t, y1, type = type, ylim = ylim, xlim = range(vgrid), ylab = ylab,
		xlab = xlab, main = main, col = cols[1], xaxt = "n", pch = pchs[1])
	axis(1, at = vgrid, label = format(vgrid, "%b"))
	lines(t, y2, type = type, col = cols[2], pch = pchs[2])
	lines(t, y3, type = type, col = cols[3], pch = pchs[3])
	# Grid
	abline(h = hgrid, col = "gray", lty = 2)
	abline(v = vgrid, col = "gray", lty = 2)
	# Vertical line(s) to spot particular time events
	if (!is.null(vline)) abline(v = as.Date(vline), lty = 2, lwd = 2, col = 2)
	if (!is.null(xleg)) legend(xleg, yleg, legend, col = cols,
		lwd = 1, pch = pchs, bg = "white")
}
# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

