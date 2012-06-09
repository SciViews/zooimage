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

processSample <- function (ZidFile, ZIClass, ZIMan, ZIDesc, abd.taxa = NULL,
abd.groups = NULL, abd.type = "absolute", bio.taxa = NULL, bio.groups = NULL,
bio.conv = c(1, 0, 1), headers = c("Abd", "Bio"), spec.taxa = NULL,
spec.groups = NULL, spec.breaks = seq(0.25, 2, by = 0.1), spec.use.Dil = TRUE,
exportdir = NULL, show.log = TRUE, SemiTab = NULL, Semi = FALSE)
{    
	## Check if the ZidFile exists
	checkFileExists(ZidFile)
	
	## Check if ZIClass is of the right class
	if (!inherits(ZIClass, "ZIClass"))
		stop("'ZIClass' must be a 'ZIClass' object")
	
	## Get ZIDat from the ZidFile
	ZIDat <- zidRead(ZidFile)
	
	## By default, we have to predict ZidFile with a classifier
	MakePredictions <- TRUE
	
	## Modified by Kevin 2010-08-03
	if (!is.null(ZIMan)) {
		## We want to use a ZIMan table
		if (!inherits(ZIMan, "ZIMan"))
			stop("ZIMan must be a data.frame of class 'ZIMan'")
		
		## List of samples allready manually validated
		AllSamples <- attr(ZIMan, "Samples")
		
		## Check if manual validation exists for this zid file
		if (noext(ZidFile) %in% AllSamples) {
			## The ZidFile was manually validated
			## --> use Class column for identification
			## Subtable of ZidFile vignettes
			Vignettes <- makeId(ZIDat)
			ZIDat <- ZIMan[ZIMan$Id %in% Vignettes,]
			## Sort the table
			ZIDat <- ZIDat[order(ZIDat$Item),]
			## We don't have to predict this sample anymore!
			MakePredictions <- FALSE
		}
	}
	
	if (isTRUE(MakePredictions)) {
		## Check if ZIClass is a classifier
		if (!inherits(ZIClass, "ZIClass"))
			stop("ZIClass must be an object of class 'ZIClass'")
		## We have to recognize the zid file with a classifier
		ZIDat <- predict(ZIClass, ZIDat)
	}
	
	## Get ZIDat from the ZidFile
	Sample <- sampleInfo(ZidFile, type = "sample",
		ext = extensionPattern(".zid"))
	
	## Check if one can get sample metadata from ZIDesc
	RES <- ZIDesc[ZIDesc$Label == Sample, ] 
	if (nrow(RES) != 1)
		stop("ZIDesc has no data for that sample!")
	
	## Use manual validation if it is present
	if (isTRUE(MakePredictions)) {
		## Use Automatic prediction
		Grp <- levels(ZIDat$Ident)	
	} else {
		## Use manual validation as identification
		Grp <- levels(ZIDat$Class)
	}
	
	if (is.null(abd.groups)) {
		## Calculate groups (list with levels to consider)
		abd.groups <- as.list(c("", Grp))
		names(abd.groups) <- c("total", Grp)
	}
	
	## Process abundances
	ABD <- sampleAbd(ZIDat, Sample, taxa = abd.taxa, groups = abd.groups,
		type = abd.type, header = headers[1])
	RES <- cbind(RES, t(ABD))
	
	## Process biomasses
	if (!is.null(bio.conv)) {
		if (is.null(bio.groups)) {
			## Calculate groups (list with levels to consider)
			bio.groups <- as.list(c("", Grp))
			names(bio.groups) <- c("total", Grp)
		}
		BIO <- sampleBio(ZIDat, Sample, taxa = bio.taxa, conv = bio.conv,
			groups = bio.groups, header = headers[2], exportdir = exportdir)
		RES <- cbind(RES, t(BIO))
	}
	
	## Process size spectra
	if (!is.null(spec.breaks)) {
		if (is.null(spec.groups)) {
			## Calculate groups (list with levels to consider)
			spec.groups <- as.list(c("", Grp))
			names(spec.groups) <- c("total", Grp)
		}
		SPC <- sampleSpectrum(ZIDat, Sample, taxa = spec.taxa,
			groups = spec.groups, breaks = spec.breaks, use.Dil = spec.use.Dil)
		SPClist <- list()
		SPClist[[Sample]] <- SPC
		attr(RES, "spectrum") <- SPClist
	}
	attr(RES, "metadata") <- attr(ZIDesc, "metadata")
	class(RES) <- c("ZI1Res", "ZIRes", "data.frame")
	return(RES)
}

processSampleAll <- function (path = ".", ZidFiles = NULL, ZIClass, ZIMan = NULL,
ZIDesc = readDescription("Description.zis"), abd.taxa = NULL, abd.groups = NULL,
abd.type = "absolute", bio.taxa = NULL, bio.groups = NULL, bio.conv = c(1, 0, 1),
headers = c("Abd", "Bio"), spec.taxa = NULL, spec.groups = NULL,
spec.breaks = seq(0.25, 2, by = 0.1), spec.use.Dil = TRUE, exportdir = NULL,
show.log = TRUE, bell = FALSE)
{
	## Determine which samples do we have to process...
	if (is.null(ZidFiles)) {
    	## Get the list of files from ZIDesc
		ZidFiles <- paste(ZIDesc$Label, ".zid", sep = "")
		if (path == ".")
			path <- getwd()
		ZidFiles <- file.path(path, ZidFiles)
	} else { # Check that all zid files have entries in ZIDesc
		Samples <- sampleInfo(ZidFiles, type = "sample",
			ext = extensionPattern(".zid"))
		if (!all(Samples %in% ZIDesc$Label))
			stop("One or more samples not in 'ZIDesc'!")
	}
	
	## Start the process
	logClear()
	ok <- TRUE
	restot <- NULL
	imax <- length(ZidFiles)
	cat("Processing",  imax, "samples...\n")
	logProcess(paste("Processing",  imax, "samples..."))
	
	results <- lapply(1:imax, function (i) {
		Progress(i, imax)
		
		## Modif. by Kevin Denis for manual validation --> Add ZIMan argument
		tryCatch({
			res <- processSample(ZidFiles[i], ZIClass = ZIClass, ZIMan = ZIMan,
				ZIDesc = ZIDesc, abd.taxa = abd.taxa, abd.groups = abd.groups,
				abd.type = abd.type, bio.taxa = bio.taxa,
				bio.groups = bio.groups, bio.conv = bio.conv,
				headers = headers, spec.taxa = spec.taxa,
				spec.groups = spec.groups, spec.breaks = spec.breaks, 
				spec.use.Dil = spec.use.Dil, exportdir = exportdir,
				show.log = FALSE)
			
			logProcess("OK", ZidFiles[i])
			return(res)
		}, zooImageError = function (e) {
			logError(e)
			return(NULL)
		})
	})
	
	clearProgress()
	
	results <- Filter(Negate(is.null), results)
	restot <- do.call(rbind, results)
	attr(restot, "spectrum") <- unlist(lapply(results, attr, "spectrum"))
	attr(restot, "metadata") <- attr(results[[length(results)]], "metadata")
	class(restot) <- c("ZI1Res", "ZIRes", "data.frame")
	
	## Final report
	finishLoop(ok = ok, show.log = show.log, bell = bell)
	return(restot)
}

## Cut a sample into ECD classes (for size spectra)
sampleSpectrum <- function (ZIDat, sample, taxa = NULL, groups = NULL,
breaks = seq(0.25, 2, by = 0.1), use.Dil = TRUE)
{	
	## Check arguments
	if (!inherits(ZIDat, "ZIDat"))
		stop("'ZIDat' must be a 'ZIDat' object")
	if (!is.character(sample) || length(sample) != 1)
		stop("'sample' must be a single character string")
	
	## Extract only data for a given sample
	## Sample is everything before a '+' sign
	Smps <- getSample(ZIDat$Label, unique = TRUE, must.have = sample)
	Smp <- ZIDat[Smps == sample, ]
	
	## Determine the number of images in this sample
	imgs <- as.character(unique(ZIDat$Label))
	lists <- lapply( imgs, function(im) {
		tryCatch({
			getSpectrum(Smp, im, taxa = taxa, groups = groups, breaks = breaks,
				use.Dil = use.Dil)
		}, zooImageError = function (e) return(NULL))
	})
	list.add(lists)
}

getSpectrum <- function (ZIDat, image,  taxa = NULL, groups = NULL,
breaks = seq(0.25, 2, by = 0.1), use.Dil = TRUE, RealT = FALSE)
{
	## Modif by Kev for manual validation 2010-08-03
	## All $Ident replaced by [, Predictions]
	if ("Class" %in% names(ZIDat)) {
		## We use manual validation
		Predictions <- "Class"
	} else {
		## We use automatic recognition
		Predictions <- "Ident"
	}
	if (!isTRUE(RealT)) {
		## Check arguments
		if (!inherits(ZIDat, "ZIDat")) 
			stop("'ZIDat' must be a 'ZIDat' object")
		if (!is.character(image) || length(image) != 1) 
			stop("'image' must be a single character string")
		
		## Select the image
		dat <- ZIDat[ZIDat$Label == image, ]
		if (nrow(dat) == 0)
			warning("ZIDat contains no '", image, "' data!")

		## Remember dilution (in case there are no data)
		Dil <- if (nrow(dat) > 0) dat$Dil[1] else 1

		## Taxa must correspond to levels in ZIDat[, Predictions]
		if (!is.null(taxa)) {
			if (!all(taxa %in% levels(dat[, Predictions]))) 
			    stop("taxa not in 'ZIDat'")
			dat <- dat[dat[, Predictions] %in% taxa, ]
		}
		if (is.null(groups)) {
			## Total spectrum only
			groups <- list("")
			names(groups) <- "total"
		}
		if (!inherits(groups, "list")) 
		    stop("'groups' must be a 'list' object")
 
		res <- lapply(groups, function (g) {
			if (length(g) == 1 && g == "") {
			    Dat <- dat$ECD
			} else {
			    ## Abundance for given groups
			    Dat <- dat$ECD[dat[, Predictions] %in% g]
			}
			spc <- table(cut(Dat, breaks = breaks))
			if (isTRUE(use.Dil))
				spc <- spc * Dil
			return(spc)
		})
		names(res) <- names(groups)
		attr(res, "breaks") <- breaks
		attr(res, "unit")  <- if(use.Dil) "ind/m^3" else "count"
		return(res)
	} else {
		## Real Time classification
		## ZIDat is a table with VIS measurements and automatic Ident
		## taxa must correspond to levels in ZIDat$Ident
		if (!is.null(taxa)) {
			if (!all(taxa %in% levels(ZIDat[, Predictions])))
				stop("taxa not in ZIDat")
			Dat <- ZIDat[ZIDat[, Predictions] %in% taxa, ] # Select taxa
		}
		if (is.null(groups)) {
			## Total spectrum only
			groups <- list("")
			names(groups) <- "total"
		}
		if (!inherits(groups, "list")) 
		    stop("'groups' must be a 'list' object")
 
		res <- lapply( groups, function (g) {
			if (length(g) == 1 && g == "") { # Total abundance
				Dat <- ZIDat$FIT_Diameter_ABD/1000 # in 'mm'
			} else { # Abundance for given groups
				Dat <- ZIDat$FIT_Diameter_ABD[
					ZIDat[, Predictions] %in% g ] / 1000 # in 'mm'
			}
			spc <- table(cut(Dat, breaks = breaks))
			if (isTRUE(use.Dil)) spc <- spc * Dil
			return(spc)
		})
	#	res <- list()
	#	gnames <- names(groups)
	#	for (i in 1: length(groups)) {
	#		if (length(groups[[i]]) == 1 && groups[[i]] == "") { # Total abundance
	#			Dat <- ZIDat$FIT_Diameter_ABD/1000 # in 'mm'
	#		} else { # Abundance for given groups
	#			Dat <- ZIDat$FIT_Diameter_ABD[ZIDat$Ident %in% groups[[i]]]/1000 # in 'mm'
	#		}
	#		spc <- table(cut(Dat, breaks = breaks))
	#		if (use.Dil) spc <- spc * Dil
	#		res[[gnames[i]]] <- spc
	#	}
		names(res) <- names(groups)
		attr(res, "breaks") <- breaks
		attr(res, "unit") <- if(isTRUE(use.Dil)) "ind/m^3" else "count"
		return(res)
	}
}

## Convert ECD (biomass calculation, etc.)
sampleBio <- function (ZIDat, sample, taxa = NULL, groups = NULL,
conv = c(1, 0, 1), header = "Bio", exportdir = NULL, realtime = FALSE)
{
	## Modif by Kev for manual validation 2010-08-03
	## All $Ident replaced by [, Predictions]
	if ("Class" %in% names(ZIDat)) {
		## We use manual classification
		Predictions <- "Class"
	} else {
		## We use automatic recogntion
		Predictions <- "Ident"
	}
	
	if (!isTRUE(realtime)) {
		## Check arguments
		if (!inherits(ZIDat, "ZIDat")) 
			stop("'ZIDat' must be a 'ZIDat' object")
		if (!is.character(sample) || length(sample) != 1) 
			stop("'sample' must be a single character string")

		## Extract only data for a given sample
		Smps <- getSample(ZIDat$Label, unique = TRUE, must.have = sample)
		Smp <- ZIDat[Smps == sample, ]

		## Subsample, depending on taxa we keep
		if (!is.null(taxa)) {
			if (!all(taxa %in% levels(Smp[, Predictions]))) 
				stop("taxa not in the sample")
			Smp <- Smp[Smp[, Predictions] %in% taxa, ] # Select taxa
		}
		if (nrow(Smp) == 0)
			stop("no data for this sample/taxa in ZIDat")

		## Add P1/P2/P3 conversion params to the table
		if (inherits(conv, "data.frame")) {

# BUG if not abc as colnmaes even if P1 , P2 and P3 are correct!
#			if (!all(names(conv)[1:4] == c("Group", "P1", "P2", "P3")) ||
#				!all(names(conv)[1:4] == c("Group", "a", "b", "c")))
#				stop("conv must have 'Group', 'P1', 'P2', 'P3' or 'a', 'b', 'c' columns!")
			if (!all(names(conv)[1:4] == c("Group", "P1", "P2", "P3")))
				stop("conv must have 'Group', 'P1', 'P2', 'P3'")
# BUG if not abc as colnmaes even if P1 , P2 and P3 are correct!

			IdSmp <- as.character(Smp[, Predictions])
			IdSmpU <- unique(IdSmp)
			IdConv <- as.character(conv$Group)
			## Eliminate [other] from the table and the list and keep its values
			## for further use
			IsOther <- (IdConv == "[other]")
			Other <- conv[IsOther, ]
			if (sum(IsOther) > 0) {
				IdConv <- IdConv[!IsOther]
				conv <- conv[!IsOther, ]
				conv$Group <- as.factor(as.character(conv$Group))
			}
			
			if (!all(IdSmpU %in% IdConv)) {
				if (nrow(Other) > 0) {
					## Fill all the other groups with the formula for other
					## and issue a warning
					NotThere <- IdSmpU[!(IdSmpU %in% IdConv)]
					warning("Applying default [other] biomass conversion for ",
						paste(NotThere, collapse = ", "))
					N <- length(NotThere)
					conv2 <- data.frame(Group = NotThere, P1 = rep(Other[1, 2], N),
						P2 = rep(Other[1, 3], N), P3 = rep(Other[1, 4], N))
					conv <- rbind(conv, conv2)
					conv$Group <- as.factor(as.character(conv$Group))
				} else {
					## All groups must be there: stop!
					stop("Not all 'Ident' in sample match 'Group' in the conv table")
				}
			}
			## Line number of the corresponding parameter
			## is calculated as the level of a factor whose levels
			## are the same as in the conversion table
			Pos <- as.numeric(factor(IdSmp, levels = as.character(conv$Group)))
			Smp$P1 <- conv[Pos, "P1"]
			Smp$P2 <- conv[Pos, "P2"]
			Smp$P3 <- conv[Pos, "P3"]
		} else {
			## Use the same three parameters for all
			if (length(conv) != 3)
				stop("You must provide a vector with three numbers")
			Smp$P1 <- conv[1]
			Smp$P2 <- conv[2]
			Smp$P3 <- conv[3]
		}
		## Individual contributions to biomass by m^3
		Smp$Biomass <- (Smp$P1 * Smp$ECD + Smp$P2) ^ Smp$P3 * Smp$Dil
		## AZTI special treatment
		## introducimos la formula de montagnes y la correccion para ESD(2.61951)
		##Smp$Biomass <- (0.109 * (pi*4/3*((2.61951*Smp$ECD)/2)^3)^0.991) * Smp$Dil
		if (!is.null(exportdir))
			write.table(Smp, file = paste(file.path(exportdir, sample),
				"_Bio.txt", sep = ""), sep = "\t", row.names = FALSE)

		if (is.null(groups)) {
			## Total biomass only
			res <- sum(Smp$Biomass)
			names(res) <- header
		} else {
			if (!inherits(groups, "list")) 
				stop("'groups' must be a 'list' object")
 			res <- if (length(groups) == 1 && groups=="") {
				sum(Smp$Biomass)
			} else {
				sapply(groups, function (g)
					sum(Smp$Biomass[Smp[, Predictions] %in% g]))
			}
			names(res) <- paste(header, names(groups))
		}
		return(res)
	} else {
		## Real time recognition -> use FlowCAM measurements
		Smp <- ZIDat
		if (!is.null(taxa)) {
			if (!all(taxa %in% levels(Smp[, Predictions])))
				stop("taxa not in the sample")
			Smp <- Smp[Smp[, Predictions] %in% taxa, ]
		}
		if (nrow(Smp) == 0)
			stop("no data for this sample/taxa in ZIDat")
		
		## Add P1/P2/P3 conversion params to the table
		if (inherits(conv, "data.frame")) {

# BUG if not abc as colnmaes even if P1 , P2 and P3 are correct!
#			if (!all(names(conv)[1:4] == c("Group", "P1", "P2", "P3")) ||
#				!all(names(conv)[1:4] == c("Group", "a", "b", "c")))
#				stop("conv must have 'Group', 'P1', 'P2', 'P3' or 'a', 'b', 'c' columns!")
			if (!all(names(conv)[1:4] == c("Group", "P1", "P2", "P3")))
				stop("conv must have 'Group', 'P1', 'P2', 'P3'columns!")
# BUG if not abc as colnmaes even if P1 , P2 and P3 are correct!

			IdSmp <- as.character(Smp[, Predictions])
			IdSmpU <- unique(IdSmp)
			IdConv <- as.character(conv$Group)
			## Eliminate [other] from the table and the list and keep its values
			## for further use
			IsOther <- (IdConv == "[other]")
			Other <- conv[IsOther, ]
			if (sum(IsOther) > 0) {
				IdConv <- IdConv[!IsOther]
				conv <- conv[!IsOther, ]
				conv$Group <- as.factor(as.character(conv$Group))
			}
			if (!all(IdSmpU %in% IdConv)) { # If groups from table not in Ident
				if (nrow(Other) > 0) {
					## Fill all the other groups with the formula for other
					## and issue a warning
					NotThere <- IdSmpU[!(IdSmpU %in% IdConv)]
					warning("Applying default [other] biomass conversion for ",
						paste(NotThere, collapse = ", "))
					N <- length(NotThere)
					conv2 <- data.frame(Group = NotThere, P1 = rep(Other[1, 2], N),
						P2 = rep(Other[1, 3], N), P3 = rep(Other[1, 4], N))
					conv <- rbind(conv, conv2)
					conv$Group <- as.factor(as.character(conv$Group))
				} else {
					## All groups must be there: stop!
					stop("Not all 'Ident' in sample match 'Group' in the conv table")
				}
			}
			## Line number of the corresponding parameter
			## is calculated as the level of a factor whose levels
			## are the same as in the conversion table
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
		## Individual contributions to biomass by m^3
		Smp$Biomass <- (Smp$P1 * Smp$FIT_Diameter_ABD + Smp$P2) ^ Smp$P3
		if (!is.null(exportdir))
			write.table(Smp, file = paste(file.path(exportdir, sample),
				"_Bio.txt", sep = ""), sep = "\t", row.names = FALSE)
		assign("Bio.tab", Smp, envir = .GlobalEnv)
		if (is.null(groups)) {
			## Biomass of all groups
			res <- NULL
			grps <- levels(Smp[, Predictions])
			for (i in 1:length(grps))
				res[i] <- sum(Smp$Biomass[Smp[, Predictions] %in% grps[i]])
			names(res) <- grps
		} else {
			if (!inherits(groups, "list")) 
				stop("'groups' must be a 'list' object")
 			res <- if (length(groups) == 1 && groups=="") {
				sum(Smp$Biomass)
			} else {
				sapply(groups, function (g)
					sum(Smp$Biomass[Smp[, Predictions] %in% g]))
			}
			names(res) <- names(groups)
		}
		return(res)
	}
}

# Calculate abundances for various taxa in a sample
sampleAbd <- function (ZIDat, sample, taxa = NULL, groups = NULL,
type = c("absolute", "log", "relative"), header = "Abd")
{
	## Check arguments
	if (!inherits(ZIDat, "ZIDat")) 
		stop("'ZIDat' must be a 'ZIDat' object")
	if (!is.character(sample) || length(sample) != 1) 
		stop("'sample' must be a single character string")
 	type <- match.arg(type, several.ok = FALSE)

	## Modif by Kev for manual validation 2010-08-03
	## All $Ident replaced by [, Predictions]
	if ("Class" %in% names(ZIDat)) {
		## We use manual classification
		Predictions <- "Class"
	} else {
		## We use automatic recogntion
		Predictions <- "Ident"
	}
	
	## Extract only data for a given sample
	Smps <- getSample(ZIDat$Label, unique = TRUE, must.have = sample)
	Smp <- ZIDat[Smps == sample, ]
	
	## Subsample, depending on taxa we keep
	if (!is.null(taxa)) {
		if (!all(taxa %in% levels(Smp[, Predictions])))
			stop("taxa not in the sample")
		Smp <- Smp[Smp[, Predictions] %in% taxa, ] # Select taxa
	}
	if (nrow(Smp) == 0)
		stop("no data for this sample/taxa in ZIDat")
	
	## If relative abundance, calculation of fraction for each individual
	if (type == "relative") {
		Table <- table(Smp$Dil)
		Coefs <- 1 / Table / length(Table)
		Dils <- as.numeric(names(Table))
		Pos <- as.numeric(factor(as.character(Smp$Dil),
			levels = as.character(Dils)))
		Smp$Coef <- Coefs[Pos]
	} else {
		## If absolute or log abundance, calculation in ind/m^3)
		Smp$Coef <- Smp$Dil
	}
	if (is.null(groups)) {
		## Total abundance only
		res <- sum(Smp$Coef)
		names(res) <- header
	} else {
		if (!inherits(groups, "list")) 
			stop("'groups' must be a 'list' object")
 		res <- if (length(groups) == 1 && groups == "") {
			sum(Smp$Coef)
		} else {
			sapply(groups, function (g)
				sum(Smp$Coef[Smp[, Predictions] %in% g]))
		}
		names(res) <- paste(header, names(groups))
	}
	if (type == "log")
		res <- log10(res + 1)
	return(res)
}

plot.ZITable <- function (x, y, ...)
	barplot(x, names.arg = attr(x, "breaks")[-1], ...)

merge.ZITable <- function (x, y, ...)
{	
	data <- list(x, y, ...)
	lapply(data, function (x) {
		if (!inherits(x, "ZITable"))
			stop("arguments must all be 'ZITable' objects")
	})
	
	mustallmatch <- function (.list, msg = "all must match") {
		n <- length(.list)
		if (n < 2) stop("need at list 2 elements")
		first <- .list[[1]]
		for (i in 2:n)
			if (!all(sort(first)  == sort(.list[[i]]))) stop(msg)
		return()
	}

	mustallmatch(lapply(data, attr, "breaks"),
		msg = "breaks of all objects must match")
	mustallmatch(lapply( data, attr, "unit"), 
		msg = "units of all objects must match")
	Reduce("+", data) 
}

histSpectrum <- function (spect, class = 1:18 * 0.3 / 3 + 0.2, lag = 0.25,
log.scale = TRUE, width = 0.1, xlab = "classes (mm)",
ylab = if (log.scale) "log(abundance + 1)/m^3" else "Abundance (ind./m^3",
main = "", ylim = c(0, 2), plot.exp = FALSE)
{	
	## Plot of histograms and optionally line for exponential decrease
	## for size spectra
	if (isTRUE(plot.exp)) {
		spect.lm <- lm(spect ~ class)
		print(summary(spect.lm))
		slope <- format(coef(spect.lm)[2], digits = 3)
		main <- paste(main, " (slope = ", slope, ")", sep = "")
		class2 <- class - lag
		spect.lm2 <- lm(spect ~ class2)
		if (!isTRUE(log.scale)) {
			spect <- 10^spect - 1
			expdat <- 10^predict(spect.lm2) - 1
		}
	}
	barplot(spect, width = 0.1, space = 0, xlab = xlab, ylab = ylab,
		main = main, ylim = ylim)
	if (plot.exp) {
		if (log.scale) {
			abline(coef = coef(spect.lm2), col = 2, lwd = 2)
		} else {
			lines(class2, expdat, col = 2, lwd = 2)
		}
		return(invisible(spect.lm2))
	}
}

plotAbdBio <- function (t, y1, y2, y3, ylim = c(0,3), xlab = "Date",
ylab = "log(abundance + 1)", main = "", cols = c("green", "blue", "red"),
pchs = 1:3, hgrid = 1:3, vgrid = t, vline = NULL, xleg = min(vgrid),
yleg = ylim[2], legend = c("series 1", "series 2", "series 3"), type = "o")
{	
	## Custom plot for abundance and biomass
	plot(t, y1, type = type, ylim = ylim, xlim = range(vgrid), ylab = ylab,
		xlab = xlab, main = main, col = cols[1], xaxt = "n", pch = pchs[1])
	axis(1, at = vgrid, labels = format(vgrid, "%b"))
	lines(t, y2, type = type, col = cols[2], pch = pchs[2])
	lines(t, y3, type = type, col = cols[3], pch = pchs[3])
	
	## Grid
	abline(h = hgrid, col = "gray", lty = 2)
	abline(v = vgrid, col = "gray", lty = 2)
	
	## Vertical line(s) to spot particular time events
	if (!is.null(vline))
		abline(v = as.Date(vline), lty = 2, lwd = 2, col = 2)
	if (!is.null(xleg))
		legend(xleg, yleg, legend, col = cols, lwd = 1, pch = pchs, bg = "white")
}
