## Copyright (c) 2008-2012, Ph. Grosjean <phgrosjean@sciviews.org>
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

## Functions and dialog box created for the real time recogntion
realtimeStart <- function ()
{
 	## Process real time recognition during a FlowCAM experiment
	## First remove existing file from the global environment before
	## we read a new sample
	realtimeReset()
	## Ask for an algorithm and one or several samples to compare with
	defval <- "Only One Sample"
	opts <- c("Only One Sample",
			  "Comparison with One Other Sample",
			  "Comparison with Several Other Samples")
	## Then, show the dialog box
 	#res <- modalAssistant(paste(getTemp("ZIname"),
	#	"Real-Time recognition for FlowCAM"),
	#	c("This is a beta version of the real time recognition",
	#	"of FlowCAM samples developed for the AMORE III project.",
	#	"Warning! This method is only developed for FlowCAM data,",
	#	"and with a classifier made with FlowCAM parameters only.",
	#	"", "Select an option:", ""), init = defval,
	#	options = opts, help.topic = "makeClass")
	#if (res == "ID_CANCEL") return(invisible())
	res <- dlgList(opts, preselect = defval, multiple = FALSE,
		title = "Select an option:")$res
	if (!length(res)) return(invisible())
	## Only one sample
	if (res == "Only One Sample") {
		## Use default values for the classifier creation
		cat("Classify one sample at a time in real-time mode\n")
		mode <- 0
	} else if (res == "Comparison with One Other Sample") {
		cat("Comparison of current sample with previous one activated\n")
		mode <- 1
	} else if (res == "Comparison with Several Other Samples") {
		cat("Comparison of current sample with a list of samples already digitized\n")
		mode <- 2
	} else stop("Unknown option!")	
	flush.console()

	## Look if we have a classifier object defined
	ZIC <- getTemp("ZI.ClassName")
	if (is.null(ZIC)) ZIC <- ""
	ZIC <- getVar("ZIClass", multi = FALSE, default = ZIC,
		title = "Choose a classifier (ZIClass object):", warn.only = FALSE)
	if (length(ZIC) == 0 || (length(ZIC) == 1 && ZIC == ""))
		return(invisible())
	ZICobj <- get(ZIC, envir = .GlobalEnv)

	## Select the current sample
	Current <- dlgOpen(title = "Select a lst file",
		filters = matrix(c("FlowCAM list file", ".lst"),
		ncol = 2, byrow = TRUE))$res 

	if (mode == 1) { # Select the Previous sample
		Prev <- dlgOpen(title = "Select the lst file for the previous sample",
			filters = matrix(c("FlowCAM list file", ".lst"),
			ncol = 2, byrow = TRUE))$res 
	} else if (mode == 2) {
		## Select the Previous samples
	    List <- list.files(dlgDir(title = "Select general directory")$res,
			recursive = TRUE, pattern = ".lst$", full.names = TRUE)
	    ListSamples <- selectSamples(Samples = List)
	} else Prev <- NULL
	
	## Select a conversion table
	ConvFile <- getKey("ConversionFile", file.path(getTemp("ZIetc"),
		"Conversion.txt"))
	## Ask for selecting a Conversion file
	ConvFile <- dlgOpen(title = "Select a conversion file",
			filters = matrix(c("Biomass Conversion table (*Conversion.txt)",
			"Conversion.txt"), ncol = 2, byrow = TRUE))$res
	if (length(ConvFile) == 0 || ConvFile == "")
		return(invisible()) # Cancelled dialog box

	## Select the size spectra option
	brks <- dlgInput("Breaks for size spectrum classes in mm (empty for no spectrum):",
		default = "seq(0.25, 2, by = 0.1)")$res
	if (!length(brks)) return(invisible())
	brks <- eval(parse(text = brks))
	## Choose options
	## Default options
	## Without sample comparison
	Abd.all <- TRUE
	Abd.gp <- NULL
	Spec.all <- NULL
	Spec.gp <- NULL
	Bio.all <- NULL
	Bio.gp <- NULL
	## With one or more samples for comparison
	ZICompAbd <- TRUE
	ZICompSpectra <- NULL
	ZICompBiomass <- NULL
	ZICompSlope <- NULL
	ZICompAbd.gp <- NULL
	ZICompBio.gp <- NULL
	## Options for all modes
	defval_Graphs <- "Total Abundance"
	if (mode == 0) {
		opts_Graphs <- c("Total Abundance", "Abundance of groups",
			"Total Size Spectra", "Size Spectra of groups",
			"Total Biomass", "Biomass of groups")
	} else {
		opts_Graphs <- c("Total Abundance", "Abundance of groups",
			"Total Size Spectra", "Total Biomass", "Biomass of groups",
			"Slope of size spectra")
	}
	#res <- modalAssistant(paste(getTemp("ZIname"),
	#	"Real-Time classification with the FlowCAM"),
	#		c("Select one type of plot you want to do",
	#		"", "Select an option:", ""), init = defval_Graphs,
	#		options = opts_Graphs, help.topic = "makeClass")
	res <- dlgList(opts_Graphs, preselect = defval_Graphs, multiple = FALSE,
		title = "Select one type of plot:")$res
	if (!length(res)) return(invisible())
	if (mode == 0) {
		if (res == "Total Abundance")
			Abd.all <- TRUE
		if (res == "Abundance of groups") {
			Abd.all <- NULL
			Abd.gp <- selectGroups(ZICobj)
		}
		if (res == "Total Size Spectra") {
			Abd.all <- NULL
			Spec.all <- TRUE
		}
		if (res == "Size Spectra of groups") {
			Abd.all <- NULL
			Spec.gp <- selectGroups(ZICobj)
		}
		if (res == "Total Biomass") {
			Abd.all <- NULL
			Bio.all <- TRUE
		}
		if (res == "Biomass of groups") {
			Abd.all <- NULL
			Bio.gp <- selectGroups(ZICobj)
		}
	} else { # mode 1 or 2
		if (res == "Total Abundance")
			ZICompAbd <- TRUE
	    if (res == "Abundance of groups") {
			ZICompAbd <- NULL
			ZICompAbd.gp <- selectGroups(ZICobj)
	    }
	    if (res == "Total Size Spectra") {
			ZICompAbd <- NULL
			ZICompSpectra <- TRUE
	    }
	    if (res == "Total Biomass") {
			ZICompAbd <- NULL
			ZICompBiomass <- TRUE
	    }
	    if (res == "Biomass of groups") {
			ZICompAbd <- NULL
			ZICompBio.gp <- selectGroups(ZICobj)
	    }
	    if (res == "Slope of size spectra") {
			ZICompAbd <- NULL
			ZICompSlope <- TRUE
	    }
	}
	## Loop parameters
	#realtimeOptions(lstdir = Current, # path of the list file of the FlowCAM run
	#	ZIClass = ZICobj, # Classifer
	#	ZIprevSmp = NULL, # Comparison with one previous sample
	#	ZIlist = NULL, # Comparison several previous samples
	#	################## One Sample
	#	Abd.all = Abd.all, # NULL or TRUE
	#	Abd.gp = Abd.gp, # NULL or groups to plot
	#	Spec.all = Spec.all, # NULL or TRUE
	#	Spec.gp = Spec.gp, # NULL or groups
	#	Bio.all = Bio.all, # NULL or TRUE
	#	Bio.gp = Bio.gp, # NULL or groups
	#	breaks = brks, # in mm
	#	conv = ConvFile, # or conversion table
	#	################## More than one sample
	#	ZICompAbd = ZICompAbd,
	#	ZICompSpectra = ZICompSpectra,
	#	ZICompBiomass = ZICompBiomass,
	#	ZICompSlope = ZICompSlope,
	#	ZICompAbd.gp = ZICompAbd.gp,
	#	ZICompBio.gp = ZICompBio.gp
	#)
	## Run automatic recognition and plot
## TODO: we need a depends on tcltk2 here!		
#	tclFun(realtimeLoop)
	realtimeLoop()
}

realtimeSave <- function ()
{
	lst <- getOption("Path")
	classif <- getOption("Classifier")
	breaks <- getOption("breaks")
	conv <- getOption("conv")
	save.dir <- dirname(getOption("Path"))

	rec <- getTemp("rtRecord")
	if (is.null(rec))
		rec <- predict(classif, lstRead(lst),
			calc.vars = TRUE, class.only = FALSE)
	if (!is.null(save.dir)) {
		if (!is.character(save.dir))
			stop("The exportation path must be a character string")
	} else save.dir <- dlgDir()$res
	
	Bio.tab <- sampleBio(ZIDat = rec, conv = conv, exportdir = NULL,
		realtime = TRUE)
	write.table(Bio.tab, file = paste(save.dir, paste(basename(dirname(lst)),
		"AbdBio.txt", sep = "_"), sep = "\\"), sep = "\t", dec = ".",
		col.names = TRUE, na = "NA", row.names = FALSE)
	## Delete objects from R environment
	rmTemp("rtData")
	rmTemp("rtRecord")
	rmTemp("rtTime")	
}

realtimeStop <- function ()
	assignTemp(".realtimeStopItFlag", TRUE)

realtimeReset <- function () {
	assignTemp("rtData", NULL)
	assignTemp("rtRecords", NULL)
	assignTemp("rtTime", NULL)
}

realtimeSlope <- function (ZIDat, breaks, log = TRUE)
{
	if (!"FIT_Diameter_ABD" %in% names(ZIDat))
		stop("The 'FIT_Diameter_ABD' column is required in 'ZIDat' but not found")
	Dat <- as.vector(table(cut(ZIDat$FIT_Diameter_ABD / 1000, breaks = breaks)))
	if (isTRUE(log)) Dat <- log10(Dat + 1)
	midpoints <- (breaks[-1] + breaks[-length(breaks)]) / 2
	Lm <- lm(Dat ~ midpoints)
	res <- coef(Lm)[2]
	attr(res, "lm") <- Lm
	return(res)
}

## Loop to run process and comparisons in real-time (delay interval is in ms)
realtimeLoop <- function (delay = 15000)
{
	continue <- TRUE
	## Function to execute at regular interval
	realtimeProcess(List = getOption("Path"), ZIClass = getOption("Classifier"),
		conv = getOption("conv"), collage = getOption("collage"),
		flow.cell = getOption("flow.cell"), images.per.sec = getOption("images.per.sec"),
		size = getOption("size"), lag = getOption("Lag"))
	#realtimePlotMobile(ZIDat = rec, group = getOption("group"),
	#	identify = getOption("identify"), breaks = getOption("breaks"),
	#	log = getOption("log"), realtime = TRUE)
	#realtimePlot(ZIDat = rec, type = getOption("type"), abd = getOption("abd"),
	#	bio = getOption("bio"), group = getOption("group"),
	#	concentration = getOption("concentration"),
	#	spectra = getOption("spectra"), breaks = getOption("breaks"),
	#	compare.smp = getOption("compare.smp"), log = getOption("log"))
  
	## Is there a stop signal?
	if (existsTemp(".realtimeStopItFlag")) {
		rmTemp(".realtimeStopItFlag")
		timer <- NULL
	} else { # Continue...
		## Run realtimeLoop after 'delay' ms
		timer <- .Tcl(paste("after", as.integer(delay)[1], "realtimeLoop"))
	}
	return(invisible(timer))
}

realtimeOptions <- function (
lstdir = ".", 		# Path of the list file of the current FlowCAM experiment
ZIClass,			# Classifier to use
type = "b", 		# "b" : barplot, "l" : line alpha code
size.threshold = NULL, # NULL or Size threshold in µm alpha code
breaks = seq(0.05, 3, by = 0.1),  # in mm
conv.dir = ".", 		# Path of the conversion table
images.per.sec = 7,
flow.cell = 600,
concentration = "p/mL",	# "Absolute", "Relative" or "p/mL"
collage = NULL, 	# NULL: no mobile window, TRUE: use collage, FALSE: use number of vignettes
size = 5,			# The size of the mobile window
lag = 2,			# The lag between two successive mobile windows
abd = NULL,			# NULL, TRUE or FALSE
bio = NULL,			# NULL, TRUE or FALSE
spectra = NULL,		# NULL, TRUE or FALSE
compare.smp = NULL,		# NULL, FALSE or a path of a list of sample to compare with
group = NULL,		# The group to recognize and/or plot
identify = FALSE,	# Identify points on plot (TRUE or FALSE)
log = FALSE,		# Transform data in log10(x + 1)
slope = FALSE)
{
	## Check and/or convert arguments
	lstdir <- as.character(lstdir)[1]
	
	if (!inherits(ZIClass, "ZIClass"))
		stop("'ZIClass' must be a classifier of class 'ZIClass'")

	type <- as.character(type)[1]
    if (!type %in% c("b", "l"))
		stop("type must be 'b' (barplot) or 'l' (lines)")

	if (!is.null(size.threshold) && !is.numeric(size.threshold))
		stop("'size.threshold' must be a numeric value in microns or NULL")
		
	if (!is.numeric(breaks))
		stop("breaks must be the size interval (a vector of numeric values)")
  
	conv.dir <- as.character(conv.dir)[1]
	
	images.per.sec <- as.numeric(images.per.sec)[1]
	if (images.per.sec < 0)
		stop("'images.per.sec' must be the number of images taken by the FlowCAM per second")

	flow.cell <- as.integer(flow.cell)[1]

	concentration <- as.character(concentration)[1]
	if (!concentration %in% c("p/mL", "Relative", "Absolute"))
		stop("'concentration' must be \"p/mL\", \"Absolute\" or \"Relative\"")

	if (!is.null(collage)) collage <- isTRUE(collage)

	size <- as.numeric(size)[1]
	if (size <= 0)
		stop("'size' must be the value of the interval size (a positivce number)")

	lag <- as.numeric(lag)[1]
	if (lag < 0)
		stop("'lag' must be the value of the lag between two mobile windows (postive or zero)")
  
	if (!is.null(abd)) abd <- isTRUE(abd)
  
	if (!is.null(bio)) bio <- isTRUE(bio)
  
	if (!is.null(spectra)) spectra <- isTRUE(spectra)

	if (!is.null(compare.smp)) {
		compare.smp <- as.character(compare.smp)
		if (length(compare.smp) == 1) {
			if(length(grep(pattern = ".[Zz][Ii][Dd]", x = compare.smp)) >= 1) {
				## This a zid file
				Smp <- zidRead(compare.smp)
			} else {
				## This is a list file
				Smp <- lstRead(compare.smp)
			}
			Smp <- predict(ZIClass, Smp, calc.vars = FALSE, class.only = FALSE)
			Smp <- calcBiomass(ZIDat = Smp, conv = conv.dir, realtime = TRUE)
			List <- list(Smp)
			names(List) <- noext(basename(compare.smp))
		} else {
			List <- list()
			if (length(grep(pattern = ".[Zz][Ii][Dd]", x = compare.smp)) >= 1) {
				## This a zid file
				for (i in 1 : length(compare.smp))
					List[[i]] <- calcBiomass(ZIDat = predict(ZIClass,
						zidRead(compare.smp[i]), calc.vars = FALSE,
						class.only = FALSE), conv = conv.dir, realtime = TRUE)
			} else {
				## This is a list file
				for (i in 1 : length(compare.smp))
					List[[i]] <- calcBiomass(ZIDat = predict(ZIClass,
						lstRead(compare.smp[i]), calc.vars = FALSE,
						class.only = FALSE), conv = conv.dir, realtime = TRUE)
			}
			names(List) <- noext(basename(compare.smp))
		}
      	compare.smp <- List
    } else compare.smp <- FALSE
  
	if (!is.null(group)) group <- as.character(group)[1] 
  
	identify <- isTRUE(identify)
  
	log <- isTRUE(log)
  
	slope <- isTRUE(slope)

	## Construct the options object and save it in options
	opts <- list(lstdir, ZIClass, type, size.threshold, breaks, conv.dir,
		images.per.sec, flow.cell, size, lag, concentration, abd, bio, spectra,
		group, compare.smp, identify, log, slope)
	options("ZIrealtimeOpts" = opts)
	return(invisible(opts))
}

realtimeProcess <- function (List, ZIClass, conv = c(1, 0, 1), collage = NULL,
flow.cell = 600, images.per.sec = 5, size = 5, lag = 2)
{
	if (!existsTemp("rtData")) {
		## First iteration
		## Calculation of elapsed time
		Time <- elapsedTime(List)
		## Read the list file
		tab <- lstRead(List, skip = 2)
		## If no measurements in the list file
		if (dim(tab)[1] == 0) {
			cat("The list file is empty\n")
			flush.console()
			rmTemp("rtData")
		} else {
			rec <- getTemp("rtRecord")
			if (is.null(rec)) {
				rec <- predict(ZIClass, tab, calc.vars = FALSE,
					class.only = FALSE) # Ident
				rec <- calcBiomass(ZIDat = rec, conv = conv,
					realtime = TRUE) # Biomass
				## Proceed to the mobile window
				if (!is.null(collage))
					rec <- mobileWindow(realtime = TRUE)
				## Add Sec and Vol column to the general table
				if (!"sec" %in% names(rec))
					rec <- addSecVol(ZIDat = rec, flow.cell = flow.cell,
						images.per.sec = images.per.sec)
				assignTemp("rtRecord", rec)
			}
		}
		## Create Attributes
		if (!is.null(rec)) {
			abd <- table(rec$Ident)
			bio <- tapply(rec$Biomass, rec$Ident, sum)
			## Remove NA and 0 from tables abd and bio to avoid any log problem
			abd[is.na(abd)] <- 1e-09
			abd[abd == 0] <- 1e-09
			bio[is.na(bio)] <- 1e-09
			bio[bio == 0] <- 1e-09
			## Add attributes to rec
			attr(rec, "abd") <- abd
			attr(rec, "bio") <- bio
			attr(rec, "skip") <- nrow(tab)
			## Used to know the number of row to skip to get new measurements
			attr(rec, "rowToSkip") <- nrow(rec)
			## Used to create a trnasect after the cruise
			attr(rec, "volumeDigitized") <- volumeDigitized(rec = rec,
				flow.cell = flow.cell, images.per.sec = images.per.sec)
			## Attribute for time elapsed
			attr(rec, "elapsedTime") <- Time
			## This parameter is used by volumeDigitized(List)
			assignTemp("rtRecord", rec)
		}
	} else {
		## There is one lst (non empty tab) list in memory
		rec1 <- rec # classification table from the previous iteration
		abd1 <- attr(rec1, "abd") # abd from the previous iteration
		bio1 <- attr(rec1, "bio") # bio from the previous iteration
		## Read the complete table to know if new results have been added
		## Calculation of elapsed time
		Time <- elapsedTime(List)
		New <- lstRead(List, skip = 2) # Read new tab after the elapsed time
		## Check if new measurements added in New
		skp <- attr(rec, "skip")
		attr(rec, "skip") <- c(skp, nrow(New))
		skp <- attr(rec, "skip")
		## Comparision with the previous skip
		if (skp[length(skp)] != skp[length(skp)- 1]) {
			## Extract only new measurements
			tab <- New[(skp[length(skp) - 1] + 1):skp[length(skp)], ]
			## Return the object in R
			tab <- getTemp("rtData")
			## recognition of tab
			rec <- predict(ZIClass, tab, calc.vars = FALSE,
				class.only = FALSE) # Ident
			## Biomass
			rec <- calcBiomass(ZIDat = rec, conv = conv, realtime = TRUE)

			## Add Sec and Vol information
			if (!"sec" %in% names(rec)) 
				rec <- addSecVol(ZIDat = rec, flow.cell = flow.cell,
					images.per.sec = images.per.sec)
			## Create new tables
			abd <- table(rec$Ident)
			bio <- tapply(rec$Biomass, rec$Ident, sum)
			## Remove NA and 0
			abd[is.na(abd)] <- 1e-09
			abd[abd == 0] <- 1e-09
			bio[is.na(bio)] <- 1e-09
			bio[bio == 0] <- 1e-09
			## Paste the two tables : the previous and the new ones
			rec <- rbind(rec1, rec)
			if (!is.null(collage)) {
				## Calculation of the rest of the mobile window
				Interv <- attr(rec1, "intervals")
				attr(rec, "intervals") <- Interv
				## Because it is used to determine the range in mobileWindow
				rec <<- mobileWindow(realtime = TRUE)
				NewInterval <- Interv
				## Extracted here (will be lost after the rbind operation)
				NewMobileTab <- attr(rec, "mobileTab")
				NewTime <- attr(rec, "time")
			}
			## When we rbind rec, we loose attributes --> Add new attributes
			attr(rec, "skip") <- c(attr(rec, "skip"), nrow(New))
			if (!is.null(abd1)) attr(rec, "abd") <<- cbind(abd1, abd)
			if (!is.null(bio1)) attr(rec, "bio") <<- cbind(bio1, bio)
			if (!is.null(collage)) {
				## Attribute of the mobile window
				recTime <- attr(rec1, "time")
				attr(rec, "time") <- c(recTime[-length(recTime)], NewTime)
				## Everything except last iteration
				Interv <- attr(rec1, "intervals")
				attr(rec, "intervals") <- cbind(Interv[ , -ncol(Interv)],
					NewInterval)
				## idem
				MobileTab <- attr(rec1, "mobileTab")
				attr(rec, "mobileTab") <- cbind(MobileTab[ , -ncol(MobileTab)],
					NewMobileTab[, -1])
				## idem
				attr(rec, "size") <- size
				attr(rec, "lag") <- lag
				attr(rec, "collage") <- collage
				assignTemp("rtRecord", rec)
			}
		} else {
			## There are no new measurements in list file
			cat("There are no new measurements in list file or run done\n")
			flush.console()
			## Remove the last element of the skip attribute
			skip <- attr(rec, "skip")
			attr(rec, "skip") <- skip[-length(skip)]
			## Add attributes
			if (!is.null(abd1)) {
				abd <- attr(rec, "abd")
				attr(rec, "abd") <- cbind(abd, rep(1e-09, nrow(abd)))
			}
			if (!is.null(bio1)) {
				bio <- attr(rec, "bio")
				attr(rec, "bio") <- cbind(bio, rep(1e-09, nrow(bio)))
			}
			assignTemp("rtRecord", rec)
		}
		## Attributes with the number of rows to skip
		attr(rec, "rowToSkip") <- c(attr(rec, "rowToSkip"), nrow(rec))
		## Attribute for time elapsed
		attr(rec, "elapsedTime") <- Time
		## This parameter is used by volumeDigitized()
		## Calculation of digitized volume
		attr(rec, "volumeDigitized") <- c(attr(rec, "volumeDigitized"),
			volumeDigitized(rec = rec, flow.cell = flow.cell,
			images.per.sec = images.per.sec))
		assignTemp("rtRecord", rec)
	}
	## Write a table with Volume and nrow of rec
	Time <- attr(rec, "elapsedTime")
	Vol <- attr(rec, "volumeDigitized")
	Row <- attr(rec, "rowToSkip")
	write.table(data.frame(Time, Vol, Row), file = file.path(dirname(List),
		paste(basename(List), "RowToSkip.txt", sep = "_")),
		sep = "\t", dec = ".", row.names = FALSE)
	## Save data as RData
	save(rec, file = file.path(dirname(List),
		paste(basename(List), "rec.Rdata", sep = "_")))
	## Change class(rec)
	if (!inherits(rec, "realtime"))
		class(rec) <- c("realtime", class(rec))
	assignTemp("rtRecord", rec)
}

#### Utility functions for real-time process ###################################
## Calculation of elapsed time and create the attr(rec, "elapsedTime")
elapsedTime <- function (List)
{
	## Info <- file.info(getOption("Path"))
	Info <- file.info(List)
	Time <- getTemp("rtTime")
	if (is.null(Time)) {
		## First iteration
		Elapsed <- difftime(time1 = Info$ctime, time2 = Sys.time(),
			units = "sec")
		Time2 <- abs(as.numeric(Elapsed))
		assignTemp("rtTime", Time2)
	} else {
	    Elapsed <- difftime(time1 = Info$ctime, time2 = Sys.time(),
			units = "sec")
	    Time2 <- abs(as.numeric(Elapsed)) - sum(Time)
	    assignTemp("rtTime", c(Time, Time2))
	}
	return(Time)
}

## Calculation of the digitized volume using the Time elapsed attirbute
volumeDigitized <- function (rec, flow.cell = 600, images.per.sec = 5)
{
	CalConst <- unique(rec$FIT_Cal_Const)
	Height <- 767 * CalConst
	Width <- 1023 * CalConst
	Area <- Height * Width
	Volume <- (Area / 10^8) * (flow.cell/10000) # mL
	ElapsedTime <- attr(rec, "elapsedTime")
	lElapsedTime <- length(ElapsedTime)
	RowToSkip <- attr(rec, "rowToSkip")
	lRowToSkip <- length(RowToSkip)
	if (all(is.na(rec$FIT_Source_Image))) {
		## We have to calculate volume using the elapsed time
		res <- Volume * images.per.sec * ElapsedTime
	} else {
		## We have the information from the new FlowCAM about the raw images
		if (lRowToSkip == 1) { 
			## First iteration completed
			Raw <- rec$FIT_Source_Image[RowToSkip]
		} else {
			## More than one iteration
			if (RowToSkip[lRowToSkip - 1] == RowToSkip[lRowToSkip]) {
				## No new data added at the list file --> Use the elapsed time
				## to approximate number of raw images
				Raw <- images.per.sec * ElapsedTime[lElapsedTime]
			} else {
				## New measurements are added at hte end of the list file
				NewRaw <- rec$FIT_Source_Image[RowToSkip[lRowToSkip]]
				PrevRaw <- rec$FIT_Source_Image[RowToSkip[lRowToSkip - 1] + 1]
				Raw <- NewRaw - PrevRaw + 1
			}
		}
		res <- Volume * Raw
	}
	return(res)
}

## Function to add a column for biomass calculation
calcBiomass <- function (ZIDat, conv = c(1, 0, 1), realtime = FALSE)
{
	if (!isTRUE(realtime)) {
		if (!inherits(ZIDat, "ZIDat"))
			stop("ZIDat must be a 'ZIDat' object")
	}
	## Convert ECD (biomass calculation, etc.)
	## Check arguments
	Smp <- ZIDat
	if (nrow(Smp) == 0)
		stop("no data for this sample/taxa in ZIDat")
	## Add P1/P2/P3 conversion params to the table
	if (inherits(conv, "data.frame")) {
		if (!all(names(conv)[1:4] == c("Group", "P1", "P2", "P3") ||
			c("Group", "a", "b", "c")))
			stop("conv must have 'Group', 'P1', 'P2', 'P3' or 'a', 'b', 'c' columns!")
		IdSmp <- as.character(Smp$Ident)
		IdSmpU <- unique(IdSmp)
		IdConv <- as.character(conv$Group)
		## Eliminate [other] from the table and the list
		## and keep its values for further use
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
	} else { # Use the same three parameters for all
		if (length(conv) != 3)
			stop("You must provide a vector with three numbers")
		Smp$P1 <- conv[1]
		Smp$P2 <- conv[2]
		Smp$P3 <- conv[3]
	}
	## Individual contributions to biomass by m^3
	if (!isTRUE(realtime)) {
		Smp$Biomass <- (Smp$P1 * Smp$ECD + Smp$P2)^Smp$P3 * Smp$Dil
	} else {
		Smp$Biomass <- (Smp$P1 * Smp$FIT_Diameter_ABD + Smp$P2)^Smp$P3
	}
    ## AZTI special treatment
    ## introducimos la formula de montagnes y la correccion para ESD(2.61951)
	#Smp$Biomass <- (0.109 * (pi*4/3*((2.61951*Smp$ECD)/2)^3)^0.991) * Smp$Dil

	## Add metadata attribute
	attr(Smp, "metadata") <- attr(ZIDat, "metadata")
	return(Smp)
}

## Add Sec and Volume column
addSecVol <- function (ZIDat, flow.cell, images.per.sec)
{
	calcVol <- function (lst, flow.cell) {
		CalConst <- unique(lst$FIT_Cal_Const) 
		Height <- 767 * CalConst
		Width <- 1023 * CalConst
		Area <- Height * Width
		Volume <- (Area / 10^8) * (flow.cell / 10000) # mL
		return(Volume)
	}
	
	if (!inherits(ZIDat, "data.frame"))
		stop("ZIDat must be an object of class 'data.frame'")
	if (!is.numeric(flow.cell))
		stop("flow.cell must be a numrical value with the depth of the flow cell used")
	if (!is.numeric(images.per.sec))
		stop("images.per.sec must be the number of image per second saved by the FlowCAM")
	ZIDat$Sec <- ZIDat$FIT_Source_Image / images.per.sec
	Vol <- calcVol(ZIDat, flow.cell)
	ZIDat$Vol <- ZIDat$FIT_Source_Image * Vol
	return(ZIDat)
}

## Mobile window
mobileWindow <- function (ZIDat, size = 1, lag = 1, collage = FALSE, flow.cell,
images.per.sec, realtime = FALSE)
{
	if (!isTRUE(realtime)) {
		rec <- getTemp("rtRecord")
		Time <- numeric()
		if (!inherits(ZIDat, "data.frame"))
			stop("ZIDat must be an object of class 'data.frame'")
		if (!"Ident" %in% colnames(ZIDat))
			stop("ZIDat must contain a column Ident")
		if (lag < 1)
			stop("lag must be higher than 1")
		if (size < lag)
			stop("size must be larger or equal to lag")
		if (!"sec" %in% names(ZIDat))
			ZIDat <- addSecVol(ZIDat = ZIDat, flow.cell = flow.cell,
				images.per.sec = images.per.sec)
		if (isTRUE(collage)) {
			for (i in 1:(length(levels(ZIDat$FIT_Filename)) - (2 * size))) {
				if (i <= 1) {
					df <- data.frame(Int = (0 + i):(i + (2* size)))
					Tab <- data.frame(table(ZIDat[ZIDat$FIT_Filename %in%
						levels(ZIDat$FIT_Filename)[df[, i]], ]$Ident))
				} else {
					df[, i] <- data.frame(Int = df[, (i - 1)] + lag)
					Tab[, i+1] <- table(ZIDat[ZIDat$FIT_Filename %in%
						levels(ZIDat$FIT_Filename)[df[, i]], ]$Ident)
				}
				Time[i] <- mean(ZIDat[ZIDat$FIT_Filename %in%
					levels(ZIDat$FIT_Filename)[df[size + 1, i]], ]$Sec)
				if (!all(df[, i] < length(levels(ZIDat$FIT_Filename)))) {
					warning("The loop is stopped because the end of the table is reached")
					break
				}
			}
		} else {
			for (i in 1:(dim(ZIDat)[1] - (2 * size))) {
				if (i <= 1) {
					df <- data.frame(Int = (0 + i) : (i + (2* size)))
					Tab <- data.frame(table(ZIDat[df[, i],]$Ident))
				} else {
					df[, i] <- data.frame(Int = df[, (i - 1)] + lag)
					Tab[, i+1] <- table(ZIDat[df[, i], ]$Ident)
				}
				Time[i] <- ZIDat[df[size, i], ]$Sec
				if (!all(df[, i] < nrow(ZIDat))) {
					warning("The loop is stopped because the end of the table is reached")
					break
				}
			}
		}
		attr(ZIDat, "time") <- Time
		attr(ZIDat, "size") <- size
		attr(ZIDat, "lag") <- lag
		attr(ZIDat, "intervals") <- df
		attr(ZIDat, "mobileTab") <- Tab
		attr(ZIDat, "collage") <- collage
		return(ZIDat)
	} else {
		Temp <- numeric()
		if (!"sec" %in% names(rec))
			assignTemp("rtRecord", addSecVol(ZIDat = rec,
				flow.cell = getOption("flow.cell"),
				images.per.sec = getOption("images.per.sec")))
		if (getOption("collage")) {
			## Determine the starting point for the loop
			if (is.null(attr(rec, "intervals"))) {
				Range <- 1:(length(levels(rec$FIT_Filename)) -
					(2 * getOption("size")))
			} else {
				Range <- attr(rec, "intervals")[1, ncol(attr(rec, "intervals"))]:
					(length(levels(rec$FIT_Filename)) - (2 * getOption("size")))
			}
			for (i in Range) {
				if (grep(i, Range)[1] <= 1) {
					df <- data.frame(Int = (0 + i):
						(i + (2* getOption("size"))))
					Tab <- data.frame(table(rec[rec$FIT_Filename %in%
						levels(rec$FIT_Filename)[df[, (i + 1) -
						Range[1]]], ]$Ident))
				} else {
					df[, (i + 1) - Range[1]] <- data.frame(Int = df[, ((i + 1) -
						Range[1] - 1)] + getOption("lag"))
					Tab[, ((i + 1) - Range[1]) + 1] <-
						table(rec[rec$FIT_Filename %in%
						levels(rec$FIT_Filename)[df[, (i + 1) -
						Range[1]]], ]$Ident)
				}
				Temp[(i + 1) - Range[1]] <- mean(rec[rec$FIT_Filename %in%
					levels(rec$FIT_Filename)[df[getOption("size") + 1, (i + 1) -
					Range[1]]], ]$Sec)
				if (!all(df[, (i + 1) - Range[1]] <
					length(levels(rec$FIT_Filename)))) {
					warning("The loop is stopped because the end of the table is reached")
					break
				}
			}
		} else {
			## Determine the starting point for the loop
			if (is.null(attr(rec, "intervals"))) {
				Range <- 1:(dim(rec)[1] - (2 * getOption("size")))
			} else {
				Range <- attr(rec, "intervals")[1, ncol(attr(rec, "intervals"))]:
					(dim(rec)[1] - (2 * getOption("size")))
			}
			for (i in Range) {
				if (grep(i, Range)[1] <= 1) {
					df <- data.frame(Int = (0 + i):
						(i + (2 * getOption("size"))))
					Tab <- data.frame(table(rec[df[, (i + 1) -
						Range[1]],]$Ident))
				} else {
					df[, (i + 1) - Range[1]] <- data.frame(Int = df[, ((i + 1) -
						Range[1] - 1)] + getOption("lag"))
					Tab[, ((i + 1) - Range[1])+1] <- table(rec[df[, (i + 1) -
						Range[1]], ]$Ident)
				}
				Temp[(i + 1) - Range[1]] <- rec[df[getOption("size"), (i + 1) -
					Range[1]] ,]$Sec
				if (!all(df[, (i + 1) - Range[1]] < nrow(rec))) {
					warning("The loop is stopped because the end of the table is reached")
					break
				}
			}
		}
		attr(rec, "time") <- Temp
		attr(rec, "size") <- getOption("size")
		attr(rec, "lag") <- getOption("lag")
		attr(rec, "intervals") <- df
		attr(rec, "mobileTab") <- Tab
		attr(rec, "collage") <- getOption("collage")
		assignTemp("rtRecord", rec)
		return(rec)
	}
}
