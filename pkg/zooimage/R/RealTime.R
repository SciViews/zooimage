# Copyright (c) 2008-2010, Ph. Grosjean <phgrosjean@sciviews.org>
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

# TODO: rework all this!!!
# Functions and dialog box created for the real time recogntion
"realtimeRun" <- function ()
{
 	# Process real time recognition during a FlowCAM experiment
	# First remove existing file from the global environment before read a new sample
	realtimeReset()
	# Ask for an algorithm and one or several sample to compare
	defval <- "Only One Sample"
	opts <- c("Only One Sample",
			  "Comparison with One Other Sample",
			  "Comparison with Several Other Samples")
	# Then, show the dialog box
 	res <- modalAssistant(paste(getTemp("ZIname"),
		"Real-Time recognition for FlowCAM"),
		c("This is a beta version of the real time recognition",
		"of FlowCAM samples developed for the AMORE III project.",
		"Warning! This method is only developed for FlowCAM data,",
		"and with a classifier made with FlowCAM parameters only.",
		"", "Select an option:", ""), init = defval,
		options = opts, help.topic = "makeClass")
	if (res == "ID_CANCEL") return(invisible())
	# Only one sample
	if (res == "Only One Sample") {
		# Use default values for the classifier creation
		print("You will only recognize in real-time one sample")

		# Look if we have a classifier object defined
		ZIC <- getTemp("ZI.ClassName")
		if (is.null(ZIC)) ZIC <- ""
		ZIC <- getVar("ZIClass", multi = FALSE, default = ZIC,
			title = "Choose a classifier (ZIClass object):", warn.only = FALSE)
		if (length(ZIC) == 0 || (length(ZIC) == 1 && ZIC == ""))
			return(invisible())
		ZICobj <- get(ZIC, envir = .GlobalEnv)

		# Select the current sample
		Current <- paste(as.character(tkgetOpenFile(filetypes =
			"{{FlowCAM list file} {.lst}}",
			title = "Select a lst file")), collapse = " ")

		# Select a conversion table
		ConvFile <- getKey("ConversionFile", file.path(getTemp("ZIetc"),
			"Conversion.txt"))
		# Does this file exists?
		if (!file.exists(ConvFile) || ConvFile == "")
			ConvFile <- file.path(getTemp("ZIetc"), "Conversion.txt")
		# Ask for selecting a Conversion file
		if (isWin()) {
			ConvFile <- choose.files(default = ConvFile,
				caption = "Select a conversion file...",
				multi = FALSE, filters = c("Biomass Conversion table (*Conversion.txt)",
				"*Conversion.txt"))
		} else {
			ConvFile <- tk_choose.files(default = ConvFile,
				caption = "Select a conversion file...",
				multi = FALSE, filters = matrix(c("Biomass Conversion table",
				".txt"), ncol = 2, byrow = TRUE))
		}
		if (length(ConvFile) == 0 || ConvFile == "")
			return(invisible()) # Cancelled dialog box

		# Select the size spectra option
		# On windows() --> To change for zooimage 1.2-2
		brks <- winDialogString("Breaks for size spectrum classes in mm (empty for no spectrum):",
			default = "seq(0.25, 2, by = 0.1)")
		#brks <- dialogString("Breaks for size spectrum classes (empty for no spectrum):",
			#  "Size spectrum classes", default = "seq(0.25, 2, by = 0.1)")
		if (is.null(brks) || length(brks) == 0 || brks == "")
			return(invisible())
		brks <- eval(parse(text = brks))
		# Choose options
		# Default options
		Abd.all <- TRUE
		Abd.gp <- NULL
		Spec.all <- NULL
		Spec.gp <- NULL
		Bio.all <- NULL
		Bio.gp <- NULL
		defval_Graphs <- "Total Abundance"
		opts_Graphs <- c("Total Abundance", "Abundance of groups",
			"Total Size Spectra", "Size Spectra of groups",
			"Total Biomass", "Biomass of groups")
		res <- modalAssistant(paste(getTemp("ZIname"),
			"Real-Time recognition for FlowCAM"),
			c("Select one type of plot you want to do",
			"", "Select an option:", ""), init = defval_Graphs,
			options = opts_Graphs, help.topic = "makeClass")
		if (res == "Total Abundance") Abd.all <- TRUE
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
		# Loop parameters
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
		#	ZICompAbd = NULL,
		#	ZICompSpectra = NULL,
		#	ZICompBiomass = NULL,
		#	ZICompSlope = NULL,
		#	ZICompAbd.gp = NULL,
		#	ZICompBio.gp = NULL
		#)
		# Run automatic recognition and plot
		tclFun(realtimeLoop)
		realtimeLoop()
	}
	if (res == "Comparison with One Other Sample") {
		cat("You will compare the current sample with sample already digitized\n")
		# Look if we have a classifier object defined
		ZIC <- getTemp("ZI.ClassName")
		if (is.null(ZIC)) ZIC <- ""
		ZIC <- getVar("ZIClass", multi = FALSE, default = ZIC,
			title = "Choose a classifier (ZIClass object):", warn.only = FALSE)
		if (length(ZIC) == 0 || (length(ZIC) == 1 && ZIC == ""))
			return(invisible())
		ZICobj <- get(ZIC, envir = .GlobalEnv)

		# Select the current sample
		Current <- paste(as.character(tkgetOpenFile(filetypes =
			"{{FlowCAM list file} {.lst}}",
			title = "Select the lst file of the current sample")), collapse = " ")
		# Select the Previous sample
		Prev <- paste(as.character(tkgetOpenFile(filetypes =
			"{{FlowCAM list file} {.lst}}",
			title = "Select the lst file of the previous sample")), collapse = " ")
		# TODO: there is no Prev argument in selectSamples()!?
		#Prev <- selectSamples(Prev = Prev)
		# Select a conversion table
		ConvFile <- getKey("ConversionFile", file.path(getTemp("ZIetc"),
			"Conversion.txt"))
		# Does this file exists?
		if (!file.exists(ConvFile) || ConvFile == "")
			ConvFile <- file.path(getTemp("ZIetc"), "Conversion.txt")
		# Ask for selecting a Conversion file
		if (isWin()) {
			ConvFile <- choose.files(default = ConvFile,
				caption = "Select a conversion file...",
				multi = FALSE, filters = c("Biomass Conversion table (*Conversion.txt)",
				"*Conversion.txt"))
		} else {
			ConvFile <- tk_choose.files(default = ConvFile,
				caption = "Select a conversion file...",
				multi = FALSE, filters = matrix(c("Biomass Conversion table",
				".txt"), ncol = 2, byrow = TRUE))
		}
		if (length(ConvFile) == 0 || ConvFile == "")
			return(invisible()) # Cancelled dialog box

		# Select the size spectra option
		# On windows() --> To change for zooimage 1.2-2
		brks <- winDialogString("Breaks for size spectrum classes in mm (empty for no spectrum):",
			default = "seq(0.25, 2, by = 0.1)")
		#brks <- dialogString("Breaks for size spectrum classes (empty for no spectrum):",
			#  "Size spectrum classes", default = "seq(0.25, 2, by = 0.1)")
		if (is.null(brks) || length(brks) == 0 || brks == "")
			return(invisible())
		brks <- eval(parse(text = brks))
		# Choose options
		# Default options
		ZICompAbd <- TRUE
		ZICompSpectra <- NULL
		ZICompBiomass <- NULL
		ZICompSlope <- NULL
		ZICompAbd.gp <- NULL
		ZICompBio.gp <- NULL
		defval_Graphs <- "Total Abundance"
		opts_Graphs <- c("Total Abundance", "Abundance of groups",
			"Total Size Spectra", "Total Biomass", "Biomass of groups",
			"Slope of size spectra")
		res <- modalAssistant(paste(getTemp("ZIname"),
			"Real-Time recognition for FlowCAM"),
			c("Select one type of plot you want to do",
			"", "Select an option you want to compare:", ""),
			init = defval_Graphs, options = opts_Graphs,
			help.topic = "makeClass")
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
		# Loop parameters
		#realtimeOptions(lstdir = Current, # path of the list file of the FlowCAM run
		#	ZIClass = ZICobj, # Classifer
		#	ZIprevSmp = Prev, # Comparison with one previous sample
		#	ZIlist = NULL, # Comparison several previous samples
		#	################## One Sample
		#	Abd.all = NULL, # NULL or TRUE
		#	Abd.gp = NULL, # NULL or groups to plot
		#	Spec.all = NULL, # NULL or TRUE
		#	Spec.gp = NULL, # NULL or groups
		#	Bio.all = NULL, # NULL or TRUE
		#	Bio.gp = NULL, # NULL or groups
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
		# Run automatic recognition and plot
		tclFun(realtimeLoop)
		realtimeLoop()
	}
	if (res == "Comparison with Several Other Samples") {
		cat("You will compare the current sample with a list of samples already digitized\n")
	    # Look if we have a classifier object defined
	    ZIC <- getTemp("ZI.ClassName")
	    if (is.null(ZIC)) ZIC <- ""
	    ZIC <- getVar("ZIClass", multi = FALSE, default = ZIC,
			title = "Choose a classifier (ZIClass object):", warn.only = FALSE)
	    if (length(ZIC) == 0 || (length(ZIC) == 1 && ZIC == ""))
			return(invisible())
	    ZICobj <- get(ZIC, envir = .GlobalEnv)
	
	    # Select the current sample
	    Current <- paste(as.character(tkgetOpenFile(filetypes =
			"{{FlowCAM list file} {.lst}}",
			title = "Select the lst file of the current sample")), collapse = " ")
	    # Select the Previous sample
	    List <- list.files(choose.dir(,caption = "Select general directory"),
			recursive = TRUE, pattern = ".lst$", full.names = TRUE)
	    ListSamples <- selectSamples(Samples = List)
	
	    # Select a conversion table
	   	ConvFile <- getKey("ConversionFile", file.path(getTemp("ZIetc"),
			"Conversion.txt"))
	  	# Does this file exists?
	  	if (!file.exists(ConvFile) || ConvFile == "")
			ConvFile <- file.path(getTemp("ZIetc"), "Conversion.txt")
	    # Ask for selecting a Conversion file
	    if (isWin()) {
			ConvFile <- choose.files(default = ConvFile,
				caption = "Select a conversion file...",
				multi = FALSE, filters = c("Biomass Conversion table (*Conversion.txt)",
				"*Conversion.txt"))
		} else {
			ConvFile <- tk_choose.files(default = ConvFile,
				caption = "Select a conversion file...",
				multi = FALSE, filters = matrix(c("Biomass Conversion table",
				".txt"), ncol = 2, byrow = TRUE))
		}
	    if (length(ConvFile) == 0 || ConvFile == "")
			return(invisible()) # Cancelled dialog box

	    # Select the size spectra option
	    # On windows() --> To change for zooimage 1.2-2
	    brks <- winDialogString("Breaks for size spectrum classes in mm (empty for no spectrum):",
			default = "seq(0.25, 2, by = 0.1)")
	    #brks <- dialogString("Breaks for size spectrum classes (empty for no spectrum):",
			#  "Size spectrum classes", default = "seq(0.25, 2, by = 0.1)")
	    if (is.null(brks) || length(brks) == 0 || brks == "") return(invisible())
	    brks <- eval(parse(text = brks))
	    # Choose options
	    # Default options
	    ZICompAbd <- TRUE
	    ZICompSpectra <- NULL
	    ZICompBiomass <- NULL
	    ZICompSlope <- NULL
	    ZICompAbd.gp <- NULL
	    ZICompBio.gp <- NULL
	    defval_Graphs <- "Total Abundance"
	    opts_Graphs <- c("Total Abundance", "Abundance of groups",
			"Total Size Spectra", "Total Biomass", "Biomass of groups",
			"Slope of size spectra")
	    res <- modalAssistant(paste(getTemp("ZIname"),
			"Real-Time recognition for FlowCAM"),
			c("Select one type of plot you want to do",
			"", "Select an option you want to compare:", ""),
			init = defval_Graphs, options = opts_Graphs,
			help.topic = "makeClass")
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
	    # Loop parameters
	#    realtimeOptions(lstdir = Current, # path of the list file of the FlowCAM run
	#		ZIClass = ZICobj, # Classifer
	#		ZIprevSmp = NULL, # Comparison with one previous sample
	#		ZIlist = ListSamples, # Comparison several previous samples
	#		################## One Sample
	#		Abd.all = NULL, # NULL or TRUE
	#		Abd.gp = NULL, # NULL or groups to plot
	#		Spec.all = NULL, # NULL or TRUE
	#		Spec.gp = NULL, # NULL or groups
	#		Bio.all = NULL, # NULL or TRUE
	#		Bio.gp = NULL, # NULL or groups
	#		breaks = brks, # in mm
	#		conv = ConvFile, # or conversion table
	#		################## More than one sample
	#		ZICompAbd = ZICompAbd,
	#		ZICompSpectra = ZICompSpectra,
	#		ZICompBiomass = ZICompBiomass,
	#		ZICompSlope = ZICompSlope,
	#		ZICompAbd.gp = ZICompAbd.gp,
	#		ZICompBio.gp = ZICompBio.gp
	#    )
	    # Run automatic recognition and plot
	    tclFun(realtimeLoop)
	    realtimeLoop()
	}
}

"realtimeSave" <- function ()
{
	save.loop.res <- function (lst, Classif, breaks = seq(0.25, 2, by = 0.1),
	conv = c(1, 0, 1), save.dir = NULL) {
		res <- getTemp("rtRecord")
		if (is.null(rec))
			rec <- predict(Classif, read.lst(lst),
				calc.vars = TRUE, class.only = FALSE)
		if (!is.null(save.dir)) {
			if (!is.character(save.dir))
				stop("The exportation path must be a character string")
		} else {
			save.dir <- choose.dir()
		}
		Bio.sample(ZIDat = rec, conv = conv, exportdir = NULL, RealT = TRUE)
		# TODO: what is Bio.tab???
		#write.table(Bio.tab, file = paste(save.dir, paste(basename(dirname(lst)),
		#	"AbdBio.txt", sep = "_"), sep = "\\"), sep = "\t", dec = ".",
		#	col.names = TRUE, na = "NA", row.names = FALSE)
		# Delete objects from R environment
		rmTemp("rtData")
		rmTemp("rtRecord")
		rmTemp("rtTime")
	}
	save.loop.res(lst = getOption("Path"), Classif = getOption("Classifier"),
		breaks = getOption("breaks"), conv = getOption("conv"),
		save.dir = dirname(getOption("Path")))	
}

"realtimeStop" <- function ()
	assignTemp(".realtimeStopItFlag", TRUE)

"realtimeReset" <- function () {
	assignTemp("rtData", NULL)
	assignTemp("rtRecords", NULL)
	assignTemp("rtTime", NULL)
}

"realtimeSlope" <- function (ZIDat, breaks, log = TRUE)
{
	if (!"FIT_Diameter_ABD" %in% names(ZIDat))
		stop("The 'FIT_Diameter_ABD' column is required in 'ZIDat' but not found")
	Dat <- as.vector(table(cut(ZIDat$FIT_Diameter_ABD/1000, breaks = breaks)))
	if (isTRUE(log)) Dat <- log10(Dat + 1)
	midpoints <- (breaks[-1] + breaks[-length(breaks)]) / 2
	Lm <- lm(Dat ~ midpoints)
	res <- coef(Lm)[2]
	attr(res, "lm") <- Lm
	return(res)
}

# Loop to run process and comparisons in real-time (delay interval is in ms)
"realtimeLoop" <- function (delay = 15000)
{
	continue <- TRUE
	# Function to execute at regular interval
	realtimeProcess(List = getOption("Path"), ZIClass = getOption("Classifier"),
		conv = getOption("conv"), Collage = getOption("Collage"),
		FlowCell = getOption("FlowCell"), ImgPerSec = getOption("ImgPerSec"),
		Size = getOption("Size"), Lag = getOption("Lag"))
	#realtimePlotMobile(ZIDat = rec, group = getOption("Group"),
	#	identify = getOption("identify"), breaks = getOption("breaks"),
	#	log = getOption("log"), RealT = TRUE)
	#realtimePlot(ZIDat = rec, type = getOption("type"), Abd = getOption("Abd"),
	#	Bio = getOption("Bio"), Group = getOption("Group"),
	#	Concentration = getOption("Concentration"),
	#	Spectra = getOption("Spectra"), breaks = getOption("breaks"),
	#	Compa = getOption("Compa"), log = getOption("log"))
  
	# Is there a stop signal?
	if (existsTemp(".realtimeStopItFlag")) {
		rmTemp(".realtimeStopItFlag")
		timer <- NULL
	} else { # Continue...
		# Run realtimeLoop after 'delay' ms
		timer <- .Tcl(paste("after", as.integer(delay)[1], "realtimeLoop"))
	}
	return(invisible(timer))
}

"realtimeOptions" <- function (
lstdir = ".", 		# Path of the list file of the current FlowCAM experiment
ZIClass,			# Classifier to use
type = "b", 		# "b" : barplot, "l" : line alpha code
SizeThreshold = NULL, # NULL or Size threshold in µm alpha code
breaks = seq(0.05, 3, by = 0.1),  # in mm
convdir = ".", 		# Path of the conversion table
ImgPerSec = 7,
FlowCell = 600,
Concentration = "p/mL",	# "Absolute", "Relative" or "p/mL"
Collage = NULL, 	# NULL: no mobile window, TRUE: use collage, FALSE: use number of vignettes
Size = 5,			# The size of the mobile window
Lag = 2,			# The lag between two successive mobile windows
Abd = NULL,			# NULL, TRUE or FALSE
Bio = NULL,			# NULL, TRUE or FALSE
Spectra = NULL,		# NULL, TRUE or FALSE
Compa = NULL,		# NULL, FALSE or a path of a list of sample to compare with
Group = NULL,		# The group to recognize and/or plot
identify = FALSE,	# Identify points on plot (TRUE or FALSE)
log = FALSE,		# Transform data in log10(x + 1)
Slope = FALSE)
{
	# Check and/or convert arguments
	lstdir <- as.character(lstdir)[1]
	
	if (!inherits(ZIClass, "ZIClass"))
		stop("'ZIClass' must be a classifier of class 'ZIClass'")

	type <- as.character(type)[1]
    if (!type %in% c("b", "l"))
		stop("type must be 'b' (barplot) or 'l' (lines)")

	if (!is.null(SizeThreshold) && !is.numeric(SizeThreshold))
		stop("'SizeThreshold' must be a numeric value in microns or NULL")
		
	if (!is.numeric(breaks))
		stop("breaks must be the size interval (a vector of numeric values)")
  
	convdir <- as.character(convdir)[1]
	
	ImgPerSec <- as.numeric(ImgPerSec)[1]
	if (ImgPerSec < 0)
		stop("'ImgPerSec' must be the number of images taken by the FlowCAM per Second")

	FlowCell <- as.integer(FlowCell)[1]

	Concentration <- as.character(Concentration)[1]
	if (!Concentration %in% c("p/mL", "Relative", "Absolute"))
		stop("'Concentration' must be \"p/mL\", \"Absolute\" or \"Relative\"")

	if (!is.null(Collage)) Collage <- isTRUE(Collage)

	Size <- as.numeric(Size)[1]
	if (Size <= 0)
		stop("'Size' must be the value of the interval size (a positivce number)")

	Lag <- as.numeric(Lag)[1]
	if (Lag < 0)
		stop("'Lag' must be the value of the lag between two mobile windows (postive or zero)")
  
	if (!is.null(Abd)) Abd <- isTRUE(Abd)
  
	if (!is.null(Bio)) Bio <- isTRUE(Bio)
  
	if (!is.null(Spectra)) Spectra <- isTRUE(Spectra)

	if (!is.null(Compa)) {
		Compa <- as.character(Compa)
		if (length(Compa) == 1) {
			if(length(grep(pattern = ".[Zz][Ii][Dd]", x = Compa)) >= 1) {
				# This a zid file
				Smp <- read.zid(Compa)
			} else {
				# This is a list file
				Smp <- read.lst(Compa)
			}
			Smp <- predict(ZIClass, Smp, calc.vars = FALSE, class.only = FALSE)
			Smp <- BiomassTab(ZIDat = Smp, conv = convdir, RealT = TRUE)
			List <- list(Smp)
			names(List) <- noext(basename(Compa))
		} else {
			List <- list()
			if (length(grep(pattern = ".[Zz][Ii][Dd]", x = Compa)) >= 1) {
				# This a zid file
				for (i in 1 : length(Compa))
					List[[i]] <- BiomassTab(ZIDat = predict(ZIClass,
						read.zid(Compa[i]), calc.vars = FALSE,
						class.only = FALSE), conv = convdir, RealT = TRUE)
			} else {
				# This is a list file
				for (i in 1 : length(Compa))
					List[[i]] <- BiomassTab(ZIDat = predict(ZIClass,
						read.lst(Compa[i]), calc.vars = FALSE,
						class.only = FALSE), conv = convdir, RealT = TRUE)
			}
			names(List) <- noext(basename(Compa))
		}
      	Compa <- List
    } else Compa <- FALSE
  
	if (!is.null(Group)) Group <- as.character(Group)[1] 
  
	identify <- isTRUE(identify)
  
	log <- isTRUE(log)
  
	Slope <- isTRUE(Slope)

	# Construct the options object and save it in options
	opts <- list(lstdir, ZIClass, type, SizeThreshold, breaks, convdir,
		ImgPerSec, FlowCell, Size, Lag, Concentration, Abd, Bio, Spectra, Group,
		Compa, identify, log, Slope)
	options("ZIrealtimeOpts" = opts)
	return(invisible(opts))
}

"realtimeProcess" <- function(List, ZIClass, conv = c(1, 0, 1), Collage = NULL,
FlowCell = 600, ImgPerSec = 5, Size = 5, Lag = 2)
{
	if (!existsTemp("rtData")) {
		# First iteration
		# Calculation of elapsed time
		TIME <- TimeElapsed(List)
		# Read the list file
		tab <- read.lst(List, skip = 2)
		# If no measurements in the list file
		if (dim(tab)[1] == 0) {
			print("The list file is empty")
			rmTemp("rtData")
		} else {
			rec <- getTemp("rtRecord")
			if (is.null(rec)) {
				rec <- predict(ZIClass, tab, calc.vars = FALSE,
					class.only = FALSE) # Ident
				rec <- BiomassTab(ZIDat = rec, conv = conv,
					RealT = TRUE) # Biomass
				# Proceed to the mobile window
				if (!is.null(Collage))
					rec <- mobileWindow(RealT = TRUE)
				# Add Sec and Vol column to the general table
				if (!"sec" %in% names(rec))
					rec <- AddSecVol(ZIDat = rec, FlowCell = FlowCell,
						ImagePerSec = ImgPerSec)
				assignTemp("rtRecord", rec)
			}
		}
		# Create Attributes
		if (!is.null(rec)) {
			Abd <- table(rec$Ident)
			Bio <- tapply(rec$Biomass, rec$Ident, sum)
			# Remove NA and 0 from tables Abd and Bio to avoid any log problem
			Abd[is.na(Abd)] <- 1e-09
			Abd[Abd == 0] <- 1e-09
			Bio[is.na(Bio)] <- 1e-09
			Bio[Bio == 0] <- 1e-09
			# Add attributes to rec
			attr(rec, "Abd") <- Abd
			attr(rec, "Bio") <- Bio
			attr(rec, "Skip") <- nrow(tab)
			# used to know the number of row to skip to get new measurements
			attr(rec, "RowToSkip") <- nrow(rec)
			# used to create a trnasect after the cruise
			attr(rec, "VolumeDigitized") <- VolumeDigi(rec = rec,
				FlowCell = FlowCell, ImgPerSec = ImgPerSec)
			# Attribute for time elapsed
			attr(rec, "TimeElapsed") <- TIME
			# this parameter is used by VolumeDigi(List)
			assignTemp("rtRecord", rec)
		}
	} else {
		# There is one lst (non empty tab) list in memory
		rec1 <- rec # recognition table from the previous iteration
		Abd1 <- attr(rec1, "Abd") # Abd from the previous iteration
		Bio1 <- attr(rec1, "Bio") # Bio from the previous iteration
		# read the complete table to know if new results have been added
		# Calculation of elapsed time
		TIME <- TimeElapsed(List)
		New <- read.lst(List, skip = 2) # read the New tab after the elapsed time
		# Check if new measurements added in New
		attr(rec, "Skip") <- c(attr(rec, "Skip"), nrow(New))
		# Comparision with the previous Skip
		if (attr(rec, "Skip")[length(attr(rec, "Skip"))] !=
			attr(rec, "Skip")[length(attr(rec, "Skip"))- 1]) {
			# Extract only new measurements
			tab <- New[(attr(rec, "Skip")[length(attr(rec, "Skip")) - 1] + 1):
				attr(rec, "Skip")[length(attr(rec, "Skip"))], ]
			# Return the object in R
			tab <- getTemp("rtData")
			# recognition of tab
			rec <- predict(ZIClass, tab, calc.vars = FALSE,
				class.only = FALSE) # Ident
			rec <- BiomassTab(ZIDat = rec, conv = conv, RealT = TRUE) # Biomass

			# Add Sec and Vol information
			if (!"sec" %in% names(rec)) 
				rec <- AddSecVol(ZIDat = rec, FlowCell = FlowCell,
					ImagePerSec = ImgPerSec)
			# Create new tables
			Abd <- table(rec$Ident)
			Bio <- tapply(rec$Biomass, rec$Ident, sum)
			# Remove NA and 0
			Abd[is.na(Abd)] <- 1e-09
			Abd[Abd == 0] <- 1e-09
			Bio[is.na(Bio)] <- 1e-09
			Bio[Bio == 0] <- 1e-09
			# Paste the two tables : the previous and the new ones
			rec <- rbind(rec1, rec)
			if (!is.null(Collage)) {
				# calculation of the rest of the mobile window
				attr(rec, "Intervals") <- attr(rec1, "Intervals")
				# because it is used to determine the range in mobileWindow
				rec <<- mobileWindow(RealT = TRUE)
				NewInterval <- attr(rec, "Intervals")
				# Extracted here because it will be lost after the rbind operation
				NewMobile_Tab <- attr(rec, "Mobile_Tab")
				NewTime <- attr(rec, "Time")
			}
			# When we rbind rec, we loose attributes --> Add new attributes
			attr(rec, "Skip") <- c(attr(rec, "Skip"), nrow(New))
			if (!is.null(Abd1)) attr(rec, "Abd") <<- cbind(Abd1, Abd)
			if (!is.null(Bio1)) attr(rec, "Bio") <<- cbind(Bio1, Bio)
			if (!is.null(Collage)) {
				# Attribute of the mobile window
				attr(rec, "Time") <- c(attr(rec1, "Time")[
					-length(attr(rec1, "Time"))], NewTime)
				# everything excepted last iteration
				attr(rec, "Intervals") <- cbind(attr(rec1, "Intervals")[
					, -ncol(attr(rec1, "Intervals"))], NewInterval)
				# everything excepted last iteration
				attr(rec, "Mobile_Tab") <- cbind(attr(rec1, "Mobile_Tab")[
					, -ncol(attr(rec1, "Mobile_Tab"))], NewMobile_Tab[, -1])
				# everything excepted last iteration
				attr(rec, "Size") <- Size
				attr(rec, "Lag") <- Lag
				attr(rec, "Collage") <- Collage
				assignTemp("rtRecord", rec)
			}
		} else {
			# There are no new measurements in list file
			cat("There are no new measurements in list file or experiment finished\n")
			# Remove the last element of the Skip attribute
			attr(rec, "Skip") <- attr(rec, "Skip")[
				-length(attr(rec, "Skip"))]
			# Add attributes
			if (!is.null(Abd1))
				attr(rec, "Abd") <- cbind(attr(rec, "Abd"),
					rep(1e-09, nrow(attr(rec, "Abd"))))
			if (!is.null(Bio1))
				attr(rec, "Bio") <- cbind(attr(rec, "Bio"),
					rep(1e-09, nrow(attr(rec, "Bio"))))
			assignTemp("rtRecord", rec)
		}
		# Attributes with the number of rows to skip
		attr(rec, "RowToSkip") <- c(attr(rec, "RowToSkip"), nrow(rec))
		# 2010-01-06
		# Attribute for time elapsed
		attr(rec, "TimeElapsed") <- TIME
		# this parameter is used by VolumeDigi()
		# Calculation of digitized volume
		# 2010-01-06

		# 2009-11-26
		attr(rec, "VolumeDigitized") <- c(attr(rec, "VolumeDigitized"),
			VolumeDigi(rec = rec, FlowCell = FlowCell, ImgPerSec = ImgPerSec))
		# 2009-11-26
		assignTemp("rtRecord", rec)
	}
	# Write a table with Volume and nrow of rec
	Time <- attr(rec, "TimeElapsed")
	Vol <- attr(rec, "VolumeDigitized")
	Row <- attr(rec, "RowToSkip")
	write.table(data.frame(Time, Vol, Row), file = file.path(dirname(List),
		paste(basename(List), "RowToSkip.txt", sep = "_")),
		sep = "\t", dec = ".", row.names = FALSE)
	# Save data as Rdata
	save(rec, file = file.path(dirname(List),
		paste(basename(List),"rec.Rdata", sep = "_")))
	# Change class(rec)
	if (!inherits(rec, "RealT"))
		class(rec) <- c("RealT", class(rec))
	assignTemp("rtRecord", rec)
}

# Calculation of elapsed time and create the attr(rec, "TimeElapsed")
"TimeElapsed" <- function (List)
{
	# Info <- file.info(getOption("Path"))
	Info <- file.info(List)
	Time <- getTemp("rtTime")
	if (is.null(Time)) {
		# First iteration
		Passed <- difftime(time1 = Info$ctime, time2 = Sys.time(), units = "sec")
		Time2 <- abs(as.numeric(Passed))
		assignTemp("rtTime", Time2)
	} else {
	    Passed <- difftime(time1 = Info$ctime, time2 = Sys.time(), units = "sec")
	    Time2 <- abs(as.numeric(Passed)) - sum(Time)
	    assignTemp("rtTime", c(Time, Time2))
	}
	return(Time)
}

# Calculation of the digitized volume using the Time elapsed attirbute
"VolumeDigi" <- function (rec, FlowCell = 600, ImgPerSec = 5)
{
	Height <- (767 - 0) * unique(rec$FIT_Cal_Const)
	Width <- (1023 - 0) * unique(rec$FIT_Cal_Const)
	Area <- Height * Width
	Volume <- (Area / (10^8)) * (FlowCell/10000) # mL
	if (all(is.na(rec$FIT_Source_Image))) {
		# We have to calculate volume using the elapsed time
		res <- Volume * ImgPerSec * attr(rec, "TimeElapsed")
	} else {
		# We have the information from the new FlowCAM about the raw images
		if (length(attr(rec, "RowToSkip")) == 1) { 
			# First iteration completed
			Raw <- rec$FIT_Source_Image[attr(rec, "RowToSkip")]
		} else {
			# more than one iteration
			if (attr(rec, "RowToSkip")[length(attr(rec, "RowToSkip")) - 1] ==
				attr(rec, "RowToSkip")[length(attr(rec, "RowToSkip"))]) {
				# No new data added at the list file --> Use the elapsed time
				# to apprixamte number of raw images
				Raw <- ImgPerSec * attr(rec, "TimeElapsed")[
					length(attr(rec, "TimeElapsed"))]
			} else {
				# New measurements are added at hte end of the list file
				# 2009-11-26
				NewRaw <- rec$FIT_Source_Image[attr(rec, "RowToSkip")[
					length(attr(rec, "RowToSkip"))]]
				PrevisousRaw <- rec$FIT_Source_Image[attr(rec, "RowToSkip")[
					length(attr(rec, "RowToSkip")) - 1] + 1]
				Raw <- (NewRaw - PrevisousRaw) + 1
				# 2009-11-26
			}
		}
		res <- Volume * Raw
	}
	return(res)
}

# Function to add a column for biomass calculation
"BiomassTab" <- function (ZIDat, conv = c(1, 0, 1), RealT = FALSE)
{
	if (!isTRUE(RealT)) {
		if (!inherits(ZIDat, "ZIDat"))
			stop("ZIDat must be a 'ZIDat' object")
	}
	# Convert ECD (biomass calculation, etc.)
	# Check arguments
	Smp <- ZIDat
	if (nrow(Smp) == 0)
		stop("no data for this sample/taxa in ZIDat")
	# Add P1/P2/P3 conversion params to the table
	if (inherits(conv, "data.frame")) {
		if (!all(names(conv)[1:4] == c("Group", "P1", "P2", "P3") ||
			c("Group", "a", "b", "c")))
			stop("conv must have 'Group', 'P1', 'P2', 'P3' or 'a', 'b', 'c' columns!")
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
                # Fill all the other groups with the formula for other
				# and issue a warning
                NotThere <- IdSmpU[!(IdSmpU %in% IdConv)]
                warning("Applying default [other] biomass conversion for ",
					paste(NotThere, collapse = ", "))
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
	if (!isTRUE(RealT)) {
		Smp$Biomass <- (Smp$P1 * Smp$ECD + Smp$P2)^Smp$P3 * Smp$Dil
	} else {
		Smp$Biomass <- (Smp$P1 * Smp$FIT_Diameter_ABD + Smp$P2)^Smp$P3
	}
    # AZTI special treatment
    # introducimos la formula de montagnes y la correccion para ESD(2.61951)
	#Smp$Biomass <- (0.109 * (pi*4/3*((2.61951*Smp$ECD)/2)^3)^0.991) * Smp$Dil

	# Add metadata attribute
	attr(Smp, "metadata") <- attr(ZIDat, "metadata")
	return(Smp)
}

# Add Sec and Volume column
"AddSecVol" <- function (ZIDat, FlowCell, ImagePerSec)
{
	if (!inherits(ZIDat, "data.frame"))
		stop("ZIDat must be an object of class 'data.frame'")
	if (!is.numeric(FlowCell))
		stop("FlowCell must be a numrical value with the depth of the FlowCell used")
	if (!is.numeric(ImagePerSec))
		stop("ImagePerSec must be the number of image per second saved by the FlowCAM")
	ZIDat$Sec <- ZIDat$FIT_Source_Image / ImagePerSec
	Vol <- Volume(Lst = ZIDat, FlowCell = FlowCell)
	ZIDat$Vol <- ZIDat$FIT_Source_Image * Vol
	return(ZIDat)
}

# Mobile window
"mobileWindow" <- function (ZIDat, size = 1, lag = 1, Collage = FALSE, FlowCell,
ImagePerSec, RealT = FALSE)
{
	if (!isTRUE(RealT)) {
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
			ZIDat <- AddSecVol(ZIDat = ZIDat, FlowCell = FlowCell,
				ImagePerSec = ImagePerSec)
		if (isTRUE(Collage)) {
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
					warning("The loop has been stoped because at the end of the table")
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
					warning("The loop has been stoped because at the end of the table")
					break
				}
			}
		}
		attr(ZIDat, "Time") <- Time
		attr(ZIDat, "Size") <- size
		attr(ZIDat, "Lag") <- lag
		attr(ZIDat, "Intervals") <- df
		attr(ZIDat, "Mobile_Tab") <- Tab
		attr(ZIDat, "Collage") <- Collage
		return(ZIDat)
	} else {
		Temp <- numeric()
		if (!"sec" %in% names(rec))
			assignTemp("rtRecord", AddSecVol(ZIDat = rec,
				FlowCell = getOption("FlowCell"),
				ImagePerSec = getOption("ImgPerSec")))
		if (getOption("Collage")) {
			# determine the starting point for the loop
			if (is.null(attr(rec, "Intervals"))) {
				Range <- 1:(length(levels(rec$FIT_Filename)) - (2 * getOption("Size")))
			} else {
				Range <- attr(rec, "Intervals")[1, ncol(attr(rec, "Intervals"))]:
					(length(levels(rec$FIT_Filename)) - (2 * getOption("Size")))
			}
			for (i in Range) {
				if (grep(i, Range)[1] <= 1) {
					df <- data.frame(Int = (0 + i) : (i + (2* getOption("Size"))))
					Tab <- data.frame(table(rec[rec$FIT_Filename %in%
						levels(rec$FIT_Filename)[df[, (i + 1) - Range[1]]], ]$Ident))
				} else {
					df[, (i + 1) - Range[1]] <- data.frame(Int = df[, ((i + 1) -
						Range[1] - 1)] + getOption("Lag"))
					Tab[, ((i + 1) - Range[1]) + 1] <-
						table(rec[rec$FIT_Filename %in%
						levels(rec$FIT_Filename)[df[, (i + 1) - Range[1]]], ]$Ident)
				}
				Temp[(i + 1) - Range[1]] <- mean(rec[rec$FIT_Filename %in%
					levels(rec$FIT_Filename)[df[getOption("Size") + 1, (i + 1) -
					Range[1]]], ]$Sec)
				if (!all(df[, (i + 1) - Range[1]] <
					length(levels(rec$FIT_Filename)))) {
					warning("The loop has been stoped because at the end of the table")
					break
				}
			}
		} else {
			# Determine the starting point for the loop
			if (is.null(attr(rec, "Intervals"))) {
				Range <- 1:(dim(rec)[1] - (2 * getOption("Size")))
			} else {
				Range <- attr(rec, "Intervals")[1, ncol(attr(rec, "Intervals"))]:
					(dim(rec)[1] - (2 * getOption("Size")))
			}
			for (i in Range) {
				if (grep(i, Range)[1] <= 1) {
					df <- data.frame(Int = (0 + i) : (i + (2 * getOption("Size"))))
					Tab <- data.frame(table(rec[df[, (i + 1) - Range[1]],]$Ident))
				} else {
					df[, (i + 1) - Range[1]] <- data.frame(Int = df[, ((i + 1) -
						Range[1] - 1)] + getOption("Lag"))
					Tab[, ((i + 1) - Range[1])+1] <- table(rec[df[, (i + 1) -
						Range[1]], ]$Ident)
				}
				Temp[(i + 1) - Range[1]] <- rec[df[getOption("Size"), (i + 1) -
					Range[1]] ,]$Sec
				if (!all(df[, (i + 1) - Range[1]] < nrow(rec))) {
					warning("The loop has been stoped because at the end of the table")
					break
				}
			}
		}
		attr(rec, "Time") <- Temp
		attr(rec, "Size") <- getOption("Size")
		attr(rec, "Lag") <- getOption("Lag")
		attr(rec, "Intervals") <- df
		attr(rec, "Mobile_Tab") <- Tab
		attr(rec, "Collage") <- getOption("Collage")
		assignTemp("rtRecord", rec)
		return(rec)
	}
}

"Volume" <- function (Lst, FlowCell)
{
	Height <- (767 - 0) * unique(Lst$FIT_Cal_Const)
	Width <- (1023 - 0) * unique(Lst$FIT_Cal_Const)
	Area <- Height * Width
	Volume <- (Area / (10^8)) * (FlowCell/10000) # mL
	return(Volume)
}
