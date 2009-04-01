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
            
"ZIDlg" <-
	function() {
	# If the window is already created, just activate it...
	if ("ZIDlgWin" %in% WinNames()) {
		ZIDlgWin <- WinGet("ZIDlgWin")
		tkfocus(ZIDlgWin)  	# Doesn't work with Rgui.exe, but next command does
		tkwm.deiconify(ZIDlgWin)
    	return(invisible())
	}

	(require(tcltk2) || stop("Package 'tcltk2' is required. Please, install it first!"))
	(require(svMisc) || stop("Package 'svMisc' (bundle 'SciViews') is required. Please, install it first!"))
	(require(svWidgets) || stop("Package 'svWidgets' is required. Please, install it first!"))

	# Construct the window
	tkWinAdd("ZIDlgWin", title = paste(getTemp("ZIname"), "assistant"), pos = "-10+10")
	ZIDlgWin <- WinGet("ZIDlgWin")
    tkwm.withdraw(ZIDlgWin)	# Do not show it until it is completelly constructed!
	on.exit(tkwm.deiconify(ZIDlgWin))
	# Change the icon of that window (if under Windows)
	if (isWin()) tk2ico.set(ZIDlgWin, getTemp("ZIico"))
	# Add a menu (load it from a spec file)
	Pkg <- getTemp("ZIguiPackage")
	if (is.null(Pkg)) Pkg <- "zooimage"
	MenuReadPackage(Pkg, file = "MenusZIDlgWin.txt")

	# Add a toolbar (read it from file 'ToolbarZIDlgWin.txt')
	ToolRead(file.path(getTemp("ZIgui"), "ToolbarsZIDlgWin.txt"))

	# Add a statusbar with a text and a progressbar
	status <- tk2frame(ZIDlgWin)
	statusText <- tk2label(status, text = paste("Ready -", getwd()), justify = "left",
		anchor = "w", width = 60)
	statusProg <- tk2progress(status, orient = "horizontal", from = 0,  to = 100)
	tkpack(statusProg, side = "right")
	tkpack(statusText, side = "left", fill= "x")
	tkpack(status, side = "bottom", fill = "x")
	tkpack(tk2separator(ZIDlgWin), side = "bottom", fill = "x")
	# Keep track of statusText / statusProg
	assignTemp("statusText", statusText)
	assignTemp("statusProg", statusProg)
	## Change value of the progressbar
	#tkconfigure(getTemp("statusProg"), value = 50)
	## Change text of the statusbar
	#tkconfigure(getTemp("statusText"), text = paste("Ready -", getwd()))

	# Disable menu pointing to nonexisten software
	if (!isWin()) {
		# The activate R console & R graph do not work elsewhere
        MenuStateItem("$Tk.ZIDlgWin/Apps", "&R Console", FALSE)
		MenuStateItem("$Tk.ZIDlgWin/Apps", "Active R &Graph", FALSE)
	}
	# For each of the six external programs, look if they are accessible, otherwise, inactivate
	if (is.null(getOption("ZIEditor")))
         MenuStateItem("$Tk.ZIDlgWin/Apps", "&Metadata editor (Sc1)", FALSE)
    if (is.null(getOption("ImageEditor")))
         MenuStateItem("$Tk.ZIDlgWin/Apps", "Image &analyzer (ImageJ)", FALSE)
    if (is.null(getOption("ImageViewer")))
         MenuStateItem("$Tk.ZIDlgWin/Apps", "Image &viewer (XnView)", FALSE)
    if (is.null(getOption("ZipViewer")))
         MenuStateItem("$Tk.ZIDlgWin/Apps", "&Zip-Zid viewer (Filzip)", FALSE)
    if (is.null(getOption("DVDBurner")))
         MenuStateItem("$Tk.ZIDlgWin/Apps", "CD-DVD &burner (DeepBurner)", FALSE)
    if (is.null(getOption("VueScan")))
         MenuStateItem("$Tk.ZIDlgWin/Apps", "Simple acquisition (&VueScan)", FALSE)

	# Change the window to non resizable and topmost (f under Windows)
	if (isWin()) tcl("wm", "attributes", ZIDlgWin, topmost = 1)
	tkwm.resizable(ZIDlgWin, 0, 0)
	# Focus on that window
	tkfocus(ZIDlgWin)	# Doesn't work with Rgui.exe, but tkwm.deiconify does
}

# Function for the RGui menu
"AboutZI" <-
	function(graphical = FALSE){
	msg <- getTemp("ZIverstring")
	### TODO: add more information here (copyright, authors, check satellite pgms, ...)
	if (graphical) {
		tkmessageBox(message = msg, title = "About...", icon = "info", type = "ok")
	} else cat(msg, "\n")
}

"ExitZI" <-
	function(){
	detach("package:zooimage")	# This is useful to allow updating the package!
	cat("zooimage package unloaded; To restart it, issue:\n> library(zooimage)\n")
}


# Functions for the assistant menu
"closeAssistant" <-
	function() {
	tkWinDelete("ZIDlgWin")
}

"closeZooImage" <-
	function() {
	closeAssistant()
	ExitZI()
}

"viewManual" <-
	function() {
	 browseURL(file.path(getTemp("ZIetc"), "ZooImageManual.pdf"))
}

"focusR" <-
	function() {
	# Switch the focus to the R console
	### TODO: notify this command is not available elsewhere (inactivate menu?)
	if (isRgui()) {
		require(grDevices)
		bringToTop(-1)
	}
}

"focusGraph" <-
	function() {
	# Switch the focus to the active R graph (create one if there is no graph device)
	### TODO: notify this command is not available elsewhere (inactivate menu?)
   	require(grDevices)
	if (is.null(dev.list())) {
		windows()
	} else {
		# Activate current graph window
		if (isRgui()) bringToTop()
	}
}

"startPgm" <-
	function(program, cmdline = "", switchdir = FALSE, iconize = FALSE, wait = FALSE) {
	# Look if the program path is recorded in the options
	pgmPath <- getOption(program)
	if (!is.null(pgmPath) && file.exists(pgmPath)) {
		# Do we need to switch directory?
		if (switchdir) {
			curdir <- getwd()
			on.exit(setwd(curdir))
			setwd(dirname(pgmPath))
		}
		# Start it
		system(paste(pgmPath, cmdline), wait = wait)
	} else stop("Program not found!")
	# Do we need to iconize the assistant?
	if (iconize && !is.null(WinGet("ZIDlgWin")))
		tkwm.iconify(WinGet("ZIDlgWin"))
}

"modalAssistant" <-
	function(title, text, init, options = NULL, check = NULL,
	select.file = NULL, returnValOnCancel = "ID_CANCEL", help.topic = NULL) {
	# Create an assistant dialog box which behaves as a modal dialog box

    (require(tcltk2) || stop("Package 'tcltk2' is required. Please, install it first!"))
	(require(svMisc) || stop("Package 'svMisc' (bundle 'SciViews') is required. Please, install it first!"))
	(require(svWidgets) || stop("Package 'svWidgets' is required. Please, install it first!"))

	text <- paste(text, collapse = "\n")
	try(tkWinAdd("ZIAssist", title = title, bind.delete = FALSE))
	ZIAssist <- WinGet("ZIAssist")
    tkbind(ZIAssist, "<Destroy>", function() {
		tkgrab.release(ZIAssist)
		tkWinDelete("ZIAssist")
		#tkfocus(WinGet("ZIDlgWin"))
	})
	# Assign cancel by default to the return value
    assignTemp("ZIret", returnValOnCancel)
    # Do not show it until it is completelly constructed!
	tkwm.withdraw(ZIAssist)
	# Change the icon of that window (if under Windows)
    if (isWin()) tk2ico.set(ZIAssist, getTemp("ZIico"))
	# This is the variable holding the result
	resVar <- tclVar(init)
	# Draw the dialog area
	dlgarea <- tk2frame(ZIAssist)
	# Place the logo to the left
    Logo <- tklabel(dlgarea,image = ImgGet("$Tk.logo"), bg = "white")
	# Place dialog box data
	txtarea <- tk2frame(ZIAssist)
	Text <- tk2label(txtarea, text = text, width = 50) #### TODO: this causes a problem in Tile 0.7.2?! , justify = "left")
	tkgrid(Text, stick = "w")
	# Do we put options?
	if (!is.null(options)) {
		for (i in 1:length(options)) {
			rb <- tk2radiobutton(txtarea)
			tkconfigure(rb, variable = resVar, value = options[i],
				text = options[i]) #### TODO: this causes a problem in Tile 0.7.2?! , justify = "left")
			tkgrid(rb, sticky = "w")
		}
	}
	# Do we have to place a checkbox?
	if (!is.null(check)) {
		cb <- tk2checkbutton(txtarea)
		tkconfigure(cb, variable = resVar, text = check) #### TODO: this causes a problem in Tile 0.7.2?! , justify = "left")
		tkgrid(cb, sticky = "w")
	}
	# Place everything in the dialog box
	tkgrid(Logo, txtarea)
	tkpack(dlgarea, anchor = "nw")
	# Place buttons

    "onOK" <- function() {
        assignTemp("ZIret", tclvalue(resVar))
        tkgrab.release(ZIAssist)
        tkWinDelete("ZIAssist")
		#tkfocus(WinGet("ZIDlgWin"))
    }
    "onCancel" <- function() {
        tkgrab.release(ZIAssist)
        tkWinDelete("ZIAssist")
		#tkfocus(WinGet("ZIDlgWin"))
    }
    butbar <- tk2frame(ZIAssist)
    OK.but <- tk2button(butbar, text = "   OK   ", command = onOK)
    Cancel.but <- tk2button(butbar, text = " Cancel ", command = onCancel)
	if (is.null(help.topic)) {
    	tkgrid(OK.but, Cancel.but, sticky = "e")
	} else {    # Create also a help button
		"onHelp" <- function() {
			eval(parse(text = paste("browseURL(findhtmlhelp('", help.topic, "'))",
				sep = "")), envir = .GlobalEnv)
		}
        Help.but <- tk2button(butbar, text = "  Help  ", command = onHelp)
        tkgrid(OK.but, Cancel.but, Help.but, sticky = "e")
	}
	tkpack(butbar, side = "bottom", fill = "x")
	tkpack(tk2separator(ZIAssist), side = "bottom", fill = "x")
    tkbind(ZIAssist, "<Return>", onOK)
	if (isWin()) tcl("wm", "attributes", ZIAssist, toolwindow = 1, topmost = 1)
	tkwm.resizable(ZIAssist, 0, 0)
	# Focus on that window
	tkfocus(ZIAssist)	# Doesn't work with Rgui.exe, but tkwm.deiconify does
    tkwm.deiconify(ZIAssist)
    tkgrab.set(ZIAssist)
    tkwait.window(ZIAssist)
    return(getTemp("ZIret"))
}

"acquireImg" <-
	function() {
	# Show an assitant dialog box allowing to choose between VueScan and a different
	# acquisition program... remember that setting in the registry under Windows

	# First read the registry to determine which software in recorded there...
 	Asoft <- getKey("AcquisitionSoftware", "VueScan")
	if (Asoft == "VueScan") {
		opts <- c("VueScan", "Another software...")
		othersoft <- ""
		defval <- "VueScan"
	} else {
		othersoft <- Asoft
       	defval <- basename(othersoft)
		opts <- c("VueScan", defval, "Another software...")
	}

	# Then, show the dialog box
 	res <- modalAssistant(paste(getTemp("ZIname"), "picture acquisition"),
		c("To acquire digital plankton images,",
		"you can use a specialized equipment or",
		"a digital camera on top of a binocular or",
		"a flatbed scanner, ...",
		"",
		"To pilot a scanner or rework RAW digicam images",
		"you can use 'Vuescan'.",
		"You can also specify to use your own software.",
		"", "", "Use:", ""), init = defval,
		options = opts, help.topic = "acquireImg")
	# Analyze result
	if (res == "ID_CANCEL") return(invisible())
	# Did we selected "Another software..."?
	if (res == "Another software...") {
		# Ask for selecting this software
        Asoft <- choose.files(caption = "Select a program...", multi = FALSE,
			filters = c("Executables (*.exe;*.com;*.bat)", "*.exe;*.com;*.bat"))
		if (Asoft == "") return(invisible()) # Cancelled dialog box
	}
	# Did we selected "VueScan"
	if (res == "VueScan") {
		startPgm("VueScan", switchdir = TRUE)
		setKey("AcquisitionSoftware", "VueScan")
		return(invisible())
	}
	# We should have selected a custom software...
	if (!file.exists(Asoft))
		stop("Program '", Asoft, "' not found!")
	# Start the program
	system(paste('"', Asoft, '"', sep = ""), wait = FALSE)
	# Record it in the registry key
    setKey("AcquisitionSoftware", Asoft)
}

"importImg" <-
	function() {
	# Import images... basically, you can select a series of images in a
	# directory, and the program asks for writing the associated .zim files,
	# or you can access other processes that automatically build .zim files
	# and/or import images/data, including custom processes defined in
	# separate 'ZIEimport' objects (see FlowCAM import routine for an example)

	# Get a list of 'ZIEimport' objects currently loaded in memory
	### TODO... Rework everything. What follows is old code!
	ImgFilters <- as.matrix(data.frame(title = c("Tiff image files (*.tif)",
		"Jpeg image files (*.jpg)", "Zooimage import extensions (Import_*.zie)", "Table and ImportTemplate.zie (*.txt)"), #, "FlowCAM zipped files (*.zfc)"),
		pattern = c("*.tif", "*.jpg", "Import_*.zie", "*.txt"))) #, "*.zfc")))
	# Get last image type that was selected
	Index <- as.numeric(getKey("ImageIndex", "1"))
	# Get a list of images
    Images <- choose.files(caption = "Select data to import...",
		multi = TRUE, filters = ImgFilters, index = Index)
	# Look if there is at least one image selected
	if (length(Images) == 0) return(invisible())
    dir <- dirname(Images[1])
	Images <- basename(Images)
	# Determine which kind of data it is
    if (regexpr("[.][zZ][fF][cC]$", Images[1]) > 0) {
        setKey("ImageIndex", "5")
		return(importFlowCamFiles(path = dir, ZFCfiles = Images, check = FALSE, show.log = TRUE))
	} else if (regexpr("^Import_.*[.]zie$", Images[1]) > 0) {
		pattern <- "[.][zZ][iI][eE]$"
        setKey("ImageIndex", "3")
        return(make.zie(path = dir, Filemap = Images[1], check = TRUE, show.log = TRUE))
    } else if (regexpr("[.][tT][xX][tT]$", Images[1]) > 0) {
		pattern <- "[.][tT][xX][tT]$"
        setKey("ImageIndex", "4")
        logProcess("Creating .zie file...")
        cat("Creating .zie file...\n")
        ziefile <- compile.zie(path = dir, Tablefile = Images[1])
        cat("...OK!\n")
		res <- make.zie(path = dirname(ziefile), Filemap = basename(ziefile), check = TRUE, show.log = TRUE)
		if (res) { # Everything is fine...
			# Move the table and copy the template to the '_raw' subdirectory too
			path <- dirname(ziefile)
			tplfile <- file.path(path, Images[1])
			file.rename(tplfile, file.path(path, "_raw", basename(tplfile)))
			# Move also possibly the .xls equivalent
			xlsfile <- sub("\\.[tT][xX][tT]$", ".xls", tplfile)
			if (xlsfile != tplfile && file.exists(xlsfile))
			    file.rename(xlsfile, file.path(path, "_raw", basename(xlsfile)))
			file.rename(file.path(path, "ImportTemplate.zie"), file.path(path, "_raw", "ImportTemplate.zie"))
		}
		return(res)
	} else if (regexpr("[.][tT][iI][fF]$", Images[1]) > 0) {
		pattern <- "[.][tT][iI][fF]$"
        setKey("ImageIndex", "1")
	} else if (regexpr("[.][jJ][pP][gG]$", Images[1]) > 0) {
        pattern <- "[.][jJ][pP][gG]$"
        setKey("ImageIndex", "2")
	} else stop("Unrecognized data type!")
	# If there is no special treatment, just make all required .zim files for currently selected images
	make.zim(dir = dir, pattern = pattern, images = Images, show.log = TRUE)
}

"processImg" <-
	function() {
	# Display a dialog box telling how to process images using ImageJ
	# When the user clicks on 'OK', ImageJ is started... + the checkbox 'close R'
     res <- modalAssistant(paste(getTemp("ZIname"), "picture processing"),
		c(paste("Once images are acquired and imported into", getTemp("ZIname")),
		"(they have correct associated metadata), they must be",
		"processed.",
		"",
		"To do so, start 'ImageJ' (just click 'OK') and select",
		paste("the method for your images in 'Plugins -> ", getTemp("ZIname"), "'.", sep = ""),
		"",
		"For very large images, or on computers with limited",
		"RAM memory, it is advised to close all other programs.",
		"Check the option below to close R in this case.", "", ""),
		init = "0", check = "Close R before running 'ImageJ'", help.topic = "processImg")
	# Analyze result
	if (res == "ID_CANCEL") return(invisible())
 	# Start ImageJ
	if (!is.null(getOption("ImageEditor")))
		startPgm("ImageEditor", switchdir = TRUE, iconize = TRUE)
	# Do we have to close R?
	if (res == "1") q()
}

"makeZid" <-
	function() {
	# Finalize .zid files (and possibly also .zip files by updating their comment)
    res <- modalAssistant(paste(getTemp("ZIname"), "data processing"),
		c("You should have processed all your images now.",
		"The next step is to finalize the .zid files (ZooImage",
		"Data files). There will be one data file per sample and",
		"it is all you need for the next part of your work...",
		"",
		"Once this step succeed, you can free disk space by",
		"transferring all files from the _raw subdirectory to",
		"archives, for instance, DVDs (Apps -> CD-DVD burner).",
		"",
        "Warning: the whole _work subdirectory with intermediary",
		"images will be deleted, and all .zim files will be",
		"moved to the _raw subdirectory.",
		"At the end, you should have only .zid files remaining",
		"in your working directory.", "",
		"Click 'OK' to proceed (select working directory)...", ""),
		init = "1", check = "Check vignettes", help.topic = "makeZid")
	# Analyze result
	if (res == "ID_CANCEL") return(invisible())
	# Confirm the directory to process...
	dir <- paste(tkchooseDirectory(), collapse = " ")
	if (length(dir) == 0) return(invisible())
 	# Do we check the vignettes?
 	check.vignettes <- (res == "1")
	# Make .zid files
    compress.zid.all(path = dir, check.vignettes = check.vignettes, replace = TRUE, delete.source = TRUE)
}

"makeTrain" <-
	function() {
	# Select samples, and a grouping template... and prepare for making a training set
    # First read the registry to determine which grouping in recorded there...
 	Grp <- getKey("DefaultGrouping", "[Basic]")
	# Does this point to an actual file?
	if (file.exists(Grp)) {
		defval <- basename(Grp)
		opts <- c("Basic", "Detailed", "Very_detailed", defval, "Another config...")
		otherGrp <- Grp
	} else {
		defval <- sub("^[[](.+)[]]$", "\\1", Grp)
		opts <- c("Basic", "Detailed", "Very_detailed", "Another config...")
		otherGrp <- ""
	}
	# Then, show the dialog box
 	res <- modalAssistant(paste(getTemp("ZIname"), "prepare training set"),
		c("This step prepares a directory in the hard disk",
		"where you will have the opportunity to manually",
		"classify vignettes in as many taxa as you want.",
		"The hierarchy of the folders and subfolders can",
		"be used to represent various levels of classification",
		"that the software will be able to use subsequently.",
		"",
		"You must specify: (1) a grouping scheme to start with,",
		"(2) a base directory where to locate the training set,",
		"(3) a series of .zid files as source of vignettes.", "",
		"Use the following grouping scheme:", ""), init = defval,
		options = opts, help.topic = "makeTrain")
	# Analyze result
	if (res == "ID_CANCEL") return(invisible())
	# Did we selected "Another config..."?
	if (res == "Another config...") {
		# Ask for selecting a .zic file containing the config
        otherGrp <- choose.files(caption = "Select a .zic file...", multi = FALSE,
			filters = c("ZooImage Classification Scheme (*.zic)", "*.zic"))
		if (length(otherGrp) == 0 || otherGrp == "") return(invisible()) # Cancelled dialog box
		res <- otherGrp
	} else if (res %in% c("Basic", "Detailed", "Very_detailed")) {
		# Did we selected a standard scheme?
		res <- paste("[", res, "]", sep = "")
	} else res <- Grp  # We should have selected the previously recorded scheme...
	# Save this config for later use
    setKey("DefaultGrouping", res)
	# Ask for the base directory
    dir <- paste(tkchooseDirectory(), collapse = " ")
	if (length(dir) == 0) return(invisible())
	# Ask for a subdir for this training set
	subdir <- winDialogString("Subdirectory where to create the training set:",
		default = "_train")
	if (is.null(subdir) || length(subdir) == 0 || subdir == "") {
		cat("Operation cancelled!\n")
		return(invisible())
	}
	# Ask for the .zid files
    zidfiles <- selectFile(type = "Zid", multi = TRUE, quote = FALSE)
	# Prepare the training set
	prepare.ZITrain(dir, subdir, zidfiles, groups.template = res, start.viewer = TRUE)
	# Remember the directory...
	assignTemp("ZI.TrainDir", file.path(dir, subdir))
}

"readTrain" <-
	function() {
 	# Read a training set and create a ZITrain object
	(require(svDialogs) || stop("Package 'svDialogs' from 'SciViews' bundle is required. Please, install it first!"))

	# Get a possibly saved directory as default one
	dir <- getTemp("ZI.TrainDir")
	if (is.null(dir) || !file.exists(dir) || !file.info(dir)$isdir) dir <- getwd()
	# Ask for a base directory of a training set...
	dir <- tkchooseDirectory(initialdir = dir, mustexist = "1", title = paste("Select a", getTemp("ZIname"), "training set base dir"))
	dir <- tclvalue(dir)
	if (is.null(dir) || dir == "" || !file.exists(dir) || !file.info(dir)$isdir)
		return(invisible())
	# Ask for a name for this ZITrain object
	if (isWin()) {
	    name <- winDialogString("Name for the ZITrain object:",
			default = "ZItrain")
	} else {
		name <- guiDlgInput("Name for the ZITrain object:",
			"Reading a manual training set", default = "ZItrain")
	}
	if (is.null(name) || length(name) == 0 || name == "") return(invisible())
	name <- make.names(name)	# Make sure it is a valid name!
	# get ZITrain and save it in .GlobalEnv under the given name
	res <- get.ZITrain(dir, creator = NULL, desc = NULL, keep_ = FALSE)
	assign(name, res, envir = .GlobalEnv)
	# Remember the object name
	assignTemp("ZI.TrainName", name)
	cat("Manual training set data collected in '", name, "'\n", sep = "")
	cat("\nClassification stats:\n")
	print(table(res$Class))
	cat("\nProportions per class:\n")
	print(table(res$Class) / length(res$Class) * 100)
	return(invisible(TRUE))
}

# new version to accept variables selection and/or new formula 1.2-2
"makeClass" <-
	function() {
 	# Create a classifier, using a ZI1Class object (new version)
	# Ask for an algorithm + additional parameters
	# Return a ZIClass object
  defval <- "linear discriminant analysis"
	opts <- c("linear discriminant analysis",
			  "recursive partitioning (tree)",
			  "k-nearest neighbour",
			  "learning vector quantization",
			  "neural network",
			  "random forest",
        "Variables Selection")	####TODO: svm is not working properly! ,
			  ###"support vector machine")
	# Then, show the dialog box
 	res <- modalAssistant(paste(getTemp("ZIname"), "make classifier"),
		c("This is a simplified version of the classifiers",
		"where you just need to select one algorithm.",
		"Warning! Many algorithms have parameters to be",
		"fine-tuned before efficient use... and this must be",
		"done for each specific data set! Here, only default",
		"parameters that have proven efficient with plankton",
		"are applied automatically. Some methods already work",
		"pretty well that way.",
		"", "Learn using an algorithm:", ""), init = defval,
		options = opts, help.topic = "makeClass")
	# Analyze result
	if (res == "ID_CANCEL") return(invisible())
  if (res != "Variables Selection") {
	# Use default values for the classifier creation
	warnings(" these defaults variables are used : logArea, Mean, StdDev, Mode, Min, Max, logPerim., logMajor, logMinor, Circ., logFeret, IntDen, Elongation, CentBoxD, GrayCentBoxD, CentroidsD, Range, MeanPos, SDNorm, CV")
  # Compute algorithm & package from res
	algorithm <- switch(res,
		`linear discriminant analysis` = "lda",
		`recursive partitioning (tree)` = "rpart",
		`random forest` = "randomForest",
		`support vector machine` = "svm",
		`k-nearest neighbour` = "ipredknn",
		`learning vector quantization` = "lvq",
		`neural network` = "nnet2")
    package <- switch(res,
		`linear discriminant analysis` = "MASS",
		`recursive partitioning (tree)` = "rpart",
		`random forest` = "randomForest",
		`support vector machine` = "e1071",
		`k-nearest neighbour` = "ipred",
		`learning vector quantization` = "class",
		`neural network` = "nnet")
	# Look if we have a manual training set object defined
	ZIT <- getTemp("ZI.TrainName")
	if (is.null(ZIT)) ZIT <- ""
	# Ask for a ZITrain object
	ZIT <- getVar("ZITrain", multi = FALSE, default = ZIT,
		title = "Choose one ZITrain objects:", warn.only = FALSE)
	if (length(ZIT) == 0 || (length(ZIT) == 1 && ZIT == "")) return(invisible())
	# Ask for a name for this ZIClass object
	if (isWin()) {
	    name <- winDialogString("Name for the ZIClass object to create:",
			default = "ZIclass")
	} else {
		name <- guiDlgInput("Name for the ZIClass object:",
			"Creating a classifier", default = "ZIclass")
	}
	 if (is.null(name) || length(name) == 0 || name == "") return(invisible())
	name <- make.names(name)	# Make sure it is a valid name!
	# Calculate results
	res <- ZIClass(get(ZIT, envir = .GlobalEnv), algorithm = algorithm, package = package)
  } else {
	# Options if 'Variables Selection is selected v 1.2-2
  opts <- c("linear discriminant analysis",
			  "recursive partitioning (tree)",
			  "k-nearest neighbour",
			  "learning vector quantization",
			  "neural network",
			  "random forest")
  # Dialog box if 'Variables Selection' is selected v1.2-2
	res <- modalAssistant(paste(getTemp("ZIname"), "make classifier"),
		c("This is a simplified version of the classifiers",
		"where you just need to select one algorithm.",
		"Warning! Many algorithms have parameters to be",
		"fine-tuned before efficient use... and this must be",
		"done for each specific data set!",
    "",
    "Here, you can select",
		"variables to use for the classifier creation.",
		"",
		"Warning! Select only pertinent and useful measurements.",
		"", "Learn using an algorithm:", ""), init = defval,
		options = opts, help.topic = "makeClass")
	if (res == "ID_CANCEL") return(invisible())
# Compute algorithm & package from res
	algorithm <- switch(res,
		`linear discriminant analysis` = "lda",
		`recursive partitioning (tree)` = "rpart",
		`random forest` = "randomForest",
		`support vector machine` = "svm",
		`k-nearest neighbour` = "ipredknn",
		`learning vector quantization` = "lvq",
		`neural network` = "nnet2")
    package <- switch(res,
		`linear discriminant analysis` = "MASS",
		`recursive partitioning (tree)` = "rpart",
		`random forest` = "randomForest",
		`support vector machine` = "e1071",
		`k-nearest neighbour` = "ipred",
		`learning vector quantization` = "class",
		`neural network` = "nnet")
	# Look if we have a manual training set object defined
	ZIT <- getTemp("ZI.TrainName")
	if (is.null(ZIT)) ZIT <- ""
	# Ask for a ZITrain object
	ZIT <- getVar("ZITrain", multi = FALSE, default = ZIT,
		title = "Choose one ZITrain objects:", warn.only = FALSE)
	if (length(ZIT) == 0 || (length(ZIT) == 1 && ZIT == "")) return(invisible())
	# Ask for a name for this ZIClass object
	if (isWin()) {
	    name <- winDialogString("Name for the ZIClass object to create:",
			default = "ZIclass")
	} else {
		name <- guiDlgInput("Name for the ZIClass object:",
			"Creating a classifier", default = "ZIclass")
	}
	 if (is.null(name) || length(name) == 0 || name == "") return(invisible())
	name <- make.names(name)	# Make sure it is a valid name!
	# Calculate formula using variables of the training set
    form <- FormVarsSelect(get(ZIT, envir = .GlobalEnv))
	# Calculate results using formula created by variables selection
    res <- ZIClass(get(ZIT, envir = .GlobalEnv), algorithm = algorithm, package = package, Formula = form)
  }
  # Print results
  assign(name, res, envir = .GlobalEnv)
	print(res)
	cat("\n")
	# Remember that ZIClass object
    assignTemp("ZI.ClassName", name)
	return(invisible(TRUE))
}

# New version of confusion matrix analysis v 1.2-2
"analyzeClass" <-
	function() {
 # Analyze a classifier, using a ZI1Class object (new version)
	# Ask for an option of analysis
 	defval <- "Confusion matrix"
	opts <- c("Confusion matrix",
			  "Confusion matrix reworked",
			  "False positive and negative"
			  )
	# Then, show the dialog box
 	res <- modalAssistant(paste(getTemp("ZIClass"), "Analyze a classifier"),
		c("This is a simplified version of the analysis of classifiers",
		"where you just need to select one classifier.",
		"These options provide some tools to analyze your classifers.",
		"", "Select a classifer and a tool:", ""), init = defval,
		options = opts)
	# Analyze result
	if (res == "ID_CANCEL") return(invisible()) # not error message is 'cancel'
 	# Analyze a classifier... currently, only calculate the confusion matrix
	# and edit it
  ZIC <- getVar("ZIClass", multi = FALSE, title = "Choose one ZIClass object:", warn.only = FALSE)
  if (is.null(ZIC)) stop("No current classifier. Please, make one first!")
	ZIC <- get(ZIC, envir = .GlobalEnv)
	classes <- attr(ZIC, "classes")
	predicted <- attr(ZIC, "kfold.predict")
	conf <- confu(classes, predicted, classes.predicted = TRUE)
  print(conf)
  if(res == "Confusion matrix") confu.map(classes, predicted)
  if(res == "Confusion matrix reworked") confusion.tree(conf, maxval = 10, margin = c(2,10), Rowv = TRUE, Colv = TRUE)
  if(res == "False positive and negative") confusion.bar(conf)
	#return(invisible(res))
}

"editDescription" <-
	function() {
	# Edit a samples description file... or create a new one!
	res <- modalAssistant(paste(getTemp("ZIname"), "edit samples description"),
		c("Samples are about to be analyzed and collected together",
		"to form a series.",
		paste(getTemp("ZIname"), "needs to know which samples should be"),
		"collected into the same series and you must provide",
		"metadata information (especially date and time of",
		"collection, location of the sampling stations, or",
		"possibly temperature, salinity, turbidity, etc. that",
		"were recorded at the same time as these samples).",
		"",
		"A .zis file (by default, Description.zis) needs to be",
		"created and edited for each of the considered series.",
		"You can here edit, or create a new samples description",
		"file from the template.", "",
		"Click 'OK' to edit a samples description file now...", ""),
		init = "1", check = "New description file from template.", help.topic = "editDescription")
	# Analyze result
	if (res == "ID_CANCEL") return(invisible())
	# Edit/create the description file...
	if (res == "1") {	# Create a Zis file
		res <- paste(as.character(tkgetSaveFile(filetypes = "{{ZooImage samples description} {.zis}}",
			initialfile = "Description.zis", title = "Create a new ZIS file")), collapse = " ")
		if (length(res) == 0 || res == "") return(invisible())
		if (regexpr("[.][zZ][iI][sS]$", res) < 0) res <- paste(res, ".zis", sep = "")
		zisfile <- createZis(res)
	} else { # Edit a Zis file
	    zisfile <- editZis(NULL)
	}
	# Remember the last zis file
    assignTemp("ZI.LastZIS", zisfile)
}

"processSamples" <-
	function() {
	# Ask for a description.zis file, look at all samples described there
	# Calculate abundances, total and partial size spectra and possibly biomasses
	# Get the last edited description.zis file
	# Get a possibly saved directory as default one
	zisfile <- getTemp("ZI.LastZIS")
	if (is.null(zisfile) || !file.exists(zisfile)) zisfile <- ""
    # Ask for a file
	if (zisfile != "") {
		zisfile <- paste(as.character(tkgetOpenFile(filetypes = "{{ZooImage samples description} {.zis}}",
			initialfile = basename(zisfile), initialdir = dirname(zisfile), title = "Select a ZIS file")), collapse = " ")
	} else if (file.exists(file.path(getwd(), "Description.zis"))) {
		zisfile <- paste(as.character(tkgetOpenFile(filetypes = "{{ZooImage samples description} {.zis}}",
			initialfile = "Description.zis", initialdir = getwd(), title = "Select a ZIS file")), collapse = " ")
	} else {
		zisfile <- paste(as.character(tkgetOpenFile(filetypes = "{{ZooImage samples description} {.zis}}",
			title = "Select a ZIS file")), collapse = " ")
	}
	if (length(zisfile) == 0 || zisfile == "") return(invisible())
	# Option dialog box
    res <- modalAssistant(paste(getTemp("ZIname"), "samples processing"),
		c("Each sample registered in the description.zis file",
		"will be processed in turn to extract ecological",
		"parameters (abundances, biomasses, size spectra).",
		"",
        "If you want to save calculation done on each",
		"particle individually, check the option below.",
        "",
		"Click 'OK' to proceed...", ""),
		init = "0", check = "Save individual calculations", help.topic = "processSamples")
	# Analyze result
	if (res == "ID_CANCEL") return(invisible())
 	# Do we save individual calculations?
 	if (res == "1") exportdir <- dirname(zisfile) else exportdir <- NULL

    # Get a list of samples from the description file
	smpdesc <- read.description(zisfile)
	smplist <- list.samples(smpdesc)
	# Are there samples in it?
	if (length(smplist) == 0) stop("No sample found in the description file!")
	# Are there corresponding .zid files for all samples?
	zisdir <- dirname(zisfile)
	if (zisdir == ".") zisdir <- getwd()
	zidfiles <- file.path(zisdir, paste(smplist, ".zid", sep = ""))
	if(!all(file.exists(zidfiles)) || !all(regexpr("[.][zZ][iI][dD]$", zidfiles) > 0))
		stop("One or more .zid files do not exist or is invalid!")
    # Get a classifier
	ZIC <- getTemp("ZI.ClassName")
	if (is.null(ZIC)) ZIC <- ""
	ZIC <- getVar("ZIClass", multi = FALSE, default = ZIC,
		title = "Choose a classifier (ZIClass object):", warn.only = FALSE)
	if (length(ZIC) == 0 || (length(ZIC) == 1 && ZIC == "")) return(invisible())
	ZICobj <- get(ZIC, envir = .GlobalEnv)

	# Read a conversion table from disk (from /etc/Conversion.txt) or an other position
	# First read the registry to determine which file to use...
	ConvFile <- getKey("ConversionFile", file.path(getTemp("ZIetc"), "Conversion.txt"))
	# Does this file exists?
	if (!file.exists(ConvFile) || ConvFile == "") ConvFile <- file.path(getTemp("ZIetc"), "Conversion.txt")
	# Ask for selecting a Conversion file
	### TODO: use something that also works on other platforms than Windows!
	ConvFile2 <- choose.files(default = ConvFile, caption = "Select a conversion file...",
		multi = FALSE, filters = c("Biomass Conversion table (*Conversion.txt)", "*Conversion.txt"))
	if (length(ConvFile2) == 0 || ConvFile2 == "") return(invisible()) # Cancelled dialog box
	# Read the data from this table
	conv <- read.table(ConvFile2, header = TRUE, sep = "\t")
	# Save this config for later use
	setKey("ConversionFile", ConvFile2)

	# Get class breaks for size spectra
	if (isWin()) {
	    brks <- winDialogString("Breaks for size spectrum classes (empty for no spectrum):",
			default = "seq(0.25, 2, by = 0.1)")
	} else {
		brks <- guiDlgInput("Breaks for size spectrum classes (empty for no spectrum):",
			"Size spectrum classes", default = "seq(0.25, 2, by = 0.1)")
	}
	 if (is.null(brks) || length(brks) == 0 || brks == "") return(invisible())
	brks <- eval(parse(text = brks))

	# Get a name for the variable containing results
	if (isWin()) {
	    name <- winDialogString("Name for the ZIRes object to create:",
			default = "ZIres")
	} else {
		name <- guiDlgInput("Name for the ZIRes object:",
			"Process samples", default = "ZIres")
	}
	 if (is.null(name) || length(name) == 0 || name == "") return(invisible())
	name <- make.names(name)	# Make sure it is a valid name!
	# Process sample by sample and collect results together in a ZIRes object
	res <- process.samples(path = dirname(zisfile), ZidFiles = NULL, ZICobj, ZIDesc = read.description(zisfile),
		abd.taxa = NULL, abd.groups = NULL, abd.type = "absolute",
		bio.taxa = NULL, bio.groups = NULL, bio.conv = conv, headers = c("Abd", "Bio"),
		spec.taxa = NULL, spec.groups = NULL, spec.breaks = brks, spec.use.Dil = TRUE,
		exportdir = exportdir, show.log = TRUE, bell = FALSE)
	# Assign this result to the variable
	assign(name, res, envir = .GlobalEnv)
	# Remember the name of the variable
	assignTemp("ZI.LastRES", name)
}

"viewResults" <-
	function() {
 	# Make graphic representations of results...
	ZIR <- getTemp("ZI.LastRES")
	if (is.null(ZIR)) ZIR <- ""
	ZIR <- getVar("ZIRes", multi = FALSE, default = ZIR,
		title = "Choose one ZIRes object:", warn.only = FALSE)
	if (length(ZIR) == 0 || (length(ZIR) == 1 && ZIR == "")) return(invisible())
	# Get the object
	ZIR <- get(ZIR, envir = .GlobalEnv)
	# Ask for selecting items in the list and make these graphs
	# Compute the list of graphs
	vars <- names(ZIR)
	# Eliminate variables that cannot be plotted...
	vars <- vars[-(1:25)]
	vars <- vars[!vars == "Note"]
	# Add the spectra graphs
	spec <- attr(ZIR, "spectrum")
	varspec <- paste("spectrum of", names(spec))
	Vars <- c(vars, varspec)
	Keep <- select.list(Vars, multiple = TRUE, title = "Select 1 to 12 graphs:")
	lKeep <- length(Keep)
	if (lKeep == 0) return(invisible())
	if (lKeep > 12) {
		Keep <- Keep[1:12]
		lKeep <- 12
	}
	# If there are spectrums, also ask for partial spectrums
	if (any(regexpr("^spectrum of ", Keep) + 1)) {
		pspec <- names(spec[[1]])
		# Replace total by [none] in this list
		pspec[pspec == "total"] <- "[none]"
		Pspec <- select.list(pspec, multiple = FALSE, title = "Select taxon for partial spectrum:")
		if (length(Pspec) == 0 || Pspec == "") return(invisible())
	} else Pspec <- "[none]"
	# Do the graphs
	# Determine number of rows and columns
	nc <- round((lKeep + 1) / 3)
	if (nc > 3) nc <- 3
	if (lKeep == 8) nc <- 2
	nr <- c(1, 2, 3, 2, 3, 3, 3, 4, 3, 4, 4, 4)[lKeep]
	op <- par(no.readonly = TRUE)
	on.exit(par(op))
	par(mfrow = c(nr, nc))
	for (i in 1:lKeep) {
    	# Determine if it is a x/y graph, or a size spectrum
		if (regexpr("^spectrum of ", Keep[i]) > 0) {	# Size spectrum
			Ser <- sub("^spectrum of ", "", Keep[i])
			plot(spec[[Ser]][["total"]], lwd = 3, col = "gray", type = "h",  main = Ser, ylab = "Frequency")
			if (Pspec != "[none]"){
				Spec <- spec[[Ser]][[Pspec]]
				Spec[Spec == 0] <- NA
				points(Spec, lwd = 6, col = 2, type = "h")
			}
		} else { # x/y graph
			 # If there is NA everywhere in a variable, the plot generates an error
			 Xdat <- ZIR[, "Date"]
			 Ydat <- ZIR[, Keep[i]]
			 if (all(is.na(Xdat)) || all(is.na(Ydat))) {
			    plot(0:1, 0:1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = Keep[i])
			    text(0.5, 0.5, "No data!", adj = c(0.4, 0.5))
			} else {
			 	plot(Xdat, Ydat, xlab = "Date", ylab = Keep[i], main = Keep[i])
			}
		}
	}
	return(invisible())
}

"exportResults" <-
	function() {
 	# Export one or more ZIRes objects to text files...
    res <- getVar("ZIRes", multi = TRUE, title = "Choose one or more ZIRes objects:", warn.only = FALSE)
	if (length(res) == 0 || (length(res) == 1 && res == "")) return(invisible())
	# Select a directory where to place these files
	dir <- paste(tkchooseDirectory(), collapse = " ")
	if (length(dir) == 0) return(invisible())
	filenames <- file.path(dir, res)
	# Export them there
	for (i in 1:length(res)) {
    	# We write those tables:
		# 1) Results [name].txt
		# 2) Metadata [name]_metadata.txt
		# 3) Size spectra [name]_spect_[sample].txt
		obj <- get(res[i], envir = .GlobalEnv)
		write.table(obj,  file = paste(filenames[i], "_AbdBio.txt", sep = ""),
			quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = getDec(),
			row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"))
		spc <- attr(obj, "spectrum")
		spcnames <- names(spc)
		if (!is.null(spcnames) && length(spcnames) > 0) {
			for (j in 1:length(spcnames)) {
				# Construct a data frame
				spc1 <- spc[[spcnames[j]]]
				breaks <- attr(spc1, "breaks")
				breaks <- breaks[1:(length(breaks) - 1)]
				spctab <- as.data.frame(spc1)
				spctab <- spctab[ , seq(2, ncol(spctab), by = 2)]
				names(spctab) <- names(spc1)
				spctab <- data.frame(breaks = breaks, spctab)
				write.table(spctab,
					file = paste(filenames[i], "_Spectrum_", spcnames[j], ".txt", sep = ""),
					quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = getDec(),
					row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"))
			}
		}
	}
	cat(i, "ZIRes object(s) exported in'", dir, "'\n")
}

"loadObjects" <-
	function(){
	file <- choose.files(caption = "Select a RData file...", multi = FALSE,
			filters = c("R data (*.RData)", "*.RData"))
		if (file == "") return(invisible()) # Cancelled dialog box
	if (file.exists(file)) load(file, envir = .GlobalEnv)
}

"saveObjects" <-
	function(){
	Objects <- getVar(c("ZIDat", "ZIDesc", "ZITrain", "ZIClass", "ZIRes", "ZIRecode"), multi = TRUE,
		title = paste("Choose", getTemp("ZIname"), "object(s):"), warn.only = FALSE)
	if (length(Objects) == 0 || (length(Objects) == 1 && Objects == "")) return(invisible())
	file <- paste(as.character(tkgetSaveFile(filetypes = "{{R data} {.RData}}",
			initialfile = paste(getTemp("ZIname"), ".RData", sep = ""), title = paste("Save", getTemp("ZIname"), "data under..."))), collapse = " ")
	if (length(file) == 0 || file == "") return(invisible())
	if (regexpr("[.][rR][dD][aA][tT][aA]$", file) < 0) file <- paste(file, ".RData", sep = "")
	save(list = Objects, file = file, compress = TRUE)
}

"listObjects" <-
	function() {
    varlist <- objects(pos = 1)
	Filter <- NULL
	for (i in 1:length(varlist)) Filter[i] <- inherits(get(varlist[i]),
		c("ZIDat", "ZIDesc", "ZITrain", "ZIClass", "ZIRes", "ZIRecode"))
	varlist <- varlist[Filter]
	if (length(varlist) == 0) {
		cat("No", getTemp("ZIname"), "objects currently loaded in memory!\n")
	} else {
    	print(varlist)
	}
}

"removeObjects" <-
	function() {
	Objects <- getVar(c("ZIDat", "ZIDesc", "ZITrain", "ZIClass", "ZIRes", "ZIRecode"), multi = TRUE,
	title = paste(getTemp("ZIname"), "object(s) to remove:"), warn.only = FALSE)
	if (length(Objects) == 0 || (length(Objects) == 1 && Objects == "")) return(invisible())
	rm(list = Objects, envir = .GlobalEnv)
}

"calib" <-
	function() {
	# Select a calibration file (*.tif or *.pgm) and calculate White/Black point
	ImgFilters <- as.matrix(data.frame(title = c("Tiff image files (*.tif)",
		"Pgm image files (*.pgm)"),	pattern = c("*.tif", "*.pgm")))
	file <- choose.files(caption = "Select a calibration image...",
		multi = FALSE, filters = ImgFilters)
		if (file == "") return(invisible()) # Cancelled
	if (file.exists(file)) {
		cat("Calibrating gray scale... [", basename(file), "]\n", sep = "")
		flush.console()
		res <- calibrate(file)
		cat("\nWhitePoint=", round(res["WhitePoint"]), "\n", sep = "")
		cat("BlackPoint=", round(res["BlackPoint"]), "\n", sep = "")
		if (length(attr(res, "msg")) > 0)
			cat("\nTake care:\n")
			cat(paste(attr(res, "msg"), collapse = "\n"), "\n")
	}
}

"optInOutDecimalSep" <-
	function() {
	# Defines what is the default numeric decimal separator for input and output

	# First read the registry to determine current value...
	Dec <- getDec()
	# Possibly ask for another one
	DecList <- c(".", ",")
	DecSel <- select.list(DecList, preselect = Dec, title = "In/Out decimal separator")
	# Is the cancel button pressed, or is it still the same decimal separator
	if (DecSel == "" || DecSel == Dec) return(invisible(Dec))
	# Record it in the registry key
    setKey("OptionInOutDecimalSep", DecSel)
    # Indicate change
    cat("In/Out decimal separator changed to '", DecSel, "'\n", sep = "")
 	return(invisible(DecSel))
}
