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
## along with ZooImage.  If not, see <http://www.gnu.org/licenses/>

ZIDlg <- function ()
{
	## In this version, we use a simpler implementation, using svDialogs
	## and menus added to RGui, JGR or ctxmenu
	ZIname <- getTemp("ZIname")
	menuDel(ZIname)
	menuAdd(ZIname)
	menuAddItem(ZIname, "Load objects", "loadObjects()")
	menuAddItem(ZIname, "Save objects", "saveObjects()")
	menuAddItem(ZIname, "List objects", "listObjects()")
	menuAddItem(ZIname, "Remove objects", "removeObjects()")
	menuAddItem(ZIname, "-", "")
	menuAddItem(ZIname, "Online help", 'help("zooimage")')
	menuAddItem(ZIname, "Manual", "viewManual()")
	menuAddItem(ZIname,
		"Web site", 'browseURL("http://www.sciviews.org/zooimage")')
	menuAddItem(ZIname, "--", "")
	menuAddItem(ZIname, "About...", "aboutZI(TRUE)")
	
	menuDel("Analyze")
	menuAdd("Analyze")
	menuAddItem("Analyze", "Acquire images...", "acquireImg()")
	menuAddItem("Analyze", "Import images...", "importImg()")
	menuAddItem("Analyze", "Process images...", "processImg()")
	menuAddItem("Analyze", "Make .zid files...", "makeZid()")
	menuAddItem("Analyze", "-", "")
	menuAddItem("Analyze", "Make training set...", "makeTrain()")
	menuAddItem("Analyze", "Add vignettes to training set", "increaseTrain()")
	menuAddItem("Analyze", "Read training set...", "readTrain()")
	menuAddItem("Analyze", "Make classifier...", "makeClass()")
	menuAddItem("Analyze", "Analyze classifier...", "analyzeClass()")
	menuAddItem("Analyze", "Automatic classification of vignettes...",
		"vignettesClass()") 
	menuAddItem("Analyze", "--", "")
	menuAddItem("Analyze", "Edit samples description", "editDescription()")
	menuAddItem("Analyze", "Process samples...", "processSamples()")
	menuAddItem("Analyze", "View results...", "viewResults()")
	menuAddItem("Analyze", "Export results...", "exportResults()")
	
	#menuDel("Real-Time")
	#menuAdd("Real-Time")
	#menuAddItem("Real-Time", "Start process...", "realtimeStart()")
	#menuAddItem("Real-Time", "Stop process...", "realtimeStop()")
	#menuAddItem("Real-Time", "Export results...", "realtimeSave()")
	#menuAddItem("Real-Time", "Remove data...", "realtimeReset()")
	
	## Menu 'Functions' not added yet!
	
	menuDel("Utilities")
	menuAdd("Utilities")
	menuAddItem("Utilities", "Calibrate grayscale (16bit)", "calib()")
	menuAddItem("Utilities", "Biomass conversion specification",
		"startPgm('ZIEditor', cmdline = file.path(getTemp('ZIetc'), 'Conversion.txt'))")
	menuAddItem("Utilities", "-", "")
	menuAddItem("Utilities", "Image viewer( XnView)", 'startPgm("ImageViewer")')
	menuAddItem("Utilities", "Image analyzer (ImageJ)",
		'startPgm("ImageEditor", switchdir = TRUE, iconize = TRUE)')
	menuAddItem("Utilities", "Metadata editor (Sc1)",
		'startPgm("ZIEditor", cmdline = selectFile("ZimZis"))')
	menuAddItem("Utilities", "Simple acquisition (Vuescan)",
		'startPgm("VueScan", switchdir = TRUE)')
	menuAddItem("Utilities", "--", "")
	menuAddItem("Utilities", "New R graph", "dev.new()")
	menuAddItem("Utilities", "Activate next graph",
		"{dev.set(); if (isRgui()) bringToTop()}")
	menuAddItem("Utilities", "Close all graphs", "graphics.off()")
	menuAdd("Utilities/Options")
	menuAddItem("Utilities/Options", "Change active dir...",
		"setwd(dlgDir()$res)")
	menuAddItem("Utilities/Options", "-", "")
	menuAddItem("Utilities/Options", "Define decimal separator",
		"optInOutDecimalSep()")
	
#	## This is the old implementation usig svWidgets
#	# If the window is already created, just activate it...
#	if ("ZIDlgWin" %in% WinNames()) {
#		ZIDlgWin <- WinGet("ZIDlgWin")
#		tkfocus(ZIDlgWin)  	# Doesn't work with Rgui.exe, but next command does
#		tkwm.deiconify(ZIDlgWin)
#    	return(invisible())
#	}
#
#	# Construct the window
#	tkWinAdd("ZIDlgWin", title = paste(getTemp("ZIname"), "assistant"),
#		pos = "-100+10")
#	ZIDlgWin <- WinGet("ZIDlgWin")
#
#	# Do not show it until it is completelly constructed!
#	tkwm.withdraw(ZIDlgWin)
#	on.exit(tkwm.deiconify(ZIDlgWin))
#
#	# Change the icon of that window (if under Windows)
#	if (isWin()) tk2ico.set(ZIDlgWin, getTemp("ZIico"))
#
#	# Add a menu (load it from a spec file)
#	Pkg <- getTemp("ZIguiPackage", default = "zooimage")
#	MenuReadPackage(Pkg, file = "MenusZIDlgWin.txt")
#
#	# Add a toolbar (read it from file 'ToolbarZIDlgWin.txt')
#	ToolRead(file.path(getTemp("ZIgui"), "ToolbarsZIDlgWin.txt"))
#
#	# Add a statusbar with a text and a progressbar
#	status <- tk2frame(ZIDlgWin)
#	statusText <- tk2label(status, text = paste("Ready -", getwd()),
#		justify = "left", anchor = "w", width = 60)
#	statusProg <- tk2progress(status, orient = "horizontal", maximum = 100)
#	tkpack(statusProg, side = "right")
#	tkpack(statusText, side = "left", fill= "x")
#	tkpack(status, side = "bottom", fill = "x")
#	tkpack(tk2separator(ZIDlgWin), side = "bottom", fill = "x")
#
#	# Keep track of statusText / statusProg
#	assignTemp("statusText", statusText)
#	assignTemp("statusProg", statusProg)
#	## Change value of the progressbar
#	#tkconfigure(getTemp("statusProg"), value = 50)
#	## Change text of the statusbar
#	#tkconfigure(getTemp("statusText"), text = paste("Ready -", getwd()))
#
#	if (!isWin()) {
#		# The activate R console & R graph do not work elsewhere
#        MenuStateItem("$Tk.ZIDlgWin/Apps", "&R Console", FALSE)
#		MenuStateItem("$Tk.ZIDlgWin/Apps", "Active R &Graph", FALSE)
#	}
#
#	# For each of the six external programs, look if they are accessible,
#	# otherwise, inactivate
#	if (is.null(getOption("ZIEditor")))
#         MenuStateItem("$Tk.ZIDlgWin/Apps", "&Metadata editor (Sc1)", FALSE)
#    if (is.null(getOption("ImageEditor")))
#         MenuStateItem("$Tk.ZIDlgWin/Apps", "Image &analyzer (ImageJ)", FALSE)
#    if (is.null(getOption("ImageViewer")))
#         MenuStateItem("$Tk.ZIDlgWin/Apps", "Image &viewer (XnView)", FALSE)
#    if (is.null(getOption("VueScan")))
#         MenuStateItem("$Tk.ZIDlgWin/Apps", "Simple acquisition (&VueScan)", FALSE)
#
#	# Change the window to non resizable and topmost (f under Windows)
#	if (isWin()) tcl("wm", "attributes", ZIDlgWin, topmost = 1)
#	tkwm.resizable(ZIDlgWin, 0, 0)
#	# Focus on that window
#	tkfocus(ZIDlgWin)	# Doesn't work with Rgui.exe, but tkwm.deiconify does
}

## Function for the RGui menu
aboutZI <- function (graphical = FALSE)
{
	msg <- getTemp("ZIverstring")
	### TODO: add more information here (copyright, authors, satellite pgms, ...)
	if (isTRUE(graphical)) {
		dlgMessage(message = msg, title = "About...", icon = "info",
			type = "ok")
	} else cat(msg, "\n")
}

exitZI <- function ()
{
	## This is useful to allow updating the package!
	detach("package:zooimage", unload = TRUE)
	cat("zooimage package unloaded; To restart it, issue:\n> library(zooimage)\n")
}

## Functions for the assistant menu
closeAssistant <- function ()
{
	try(menuDel(getTemp("ZIname")), silent = TRUE)
	try(menuDel("Analyze"), silent = TRUE)
	try(menuDel("Real-Time"), silent = TRUE)
	try(menuDel("Utilities"), silent = TRUE)
	## Destroy the ZooImage Tk window, if it is currently displayed
	#tkWinDel("ZIDlgWin")
}

closeZooImage <- function ()
{
	closeAssistant()
	exitZI()
}

viewManual <- function ()
{
	manual <- file.path(getTemp("ZIetc"), "ZooImageManual.pdf")
	pdfviewer <- getOption( "pdfviewer" )
	if (!is.null(pdfviewer)) {
		if (.Platform$OS.type == "windows") {
            shell.exec(manual)
        } else {
			system(paste(shQuote(getOption("pdfviewer")), shQuote(manual)),
				wait = FALSE)
		}
	} else {
		browseURL(manual)
	}
}

focusR <- function ()
{
	## Switch the focus to the R console
	### TODO: notify this command is not available elsewhere (inactivate menu?)
	if (isRgui()) bringToTop(-1)
}

focusGraph <- function ()
{
	## Focus to the active R graph (create one if there is no graph device)
	### TODO: notify this command is not available elsewhere (inactivate menu?)
	if (is.null(dev.list())) {
		device <- match.fun(getOption("device"))
		device()
	} else {
		## Activate current graph window
		if (isRgui()) bringToTop()
	}
}

startPgm <- function (program, cmdline = "", switchdir = FALSE,
iconize = FALSE, wait = FALSE)
{
	## Look if the program path is recorded in the options
	pgmPath <- getOption(program)
	if (!is.null(pgmPath) && file.exists(pgmPath)) {
		## Do we need to switch directory?
		if (switchdir) {
			curdir <- getwd()
			on.exit(setwd(curdir))
			setwd(dirname(pgmPath))
		}
		## Start it
		system(paste(pgmPath, cmdline), wait = wait, ignore.stdout = TRUE,
			ignore.stderr = TRUE)
	} else stop("Program '", program, "' not found!")
	## Do we need to iconize the assistant?
#	if (iconize && !is.null(WinGet("ZIDlgWin")))
#		tkwm.iconify(WinGet("ZIDlgWin"))
}

modalAssistant <- function (title, text, init, options = NULL, check = NULL,
select.file = NULL, returnValOnCancel = "ID_CANCEL", help.topic = NULL)
{
	## TODO!!!!
	cat("Modal assistant temporarily disabled!\n")
	return(returnValOnCancel)
	
#	## Create an assistant dialog box which behaves as a modal dialog box
#	text <- paste(text, collapse = "\n")
#	try(tkWinAdd("ZIAssist", title = title, bind.delete = FALSE))
#	ZIAssist <- WinGet("ZIAssist")
#    tkbind(ZIAssist, "<Destroy>", function () {
#		tkgrab.release(ZIAssist)
#		tkWinDel("ZIAssist")
#		#tkfocus(WinGet("ZIDlgWin"))
#	})
#	## Assign cancel by default to the return value
#    assignTemp("ZIret", returnValOnCancel)
#    ## Do not show it until it is completelly constructed!
#	tkwm.withdraw(ZIAssist)
#	## Change the icon of that window (if under Windows)
#    if (isWin()) tk2ico.set(ZIAssist, getTemp("ZIico"))
#	## This is the variable holding the result
#	resVar <- tclVar(init)
#	## Draw the dialog area
#	dlgarea <- tk2frame(ZIAssist)
#	## Place the logo to the left
#    Logo <- tklabel(dlgarea,image = ImgGet("$Tk.logo"), bg = "white")
#	## Place dialog box data
#	txtarea <- tk2frame(ZIAssist)
#	Text <- tk2label(txtarea, text = text, width = 50)
#	#### TODO: this causes a problem in Tile 0.7.2?! , justify = "left")
#	tkgrid(Text, stick = "w")
#	## Do we put options?
#	if (!is.null(options)) {
#		for (i in 1:length(options)) {
#			rb <- tk2radiobutton(txtarea)
#			tkconfigure(rb, variable = resVar, value = options[i],
#				text = options[i])
#			#### TODO: this causes a problem in Tile 0.7.2?! , justify = "left")
#			tkgrid(rb, sticky = "w")
#		}
#	}
#	## Do we have to place a checkbox?
#	if (!is.null(check)) {
#		cb <- tk2checkbutton(txtarea)
#		tkconfigure(cb, variable = resVar, text = check)
#		#### TODO: this causes a problem in Tile 0.7.2?! , justify = "left")
#		tkgrid(cb, sticky = "w")
#	}
#	## Place everything in the dialog box
#	tkgrid(Logo, txtarea)
#	tkpack(dlgarea, anchor = "nw")
#	## Place buttons
#
#    "onOK" <- function () {
#        assignTemp("ZIret", tclvalue(resVar))
#        tkgrab.release(ZIAssist)
#        tkWinDel("ZIAssist")
#		#tkfocus(WinGet("ZIDlgWin"))
#    }
#    "onCancel" <- function () {
#        tkgrab.release(ZIAssist)
#        tkWinDel("ZIAssist")
#		#tkfocus(WinGet("ZIDlgWin"))
#    }
#    butbar <- tk2frame(ZIAssist)
#    OK.but <- tk2button(butbar, text = "   OK   ", command = onOK)
#    Cancel.but <- tk2button(butbar, text = " Cancel ", command = onCancel)
#	if (is.null(help.topic)) {
#    	tkgrid(OK.but, Cancel.but, sticky = "e")
#	} else {    # Create also a help button
#		"onHelp" <- function () {
#			eval(browseURL(help(help.topic , htmlhelp=TRUE)[1] ),
#				envir = .GlobalEnv )
#		}
#        Help.but <- tk2button(butbar, text = "  Help  ", command = onHelp)
#        tkgrid(OK.but, Cancel.but, Help.but, sticky = "e")
#	}
#	tkpack(butbar, side = "bottom", fill = "x")
#	tkpack(tk2separator(ZIAssist), side = "bottom", fill = "x")
#    tkbind(ZIAssist, "<Return>", onOK)
#	if (isWin()) tcl("wm", "attributes", ZIAssist, toolwindow = 1, topmost = 1)
#	tkwm.resizable(ZIAssist, 0, 0)
#	## Focus on that window
#	tkfocus(ZIAssist)	# Doesn't work with Rgui.exe, but tkwm.deiconify does
#    tkwm.deiconify(ZIAssist)
#    tkgrab.set(ZIAssist)
#    tkwait.window(ZIAssist)
#    return(getTemp("ZIret"))
}

## Show an assitant dialog box allowing to choose between VueScan and a different
## acquisition program... remember that setting in the registry under Windows
acquireImg <- function ()
{
	## First read the registry to determine which software in recorded there...
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

	## Then, show the dialog box
 	#res <- modalAssistant(paste(getTemp("ZIname"), "picture acquisition"),
	#	c("To acquire digital plankton images,",
	#	"you can use a specialized equipment or",
	#	"a digital camera on top of a binocular or",
	#	"a flatbed scanner, ...",
	#	"",
	#	"To pilot a scanner or rework RAW digicam images",
	#	"you can use 'Vuescan'.",
	#	"You can also specify to use your own software.",
	#	"", "", "Use:", ""), init = defval,
	#	options = opts, help.topic = "acquireImg")
	## Analyze result
	#if (res == "ID_CANCEL") return(invisible())
	res <- dlgList(opts, preselect = defval, multiple = FALSE,
		title = "Acquire images with:")$res	
	if (!length(res)) return(invisible())	
	## Did we selected "Another software..."?
	if (res == "Another software...") {
		## Ask for selecting this software
		Asoft <- dlgOpen(title = "Select a program...", multiple = FALSE)$res
		if (!length(Asoft)) return(invisible()) # Cancelled dialog box
	}
	## Did we selected "VueScan"
	if (res == "VueScan") {
		startPgm("VueScan", switchdir = TRUE)
		setKey("AcquisitionSoftware", "VueScan")
		return(invisible())
	}
	## We should have selected a custom software...
	if (!file.exists(Asoft))
		stop("Program '", Asoft, "' not found!")
	## Start the program
	system(paste('"', Asoft, '"', sep = ""), wait = FALSE)
	## Record it in the registry key
    setKey("AcquisitionSoftware", Asoft)
}

importImg <- function ()
{
	# Import images... basically, you can select a series of images in a
	# directory, and the program asks for writing the associated .zim files,
	# or you can access other processes that automatically build .zim files
	# and/or import images/data, including custom processes defined in
	# separate 'ZIEimport' objects (see FlowCAM import routine for an example)
	# Get a list of 'ZIEimport' objects currently loaded in memory

	Images <- selectFile("Img", multi = TRUE, quote = FALSE,
		title = "Select data to import...")

	## Look if there is at least one image selected
	if (!length(Images)) return(invisible())
	dir <- dirname(Images[1])
	Images <- basename(Images)

	has <- function (extension, pattern = extensionPattern(extension))
		grepl(pattern, Images[1])

	## Determine which kind of data it is
	if (has(pattern = "^Import_.*[.]zie$")) {
		setKey("ImageIndex", "3")
		return(zieMake(path = dir, Filemap = Images[1], check = TRUE,
			show.log = TRUE))
    } else if (has("txt")) {
		## Special Case for flowCAM images
		FlowCAMPath  <- file.path(dir, Images)
		FlowCAM.txt <- read.table(FlowCAMPath, header = TRUE, sep = "\t", dec = ".")
		TargetName <- c("Station", "Date", "FlowCell", "Mode", "Magnification",
			"Exp_Name", "Sample", "Dilution", "Sieve", "Volume", "Pump_Speed",
			"Duration", "Temperature", "Salinity", "Gain_Fluo_Ch1",
			"Threshold_Fluo_Ch1", "Gain_Fluo_Ch2", "Threshold_Fluo_Ch2",
			"Threshold_Scatter", "Min", "Max", "Size", "Dark_Threshold",
			"Light_Threshold", "Dist_To_Nearest", "Lugol")
		
		if (all(TargetName %in% names(FlowCAM.txt))) {
			res <- zimMakeFlowCAM(import = FlowCAMPath, check.names = FALSE)
			return(invisible(res))
		}
		pattern <- extensionPattern(".txt")
		setKey("ImageIndex", "4")
		logProcess("Creating .zie file...")
		cat("Creating .zie file...\n")
		ziefile <- zieCompile(path = dir, Tablefile = Images[1])
		cat("...OK!\n")
		res <- zieMake(path = dirname(ziefile), Filemap = basename(ziefile),
			check = TRUE, show.log = TRUE)
		if (res) { # Everything is fine...
			## Move the table and copy the template to the '_raw' subdirectory too
			path <- dirname(ziefile)
			tplfile <- file.path(path, Images[1])
			file.rename(tplfile, file.path(path, "_raw", basename(tplfile)))
			## Move also possibly the .xls equivalent
			xlsfile <- sub( pattern, ".xls", tplfile)
			if (xlsfile != tplfile && file.exists(xlsfile))
			    file.rename(xlsfile, file.path(path, "_raw", basename(xlsfile)))
			file.rename(file.path(path, "ImportTemplate.zie"), file.path(path,
				"_raw", "ImportTemplate.zie"))
		}
		return(res)
	} else if (has(".tif")) {
		pattern <- extensionPattern(".tif")
        setKey("ImageIndex", "1")
	} else if (has("jpg")) {
        pattern <- extensionPattern("jpg")
        setKey("ImageIndex", "2")
	} else stop("Unrecognized data type!")

	## If there is no special treatment, just make all required .zim files
	## for currently selected images
	zimMake(dir = dir, pattern = pattern, images = Images, show.log = TRUE)
}

## TODO: the text appears only on one line on the Mac???
processImg <- function ()
{
	## Display a dialog box telling how to process images using ImageJ
	## When the user clicks on 'OK', ImageJ is started... + the checkbox 'close R'
	#res <- modalAssistant(paste(getTemp("ZIname"), "picture processing"),
	#	c(paste("Once images are acquired and imported into", getTemp("ZIname")),
	#	"(they have correct associated metadata), they must be",
	#	"processed.",
	#	"",
	#	"To do so, start 'ImageJ' (just click 'OK') and select",
	#	paste("the method for your images in 'Plugins -> ", getTemp("ZIname"),
	#		"'.", sep = ""),
	#	"",
	#	"For very large images, or on computers with limited",
	#	"RAM memory, it is advised to close all other programs.",
	#	"Check the option below to close R in this case.", "", ""),
	#	init = "0", check = "Close R before running 'ImageJ'",
	#	help.topic = "processImg")
	## Analyze result
	#if (res == "ID_CANCEL") return(invisible())
	res <- dlgMessage(paste("You will switch now to ImageJ to process your",
		"images. Do you want to continue?"), type = "okcancel")$res
	if (res == "cancel") return(invisible())
 	## Start ImageJ
	if (!is.null(getOption("ImageEditor")))
		startPgm("ImageEditor", switchdir = TRUE, iconize = TRUE)
	## Do we have to close R?
	#if (res == "1") q()
}

makeZid <- function ()
{
	## Create ZID files, possibly processing imqges first
	## TODO: get the list of all available processes
	## and select it automatically from the ZIM file
	defval <- "Scanner_Gray16"
	processes <- getProcessList()
	opts <- c( processes, "-- None --")
	## Then, show the dialog box
 	#plugin <- modalAssistant(paste(getTemp("ZIname"), "process images"),
	#	c("Process images with associated metadata (ZIM files)",
	#	"in batch mode from one directory and make ZID files.",
	#	"", "Select an image processor:", ""), init = defval,
	#	options = opts, help.topic = "processIJ")
	## Analyze result
	#if (plugin == "ID_CANCEL") return(invisible())
	plugin <- dlgList(opts, preselect = defval, multiple = FALSE,
		title = "Select a batch image processor:")$res	
	if (!length(plugin)) return(invisible())
	## Select zim file or directory
	dir <- dlgDir()$res
	if (!length(dir)) return(invisible())
	## Do we need to process the images with ImageJ?
	if (plugin != "-- None --") {
		## TODO: update a progress bar from ImageJ (using sockets ?)
		ijplugin(dir, ij.plugin = plugin) 
	}

	## Finalize .zid files (and possibly also .zip files by updating their comment)
#    res <- modalAssistant(paste(getTemp("ZIname"), "data processing"),
#		c("You should have processed all your images now.",
#		"The next step is to finalize the .zid files (ZooImage",
#		"Data files). There will be one data file per sample and",
#		"it is all you need for the next part of your work...",
#		"",
#		"Once this step succeed, you can free disk space by",
#		"transferring all files from the _raw subdirectory to",
#		"archives, for instance, DVDs (Apps -> CD-DVD burner).",
#		"",
#        "Warning: the whole _work subdirectory with intermediary",
#		"images will be deleted, and all .zim files will be",
#		"moved to the _raw subdirectory.",
#		"At the end, you should have only .zid files remaining",
#		"in your working directory.", "",
#		"Click 'OK' to proceed (select working directory)...", ""),
#		init = "1", check = "Check vignettes", help.topic = "makeZid")
#	# Analyze result
#	if (res == "ID_CANCEL") return(invisible())
#	# Confirm the directory to process...
#	dir <- dlgDir()$res
#	if (length(dir) == 0) return(invisible())
 	## Do we check the vignettes (only if images were not processed)?
 	check.vignettes <- (plugin == "-- None --")
	## Make .zid files
    cat("\n")
	## TODO: combine the log from ImageJ with this one!
	zidCompressAll(path = dir, check.vignettes = check.vignettes,
		replace = TRUE, delete.source = TRUE)
}

makeTrain <- function ()
{
	## Select samples, and a grouping template... and prepare
	## for making a training set
    ## First read the registry to determine which grouping in recorded there...
 	Grp <- getKey("DefaultGrouping", "[Basic]")
	## Does this point to an actual file?
	if (file.exists(Grp)) {
		defval <- basename(Grp)
		opts <- c("Basic", "Detailed", "Very_detailed", defval, "Another config...")
		otherGrp <- Grp
	} else {
		defval <- sub("^[[](.+)[]]$", "\\1", Grp)
		opts <- c("Basic", "Detailed", "Very_detailed", "Another config...")
		otherGrp <- ""
	}
	## Then, show the dialog box
 	#res <- modalAssistant(paste(getTemp("ZIname"), "prepare training set"),
	#	c("This step prepares a directory in the hard disk",
	#	"where you will have the opportunity to manually",
	#	"classify vignettes in as many taxa as you want.",
	#	"The hierarchy of the folders and subfolders can",
	#	"be used to represent various levels of classification",
	#	"that the software will be able to use subsequently.",
	#	"",
	#	"You must specify: (1) a grouping scheme to start with,",
	#	"(2) a base directory where to locate the training set,",
	#	"(3) a series of .zid files as source of vignettes.", "",
	#	"Use the following grouping scheme:", ""), init = defval,
	#	options = opts, help.topic = "makeTrain")

	## Analyze result
	#if (res == "ID_CANCEL") return(invisible())
	res <- dlgList(opts, preselect = defval, multiple = FALSE,
		title = "Select the default groups to use to initialize your training set:")$res	
	if (!length(res)) return(invisible())

	## Did we selected "Another config..."?
	if (res == "Another config...") {
		## Ask for selecting a .zic file containing the config
        otherGrp <- selectFile("Zic", multi = FALSE, quote = FALSE,
			title = "Select a .zic file...")
		if (!length(otherGrp)) return(invisible())
		## Cancelled dialog box
		res <- otherGrp
	} else if (res %in% c("Basic", "Detailed", "Very_detailed")) {
		## Did we selected a standard scheme?
		res <- paste("[", res, "]", sep = "")
	} else res <- Grp  # We should have selected the previously recorded scheme...

	## Save this config for later use
    setKey("DefaultGrouping", res)

	## Ask for the base directory
    dir <- dlgDir()$res
	if (!length(dir)) return(invisible())

	## Ask for a subdir for this training set
	subdir <- dlgInput("Subdirectory where to create the training set:",
		default = "_train")$res
	if (!length(subdir)) {
		cat("Operation cancelled!\n")
		return(invisible())
	}

	## Ask for the .zid files
    zidfiles <- selectFile(type = "Zid", multi = TRUE, quote = FALSE)
	if (!length(zidfiles)) return(invisible(NULL))

	## Prepare the training set
	prepare.ZITrain(dir, subdir, zidfiles, groups.template = res,
		start.viewer = TRUE)

	## Remember the directory...
	assignTemp("ZI.TrainDir", file.path(dir, subdir))
}

## Read a training set and create a ZITrain object
readTrain <- function ()
{
	## Get a possibly saved directory as default one
	dir <- getTemp("ZI.TrainDir")
	if (is.null(dir) || !file.exists(dir) || !file.info(dir)$isdir)
		dir <- getwd()
	## Ask for a base directory of a training set...
	dir <- dlgDir(default = dir, title = paste("Select a", getTemp("ZIname"),
		"training set base dir"))$res
	if (!length(dir) || !file.exists(dir) || !file.info(dir)$isdir)
		return(invisible())
	## Ask for a name for this ZITrain object
	name <- dlgInput("Name for the ZITrain object:", default = "ZItrain")$res
	if (!length(name)) return(invisible())
	name <- make.names(name)	# Make sure it is a valid name!
	## Get ZITrain and save it in .GlobalEnv under the given name
	res <- get.ZITrain(dir, creator = NULL, desc = NULL, keep_ = FALSE)
	assign(name, res, envir = .GlobalEnv)
	## Remember the object name
	assignTemp("ZI.TrainName", name)
	cat("Manual training set data collected in '", name, "'\n", sep = "")
	cat("\nClassification stats:\n")
	print(table(res$Class))
	cat("\nProportions per class:\n")
	print(table(res$Class) / length(res$Class) * 100)
	return(invisible(TRUE))
}

## Add data to an existing training set
increaseTrain <- function ()
{
	## Select zid files to add in the training set
	zid <- selectFile(type = "Zid", multi = TRUE, quote = FALSE)
	if (!length(zid)) return(invisible(NULL))
	## Select the training set in which we add new vignettes
	dir <- dlgDir(title = "Select training set root dir")$res
	if (!length(dir)) return(invisible(NULL))
	## Extract vignettes in the training set in a _NewVignettesX directory
	increase.ZITrain(zidfiles = zid, train = dir)  
}

## New version to accept variables selection and/or new formula 1.2-2
## TODO: avoid duplication of code here
makeClass <- function ()
{
 	## Create a classifier, using a ZI1Class object (new version)
	## Ask for an algorithm + additional parameters
	## Return a ZIClass object
	defval <- "linear discriminant analysis"
	opts <- c("linear discriminant analysis",
			  "recursive partitioning (tree)",
			  "k-nearest neighbour",
			  "learning vector quantization",
			  "neural network",
			  "random forest",
        "Variables Selection")	####TODO: svm is not working properly! ,
			  ###"support vector machine")
	## Then, show the dialog box
 	#res <- modalAssistant(paste(getTemp("ZIname"), "make classifier"),
	#	c("This is a simplified version of the classifiers",
	#	"where you just need to select one algorithm.",
	#	"Warning! Many algorithms have parameters to be",
	#	"fine-tuned before efficient use... and this must be",
	#	"done for each specific data set! Here, only default",
	#	"parameters that have proven efficient with plankton",
	#	"are applied automatically. Some methods already work",
	#	"pretty well that way.",
	#	"", "Learn using an algorithm:", ""), init = defval,
	#	options = opts, help.topic = "makeClass")
	## Analyze result
	#if (res == "ID_CANCEL") return(invisible())
	res <- dlgList(opts, preselect = defval, multiple = FALSE,
		title = "Select an algorithm for creating your classifier:")$res	
	if (!length(res)) return(invisible())

	if (res != "Variables Selection") {
		## Use default values for the classifier creation
		warnings("These defaults variables are used : logArea, Mean, StdDev, ",
			"Mode, Min, Max, logPerim., logMajor, logMinor, Circ., logFeret, ",
			"IntDen, Elongation, CentBoxD, GrayCentBoxD, CentroidsD, Range, ",
			"MeanPos, SDNorm, CV")
		## Compute algorithm & package from res
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
		## Look if we have a manual training set object defined
		ZIT <- getTemp("ZI.TrainName")
		if (is.null(ZIT)) ZIT <- ""
		## Ask for a ZITrain object
		ZIT <- getVar("ZITrain", multi = FALSE, default = ZIT,
			title = "Choose one ZITrain objects:", warn.only = FALSE)
		if (length(ZIT) == 0 || (length(ZIT) == 1 && ZIT == ""))
			return(invisible())
		## Ask for a name for this ZIClass object
		name <- dlgInput("Name for the ZIClass object to create:",
			default = "ZIclass")$res
		if (!length(name)) return(invisible())
		name <- make.names(name)	# Make sure it is a valid name!
		## Calculate results
		res <- ZIClass(get(ZIT, envir = .GlobalEnv), algorithm = algorithm,
			package = package)
	} else {
		## Options if 'Variables Selection is selected v 1.2-2
		opts <- c("linear discriminant analysis",
				"recursive partitioning (tree)",
				"k-nearest neighbour",
				"learning vector quantization",
				"neural network",
				"random forest")
		## Dialog box if 'Variables Selection' is selected v1.2-2
		#res <- modalAssistant(paste(getTemp("ZIname"), "make classifier"),
		#	c("This is a simplified version of the classifiers",
		#	"where you just need to select one algorithm.",
		#	"Warning! Many algorithms have parameters to be",
		#	"fine-tuned before efficient use... and this must be",
		#	"done for each specific data set!",
		#	"",
		#	"Here, you can select",
		#	"variables to use for the classifier creation.",
		#	"",
		#	"Warning! Select only pertinent and useful measurements.",
		#	"", "Learn using an algorithm:", ""), init = defval,
		#	options = opts, help.topic = "makeClass")
		#if (res == "ID_CANCEL") return(invisible())
		res <- dlgList(opts, preselect = defval, multiple = FALSE,
			title = "Select an algorithm for creating your classifier:")$res	
		if (!length(res)) return(invisible())

		## Compute algorithm & package from res
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
		## Look if we have a manual training set object defined
		ZIT <- getTemp("ZI.TrainName")
		if (is.null(ZIT)) ZIT <- ""
		## Ask for a ZITrain object
		ZIT <- getVar("ZITrain", multi = FALSE, default = ZIT,
			title = "Choose one ZITrain objects:", warn.only = FALSE)
		if (length(ZIT) == 0 || (length(ZIT) == 1 && ZIT == ""))
			return(invisible())
		## Ask for a name for this ZIClass object
		name <- dlgInput("Name for the ZIClass object to create:",
			title = "Creating a classifier", default = "ZIclass")$res
		if (!length(name)) return(invisible())
		name <- make.names(name)	# Make sure it is a valid name!
		## Calculate formula using variables of the training set
		form <- formulaVarSel(get(ZIT, envir = .GlobalEnv))
		## Calculate results using formula created by variables selection
		res <- ZIClass(get(ZIT, envir = .GlobalEnv), algorithm = algorithm,
			package = package, Formula = form)
	}
	## Print results
	assign(name, res, envir = .GlobalEnv)
	print(res)
	cat("\n")
	## Remember that ZIClass object
    assignTemp("ZI.ClassName", name)
	return(invisible(TRUE))
}

## New version of confusion matrix analysis v 1.2-2
analyzeClass <- function ()
{
	## Analyze a classifier, using a ZI1Class object (new version)
	## Ask for an option of analysis
 	defval <- "Print Confusion Matrix"
	opts <- c("Print Confusion Matrix", "Plot Confusion Matrix",
		"Print Precision/recall", "Plot Precision/recall")
	## Then, show the dialog box
 	#res <- modalAssistant(paste(getTemp("ZIClass"), "Analyze a classifier"),
	#	c("This is a simplified version of the analysis of classifiers",
	#	"where you just need to select one classifier.",
	#	"These options provide some tools to analyze your classifers.",
	#	"", "Select a classifer and a tool:", ""), init = defval,
	#	options = opts)
	## Analyze result
	#if (res == "ID_CANCEL") return(invisible()) # not error message is 'cancel'
	res <- dlgList(opts, preselect = defval, multiple = FALSE,
		title = "Select a classifier to be analyzed:")$res	
	if (!length(res)) return(invisible())
		
 	## Analyze a classifier... currently, only calculate the confusion matrix
	## and edit it
	ZIC <- getVar("ZIClass", multi = FALSE, title = "Choose one ZIClass object:",
		warn.only = FALSE)
	if (is.null(ZIC)) stop("No current classifier. Please, make one first!")
	ZIC <- get(ZIC, envir = .GlobalEnv)
	conf <- ZIConf(ZIC)
	switch(res,
		`Print Confusion Matrix` = print(conf),
		`Plot Confusion Matrix` = confusionPlot(ZIC),
		`Print Precision/recall` = print(confusionStat(ZIConf = conf)),
		`Precision/recall` = plot(conf, type = "precision_recall"))
	return(invisible(res))
}

## Edit a samples description file... or create a new one!
editDescription <- function ()
{
	#res <- modalAssistant(paste(getTemp("ZIname"), "edit samples description"),
	#	c("Samples are about to be analyzed and collected together",
	#	"to form a series.",
	#	paste(getTemp("ZIname"), "needs to know which samples should be"),
	#	"collected into the same series and you must provide",
	#	"metadata information (especially date and time of",
	#	"collection, location of the sampling stations, or",
	#	"possibly temperature, salinity, turbidity, etc. that",
	#	"were recorded at the same time as these samples).",
	#	"",
	#	"A .zis file (by default, Description.zis) needs to be",
	#	"created and edited for each of the considered series.",
	#	"You can here edit, or create a new samples description",
	#	"file from the template.", "",
	#	"Click 'OK' to edit a samples description file now...", ""),
	#	init = "1", check = "New description file from template.",
	#	help.topic = "editDescription")
	## Analyze result
	#if (res == "ID_CANCEL") return(invisible())
	res <- dlgMessage(paste("Create a new description file from scratch?"),
		type = "yesnocancel")$res
	if (res == "cancel") return(invisible())
	## Edit/create the description file...
	if (res == "yes") {	# Create a Zis file ()take care: was "1" for modalAssistant!
		res <- dlgSave(default = "Description.zis",
			title = "Create a new ZIS file",
			filters = matrix(c("ZooImage samples description", ".zis"),
			ncol = 2, byrow = TRUE))$res
		if (!length(res)) return(invisible())
		if (regexpr("[.][zZ][iI][sS]$", res) < 0) res <- paste(res, ".zis",
			sep = "")
		zisfile <- zisCreate(res)
	} else { # Edit a Zis file
	    zisfile <- zisEdit(NULL)
	}
	## Remember the last zis file
    assignTemp("ZI.LastZIS", zisfile)
}

processSamples <- function()
{
	## Ask for a description.zis file, look at all samples described there
	## Calculate abundances, total and partial size spectra and possibly biomasses
	## Get the last edited description.zis file
	## Get a possibly saved directory as default one
	zisfile <- getTemp("ZI.LastZIS")
	if (is.null(zisfile) || !file.exists(zisfile))
		zisfile <- ""
	## Ask for a file
	if (zisfile != "") {	
		zisfile <- dlgOpen(default = zisfile, title = "Select a ZIS file",
			filters = matrix(c("ZooImage samples description", ".zis"),
			ncol = 2, byrow = TRUE))$res	
	} else if (file.exists(file.path(getwd(), "Description.zis"))) {
		zisfile <- dlgOpen(default = file.path(getwd(), "Description.zis"),
			title = "Select a ZIS file",
			filters = matrix(c("ZooImage samples description", ".zis"),
			ncol = 2, byrow = TRUE))$res	
	} else {
		zisfile <- dlgOpen(title = "Select a ZIS file",
			filters = matrix(c("ZooImage samples description", ".zis"),
			ncol = 2, byrow = TRUE))$res	
	}
	if (!length(zisfile)) return(invisible())

	## Add Kevin to use manual validation 2010-08-03
	## Option dialog box
	#res <- modalAssistant(paste(getTemp("ZIname"), "samples processing"),
	#	c(
	#		"Each sample registered in the description.zis file",
	#		"will be processed in turn to extract ecological",
	#		"parameters (abundances, biomasses, size spectra).",
	#		"",
	#		"If you want to save calculation done on each",
	#		"particle individually, check the option below.",
	#		"",
	#		"Click 'OK' to proceed...", ""
	#	), init = "0",
	#	options = "Manual Validation", check = "Save individual calculations",
	#	help.topic = "processSamples")
	## Analyze result
	#if (res == "ID_CANCEL") return(invisible())
	res <- dlgMessage(paste("Save also calculations done on each particle individually?"),
		type = "yesnocancel")$res
	if (res == "cancel") return(invisible())
	## Do we save individual calculations?
	if (res == "yes")	# Note that for modalAssistant, it was "1"!
		exportdir <- dirname(zisfile) else exportdir <- NULL
	## Added by Kevin for semi auto classif
	## Do we use Semi automatic classification?
	if (res == "Manual Validation") {
		#res <- modalAssistant(paste(getTemp("ZIname"), "samples processing"),
		#c(
		#	"Each sample registered in the description.zis file",
		#	"will be processed in turn to extract ecological",
		#	"parameters (abundances, biomasses, size spectra)",
		#	"after manual validation of automatic predictions",
		#	"done in the '_manualValidation' directory", 
		#	"",
		#	"If you want to save calculation done on each",
		#	"particle individually, check the option below.",
		#	"",
		#	"Click 'OK' to proceed...", ""
		#), init = "0",
		#check = "Save individual calculations", help.topic = "processSamples")
		## Analyze result
		#if (res == "ID_CANCEL") return(invisible())
		res <- dlgMessage(paste("Save also calculations done on each particle individually?"),
			type = "yesnocancel")$res
		if (res == "cancel") return(invisible())
		## Do we save individual calculations?
		if (res == "yes") # Note that for modalAsisstant, it was "1"!
			exportdir <- dirname(zisfile) else exportdir <- NULL
		
		## Select the directory where manual validation is done
		dir <- getTemp("ZI.TrainDir")
		if (is.null(dir) || !file.exists(dir) || !file.info(dir)$isdir)
			dir <- getwd()
		## Ask for a base directory of a training set...
		dir <- dlgDir(default = dir, title = paste("Select a",
			getTemp("ZIname"), "Manual validation base dir"))$res
		if (!length(dir) || !file.exists(dir) || !file.info(dir)$isdir)
			return(invisible())
		## Read the directory
		ZIManTable <- ZIManRead(dir)
		cat("Read the manual validation directory -- Done --\n")		
		ManValid <- TRUE
	} else {
		## Classification without any manual validation
		ManValid <- FALSE
	} 
	
	## Get a list of samples from the description file
	smpdesc <- readDescription(zisfile)
	smplist <- listSamples(smpdesc)
	
	## Are there samples in it?
	if (length(smplist) == 0)
		stop("No sample found in the description file!")	
	
	## Are there corresponding .zid files for all samples?
	zisdir <- dirname(zisfile)
	if (zisdir == ".") zisdir <- getwd()
	zidfiles <- file.path(zisdir, paste(smplist, ".zid", sep = ""))
	if (!all(file.exists(zidfiles)) ||
		!all(regexpr("[.][zZ][iI][dD]$", zidfiles) > 0))
		stop("One or more .zid files do not exist or is invalid!")
	
	## Get a classifier
	ZIC <- getTemp("ZI.ClassName")
	if (is.null(ZIC)) ZIC <- ""
	ZIC <- getVar("ZIClass", multi = FALSE, default = ZIC,
		title = "Choose a classifier (ZIClass object):", warn.only = FALSE)
	if (length(ZIC) == 0 || (length(ZIC) == 1 && ZIC == ""))
		return(invisible())
	ZICobj <- get(ZIC, envir = .GlobalEnv)	
	
	## Read a conversion table from disk (from /etc/Conversion.txt)
	## or an other position
	## First read the registry to determine which file to use...
	ConvFile <- getKey("ConversionFile", file.path(getTemp("ZIetc"),
		"Conversion.txt"))
	## Does this file exists?
	if (!file.exists(ConvFile) || ConvFile == "")
		ConvFile <- file.path(getTemp("ZIetc"), "Conversion.txt")
	## Ask for selecting a Conversion file
	ConvFile2 <- dlgOpen(default = ConvFile,
		title = "Select a conversion file...", multiple = FALSE,
		filters = matrix(c("Biomass Conversion table (*Conversion.txt)", "Conversion.txt"),
		ncol = 2, byrow = TRUE))$res
	if (!length(ConvFile2)) return(invisible()) # Cancelled dialog box
	
	## Read the data from this table
	conv <- read.table(ConvFile2, header = TRUE, sep = "\t")
	
	## Save this config for later use
	setKey("ConversionFile", ConvFile2)
	
	## Get class breaks for size spectra
	brks <- dlgInput("Breaks for size spectrum classes (empty for no spectrum):",
		default = "seq(0.25, 2, by = 0.1)")$res
 	if (!length(brks)) return(invisible())
	brks <- eval(parse(text = brks))

	## Get a name for the variable containing results
	name <- dlgInput("Name for the ZIRes object to create:",
		default = "ZIres")$res
	if (!length(name)) return(invisible())
	name <- make.names(name)
	## Add Kevin for manual validation
	if (!isTRUE(ManValid)) ZIManTable <- NULL 
	res <- processSampleAll(path = dirname(zisfile), ZidFiles = NULL, ZICobj,
		ZIDesc = readDescription(zisfile), abd.taxa = NULL, abd.groups = NULL,
		abd.type = "absolute", bio.taxa = NULL, bio.groups = NULL,
		bio.conv = conv, headers = c("Abd", "Bio"), spec.taxa = NULL,
		spec.groups = NULL, spec.breaks = brks, spec.use.Dil = TRUE,
		exportdir = exportdir, show.log = TRUE, bell = FALSE, ZIMan = ZIManTable)
	
	## Assign this result to the variable
	assign(name, res, envir = .GlobalEnv)
	## Remember the name of the variable
	assignTemp("ZI.LastRES", name)
}

viewResults <- function ()
{
 	## Make graphic representations of results...
	ZIR <- getTemp("ZI.LastRES")
	if (is.null(ZIR)) ZIR <- ""
	ZIR <- getVar("ZIRes", multi = FALSE, default = ZIR,
		title = "Choose one ZIRes object:", warn.only = FALSE)
	if (length(ZIR) == 0 || (length(ZIR) == 1 && ZIR == ""))
		return(invisible())
	## Get the object
	ZIR <- get(ZIR, envir = .GlobalEnv)
	## Ask for selecting items in the list and make these graphs
	## Compute the list of graphs
	vars <- names(ZIR)
	## Eliminate variables that cannot be plotted...
	vars <- vars[-(1:25)]
	vars <- vars[!vars == "Note"]
	## Add the spectra graphs
	spec <- attr(ZIR, "spectrum")
	varspec <- paste("spectrum of", names(spec))
	Vars <- c(vars, varspec)
	Keep <- dlgList(Vars, multiple = TRUE, title = "Select 1 to 12 graphs:")$res
	lKeep <- length(Keep)
	if (lKeep == 0) return(invisible())
	if (lKeep > 12) {
		Keep <- Keep[1:12]
		lKeep <- 12
	}
	## If there are spectrums, also ask for partial spectrums
	if (any(regexpr("^spectrum of ", Keep) + 1)) {
		pspec <- names(spec[[1]])
		## Replace total by [none] in this list
		pspec[pspec == "total"] <- "[none]"
		Pspec <- dlgList(pspec, multiple = FALSE,
			title = "Select taxon for partial spectrum:")$res
		if (!length(Pspec)) return(invisible())
	} else Pspec <- "[none]"
	## Do the graphs
	## Determine number of rows and columns
	nc <- round((lKeep + 1) / 3)
	if (nc > 3) nc <- 3
	if (lKeep == 8) nc <- 2
	nr <- c(1, 2, 3, 2, 3, 3, 3, 4, 3, 4, 4, 4)[lKeep]
	op <- par(no.readonly = TRUE)
	on.exit(par(op))
	par(mfrow = c(nr, nc))
	for (i in 1:lKeep) {
    	## Determine if it is a x/y graph, or a size spectrum
		if (regexpr("^spectrum of ", Keep[i]) > 0) { # Size spectrum
			Ser <- sub("^spectrum of ", "", Keep[i])
			plot(spec[[Ser]][["total"]], lwd = 3, col = "gray", type = "h",
				main = Ser, ylab = "Frequency")
			if (Pspec != "[none]"){
				Spec <- spec[[Ser]][[Pspec]]
				Spec[Spec == 0] <- NA
				points(Spec, lwd = 6, col = 2, type = "h")
			}
		} else { # x/y graph
			 ## If there is NA in a variable, the plot generates an error
			 Xdat <- ZIR[, "Date"]
			 Ydat <- ZIR[, Keep[i]]
			 if (all(is.na(Xdat)) || all(is.na(Ydat))) {
			    plot(0:1, 0:1, type = "n", xlab = "", ylab = "", xaxt = "n",
					yaxt = "n", main = Keep[i])
			    text(0.5, 0.5, "No data!", adj = c(0.4, 0.5))
			} else {
			 	plot(Xdat, Ydat, xlab = "Date", ylab = Keep[i], main = Keep[i])
			}
		}
	}
	return(invisible())
}

exportResults <- function ()
{
 	## Export one or more ZIRes objects to text files...
    res <- getVar("ZIRes", multi = TRUE,
		title = "Choose one or more ZIRes objects:", warn.only = FALSE)
	if (length(res) == 0 || (length(res) == 1 && res == "")) return(invisible())
	## Select a directory where to place these files
	dir <- dlgDir()$res
	if (!length(dir)) return(invisible())
	filenames <- file.path(dir, res)
	## Export them there
	for (i in 1:length(res)) {
    	## We write those tables:
		## 1) Results [name].txt
		## 2) Metadata [name]_metadata.txt
		## 3) Size spectra [name]_spect_[sample].txt
		obj <- get(res[i], envir = .GlobalEnv)
		write.table(obj,  file = paste(filenames[i], "_AbdBio.txt", sep = ""),
			quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = getDec(),
			row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"))
		spc <- attr(obj, "spectrum")
		spcnames <- names(spc)
		if (!is.null(spcnames) && length(spcnames) > 0) {
			for (j in 1:length(spcnames)) {
				## Construct a data frame
				spc1 <- spc[[spcnames[j]]]
				breaks <- attr(spc1, "breaks")
				breaks <- breaks[1:(length(breaks) - 1)]
				spctab <- as.data.frame(spc1)
				spctab <- spctab[ , seq(2, ncol(spctab), by = 2)]
				names(spctab) <- names(spc1)
				spctab <- data.frame(breaks = breaks, spctab)
				write.table(spctab,
					file = paste(filenames[i], "_Spectrum_", spcnames[j],
					".txt", sep = ""), quote = FALSE, sep = "\t", eol = "\n",
					na = "NA", dec = getDec(), row.names = FALSE,
					col.names = TRUE, qmethod = c("escape", "double"))
			}
		}
	}
	cat(i, "ZIRes object(s) exported in'", dir, "'\n")
}

loadObjects <- function ()
{
	file <- selectFile("RData", multi = FALSE, quote = FALSE,
		title = "Select a RData file...")
	if (!length(file)) return(invisible()) # Cancelled dialog box
	if (file.exists(file)) load(file, envir = .GlobalEnv)
}

saveObjects <- function ()
{
	Objects <- getVar(c("ZIDat", "ZIDesc", "ZITrain", "ZIClass", "ZIRes",
		"ZIRecode"), multi = TRUE, title = paste("Choose", getTemp("ZIname"),
		"object(s):"), warn.only = FALSE)
	if (length(Objects) == 0 || (length(Objects) == 1 && Objects == ""))
		return(invisible())
	file <- dlgSave(default = paste(getTemp("ZIname"), ".RData", sep = ""),
		title = paste("Save", getTemp("ZIname"), "data under..."),
		multiple = FALSE, filters = matrix(c("R data", ".RData"),
		ncol = 2, byrow = TRUE))$res
	if (!length(file)) return(invisible())
	if (regexpr("[.][rR][dD][aA][tT][aA]$", file) < 0)
		file <- paste(file, ".RData", sep = "")
	save(list = Objects, file = file, compress = TRUE)
}

listObjects <- function ()
{
    varlist <- objects(pos = 1)
	if (length(varlist) == 0)
		stop("No objects currently loaded in memory!\n")
	Filter <- NULL
	for (i in 1:length(varlist)) Filter[i] <- inherits(get(varlist[i]),
		c("ZIDat", "ZIDesc", "ZITrain", "ZIClass", "ZIRes", "ZIRecode"))
	varlist <- varlist[Filter]
	if (length(varlist) == 0) {
		stop("No ", getTemp("ZIname"), " objects currently loaded in memory!\n")
	} else {
    	print(varlist)
	}
}

removeObjects <- function ()
{
	Objects <- getVar(c("ZIDat", "ZIDesc", "ZITrain", "ZIClass", "ZIRes",
		"ZIRecode"), multi = TRUE, title = paste(getTemp("ZIname"),
		"object(s) to remove:"), warn.only = FALSE)
	if (length(Objects) == 0 || (length(Objects) == 1 && Objects == ""))
		return(invisible())
	rm(list = Objects, envir = .GlobalEnv)
}

calib <- function ()
{
	## Select calibration file (*.tif or *.pgm) and calculate White/Black point
	file <- selectFile("TifPgm", multi = FALSE, quote = FALSE,
		title = "Select a calibration image...")
	if (!length(file)) return(invisible()) # Cancelled
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

optInOutDecimalSep <- function ()
{
	## Define the default numeric decimal separator for input and output

	## First read the registry to determine current value...
	Dec <- getDec()
	## Possibly ask for another one
	DecList <- c(".", ",")
	DecSel <- dlgList(DecList, preselect = ".", multiple = FALSE,
		title = "In/Out decimal separator")$res
	## Is the cancel button pressed, or is it still the same decimal separator
	if (!length(DecSel) || DecSel == Dec) return(invisible(Dec))
	## Record it in the registry key
    setKey("OptionInOutDecimalSep", DecSel)
    ## Indicate change
    cat("In/Out decimal separator changed to '", DecSel, "'\n", sep = "")
 	return(invisible(DecSel))
}

## Utility functions ###########################################################
## Little function to help the user
## Function to select groups to keep for the comparison
selectGroups <- function (ZIC, multiple = TRUE,
title = "Select taxa you want to plot")
	dlgList(levels(attr(ZIC, "classes")), multiple = multiple, title = title)

selectSamples <- function (Samples = NULL)
{
	if (is.null(Samples)) {
		Files <- selectFile(type = "LstZid", multi = TRUE, quote = FALSE)
		if (length(Files) == 1 &&
			length(grep(pattern = ".[Ll][Ss][Tt]", x = Files)) >= 1) {
			## List files
			Samples <- list.files(dirname(dirname(Files)), recursive = TRUE,
				pattern = "\\.lst$", full.names = TRUE)
		} else Samples <- Files	
	}
	Names <- sub(".lst$", "", basename(Samples)) # Names of the list
	Sel <- dlgList(Names, multiple = TRUE,
		title = "Select samples to compare")$res
	if (!length(Sel)) return(character(0))
	res <- Samples[Names %in% Sel]
	return(res)
}

vignettesClass <- function ()
{
	## Extract on zid to respective directories
	## Select zid files to be classified
	zid <- selectFile(type = "Zid", multi = TRUE, quote = FALSE)
	if (!length(zid)) return(invisible(NULL))
	## Look if we have a classifier object defined
	zic <- getTemp("ZI.ClassName", default = "")
	zic <- getVar("ZIClass", multi = FALSE, default = zic,
		title = "Choose a classifier (ZIClass object):", warn.only = FALSE)
	if (!length(zic)) return(invisible())
	zicObj <- get(zic, envir = .GlobalEnv)

	## Classify vignettes  
	if (length(zid) > 1) {
		classVignettesAll(zidfiles = zid, Dir = "_manuValidation", ZIClass = zicObj)
	} else {
		classVignettes(zidfile = zid, Dir = noext(zid), ZIClass = zicObj)
	}
}

## Subpart of zid file and return a subtable corresponding to the threshold
subpartZIDat <- function ()
{
    ## Select files to use
    zidFile <- selectFile(type = "Zid", multi = FALSE, quote = FALSE)
	if (!length(zidFile)) return(invisible(NULL))

    ## Read the zid file
    zid <- zidRead(zidFile)

    ## Select a parameter to use for the threshold
    threshold <- createThreshold(ZIDat = zid)    

    ## Apply the thresold
    res <- subpartThreshold(ZIDat = zid, Filter = threshold)
    return(res)
}

## Classify vignettes after Filter
classifyAfterFilter <- function ()
{
    ## Extract on zid to respective directories
    zid <- selectFile(type = "Zid", multi = FALSE, quote = FALSE)
    if (!length(zid)) return(invisible(NULL))
	
    ## Look if we have a classifier object defined
    zic <- getTemp("ZI.ClassName", default = "")
    zic <- getVar("ZIClass", multi = FALSE, default = zic,
		title = "Choose a classifier (ZIClass object):", warn.only = FALSE)
    if (!length(zic)) return(invisible())
    zicObj <- get(zic, envir = .GlobalEnv)

    ## Give a name for the final directory
    finalDir <- dlgInput("Name for the automatic classification directory:",
		default = "filterClassification", title = "Parameter filter")$res
    if (!length(finalDir)) return(invisible(NULL))
	
    ## Read the zid file
    ZIDat <- zidRead(zid)
    
    ## Select a parameter to use for the threshold
    threshold <- createThreshold(ZIDat = ZIDat)        
    
    ## Classify vignettes
    classVignettes(zidfile = zid, ZIDat = ZIDat, ZIClass = zicObj, Dir = finalDir,
		Filter = threshold)
}

## Create a batch file for FlowCAM image analysis
## TODO: make a menu entry + an entry in NAMESPACE for this function!
batchFilePlugin <- function ()
{
	## Select a FlowCAM context file
	ctxFile <- dlgOpen(multiple = FALSE, title = "Select a context file...",
		filters = matrix(c("FlowCAM Context file", ".ctx"), ncol = 2,
		byrow = TRUE))$res
	if (!length(ctxFile)) return(invisible(NULL))
	## Create the table
	createBatchFile(ctx = ctxFile, fil = FALSE, largest = FALSE,
		vignettes = TRUE, scalebar = TRUE, enhance = FALSE, outline = FALSE,
		masks = FALSE, verbose = TRUE, txt = FALSE,
		import.name = "batchExampleParameters")
}

## Select one or several files of a given type
selectFile <- function (type = c("ZipZid", "ZimZis", "LstZid", "Zip", "Zid",
"Zim", "Zis", "Zie", "Zic", "Img", "TifPgm", "RData"),
multi = FALSE, quote = TRUE, title = NULL)
{	
	type <- tryCatch(match.arg(type), error = function (e) {
		stop("unrecognized type")
	})
	
	Type <- switch(type,
		ZipZis = "Zip/Zis",
		ZimZis = "Zim/Zis",
		LstZis = "Lst/Zis",
		TifPgm = "Tiff/Pgm",
		type)
	
	## Adapt title according to 'multi'
	if (isTRUE(multi) && !is.null(title)) {
		title <- paste("Select one or several", Type, "files...")
	} else title <- paste("Select one", Type, "file...")
	
	filters <- switch(type,
		ZipZid 	= c("ZooImage files"          , ".zip",
					"ZooImage files"          , ".zid"),
		ZimZis 	= c("ZooImage metadata files" , ".zim",
					"ZooImage metadata files" , ".zis"),
		LstZid  = c("FlowCAM list files"      , ".lst",
					"ZooImage files"          , ".zid"),
		Zip		= c("ZooImage picture files"  , ".zip"),
		Zid		= c("ZooImage data files"     , ".zid"),
		Zim		= c("ZooImage metadata files" , ".zim"),
		Zis		= c("ZooImage sample files"   , ".zis"),
		Zie		= c("ZooImage extension files", ".zie"),
		Zic     = c("ZooImage Classification Scheme",".zic" ),
		Img     = c("Tiff image files"        , ".tif",
					"Jpeg image files"        , ".jpg",
					"Zooimage import extensions",".zie",
					"Table and ImportTemplate.zie",".txt",
					"FlowCAM Table and ImportTemplate.zie",".txt"),
		TifPgm  = c("Tiff image files"        , ".tif",
					"Pgm image files"         , ".pgm"),
		RData   = c("R data"                  , ".RData"))
	filters <- matrix(filters, ncol = 2, byrow = TRUE)
	
	res <- dlgOpen(title = title, multiple = multi, filters = filters)$res	
	if (length(res) && res != "" && quote)
		res <- paste('"', res, '"', sep = "")
	return(res)
}

## Get the name of one or several variables of a given class
getVar <- function (class = "data.frame", default = "", multi = FALSE,
title = paste("Choose a ", class, ":", sep = ""), warn.only = TRUE)
{	
	## Get one or several variables of a given object class
	varlist <- objects(pos = 1)	# Get objects in .GlobalEnv
	if (length(varlist) == 0) {
		msg <- paste("There is no object of class '",
			paste(class, collapse = " "), "' in the user workspace!", sep = "")
		if (isTRUE(warn.only)) warning(msg) else stop(msg)
		return("")
	}
	## Filter this list to keep only object inheriting a giving class...
	Filter <- NULL
	for (i in 1:length(varlist))
		Filter[i] <- inherits(get(varlist[i]), class)
	
	## Keep only those objects
	varlist <- varlist[Filter]	
	if (length(varlist) == 0) {	# No such objects in .GlobalEnv
		msg <- paste("There is no object of class '",
			paste(class, collapse = " "), "' in the user workspace!", sep = "")
		if (isTRUE(warn.only)) warning(msg) else stop(msg)
		varsel <- "" 
	} else {
		if (default == "") default <- varlist[1]
		varsel <- dlgList(varlist, preselect = default, multiple = multi,
			title = title)$res
	}
    return(varsel)		
}

## Get the name of one or more lists with their components of a given class
## Note: this is used as a collection in other languages
## (there is no such collection in R, so, we use a list here!)
getList <- function (class = "data.frame", default = "", multi = FALSE,
title = paste("Choose a list (of ", class, "s):", sep = ""), warn.only = TRUE)
{	
	## Get objects in .GlobalEnv
	filter <- function(x) {
		item <- get(x)
		is.list(item) && all(sapply(item, function (y) inherits(y, class)))
	}
	varlist <- Filter(filter, objects(pos = 1))	
	if (length(varlist) == 0) {
		msg <- paste("There is no list of '", class,
			"' objects in the user workspace", sep = "")
		if (isTRUE(warn.only)) warning(msg) else stop(msg)
		return("")
	}
	if (default == "") default <- varlist[1]
	varsel <- dlgList(varlist, preselect = default, multiple = multi,
		title = title)
	return(varsel)		
}

## Create a threshold formula
createThreshold <- function (ZIDat)
{
    ## Select the parameter to use
    Param <- dlgList(names(ZIDat), multiple = FALSE,
		title = "Parameter to use")$res
    ## Select the threshold
    Message <- paste("Range:", "From", round(range(ZIDat[, Param])[1],
		digits = 1), "To", round(range(ZIDat[, Param])[2], digits = 1),
		";", "Select the threshold:")
    Threshold <- dlgInput(Message, default = paste(Param, "< 50"))$res
    if (!length(Threshold)) return(invisible(NULL)) else return(Threshold)
}

## Formula calculation by variables selection for the classifier creation v1.2-2
formulaVarSel <- function (ZITrain)
{
	## ZITrain must be a ZItrain object
	if (!inherits(ZITrain, "ZITrain"))
		stop("'ZITrain' must be a 'ZITrain' object")

	## Parameters measured on particles and new variables calculated
	mes <- as.vector(colnames(calcVars(ZITrain)))
	presel <- c("Id", "FIT_Cal_Const", "Item", "FIT_Raw_Area",
		"FIT_Raw_Feret_Max", "FIT_Raw_Feret_Min", "FIT_Raw_Feret_Mean",
		"FIT_Raw_Perim", "FIT_Raw_Convex_Perim", "FIT_Feret_Max_Angle",
		"FIT_Feret_Min_Angle", "FIT_Avg_Red", "FIT_Avg_Green", "FIT_Avg_Blue",
		"FIT_PPC", "FIT_Ch3_Peak", "FIT_Ch3_TOF", "FIT_Ch4_Peak", "FIT_Ch4_TOF",
		"FIT_SaveX", "FIT_SaveY", "FIT_PixelW", "FIT_PixelH", "FIT_CaptureX",
		"FIT_CaptureY", "FIT_Edge_Gradient", "FIT_Timestamp1", "FIT_Timestamp2",
		"FIT_Source_Image", "FIT_Calibration_Image", "FIT_High_U32",
		"FIT_Low_U32", "FIT_Total", "FIT_Red_Green_Ratio",
		"FIT_Blue_Green_Ratio", "FIT_Red_Blue_Ratio", "FIT_Ch2_Ch1_Ratio",
		"X.Item.1", "X", "Y", "XM", "YM", "BX", "BY", "Width", "Height",
		"Angle", "XStart", "YStart", "Count",  "Label", "Dil", "Class")
	DontKeep <-  dlgList(mes, preselect = presel, multiple = TRUE,
		title = "Select variable you don't want to use in the classification")$res
	
	## Selection of features for the creation of the classifier
#	keep <- dlgList(mes, preselect = c("ECD", "FIT_Area_ABD",
#		"FIT_Diameter_ABD", "FIT_Volume_ABD", "FIT_Diameter_ESD",
#		"FIT_Volume_ESD", "FIT_Length", "FIT_Width", "FIT_Aspect_Ratio",
#		"FIT_Transparency", "FIT_Intensity", "FIT_Sigma_Intensity",
#		"FIT_Sum_Intensity", "FIT_Compactness", "FIT_Elongation",
#		"FIT_Perimeter", "FIT_Convex_Perimeter", "FIT_Roughness",
#		"FIT_Ch1_Peak", "FIT_Ch1_TOF", "FIT_Ch2_Peak", "FIT_Ch2_TOF",
#		"Area", "Mean", "StdDev", "Mode", "Min", "Max", "Perim.", "Width",
#		"Height", "Major", "Minor", "Circ.", "Feret", "IntDen", "Median",
#		"Skew", "Kurt", "Elongation", "CentBoxD", "GrayCentBoxD", "CentroidsD",
#		"Range", "MeanPos", "SDNorm", "CV", "logArea", "logPerim.", "logMajor",
#		"logMinor", "logFeret"),
#		multiple = TRUE, title = "Select variables to use for classification")$res
	
	## Creation of one formula for classifier calculation
	keep <- mes[!mes %in% DontKeep]
	res <- as.formula(paste("Class ~ ", paste(keep, collapse = "+")))
	return(res)
}
