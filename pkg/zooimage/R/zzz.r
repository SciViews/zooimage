# Loading and unloading ZooImage.
#
# Copyright (c) 2004-2007, Ph. Grosjean <phgrosjean@sciviews.org>

".First.lib" <-
function(libname, pkgname) {
	(require(utils) || stop("Package 'utils' is required!"))
	(require(svMisc) || stop("Package 'svMisc' from SciViews bundle is required!"))
	(require(svWidgets) || stop("Package 'svWidgets' is required!"))
	(require(svDialogs) || stop("Package 'svDialogs' from SciViews bundle is required!"))
	(require(tcltk2) || stop("Package 'tcltk2' is required!"))

	# Use the SciViews style for dialog boxes
	options(guiStyle = "SciViews")
	
	# Did we redefined the ZooImage config?
	redef <- getOption("ZIredefine")
	if (is.null(redef)) redef <- FALSE
	options(ZIredefine = NULL) 
	
	# Create some strings in TempEnv
	ZIversion <- "1.2-1"
	assignTemp("ZIversion", ZIversion)

	ZIname <- getTemp("ZIname")
	if (!redef || is.null(ZIname)) ZIname <- "ZooImage1"
	assignTemp("ZIname", ZIname) 
	assignTemp("ZIverstring", paste(ZIname, "version", ZIversion))
	
	ZIetc <- getTemp("ZIetc")
	if (!redef || is.null(ZIetc)) {
		# This fails?
		ZIetc <- file.path(.path.package(package = "zooimage")[1], "etc")
		#ZIetc <- file.path(libname, pkgname, "etc")
	}
	assignTemp("ZIetc", ZIetc)
	
	ZIgui <- getTemp("ZIgui")
	if (!redef || is.null(ZIgui)) {	
		ZIgui <- file.path(.path.package(package = "zooimage")[1], "gui")
		#ZIgui <- file.path(libname, pkgname, "gui")
	}
	assignTemp("ZIgui", ZIgui)
	
	if (isWin()) {
		ZIico <- getTemp("ZIico")
		if (!redef || is.null(ZIgui))
			ZIico <- tk2ico.create(file.path(getTemp("ZIgui"), "ZooImage.ico"))
		assignTemp("ZIico", ZIico)
		# Make sure there is a key for ZooImage in the registry
		ZIkey <- "HKEY_LOCAL_MACHINE\\Software\\ZooImage"
		tk2reg.setkey(ZIkey)
		assignTemp("ZIkey", ZIkey)
	}

	# Load the various image resources
	if (!redef) tkImgReadPackage("zooimage")
	# Load the menus
	if (!redef) MenuReadPackage("zooimage")
	# Possibly create the ZIguiPackage variable to indicate from where to load other GUI resources
	ZIguiPackage <- getTemp("ZIguiPackage")
	if (!redef || is.null(ZIguiPackage))	
		ZIguiPackage <- "zooimage"
	assignTemp("ZIguiPackage", ZIguiPackage)

	# Determine where to find the metadata editor
	## TODO... currently, it is in a fixed position
	bindir <- dirname(dirname(R.home()))
	ZIEditorExe <- file.path(bindir, "MetaEditor", "Sc1.exe") # Note: only under Windows!
	if (file.exists(ZIEditorExe))
		options(ZIEditor = ZIEditorExe)

	# Determine where to find ImageJ
	## TODO... currently, it is in a fixed position
	ImageJExe <- file.path(bindir, "ImageJ", "ImageJ.exe")
	if (file.exists(ImageJExe))
		options(ImageEditor = ImageJExe)

	# Determine where to find XnView
	## TODO... currently, it is in a fixed position
	XnViewExe <- file.path(bindir, "XnView", "XnView.exe")
	if (file.exists(XnViewExe))
		options(ImageViewer = XnViewExe)

	# Determine where to find the zip viewer (Filzip under Windows)
	## TODO... currently, it is in a fixed position
	if (isWin()) {
		FilzipExe <- file.path(bindir, "Filzip", "Filzip.exe")
		if (file.exists(FilzipExe))
			options(ZipViewer = FilzipExe)
	}

	# Determine where to find the DVD burner (DeepBurner under Windows)
	## TODO... currently, it is in a fixed position
	if (isWin()) {
		DeepBurnerExe <- file.path(bindir, "DeepBurner", "DeepBurner.exe")
		if (file.exists(DeepBurnerExe))
			options(DVDBurner = DeepBurnerExe)
	}

	# Determine where to find XnView
	## TODO... currently, it is in a fixed position
	VueScanExe <- file.path(bindir, "VueScan", "VueScan.exe")
	if (file.exists(VueScanExe))
		options(VueScan = VueScanExe)

	# Possibly load the ZooImage assistant
	LoadIt <- getOption("ZIAssistant")
	if (is.null(LoadIt) || LoadIt == TRUE) ZIDlg()
	
	# Switch to the default directory, if defined
	defdir <- getKey("DefaultDirectory", "")
    if (defdir != "" && file.exists(defdir) && file.info(defdir)$isdir)
        Setwd(defdir)
}

".Last.lib" <-
function(libpath) {
	# Eliminate the ZooImage menu entries
	if (.Platform$GUI[1] == "Rgui") {
		require(svWidgets)
		try(MenuDel("$ConsoleMain/ZooImage"), silent = TRUE)
		try(MenuDel("$ConsolePopup/ZooImage"), silent = TRUE)
	}
	# Destroy the ZooImage Tk window, if it is currently displayed
	tkWinDelete("ZIDlgWin")	 
}
