## Copyright (c) 2004-2015, Ph. Grosjean <phgrosjean@sciviews.org>
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

## Loading and unloading ZooImage
.onLoad <- function (libname, pkgname)
{
	if (!interactive()) options(ZIAssistant  = FALSE)

	## Use the SciViews style for dialog boxes
	options(guiStyle = "SciViews")

	## Did we redefined the ZooImage config?
	redef <- getOption("ZI.redefine")
	if (is.null(redef)) redef <- FALSE else redef <- TRUE
	options(ZI.redefine = NULL)

	## Create some strings in TempEnv
	ZIversion <- packageDescription("zooimage", fields = "Version")
	assignTemp("ZIversion", ZIversion)

	ZIname <- getTemp("ZIname")
	if (!redef || is.null(ZIname)) ZIname <- "ZooImage"
	assignTemp("ZIname", ZIname)
	assignTemp("ZIverstring", paste(ZIname, "version", ZIversion))

	ZIetc <- getTemp("ZIetc")
	if (!redef || is.null(ZIetc))
		ZIetc <- system.file("etc", package = "zooimage")
	assignTemp("ZIetc", ZIetc)

	ZIgui <- getTemp("ZIgui")
	if (!redef || is.null(ZIgui))
		ZIgui <- system.file("gui", package = "zooimage")
	assignTemp("ZIgui", ZIgui)

	## Windows specific things
	#if (isWin()) {
		#if (interactive()) {
		#	ZIico <- getTemp("ZIico")
		#	if (!redef || is.null(ZIgui))
		#		ZIico <- tk2ico.create(file.path(getTemp("ZIgui"),
		#			"ZooImage.ico"))
		#	assignTemp("ZIico", ZIico)
		#}

		## Make sure there is a key for ZooImage in the registry
		## PhG: what is the purpose of this code?
		#ZIkey <- "HKEY_LOCAL_MACHINE\\Software\\ZooImage"
		#res <- try(tk2reg.setkey(ZIkey), silent = TRUE)
		#assignTemp("ZIkey", ZIkey)
	#}

	## Load the various image resources
	#if (!redef && interactive()) ImgReadPackage("zooimage")

	## Load the menus
	#if (!redef && interactive()) MenuReadPackage("zooimage")

	## Possibly create the ZIguiPackage variable to indicate from where to load
	## other GUI resources
	ZIguiPackage <- getTemp("ZIguiPackage")
	if (!redef || is.null(ZIguiPackage))
		ZIguiPackage <- "zooimage"
	assignTemp("ZIguiPackage", ZIguiPackage)

	## The directory that contains binary executables
	#bindir <- system.file("bin", package = "zooimage")
	## PhG: executables are not provided anymore with zooimage (not allowed by
	## CRAN where it is distributed now), but you must install them manually
	## in a given directory...
	if (isWin()) {
		bindir <- "c:/progra~1/Zooimage/bin"
		if (!file.exists(bindir))
			bindir <- "c:/progra~2/Zooimage/bin"
		if (!file.exists(bindir)) {
			bindir <- ""
		} else options(zooimage.bindir = bindir)
	}

	## Determine where to find ImageJ
	options(ImageEditor = "")
	if (interactive()) {
		if (isWin()) {
			ImageJExe <- file.path(bindir, "Fiji.app", "ImageJ.exe")
			if (!file.exists(ImageJExe))
				ImageJExe <- file.path(bindir, "ImageJ", "ImageJ.exe")
			if (file.exists(ImageJExe)) options(ImageEditor = ImageJExe)
		} else if (isMac()) {
			#ImageJExe <- "/Applications/Fiji/Fiji.app/Contents/MacOS/fiji-macosx"
			ImageJExe <- "open /Applications/Fiji/Fiji.app"
			if (file.exists(ImageJExe)) options(ImageEditor = ImageJExe)
		} else {
			# Check for fiji and imagej
			if (system("which fiji > /dev/null") == 0) {
			  ImageJExe <- "fiji"
			} else if (system("which imagej > /dev/null") == 0) {
			  ImageJExe <- "imagej"
			} else {
			  ImageJExe <- ""
			}
		  options(ImageEditor = ImageJExe)
		}
	}

	## Determine where to find XnView
	options(ImageViewer = "")
	if (interactive()) {
		if (isWin()) {
			XnViewExe <- file.path(bindir, "XnView", "XnView.exe")
			if (file.exists(XnViewExe)) options(ImageViewer = XnViewExe)
		} else if (isMac()) {
			XnViewExe <- "/Applications/Utilities/XnViewMP.app/Contents/MacOS/xnview"
			if (file.exists(XnViewExe)) options(ImageViewer = XnViewExe)
		} else {# Linux
		  if (system("which xnview > /dev/null") == 0) {
		    XnViewExe <- "xnview"
		  } else if (system("which nautilus > /dev/null") == 0) {
		    XnViewExe <- "nautilus" # Gnome file manager
		  } else if (system("which dolphin > /dev/null") == 0) {
		    XnViewExe <- "dolphin" # KDE file manager
		  } else if (system("which thunar > /dev/null") == 0) {
		    XnViewExe <- "thunar" # XFCE file manager
		  } else {
		    XnViewExe <- ""
		  }
		  options(ImageViewer = XnViewExe)
		}
	}

	## Determine where to find VueScan
	options(VueScan = "")
	if (interactive()) {
		if (isWin()) {
			VueScanExe <- file.path(bindir, "VueScan", "VueScan.exe")
			if (file.exists(VueScanExe)) options(VueScan = VueScanExe)
		} else if (isMac()) {
			VueScanExe <- "/Applications/VueScan.app/Contents/MacOS/VueScan"
			if (file.exists(VueScanExe)) options(VueScan = VueScanExe)
		} else {# Linux
		  if (system("which vuescan > /dev/null") == 0) {
		    VueScanExe <- "vuescan"
		  } else {
		    VueScanExe <- ""
		  }
		  options(VueScan = VueScanExe)
		}
	}

	## Define the metadata editor
	options(fileEditor = "")
	if (interactive()) {
		if (isWin()) {
			Metaeditor <- file.path(bindir, "MetaEditor", "Sc1.exe")
			if (file.exists(Metaeditor)) options(fileEditor = Metaeditor)
		} else if (isMac()) {
			Metaeditor <- "open -t"
			options(fileEditor = Metaeditor)
		} else {# Linux
		  if (system("which gedit > /dev/null") == 0) {
		    Metaeditor <- "gedit" # Gnome text manager
		  } else if (system("which kate > /dev/null") == 0) {
		    Metaeditor <- "kate" # KDE text manager
		  } else if (system("which mousepad > /dev/null") == 0) {
		    Metaeditor <- "mousepad" # XFCE text manager
		  } else if (system("which editor > /dev/null") == 0) {
		    Metaeditor <- "editor" # Default text manager
		  } else {
		    Metaeditor <- ""
		  }
		  options(fileEditor = Metaeditor)
		}

	} else options(fileEditor = "")

	## Possibly load the ZooImage assistant
	LoadIt <- getOption("ZIAssistant")
	if (is.null(LoadIt) || LoadIt == TRUE) ZIDlg()

	## Set the default template directory
	if (is.null(getOption("ZITemplates")))
		options(ZITemplates = system.file("templates", package = "zooimage"))

	## Switch to the default directory, if defined
	defdir <- getOption("ZI.DefaultDirectory", "")
    if (defdir != "" && file.exists(defdir) && file.info(defdir)$isdir)
        setwd(defdir)
}

## Unloading ZooImage
.onUnload <- function (libpath)
{
	## Eliminate the ZooImage menu entries
	if (.Platform$GUI[1] == "Rgui") {
		try(menuDel("$ConsoleMain/ZooImage"), silent = TRUE)
		try(menuDel("$ConsolePopup/ZooImage"), silent = TRUE)
	}
	closeAssistant()
}

## R version < 2.15.0 does not have paste0 => create it here
if (compareRVersion("2.15.0") < 0) {
	paste0 <- function (..., collapse = NULL)
		paste(..., sep = "", collapse = collapse)
}
