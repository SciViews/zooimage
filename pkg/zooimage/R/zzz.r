# Copyright (c) 2004-2007, Ph. Grosjean <phgrosjean@sciviews.org>
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

# Loading and unloading ZooImage.

# {{{ Loading ZooImage
".onAttach" <- function(libname, pkgname) {
	# {{{ check package dependencies
	(require(utils) || stop("Package 'utils' is required!"))
	(require(svMisc) || stop("Package 'svMisc' from SciViews bundle is required!"))
	(require(svWidgets) || stop("Package 'svWidgets' is required!"))
	(require(svDialogs) || stop("Package 'svDialogs' from SciViews bundle is required!"))
	(require(tcltk2) || stop("Package 'tcltk2' is required!"))
	# }}}
	
	# {{{ check capabilities
	checkZipAvailable() 
	checkUnzipAvailable() 
	checkZipnoteAvailable()
	# }}}
	
	# {{{ Use the SciViews style for dialog boxes
	options(guiStyle = "SciViews")
	# }}}
	
	# {{{ Did we redefined the ZooImage config?
	redef <- getOption("ZIredefine")
	if (is.null(redef)) {
		redef <- FALSE
	}
	options(ZIredefine = NULL) 
	# }}}
	
	# {{{ Create some strings in TempEnv
	ZIversion <- packageDescription( "zooimage", field = "Version" )
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
	# }}}
	
	# {{{ windows specific things
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
	# }}}

	# {{{ Load the various image resources
	if (!redef){
		ImgReadPackage("zooimage")
	}
	# }}}
	
	# {{{ Load the menus
	if (!redef) MenuReadPackage("zooimage")
	# }}}
	
	# {{{ Possibly create the ZIguiPackage variable to indicate from where to load other GUI resources
	ZIguiPackage <- getTemp("ZIguiPackage")
	if (!redef || is.null(ZIguiPackage))	
		ZIguiPackage <- "zooimage"
	assignTemp("ZIguiPackage", ZIguiPackage)
	# }}}

	# {{{ Determine where to find the metadata editor
	if( isWin() ){
		ZIEditorExe <- system.file( "MetaEditor", "Sc1.exe", package = "zooimage" )
		if (file.exists(ZIEditorExe)){
			options(ZIEditor = ZIEditorExe)
		} 
	} 
	# }}}
		
	# {{{ the directory that contains binary executables
	bindir <- system.file( "bin", package = "zooimage" )
	# }}}
	
	# {{{ Determine where to find ImageJ
	## TODO... currently, it is in a fixed position
	# TODO: no need to ship the exe file, we can just ship a simple
	#       bat file with 
	#       java -jar ij.jar -ijpath=./plugins
	if( isWin() ){
		ImageJExe <- file.path(bindir, "ImageJ", "ImageJ.exe")
		if (file.exists(ImageJExe)){
			options(ImageEditor = ImageJExe)
		}
	}
	# }}}

	# {{{ Determine where to find XnView
	## TODO... currently, it is in a fixed position
	XnViewExe <- file.path(bindir, "XnView", "XnView.exe")
	if( isWin() ){
		if (file.exists(XnViewExe)){
			options(ImageViewer = XnViewExe)
		}
	}
	# }}}

	# {{{ Determine where to find the zip viewer (Filzip under Windows)
	## TODO... currently, it is in a fixed position
	if (isWin()) {
		FilzipExe <- file.path(bindir, "Filzip", "Filzip.exe")
		if (file.exists(FilzipExe)){
			options(ZipViewer = FilzipExe)
		}
	} else{
		
	}
	# }}}

	# {{{ Determine where to find the DVD burner (DeepBurner under Windows)
	## TODO... currently, it is in a fixed position
	if (isWin()) {
		DeepBurnerExe <- file.path(bindir, "DeepBurner", "DeepBurner.exe")
		if (file.exists(DeepBurnerExe)){
			options(DVDBurner = DeepBurnerExe)
		}
	}
	# }}}

	# {{{ Determine where to find VueScan
	## TODO... currently, it is in a fixed position
	VueScanExe <- file.path(bindir, "VueScan", "VueScan.exe")
	if( isWin() ){
		if (file.exists(VueScanExe)){
			options(VueScan = VueScanExe)
		}
	}
	# }}}

	# {{{ Possibly load the ZooImage assistant
	LoadIt <- getOption("ZIAssistant")
	if (is.null(LoadIt) || LoadIt == TRUE) {
		ZIDlg()
	}
	# }}}
	
	# {{{ Set the default template directory
	if( is.null( getOption( "ZITemplates" ) ) ){
		options( ZITemplates = system.file( "templates", package = "zooimage" ) )
	}
	# }}}
	
	# {{{ Switch to the default directory, if defined
	defdir <- getKey("DefaultDirectory", "")
    if (defdir != "" && file.exists(defdir) && file.info(defdir)$isdir){
        Setwd(defdir)
	}
	# }}}

}
# }}}

# {{{ Unloading ZooImage
".onUnload" <- function(libpath) {
	# Eliminate the ZooImage menu entries
	if (.Platform$GUI[1] == "Rgui") {
		require(svWidgets)
		try(MenuDel("$ConsoleMain/ZooImage"), silent = TRUE)
		try(MenuDel("$ConsolePopup/ZooImage"), silent = TRUE)
	}
	# Destroy the ZooImage Tk window, if it is currently displayed
	tkWinDel("ZIDlgWin")	 
}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:
