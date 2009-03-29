".First.lib" <-
function(libname, pkgname) {
	(require(svMisc) || stop("Package 'svMisc' from SciViews bundle is required!"))
	(require(svWidgets) || stop("Package 'svWidgets' from SciViews bundle is required!"))
	(require(tcltk2) || stop("Package 'tcltk2' is required!"))

	# Define name and icon we want for the ZooImage application
	ZIname <- "PhytoImage1"
	assignTemp("ZIname", ZIname)
	ZIetc <- file.path(.path.package(package = "phytoimage")[1], "etc")
	assignTemp("ZIetc", ZIetc)
	ZIgui <- file.path(.path.package(package = "phytoimage")[1], "gui")
	assignTemp("ZIgui", ZIgui)
	if (isWin())
		assignTemp("ZIico", tk2ico.create(file.path(getTemp("ZIgui"), "PhytoImage.ico")))

	# Load the various image resources
	tkImgReadPackage("phytoimage")
	# Load the menus
	MenuReadPackage("phytoimage")
	# Indicate that other GUI resources should be loaded from the phytoimage package
	ZIguiPackage <- "phytoimage"
	assignTemp("ZIguiPackage", ZIguiPackage)

	# Make sure that ZooImage will not overwrite these entries
	options(ZIredefine = TRUE)

	# Load the initial zooimage package now
	(require(zooimage) || stop("Package 'zooimage' is required!"))
}

".Last.lib" <-
function(libpath) {
	# Eliminate the PhytoImage menu entries
	if (.Platform$GUI[1] == "Rgui") {
		require(svWidgets)
		try(MenuDel("$ConsoleMain/PhytoImage"), silent = TRUE)
		try(MenuDel("$ConsolePopup/PhytoImage"), silent = TRUE)
	}
}
