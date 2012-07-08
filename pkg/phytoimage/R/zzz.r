## Copyright (c) 2004-2012, Ph. Grosjean <phgrosjean@sciviews.org>
##
## This file is part of PhytoImage
##
## PhytoImage is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## PhytoImage is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with PhytoImage.  If not, see <http://www.gnu.org/licenses/>.

## Loading and unloading PhytoImage

.onLoad <- function (libname, pkgname)
{
	## Define name and icon we want for the ZooImage application
	ZIname <- "PhytoImage"
	assignTemp("ZIname", ZIname)
	ZIetc <- system.file("etc", package = "phytoimage")
	assignTemp("ZIetc", ZIetc)
	ZIgui <- system.file("gui", package = "phytoimage")
	assignTemp("ZIgui", ZIgui)
	#if (isWin())
	#	assignTemp("ZIico", tk2ico.create(file.path(getTemp("ZIgui"), "PhytoImage.ico")))

	## Load the various image resources
	#tkImgReadPackage("phytoimage")
	## Load the menus
	#MenuReadPackage("phytoimage")
	## Indicate that other GUI resources should be loaded from the phytoimage package
	ZIguiPackage <- "phytoimage"
	assignTemp("ZIguiPackage", ZIguiPackage)

	## Make sure that ZooImage will not overwrite these entries
	options(ZI.redefine = TRUE)

	## Load the initial zooimage package now
	## No, this is now done in NAMESPACE import!
	#if (!require(zooimage)) stop("Package 'zooimage' is required!")
}

## Unloading PhytoImage
.onUnload <- function (libpath)
{
	## Eliminate the PhytoImage menu entries
	if (.Platform$GUI[1] == "Rgui") {
		try(menuDel("$ConsoleMain/PhytoImage"), silent = TRUE)
		try(menuDel("$ConsolePopup/PhytoImage"), silent = TRUE)
	}
	try(detach("package:zooimage", unload = TRUE), silent = TRUE)
}
