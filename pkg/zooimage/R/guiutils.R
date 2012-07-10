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
## along with ZooImage. If not, see <http://www.gnu.org/licenses/>.

## Get the name of one or several objects of a given class
selectObject <- function (class = "data.frame", default = "", multiple = FALSE,
title = paste0("Choose a ", class, ":"))
{	
	objlist <- ls(envir = .GlobalEnv,)	# Get objects in .GlobalEnv
	if (!length(objlist)) {
		warning("There is no object of class '", paste(class, collapse = " "),
			"' in the user workspace!")
		return(character(0))
	}
	## Filter this list to keep only object inheriting a giving class...
	Filter <- NULL
	for (i in 1:length(objlist))
		Filter[i] <- inherits(get(objlist[i], envir = .GlobalEnv,
			inherits = FALSE), class)
	
	## Keep only those objects
	objlist <- objlist[Filter]	
	if (!length(objlist)) {	# No such objects in .GlobalEnv
		warning("There is no object of class '", paste(class, collapse = " "),
			"' in the user workspace!")
		return(character(0))
	}
	if (default == "") default <- objlist[1]
	dlgList(objlist, preselect = default, multiple = multiple,
		title = title)$res		
}

## Get the name of one or more lists with their components of a given class
selectList <- function (class = "data.frame", default = "", multiple = FALSE,
title = paste0("Choose a list (of ", class, "s):"))
{	
	filter <- function (x) {
		item <- get(x, envir = .GlobalEnv, inherits = FALSE)
		is.list(item) && all(sapply(item, function (y) inherits(y, class)))
	}
	varlist <- Filter(filter, objects(pos = 1))	
	if (length(varlist) == 0) {
		warning("no list of '", class, "' objects in the user workspace")
		return(character(0))
	}
	if (default == "") default <- varlist[1]
	dlgList(varlist, preselect = default, multiple = multiple,
		title = title)$res		
}

## Select one or several files of a given type
selectFile <- function (type = c("ZipZid", "ZimZis", "LstZid", "ZidZidb",
"Zip", "Zid", "Zidb", "Zim", "Zis", "Zie", "Zic", "Img", "TifPgm", "RData"),
multiple = FALSE, quote = TRUE, title = NULL)
{	
	type <- match.arg(type)
	Type <- switch(type,
		ZipZis = "Zip/Zis",
		ZimZis = "Zim/Zis",
		LstZis = "Lst/Zis",
		TifPgm = "Tiff/Pgm",
		ZidZidb = "Zid/Zidb",
		type)
	
	## Adapt title according to 'multiple'
	if (isTRUE(as.logical(multiple)) && !is.null(title)) {
		title <- paste("Select one or several", Type, "files...")
	} else title <- paste("Select one", Type, "file...")
	
	filters <- switch(type,
		ZipZid 	= c("ZooImage files"          		, ".zip",
					"ZooImage files"          		, ".zid"),
		ZimZis 	= c("ZooImage metadata files" 		, ".zim",
					"ZooImage metadata files" 		, ".zis"),
		LstZid  = c("FlowCAM list files"      		, ".lst",
					"ZooImage files"          		, ".zid"),
		ZidZidb = c("ZooImage files"          		, ".zid",
					"ZooImage databases"      		, ".zidb"),
		Zip		= c("ZooImage picture files"  		, ".zip"),
		Zid		= c("ZooImage data files"     		, ".zid"),
		Zidb    = c("ZooImage databases"      		, ".zidb"),
		Zim		= c("ZooImage metadata files" 		, ".zim"),
		Zis		= c("ZooImage sample files"   		, ".zis"),
		Zie		= c("ZooImage extension files"		, ".zie"),
		Zic     = c("ZooImage Classification Scheme",".zic" ),
		Img     = c("Tiff image files"        		, ".tif",
					"Jpeg image files"        		, ".jpg",
					"Zooimage import extensions"	,".zie",
					"Table and ImportTemplate.zie"	,".txt",
					"FlowCAM Table and ImportTemplate.zie",".txt"),
		TifPgm  = c("Tiff image files"        		, ".tif",
					"Pgm image files"         		, ".pgm"),
		RData   = c("R data"                  		, ".RData"))
	filters <- matrix(filters, ncol = 2, byrow = TRUE)
	
	res <- dlgOpen(title = title, multiple = multiple, filters = filters)$res	
	if (length(res) && res != "" && quote)
		res <- paste('"', res, '"', sep = "")
	res
}


## Select groups (taxa) from a a list
## Note: from a ZIC object, use: groups <- levels(attr(ZIC, "classes"))
selectGroups <- function (groups, multiple = TRUE,
title = "Select taxa you want to plot") {
	dlgList(groups, multiple = multiple, title = title)$res
}

## Start the image viewer application on the specified dir
## TODO: rework this!
imageViewer <- function (dir = getwd(), pgm = getOption("ZI.ImageViewer"))
{
	if (isWin()) {
#		startPgm("ImageViewer", sprintf('"%s"',
#			tools:::file_path_as_absolute(dir)))
	} else if (isMac()) {
		cmd <- sprintf('/Applications/Utilities/XnViewMP.app/Contents/MacOS/xnview "%s"',
			dir)
		system(cmd, wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
	} else {
		cmd <- sprintf('nautilus --geometry 600x600 "%s"', dir) 
		system(cmd, wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
	}
}
