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
## along with ZooImage.  If not, see <http://www.gnu.org/licenses/>.

## Prepare 'dir\subdir' for a manual classification by expanding all vignettes
## from a given number of zidfiles to the '_' subdir, and making
## a template for subdirs
## TODO: eliminate zidfiles and detect if it is zidfiles or zidbfiles like in addToTrain()
prepareTrain <- function (traindir, zidbfiles,
template = c("[Basic]", "[Detailed]", "[Very detailed]"), ident = NULL)
{
	## First, check that dirname of traindir is valid
	if (!checkDirExists(dirname(traindir))) return(invisible(FALSE))

	if (!checkEmptyDir(traindir,
		message = 'dir "%s" is not empty. Use AddToTrain() instead!'))
		return(invisible(FALSE))

	## Then, check that all zidfiles or zidbfiles exist
	if (hasExtension(zidbfiles[1], "zidb")) dbext <- "zidb" else dbext <- "zid"
    if (!checkFileExists(zidbfiles, dbext)) return(invisible(FALSE))
    zmax <- length(zidbfiles)

	## Also look for the template
	## If the object has a path template, use it...
	path <- attr(template, "path")
	if (!length(path)) { # Look for a .zic file with classes
		template <- as.character(template)[1]
		rx <- "^[[](.+)[]]$"
		if (grepl(rx, template)) {
			## This should be a template file in the default directory
			template <- paste(sub(rx, "\\1", template), ".zic",
				sep = "")
			template <- file.path(getTemp("ZIetc"), template)
			if (!file.exists(template)) {
				warning("The file '", template, "' is not found")
				return(invisible(FALSE))
			}
		}
		## Check that this is a .zic file
		if (!zicCheck(template)) return(invisible(FALSE))
	
		## Create the other directories
		path <- scan(template, character(), sep = "\n", skip = 2,
			quiet = TRUE)
		if (!length(path)) {
			warning(sprintf("'%s' is empty or corrupted!", template))
			return(invisible(FALSE))	
		}
	}

	## Create '_' subdir and unzip all vignettes there
	dir_ <- file.path(traindir, "_")
	if (!forceDirCreate(dir_)) return(invisible(FALSE))

	## Create subdirectories representing classes hierarchy
	message("Making directories...")
	path <- file.path(traindir, path)
	for (i in 1:length(path)) {
		#message(path[i])
		dir.create(path[i], recursive = TRUE)
	}
	
	## Place the vignettes...
	message("Extracting data and vignettes ...")
	for (i in 1:zmax) {
		progress(i, zmax)
        if (dbext != "zidb") {
            ## Using a temporary directory to unzip all files and then copy
    		## the RData files to the train directory
    		td <- tempfile()
    		unzip(zipfile = zidbfiles[i], exdir = td)
    		datafiles <- file.path(td, list.files(td,
    			pattern = extensionPattern(".RData"), recursive = TRUE))
    		if (length(datafiles)) file.copy(datafiles, traindir)
    		vignettes <- file.path(td, list.files(td,
    			pattern = extensionPattern(".jpg"), recursive = TRUE))
    		if (length(vignettes)) file.copy(vignettes, dir_)
    		unlink(td, recursive = TRUE)
		} else {  # Use .zidb files
            ## Link .zidb database to R objects in memory
            Zidb <- zidbLink(zidbfiles[i])
            AllItems <- ls(Zidb)
            Vigns <- AllItems[-grep("_dat1", AllItems)]
            ## Copy all vignettes in the "_" directory
            imgext <- Zidb[[".ImageType"]]
			for (j in 1:length(Vigns)){
                From <- Vigns[j]
                To <- file.path(dir_, paste(From, imgext, sep = "."))
                writeBin(Zidb[[From]], To)
            }
            ## Save vignettes
            ZI.sample <- Zidb$.Data
            save(ZI.sample, file = file.path(traindir, paste(sub(".zidb", "",
				basename(zidbfiles[i])), "_dat1.RData", sep = "")))
		}
	}
	progress(zmax + 1) # Clear progression indicator
	
	### TODO: relocate vignettes in subdirectories, if ident is not NULL
	if (length(ident)) {
		
	}

	message(" -- Done! --")
	invisible(TRUE)
}

## Function to add new vignettes in a training set
addToTrain <- function (traindir, zidbfiles, ident = NULL)
{
	## Check if selected zid(b) files are already classified in the training set
	Rdata <- list.files(traindir, pattern = "[.]RData$")
	RdataNew <- paste0(sub("[.]zidb?$", "", basename(zidbfiles)), "_dat1.RData")
	NewZidb <- !RdataNew %in% Rdata
	
	if (!any(NewZidb)) { # All zidbs are already in the training set
		warning("All selected ZID(B) files already in the training set")
		return(invisible(FALSE))
	} else { # Keep only new zid(b) files
		zidbfiles <- zidbfiles[NewZidb]
		warning("You have selected ", length(zidbfiles), " new ZID(B) files.\n",
			"The others files are already included in the training set")
	}
	
	## Extract vignettes to a new subdir in '_' and .RData to parent directory
	NewDir <- "_/_NewVignettes1"
	## Check if the new directory name already exists
	if (file.exists(file.path(traindir, NewDir))) {
		DirLst <- dir(file.path(traindir, "_"), pattern = "_NewVignettes")
		NewDir <- paste("_/_NewVignettes", (length(DirLst) + 1), sep = "")
	}
	
	## Check if NewDir exist
	ToPath <- file.path(traindir, NewDir)
	if (!file.exists(ToPath))
		if (!forceDirCreate(ToPath)) return(invisible(FALSE))
	
	## Extract RData in the root directory
	zmax <- length(zidbfiles)
	message("Adding data and vignettes to the training set...")
	for (i in 1:zmax) {
		progress(i, zmax)
		## treatment depends if it is a .zid or .zidb file
		zidbfile <- zidbfiles[i]
		if (grepl("[.]zidb$", zidbfile)) { # .zidb file
			## Link .zidb database to R objects in memory
            Zidb <- zidbLink(zidbfile)
            AllItems <- ls(Zidb)
            Vigns <- AllItems[-grep("_dat1", AllItems)]
            ## Copy all vignettes in the TopPath directory
            imgext <- Zidb[[".ImageType"]]
			for (j in 1:length(Vigns)){
                From <- Vigns[j]
                To <- file.path(ToPath, paste(From, imgext, sep = "."))
                writeBin(Zidb[[From]], To)
            }
            ## Save RData file
            ZI.sample <- Zidb$.Data
            save(ZI.sample, file = file.path(traindir, paste(sub(".zidb", "",
				basename(zidbfile)), "_dat1.RData", sep = "")))

		} else { # .zid file
			## Using a temporary directory to unzip all files and then copy
			## the RData files to the train directory
			td <- tempfile()
			unzip(zipfile = zidbfile, exdir = td)
			datafiles <- file.path(td, list.files(td,
				pattern = extensionPattern(".RData"), recursive = TRUE))
			if (length(datafiles))
				file.copy(datafiles, file.path(traindir, basename(datafiles)))
			vignettes <- file.path(td, list.files(td,
				pattern = extensionPattern(".jpg"), recursive = TRUE))
			if (!length(vignettes))
				vignettes <- file.path(td, list.files(td,
					pattern = extensionPattern(".png"), recursive = TRUE))
			if (length(vignettes))
				file.copy(vignettes, file.path(ToPath, basename(vignettes)))
			unlink(td, recursive = TRUE)	
		}
	}
	progress(zmax + 1) # Clear progression indicator
	message("-- Done --\n")
	invisible(TRUE)
}

## Retrieve information from a manual training set in a 'ZITrain' object	
getTrain <- function (traindir, creator = NULL, desc = NULL, keep_ = FALSE,
na.rm = FALSE)
{
	## 'traindir' must be the base directory of the manual classification
	if (!checkDirExists(traindir)) return(invisible(FALSE))

	## Make sure we have .RData files in this traindir (otherwise it is
	## perhaps not a training set root dir!
	Dats <- list.files(traindir, pattern = "_dat1[.]RData$", full.names = TRUE)
	if (!length(Dats)) {
		warning("'traindir' does not appear to be a ", getTemp("ZIname"),
			" training set root dir!")
		return(invisible(FALSE))
	}

	## List the .jpg or .png files (recursively) in the dir
	res <- jpgList(traindir, recursive = TRUE)
	if (!length(res)) res <- pngList(traindir, recursive = TRUE)

	## Check the result...
	if (!length(res)) {
		warning("no PNG or JPEG vignettes found in this tree")
		return(invisible(FALSE))
	}

	## Replace "\\" by "/"
	res <- gsub("[\\]", "/", res)

	## Do we eliminate the '_' directory?
	if (!isTRUE(as.logical(keep_)))
		res <- grep("^[^_]", res, value = TRUE)

	## 'Id' is the name of the vignettes, minus the extension
	Id <- noExtension(res)

	## 'Path' is the directory path
	Path <- dirname(res)

	## 'Class' is the last directory where the vignettes are located
	Class <- basename(Path)

	## Create a  data frame with Id and Class
	df <- data.frame(Id = Id, Class = Class)
	df$Id <- as.character(df$Id)
	nitems <- nrow(df)

	## Read in all the .RData files from the root directory and merge them
	## Get measurement infos
	ZI.sample <- NULL
	load(Dats[1])
	Dat <- ZI.sample
	Classes <- class(Dat)
	
	## Modif Kev to free memory
	Dat <- cbind(Id = makeId(Dat), Dat)
	Dat <- merge(Dat, df, by = "Id")

	if (length(Dats) > 1) {
		for (i in 2:length(Dats)) {
			load(Dats[i])
			ZI.sample <- cbind(Id = makeId(ZI.sample), ZI.sample)
			ZI.sample <- merge(ZI.sample, df, by = "Id")
			Dat <- rbind(Dat, ZI.sample)
		}
	}
	rownames(Dat) <- 1:nrow(Dat)

	## Rename Dat in df
	df <- Dat
	## Problem if there is no remaining row in the data frame
	if (nrow(df) == 0) {
		warning("No valid item found (no vignettes with valid measurement data)")
		return(invisible(FALSE))
	}

	## Check that all items have associated measurements
	if (nrow(df) < nitems)
		warning(nitems - nrow(df),
			" vignettes without measurement data are eliminated (",
			nrow(df), " items remain in the object)")

	if (any(is.na(df)))
		if (isTRUE(as.logical(na.rm))) {
  	  		message("NAs found in the table of measurements and deleted")
  	  		df <- na.omit(df)
		} else message("NAs found in the table of measurements and left there")
	
	## Add attributes
	attr(df, "basedir") <- dir
	attr(df, "path") <- sort(unique(Path))
	if (length(creator)) attr(df, "creator") <- creator
	if (length(desc)) attr(df, "desc") <- desc
	Classes <- c("ZI3Train", "ZITrain", Classes)
	class(df) <- Classes
	
	## Be sure that variables are numeric (sometimes, wrong importation)
#	as.numeric.Vars <- function (ZIDat, numvars) {
#	    if (is.null(numvars)) # Default values
#	        numvars <- c("ECD",
#	            "FIT_Area_ABD", "FIT_Diameter_ABD", "FIT_Volume_ABD",
#				"FIT_Diameter_ESD", "FIT_Volume_ESD", "FIT_Length", "FIT_Width",
#				"FIT_Aspect_Ratio", "FIT_Transparency", "FIT_Intensity",
#				"FIT_Sigma_Intensity", "FIT_Sum_Intensity", "FIT_Compactness",
#				"FIT_Elongation", "FIT_Perimeter", "FIT_Convex_Perimeter",
#				"FIT_Roughness", "FIT_Feret_Max_Angle", "FIT_PPC", "FIT_Ch1_Peak",
#				"FIT_Ch1_TOF", "FIT_Ch2_Peak", "FIT_Ch2_TOF", "FIT_Ch3_Peak",
#				"FIT_Ch3_TOF", "FIT_Avg_Red", "FIT_Avg_Green", "FIT_Avg_Blue",
#				"FIT_Red_Green_Ratio", "FIT_Blue_Green", "FIT_Red_Blue_Ratio",
#				"FIT_CaptureX", "FIT_CaptureY", "FIT_SaveX", "FIT_SaveY",
#				"FIT_PixelW", "FIT_PixelH", "FIT_Cal_Const",
#	            "Area", "Mean", "StdDev", "Mode", "Min", "Max", "X", "Y", "XM",
#	            "YM", "Perim.", "BX", "BY", "Width", "Height", "Major", "Minor",
#				"Angle", "Circ.", "Feret", "IntDen", "Median", "Skew", "Kurt",
#				"XStart", "YStart", "Dil")
#
#	    ## Make sure numvars are numeric
#		Names <- names(ZIDat)
#	    for (numvar in numvars) {
#	        if (numvar %in% Names && !is.numeric(ZIDat[, numvar]))
#	            ZIDat[, numvar] <- as.numeric(ZIDat[, numvar])
#	    }
#	    ZIDat
#	}
#	as.numeric.Vars(df, numvars = numvars)

	df
}

.recodeLevels <- function (object, depth = 1)
{
	if (!inherits(object, "ZITrain"))
		stop("'ZITrain' must be a 'ZITrain' object")
	
	depth <- as.integer(depth)[1]
	
	## Get the "path" attribute
	path <- attr(object, "path")
	
	## Split strings on "/"
	path <- strsplit(path, "/", fixed = TRUE)
	
	## Functions to get last item, or an item at a given level
	level <- function (x, depth = 1)
		ifelse(length(x) >= depth, x[depth], x[length(x)])
	
	## Return a list with new levels
	sapply(path, level, depth = depth)
}

recode <- function (object, ...)
	UseMethod("recode")

recode.ZITrain <- function (object, new.levels, depth, ...)
{	
	if (!inherits(object, "ZITrain"))
		stop("'ZITrain' must be a 'ZITrain' object")
	
	if (!missing(depth)) {
		if (!missing(new.levels))
			warning("depth is provided, so, new.levels is ignored and recomputed")
		new.levels <- .recodeLevels(object, depth)
	}
	
	## Check that new.levels is of the same length as levels(object$Class)
	## [and object$Ident or Ident2, possibly]
	levels <- levels(object$Class)
	new.levels <- as.character(new.levels)
	if (length(new.levels) != length(levels))
		stop("length of new.levels must match levels in object$Class")
	
	relevel <- function (x, levels, new.levels) {
		x <- as.character(x)
		for (i in 1:length(levels))
			if (new.levels[i] != levels[i])
				x[x == levels[i]] <- new.levels[i]
		as.factor(x)
	}
	
	object$Class <- relevel(object$Class, levels, new.levels)
	if (!is.null(object$Ident))
		object$Ident <- relevel(object$Ident, levels, new.levels)
	if (!is.null(object$Ident2))
		object$Ident2 <- relevel(object$Ident2, levels, new.levels)
	
	## If a new path is given for these new classes, change it
	path <- attr(new.levels, "path")
	### TODO: check its validity here
	if (!is.null(path)) attr(object, "path") <- path
	object
}
