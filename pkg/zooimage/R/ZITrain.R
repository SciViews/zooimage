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
prepareTrain <- function (rootdir, subdir = "_train", zidfiles, zidbfiles = NULL,
groups.template = c("[Basic]", "[Detailed]", "[Very detailed]"),
ident = NULL, start.viewer = FALSE)
{
	## First, check that rootdir is valid
	if (!checkDirExists(rootdir)) return(invisible(FALSE))

	## New dir is rootdir + subdir
	dir <- file.path(rootdir, as.character(subdir)[1])
	if (!checkEmptyDir(dir,
		message = 'dir "%s" must be empty. Clean it first!'))
		return(invisible(FALSE))

	## Then, check that all zidfiles or zidbfiles exist
	if (is.null(zidbfiles)) {
        if (!checkFileExists(zidfiles, "zid")) return(invisible(FALSE))
        zmax <- length(zidfiles)
    } else {
        if (!checkFileExists(zidbfiles, "zidb")) return(invisible(FALSE))
        zmax <- length(zidbfiles)
    }

	## Finally, look for the groups.template
	groups.template <- as.character(groups.template)[1]
	rx <- "^[[](.+)[]]$"
	if (grepl(rx, groups.template)) {
		## This should be a template file in the default directory
		groups.template <- paste(sub(rx, "\\1", groups.template), ".zic",
			sep = "")
		groups.template <- file.path(getTemp("ZIetc"), groups.template)
		if (!file.exists(groups.template)) {
			warning("The file '", groups.template, "' is not found")
			return(invisible(FALSE))
		}
	}
	## Check that this is a .zic file
	if (!zicCheck(groups.template)) return(invisible(FALSE))

	## Do the job...
	message("Extracting data and vignettes ...")

	## Create '_' subdir and unzip all vignettes there
	dir_ <- file.path(dir, "_")
	if (!forceDirCreate(dir_)) return(invisible(FALSE))

	for (i in 1:zmax) {
		Progress(i, zmax)
        if (is.null(zidbfiles)) {
    		logProcess("data", zidfiles[i])
            ## Using a temporary directory to unzip all files and then copy
    		## the RData files to the train directory
    		td <- tempfile()
    		unzip(zipfile = zidfiles[i], path = td, delete.source = FALSE)
    		datafiles <- file.path(td, list.files(td,
    			pattern = extensionPattern(".RData"), recursive = TRUE))
    		if (length(datafiles)) file.copy(datafiles, dir)
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
            ext <- Zidb[[".ImageType"]]
			for (j in 1:length(Vigns)){
                From <- Vigns[j]
                To <- file.path(dir_, paste(From, ext, sep = "."))
                writeBin(Zidb[[From]], To)
            }
            ## Save vignettes
            ZI.sample <- Zidb$.Data
            save(ZI.sample, file = file.path(dir, paste(sub(".zidb", "", basename(zidbfiles[i])), "_dat1.RData", sep = "")))
		}
	}
	clearProgress()

	## Create the other directories
    Lines <- scan(groups.template, character(), sep = "\n", skip = 2,
		quiet = TRUE)
	if (!length(Lines)) {
 		warning(sprintf("'%s' is empty or corrupted!", groups.template))
		return(invisible(FALSE))	
	}
	Lines <- file.path(dir, Lines)
	message("Making directories...")
	for (i in 1:length(Lines)) {
		message(Lines[i])
		dir.create(Lines[i], recursive = TRUE)
	}
	### TODO: relocate vignettes in subdirectories, if ident is not NULL

	## Finish and possibly start the image viewer
	message(" -- Done! --")
	if (isTRUE(as.logical(start.viewer))) imageViewer(dir_)
	return(invisible(TRUE))
}

## Function to add new vignettes in a training set
increaseTrain <- function (traindir, zidbfiles)
{
	## Check if selected zid(b) files are already classified in the training set
	Rdata <- list.files(traindir, pattern = "[.]RData$")
	RdataNew <- paste0(sub("[.]zidb?$", "", basename(zidbfiles)), "_dat1.RData")
	NewZidb <- !RdataNew %in% Rdata
	
	if (!any(NewZidb)) { # All zidbs are already in the training set
		warning("All selected zid(b) files already in the training set")
		return(invisible(FALSE))
	} else { # Keep only new zid(b) files
		zidbfiles <- zidbfiles[NewZidb]
		warning("You have selected ", length(zidbfiles), " new zid(b) files.\n",
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
		Progress(i, zmax)
		## treatment depends if it is a .zid or .zidb file
		zidbfile <- zidbfiles[i]
		if (grepl("[.]zidb$", zidbfile)) { # .zidb file
			
		} else { # .zid file
			## Using a temporary directory to unzip all files and then copy
			## the RData files to the train directory
			td <- tempfile()
			unzip(zipfile = zidbfiles[i], path = td, delete.source = FALSE)
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
	clearProgress()
	message("-- Done --\n")
	return(invisible(TRUE))
}

## Retrieve information from a manual training set in a 'ZITrain' object	
getTrain <- function (traindir, creator = NULL, desc = NULL, keep_ = FALSE,
na.rm = FALSE, numvars = NULL)
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
		warning("no .png or .jpg vignettes found in this tree")
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
    ### TODO: also collect metadata and merge them => make a merge function for
	## ZIDat!!!
	## Get measurement infos
    #### TODO: Kevin, you cannot use this! You must refer to ZI.sample directly
	## in the arguments of the function!
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

	## Create the Id column
# Done in the loop!
#	Dat <- cbind(Id = makeId(Dat), Dat)

	## Merge Dat & df by "Id"
#	df <- merge(Dat, df, by = "Id")
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

	## Delete lines which contain NA values v1.2-2
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
	as.numeric.Vars <- function (ZIDat, numvars) {
	    if (is.null(numvars)) # Default values
	        numvars <- c("ECD",
	            "FIT_Area_ABD", "FIT_Diameter_ABD", "FIT_Volume_ABD",
				"FIT_Diameter_ESD", "FIT_Volume_ESD", "FIT_Length", "FIT_Width",
				"FIT_Aspect_Ratio", "FIT_Transparency", "FIT_Intensity",
				"FIT_Sigma_Intensity", "FIT_Sum_Intensity", "FIT_Compactness",
				"FIT_Elongation", "FIT_Perimeter", "FIT_Convex_Perimeter",
				"FIT_Roughness", "FIT_Feret_Max_Angle", "FIT_PPC", "FIT_Ch1_Peak",
				"FIT_Ch1_TOF", "FIT_Ch2_Peak", "FIT_Ch2_TOF", "FIT_Ch3_Peak",
				"FIT_Ch3_TOF", "FIT_Avg_Red", "FIT_Avg_Green", "FIT_Avg_Blue",
				"FIT_Red_Green_Ratio", "FIT_Blue_Green", "FIT_Red_Blue_Ratio",
				"FIT_CaptureX", "FIT_CaptureY", "FIT_SaveX", "FIT_SaveY",
				"FIT_PixelW", "FIT_PixelH", "FIT_Cal_Const",
	            "Area", "Mean", "StdDev", "Mode", "Min", "Max", "X", "Y", "XM",
	            "YM", "Perim.", "BX", "BY", "Width", "Height", "Major", "Minor",
				"Angle", "Circ.", "Feret", "IntDen", "Median", "Skew", "Kurt",
				"XStart", "YStart", "Dil")

	    ## Make sure numvars are numeric
		Names <- names(ZIDat)
	    for (numvar in numvars) {
	        if (numvar %in% Names && !is.numeric(ZIDat[, numvar]))
	            ZIDat[, numvar] <- as.numeric(ZIDat[, numvar])
	    }
	    ZIDat
	}
	as.numeric.Vars(df, numvars = numvars)
}

recode.ZITrain <- function (ZITrain, ZIRecode, warn.only = FALSE)
{	
	if (!inherits(ZITrain, "ZITrain"))
		stop("'ZITrain' must be a 'ZITrain' object")
	if (!inherits(ZIRecode, "ZIRecode"))
		stop("'ZIRecode' must be a 'ZIRecode' object")
	
	## Check that all levels in ZITrain$Class are represented in ZIRecode
	if (!all(sort(levels(ZITrain$Class))  == sort(levels(ZIRecode[ , 1]))))
			stop("Not all levels of ZIRecode match levels of ZITrain")
	
	## Class column of ZITrain is transformed into a character vector
	Class <- as.character(ZITrain$Class)
	## It is then recoded
	for (i in 1:nrow(ZIRecode)) {
		if (ZIRecode[i, 1] != ZIRecode[i, 2])
			Class[Class == ZIRecode[i, 1]] <- ZIRecode[i, 2]
	}
	## ...and transformed back into a factor
	ZITrain$Class <- as.factor(Class)
	
	## If a new path is given for these new groups, change it
	path <- attr(ZIRecode, "path")
	### TODO: check its validity here
	if (!is.null(path)) attr(ZITrain, "path") <- path
	ZITrain
}

## Merge with previous one!
ZIRecodeLevels <- function (ZITrain, level = 1)
{
	if (!inherits(ZITrain, "ZITrain"))
		stop("'ZITrain' must be a 'ZITrain' object")
	
	## Get the "path" attribute
	Path <- attr(ZITrain, "path")
	
	## Split strings on "/"
	Path <- strsplit(Path, "/", fixed = TRUE)
	
	## Functions to get last item, or an item at a given level
	Last <- function (x) x[length(x)]
	Level <- function (x, level = 1)
		ifelse(length(x) >= level, x[level], x[length(x)])
	res <- data.frame(Class = I(sapply(Path, Last)),
		Recode = I(sapply(Path, Level, level = level)))
	class(res) <- c("ZIRecode", "data.frame")
	attr(res, "call") <- match.call()
	## We do not need to change the path here: it is still the same one
	res
}
