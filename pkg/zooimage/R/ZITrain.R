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
prepare.ZITrain <- function (dir, subdir = "_train", zidfiles, 
groups.template = c("[Basic]", "[Detailed]", "[Very detailed]"), 
ident = NULL, show.log = TRUE, bell = FALSE, start.viewer = FALSE)
{ 	                       
	## Make sure unzip is available
	checkCapable("unzip")
	
	## First, check that dir is valid
	checkDirExists(dir)
	
	## New dir is dir + subdir
	dir <- file.path(dir, subdir)
	
	checkEmptyDir(dir, message = "must be empty. Clean it first!")
	
	## Then, check that all zidfiles exist
	checkFileExistAll(zidfiles, "zid")

	## Finally, look for the groups.template
	groups.template <- groups.template[1]
	rx <- "^[[](.+)[]]$"
	if (grepl(rx, groups.template)) {
		## This should be a template file in the default directory
		groups.template <- paste(sub(rx, "\\1", groups.template), ".zic",
			sep = "")
		groups.template <- file.path(getTemp("ZIetc"), groups.template) 
	}
	
	## Check that this is a zic file
	zicCheck(groups.template)

	## Do the job...
	cat("Extracting data and vignettes ...\n")
	logProcess("\nExtracting data and vignettes ...")
	zmax <- length(zidfiles)
	
	## Create '_' subdir and unzip all vignettes there
	dir_ <- file.path(dir, "_")
	forceDirCreate(dir_)
	
	for (i in 1:zmax) {
		logProcess("data", zidfiles[i])
		Progress(i, zmax)
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
	}
	clearProgress()
	
	## Create the other directories
    Lines <- scan(groups.template, character(), sep = "\n", skip = 2,
		quiet = TRUE)
	if (length(Lines) < 1)
 		stop(sprintf("'%s' is empty or corrupted!", groups.template))
	Lines <- file.path(dir, Lines)
	cat("Making directories...\n")
	logProcess("\nMaking directories...")
	for (i in 1:length(Lines)) {
		logProcess(Lines[i])
		dir.create(Lines[i], recursive = TRUE)
	}
	### TODO: relocate vignettes in subdirectories, if ident is not NULL
	
	finishLoop(ok = TRUE, bell = bell, show.log = show.log, 
	  ok.console.msg = " -- Done! --\n", ok.log.msg = "\n-- Done! --")
	
	if (start.viewer) imageViewer(dir_)
	return(invisible(TRUE))
}

## Retrieve information from a manual training set
## and store it in a 'ZITrain' object	
get.ZITrain <- function (dir, creator = NULL, desc = NULL, keep_ = FALSE,
na.rm = FALSE)
{
	## 'dir' must be the base directory of the manual classification
	checkDirExists(dir)

	## Make sure we have .RData files in this dir (otherwise it is perhaps not a
	## training set root dir!
	Dats <- list.files(dir, pattern = "_dat1[.]RData$", full.names = TRUE)
	if (length(Dats) == 0)
		stop("does not appear to be a ", getTemp("ZIname"),
			" training set root dir!")

	## list the jpg files (recursively) in the dir
	res <- jpgList(dir, recursive = TRUE)

	## Check the result...
	if (length(res) < 1)
		stop("Error while getting data")

	## Replace "\\" by "/"
	res <- gsub("[\\]", "/", res)

	## Do we eliminate the '_' directory?
	if (!keep_) res <- grep("^[^_]", res, value = TRUE)

	## 'Id' is the name of the vignettes, minus the extension
	Id <- noext(basename(res))

	## 'Path' is the directory path
	Path <- dirname(res)

	## 'Class' is the last directory where the files are located
	Class <- basename(Path)

	## Create a directory (a data frame with: Id, Class)
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
	## Issue an error if there is no remaing row in the data frame
	if (nrow(df) == 0)
		stop("No valid item found (both with a vignette and with valid measurement data!")

	## Check that all items have associated measurements
	if (nrow(df) < nitems) {
    	nmiss <- nrow(df) - nitems
		warning(nmiss, " vignettes do not have associated measurement data. They are eliminated (",
			nrow(df), " items remain in the object)")
	}

	## Delete lines which contain NA values v1.2-2
	if (any(is.na(df))) {
		cat("NAs found in the table of measurements")
		if (na.rm) {
  	  		cat("... deleted\n")
  	  		df <- na.omit(df)
		} else cat("... left there\n")
  	}
	attr(df, "basedir") <- dir
	attr(df, "path") <- sort(unique(Path))
	if (!is.null(creator)) attr(df, "creator") <- creator
	if (!is.null(desc)) attr(df, "desc") <- desc
	Classes <- c("ZI1Train", "ZITrain", Classes)
	class(df) <- Classes
	## Be sure that variables are in numeric
	df <- as.numeric.Vars(df)
	return(df)
}

recode.ZITrain <- function (ZITrain, ZIRecode, warn.only = FALSE)
{	
	## Check classes
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
	return(ZITrain)
}

ZIRecodeLevels <- function (ZITrain, level = 1)
{
	## Check class
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
	return(res)
}

expand.ZITrain <- function (ZITrain, ZIDdir, destination)
{
	### TODO: make directories and extract vignettes for a classification
	stop("Not implemented yet!")
}

read.ZITrain <- function (file)
{
    ### TODO: read data from a text file
	stop("Not implemented yet!")
}

write.ZITrain <- function (ZITrain, file)
{
    ### TODO: write data to a text file
	stop("Not implemented yet!")
}

zip.ZITrain <- function (dir, zipfile, overwrite = FALSE)
{
    ### TODO: compress a classification tree
	stop("Not implemented yet!")
}

unzip.ZITrain <- function (zipfile, dir, overwrite = FALSE)
{
    ### TODO: uncompress a classification tree
	stop("Not implemented yet!")
}

## Function to add new vignettes in a training set
increase.ZITrain <- function (zidfiles, train)
{
	## Check if selected zid files are already classified in the training set
	Rdata <- list.files(train, pattern = ".RData")
	Rdata_New <- paste(sub("[.]zid$", "", basename(zidfiles)), "_dat1.RData",
		sep = "")
	NewZid <- !Rdata_New %in% Rdata
	
	if (!any(NewZid)) { # All zids are already in the training set
		stop("All selected zid files are already included in the training set")
	} else { # Keep only new zid files
		zidfiles <- zidfiles[NewZid]
		warning("You have selected ", length(zidfiles), " new zid files. ",
		"The others files are already included in the training set")
	}
	
	## Extract vignettes to a new subdir in '_' and RData to parent directory
	NewDir <- "_/_NewVignettes1"
	## Check if the new directory name already exists
	if (file.exists(file.path(train, NewDir))) {
		DirLst <- dir(file.path(train, "_"), pattern = "_NewVignettes")
		NewDir <- paste("_/_NewVignettes", (length(DirLst) + 1), sep = "")
	}
	
	## Check if NewDir exist
	ToPath <- file.path(dir, NewDir)
	if (!file.exists(ToPath)) forceDirCreate(ToPath)
	
	zmax <- length(zidfiles)
	## Extract RData in the root directory
	for (i in 1:zmax) {
		logProcess("data", zidfiles[i])
		Progress(i, zmax)
		## Using a temporary directory to unzip all files and then copy
		## the RData files to the train directory
		td <- tempfile()
		unzip(zipfile = zidfiles[i], path = td, delete.source = FALSE)
		datafiles <- file.path(td, list.files(td,
			pattern = extensionPattern(".RData"), recursive = TRUE))
		if (length(datafiles)) file.copy(datafiles, dir)
		vignettes <- file.path(td, list.files(td,
			pattern = extensionPattern(".jpg"), recursive = TRUE))
		if (length(vignettes))
			file.copy(vignettes, file.path(ToPath, basename(vignettes)))
		unlink(td, recursive = TRUE)
	}
	clearProgress()
	cat("-- Done --\n")
}
