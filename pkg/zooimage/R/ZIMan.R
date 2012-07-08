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

## Function to classify vignettes using automatic prediction by a ZIClass object
classVignettes <- function (zidfile, ZIDat = NULL, Dir = "_manuValidation",
ZIClass, log = TRUE, ProbaThreshold = NULL, ProbaBio = NULL, DiffMax = 0.2,
Filter = NULL)
{
	DirName <- dirname(zidfile)
	ZidName <- noExtension(zidfile)
	ZidDir <- file.path(DirName, ZidName)
  
	## Check if Directory with the same names as ZIdfile
	if (file.exists(ZidDir) && file.info(ZidDir)$isdir) {
		## There is a directory which already exists
		stop(paste(ZidName, "already exists in your directory!"))
	}
  
	## If we want to extract vignette in a directory with a special name
	if (Dir == ZidName) {
		## Temporary name for vignettes extraction
		## ... will be renamed at the end of process!
		dir <- paste(ZidName, "_Temp", sep = "")
		## We have to rename the directory at the end of the process!
		Rename <- TRUE
	} else {
		dir <- Dir
		## We don't have to rename this directory at the end!
		Rename <- FALSE
	}
  
	## What is the final Directory name?
	if (Rename) {
		FinalDirName <- ZidDir
	} else {
		FinalDirName <- file.path(DirName, dir)
	}

	## Check Rdata if Rdata exist in the final directory:
	## useful to add new vignettes to one directory
	ContinueProcess <- TRUE
	if (file.exists(FinalDirName) && file.info(FinalDirName)$isdir) {
		## There is a directory which already exists
		Pattern <- "_dat1.RData"
		RdataFiles <- list.files(path = FinalDirName, pattern = Pattern)
		## List Rdata Files in this direcotroy
		if (length(RdataFiles) > 0) { # At least one Rdata in the directory
			## Check if the current zid file correspond to one RData file
			if (ZidName %in% gsub(Pattern, "", RdataFiles)) {
				## Stop this zidfile has already been validated
				cat(paste(ZidName, "has already been manually validated in",
					basename(FinalDirName), "directory", "\n", sep = " "))
				ContinueProcess <- FALSE
			} 
		}
	}
  
	## Do we continue the process for this zid file?
	if (isTRUE(ContinueProcess)) {
		## Do we use a ZIDat object allready recognized?
		if (is.null(ZIDat)) {
			Zid <- zidDatRead(zidfile)
		} else Zid <- ZIDat
    
		## Code for suspect detection!
#    	# Recognition of the zid file only if we don't have a probability
#    	if (is.null(attr(Zid, "ProbaParam"))) {
#    	  Rec <- predict(ZIClass, Zid, proba = TRUE, ProbaBio = ProbaBio,
#    	       DiffMax = DiffMax)
#    	} else {
#    	  Rec <- Zid
#    	}
		## Code for simple prediction!
		## Recognition of the zid file only if we don't have an 'Ident' column
		if (!isTRUE("Ident" %in% names(Zid))) {
			Rec <- predict(ZIClass, Zid)
		} else Rec <- Zid
    
		## Prediction of table
		Predictions <- Rec$Ident
    
		## Classify only suspect particles
		## TODO: Suspect_Threshold() is not found!!!
#		if (!is.null(ProbaThreshold))
#   	   Rec <- Suspect_Threshold(ZIDat = Rec, Threshold = ProbaThreshold)

		## Do we apply a filter?
		if (!is.null(Filter)) {
			Rec <- subpartThreshold(ZIDat = Rec, Filter = Filter)
			cat("Only", nrow(Rec), "filtered vignettes have been classified\n")
		}
    
		## List of groups in the sample
		Gp <- unique(Rec$Ident)
    
		## Path of all directories  
		if (!is.null(attr(ZIClass, "path"))) {
			## There is a 'path' attribute associated with the classifer
			GpDir <- file.path(DirName, dir, attr(ZIClass, "path"))
		} else { # Only create classifier without taxonomic relationship
			GpDir <- file.path(DirName, dir, Gp)
		}
		
		## Create directories for new groups on harddisk
		for (i in 1:length(GpDir)) {
			if (!file.exists(GpDir)[i])
				dir.create(GpDir[i], showWarnings = TRUE, recursive = TRUE)
		}
		zidUncompress(zidfile)
    
		## Copy vignettes from zidfile directory to group directories
		Rec$Vign <- makeId(Rec)
    
		for (i in 1:nrow(Rec)) {
			From <- file.path(ZidDir, paste(Rec$Vign[i], "jpg", sep = "."))
			To <- file.path(GpDir[basename(GpDir) %in% as.character(Rec$Ident[i])],
				paste(Rec$Vign[i], "jpg", sep = "."))
			file.copy(from = From, to = To, overwrite = FALSE)
			file.remove(From)
		}
		
		## Copy RData in root directory
		From <- file.path(ZidDir, paste(ZidName, "_dat1.RData", sep = ""))
		To <- file.path(file.path(DirName, dir),
			paste(ZidName, "_dat1.RData", sep = ""))
		file.copy(from = From, to = To, overwrite = FALSE)
		Rdata <- To
		file.remove(From)
  
		## Remove directory
		unlink(ZidDir, recursive = TRUE)
    
		## Add Automatic recognition column to Rdata!
		addIdent(RdataFile = Rdata, Auto = Predictions)
    
		if (Rename) # Rename the Directory where the zid file were exported!
			file.rename(from = file.path(dirname(zidfile), dir),
				to = FinalDirName)
    
		## Message to confirm the end of the treatment
		if (log)
			cat("Vignettes of", ZidName,"have been exported into",
				basename(FinalDirName), "directory\n")
	}
}

## Loop to classify vignettes from several zid files in _manuValidation
classVignettesAll <- function (zidfiles, ZIClass, Dir = "_manuValidation",
log = TRUE)
{
	for (i in 1:length(zidfiles))
		classVignettes(zidfile = zidfiles[i], ZIClass = ZIClass, Dir = Dir,
			log = log) 
	cat("--- Process Done ---\n")
}

## Function to add 'Ident' column to a ZIDat directly in the Rdata file
addIdent <- function (RdataFile, Auto)
{
	if (!is.character(RdataFile))
		stop("'RdataFile' muste be the path of the Rdata to modify")
	## Load Rdata in memory
	load(file = RdataFile, envir = .GlobalEnv)
  
	## Add the Ident column
	ZI.sample$Ident <- as.factor(Auto)
  
	## Replace existing Rdata
	save(ZI.sample, file = RdataFile)
  
	## Remove Rdata from memory
	rm(ZI.sample, envir = .GlobalEnv)
}

# Read manual validation
ZIManRead <- function (dir, creator = NULL, desc = NULL, keep_ = FALSE,
na.rm = FALSE)
{
	## Use getTrain() function to read vignette
	ManValidation <- getTrain(traindir = dir, creator = creator, desc = desc,
		keep_ = keep_, na.rm = na.rm)
  
	## Add attributes with names of samples already manually validated
	RDataFiles <- list.files(dir, pattern = "_dat1.RData")
	RDataSamples <- gsub("_dat1.RData", "", RDataFiles)
	attr(ManValidation, "Samples") <- RDataSamples
  
	## Change classes of the object
	class(ManValidation) <- c("ZIMan", class(ManValidation))
	return(ManValidation)
}

## Provide groups after manual validation
reclass <- function (ZIMan)
{
	## Check arguments
	if (!inherits(ZIMan, "ZIMan"))
		stop("ZIMan must be an object of class 'ZIMan'")
	if (!isTRUE("Class" %in% names(ZIMan)))
		stop("ZIMan doesn't contain a column named 'Class'")
  
	## New identification
	res <- table(ZIMan$Class)
	return(res)
}

## Confusion matrix before and after Manual validation
confusionCompa <- function (ZIMan)
{
	## Check arguments
	if (!inherits(ZIMan, "ZIMan"))
		stop("ZIMan must be an object of class 'ZIMan'")
	if (!isTRUE("Class" %in% names(ZIMan)))
		stop("ZIMan doesn'y contain a column named 'Class'")
	if (!isTRUE("Ident" %in% names(ZIMan)))
		stop("ZIMan doesn'y contain a column named 'Ident'")
	## Confusion matrix
	res <- table(Class = ZIMan$Class, Predict = ZIMan$Ident)
	return(res)
}

## Difference between prediction
ZIManCompa <- function (ZIMan)
{
	# Check arguments
	if (!inherits(ZIMan, "ZIMan"))
		stop("ZIMan must be an object of class 'ZIMan'")
	if (!isTRUE("Class" %in% names(ZIMan)))
		stop("ZIMan doesn'y contain a column named 'Class'")
	if (!isTRUE("Ident" %in% names(ZIMan)))
		stop("ZIMan doesn'y contain a column named 'Ident'")
	## Difference
	Before <- table(ZIMan$Ident)
	After <- table(ZIMan$Class)
	res <- list(Predicted = Before, Validated = After)
	return(res)  
}

## Substract a ZIDat table according a threshold formula
subpartThreshold  <- function (ZIDat, Filter = NULL)
{    
    ## Do we use a Filter directly?
    if (is.null(Filter)) {
		Threshold <- createThreshold(ZIDat = ZIDat)
    } else {
        if (!is.character(Filter))
			stop("Filter must be like 'Parameter < Value'")
        Threshold <- Filter
    }
    ## Determine particle responding to the threshold
    SubPart <- within(ZIDat, {
        Index <- eval(parse(text = (Threshold)))
    })
    
    res <- ZIDat[SubPart$Index, ]
    attr(res, "Threshold") <- Threshold
    return(res)
}
