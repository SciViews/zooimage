# Copyright (c) 2004-2010, Ph. Grosjean <phgrosjean@sciviews.org>
#
# This file is part of ZooImage
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

# Function to classify vignettes according automatic prediction by a ZIClass object
"classifVign" <- function(zidfile, ZIDat = NULL, Dir = "_manuValidation", ZIClass, log = TRUE,
  ProbaThreshold = NULL, ProbaBio = NULL, DiffMax = 0.2, Filter = NULL)
{
  
  DirName <- dirname(zidfile)
  ZidName <- noext(zidfile)
  ZidDir <- file.path(DirName, ZidName)
  
  # Check if Directory with the same names as ZIdfile
  if(file.exists(ZidDir) && file.info(ZidDir)$isdir){
    # There is a directory which already exists
    stop(paste(ZidName, "already exists in your directory!"))
  }
  
  # If we want to extract vignette in a directory with a special name
  if(Dir == ZidName){
    # Temporary name for vignettes extraction will be renamed at the end of process!
    dir <- paste(ZidName, "_Temp", sep = "")
    # We have to rename the directory at the end of the process!
    Rename <- TRUE
  } else {
    dir <- Dir
    # We don't have to rename this directory at the end!
    Rename <- FALSE
  }
  
  # What is the final Directory name?
  if(isTRUE(Rename)){
    FinalDirName <- ZidDir
  } else {
    FinalDirName <- file.path(DirName, dir)
  }

  # Check Rdata if Rdata exist in the final directory: useful to add new vignettes to one directory
  ContinueProcess <- TRUE
  if(file.exists(FinalDirName) && file.info(FinalDirName)$isdir){
    # There is a directory which already exists
    Pattern <- "_dat1.RData"
    RdataFiles <- list.files(path = FinalDirName, pattern = Pattern)
    # List Rdata Files in this direcotroy
    if(length(RdataFiles) >0 ){ # At least one Rdata in the directory
      # Check if the current zid file correspond to one RData file
      if(ZidName %in% gsub(Pattern, "", RdataFiles)){
	# Stop this zidfile has already been validated
	cat(paste(ZidName, "has already been manually validated in", basename(FinalDirName), "directory", "\n", sep = " ")) # Process continues with cat!
	ContinueProcess <- FALSE
      } 
    }
  }
  
  # Do we continue the process for this zid file?
  if(isTRUE(ContinueProcess)){
    # Do we use a ZIDat object allready recognized?
    if(is.null(ZIDat)){
      Zid <- read.zid(zidfile)
    } else {
      Zid <- ZIDat
    }
    
    ##### Code for suspect detection!
#    # Recognition of the zid file only if we don't have a probability
#    if(is.null(attr(Zid, "ProbaParam"))){
#      Rec <- predict(ZIClass, Zid, proba = TRUE, ProbaBio = ProbaBio, DiffMax = DiffMax)
#    } else {
#      Rec <- Zid
#    }
    # Code for simple prediction!
    # Recognition of the zid file only if we don't have an 'Ident' column
    if(!isTRUE("Ident" %in% names(Zid))){
      Rec <- predict(ZIClass, Zid)
    } else {
      Rec <- Zid
    }
    
    # Prediction of table
    Predictions <- Rec$Ident
    
    # Classify only suspect particles
    if(!is.null(ProbaThreshold)){
      Rec <- Suspect_Threshold(ZIDat = Rec, Threshold = ProbaThreshold)
    }

    # Do we apply a filter?
    if(!is.null(Filter)){
      Rec <- SubpartThreshold(ZIDat = Rec, Filter = Filter)
      cat(paste("Only", nrow(Rec), "filtered vignettes have been classified\n", sep = " "))
    }
    
    # List of groups in the sample
    Gp <- unique(Rec$Ident)
    
    # Path of all directories  
    if(!is.null(attr(ZIClass, "path"))){
      # There is a 'path' attribute associated with the classifer
      GpDir <- file.path(DirName, dir, attr(ZIClass, "path"))
    } else {
      # only create classifier without taxonomic relationship
      GpDir <- file.path(DirName, dir, Gp)
    }
    
    # Create directories for new groups on harddisk
    for(i in 1 : length(GpDir)){
      if(!file.exists(GpDir)[i]){
	dir.create(GpDir[i], showWarnings = TRUE, recursive = TRUE)
      }
    }
    
    uncompress.zid(zidfile)
    
    # Copy vignettes from zidfile directory to group directories
    Rec$Vign <- make.Id(Rec)
    
    for(i in 1:nrow(Rec)){
      # Debug 2010-04-08
       From <- file.path(ZidDir, paste(Rec$Vign[i], "jpg", sep = "."))
       To <- file.path(GpDir[basename(GpDir) %in% as.character(Rec$Ident[i])], paste(Rec$Vign[i], "jpg", sep = "."))
       file.copy(from = From, to = To, overwrite = FALSE)
       file.remove(From)
    }
    
    # Copy Rdata in root directory
    From <- file.path(ZidDir, paste(ZidName, "_dat1.RData", sep = ""))
    To <- file.path(file.path(DirName, dir), paste(ZidName, "_dat1.RData", sep = ""))
    file.copy(from = From, to = To, overwrite = FALSE)
    Rdata <- To
    file.remove(From)
  
    # Remove directory
    unlink(ZidDir, recursive = TRUE)
    
    # Add Automatic recognition column to Rdata!
    AddIdent(RdataFile = Rdata, Auto = Predictions)
    
    if(isTRUE(Rename)){
      # Rename correctly the Directory wher the zid file have been exported!
      file.rename(from = file.path(dirname(zidfile), dir), to = FinalDirName)
    }
    
    # Message to confirm the end of the treatment
    if(log){
      cat(paste("Vignettes of", ZidName,"have been exported into", basename(FinalDirName), "directory", "\n", sep = " "))
    }
  }
}

# loop to classify vignettes from several zid files in _manuValidation
"classifVign.all" <- function(zidfiles, ZIClass, Dir = "_manuValidation", log = TRUE)
{
  for(i in 1 : length(zidfiles)){
    classifVign(zidfile = zidfiles[i], ZIClass = ZIClass, Dir = Dir, log = log) 
  }
  cat("--- Process Done ---\n")
}

# function to add 'Ident' column to a ZIDat directly in the Rdata file
"AddIdent" <- function(RdataFile, Auto)
{
  if(!is.character(RdataFile)){
    stop("'RdataFile' muste be the path of the Rdata to modify")
  }
  # Load Rdata in memory
  load(file = RdataFile, envir = .GlobalEnv)
  
  # Add the Ident column
  ZI.sample$Ident <- as.factor(Auto)
  
  # Replace existing Rdata
  save(ZI.sample, file = RdataFile)
  
  # Remove Rdata from memory
  rm(ZI.sample, envir = .GlobalEnv)
}


# function to read Manual Validation
"get.ZIMan" <- function(dir, creator = NULL, desc = NULL, keep_ = FALSE, na.rm = FALSE)
{
  # Use get.ZITrain function to read vignette
  ManValidation <- get.ZITrain(dir = dir, creator = creator, desc = desc, keep_ = keep_, na.rm = na.rm)
  
  # Add attributes with names of samples already manually validated
  RDataFiles <- list.files(dir, pattern = "_dat1.RData")
  RDataSamples <- gsub("_dat1.RData", "", RDataFiles)
  attr(ManValidation, "Samples") <- RDataSamples
  
  # Change classes of the object
  class(ManValidation) <- c("ZIMan", class(ManValidation))
  return(ManValidation)
}

# Function to provide groups avfter manual validation
"NewClass" <- function(ZIMan)
{
  # Check arguments
  if(!inherits(ZIMan, "ZIMan"))
    stop("ZIMan must be an object of class 'ZIMan'")
  if(!isTRUE("Class" %in% names(ZIMan)))
    stop("ZIMan doesn't contain a column named 'Class'")
  
  # New identification
  res <- table(ZIMan$Class)
  return(res)
}

# confusion matrix before and after Manual validation
"ZIConf.ZIMan" <- function(ZIMan)
{
  # Chack arguments
  if(!inherits(ZIMan, "ZIMan"))
    stop("ZIMan must be an object of class 'ZIMan'")
  if(!isTRUE("Class" %in% names(ZIMan)))
    stop("ZIMan doesn'y contain a column named 'Class'")
  if(!isTRUE("Ident" %in% names(ZIMan)))
    stop("ZIMan doesn'y contain a column named 'Ident'")
  # Confusion matrix
  res <- table(Class = ZIMan$Class, Predict = ZIMan$Ident)
  return(res)
}

# Difference between prediction
"ZIManCompa" <- function(ZIMan)
{
  # Chack arguments
  if(!inherits(ZIMan, "ZIMan"))
    stop("ZIMan must be an object of class 'ZIMan'")
  if(!isTRUE("Class" %in% names(ZIMan)))
    stop("ZIMan doesn'y contain a column named 'Class'")
  if(!isTRUE("Ident" %in% names(ZIMan)))
    stop("ZIMan doesn'y contain a column named 'Ident'")
  # Difference
  Before <- table(ZIMan$Ident)
  After <- table(ZIMan$Class)
  res <- list(Predicted = Before, Validated = After)
  return(res)  
}

# Tools to subpart a ZIDat table in fuction of one parameter
# Select a parameter in the list of variable
"SelectParam" <- function(ZIDat){
    res <- select.list(names(ZIDat), multiple = FALSE, title = "Parameter to use")
    return(res)
}

# Create a threshold formula
"createThreshold" <- function(ZIDat)
{
    # Select the parameter to use
    Param <- SelectParam(ZIDat = ZIDat)
    # Select the threshold
    Message <- paste("Range:", "From", round(range(ZIDat[, Param])[1], digits = 1),
        "To", round(range(ZIDat[, Param])[2], digits = 1), ";", "Select the threshold:",
        sep = " ")
    Threshold <- zooimage:::dialogString(Message,
        default = paste(Param, "< 50", sep = " "))
        if (is.null(Threshold) || length(Threshold) == 0 || Threshold == "") return(invisible())
    return(Threshold)
}

# Substract a ZIDat table according a threshold formula
"SubpartThreshold" <- function(ZIDat, Filter = NULL){
    
    # Do we use a Filter directly?
    if(is.null(Filter)){
        Threshold <- createThreshold(ZIDat = ZIDat)
    } else {
        if(!is.character(Filter)) stop("Filter must be like 'Parameter < Value'")
        Threshold <- Filter
    }
    # Determine particle responding to the threshold
    SubPart <- within(ZIDat, {
        Index <- eval(parse(text = (Threshold)))
        })
    
    res <- ZIDat[SubPart$Index,]
    attr(res, "Threshold") <- Threshold
    return(res)
}
