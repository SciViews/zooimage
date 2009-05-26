# {{{ Copyright (c) 2004, Ph. Grosjean <phgrosjean@sciviews.org>
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
# }}}

# {{{ prepare.ZITrain
#' Prepare 'dir\subdir' for a manual classification by expanding all vignettes
#' from a given number of zidfiles to the '_' subdir, and making
#' a template for subdirs
"prepare.ZITrain" <- function(dir, subdir = "_train", zidfiles, 
	groups.template = c("[Basic]", "[Detailed]", "[Very detailed]"), 
	ident = NULL, show.log = TRUE, bell = FALSE, start.viewer = FALSE) {
 	                       
	# Make sure unzip is available
	checkCapable( "unzip" )
	
	# First, check that dir is valid
	checkDirExists( dir )
	
	# New dir is dir + subdir
	dir <- file.path(dir, subdir)
	
	checkEmptyDir( dir , message = "must be empty. Clean it first!" )
	
	# Then, check that all zidfiles exist
	checkAllFileExist( zidfiles, "zid" )

	# Finally, look for the groups.template
	groups.template <- groups.template[1]
	rx <- "^[[](.+)[]]$"
	if ( grepl(rx, groups.template) ) {
		# This should be a template file in the default directory
		groups.template <- paste(sub(rx, "\\1", groups.template), ".zic", sep = "")
		groups.template <- file.path(getTemp("ZIetc"), groups.template) 
	}
	
	# check that this is a zic file
	check.zic( groups.template )

	# Do the job...
	cat("Extracting data and vignettes ...\n")
	logProcess("\nExtracting data and vignettes ...")
	zmax <- length(zidfiles)
	
	# C-reate '_' subdir and unzip all vignettes there
	dir_ <- file.path(dir, "_")
	force.dir.create( dir_ )
	
	for (i in 1:zmax) {
		logProcess("data", zidfiles[i])
		Progress(i, zmax)
		# using a temporary directory to unzip all files and then copy
		# the RData files to the train directory
		td <- tempfile()
		unzip( zipfile = zidfiles[i] , path = td, delete.source = FALSE )
		datafiles <- file.path( td, list.files( td, pattern = extensionPattern(".RData"), 
			recursive = TRUE ) )
		if( length(datafiles) ){
			file.copy( datafiles, dir )
		}
		vignettes <- file.path( td, list.files( td, pattern = extensionPattern(".jpg"), 
			recursive = TRUE ) )
		if(length( vignettes) ){
			file.copy( vignettes, dir_ )
		}
		unlink(td, recursive = TRUE )
	}
	ClearProgress()
	
	# Create the other directories
    Lines <- scan(groups.template, character(), sep = "\n", skip = 2, quiet = TRUE)
	if (length(Lines) < 1) {
 		stop(sprintf( "'%s' is empty or corrupted!", groups.template) )
	}
	Lines <- file.path(dir, Lines)
	cat("Making directories...\n")
	logProcess("\nMaking directories...")
	for (i in 1:length(Lines)) {
		logProcess(Lines[i])
		dir.create(Lines[i], recursive = TRUE)
	}
	### TODO: relocate vignettes in subdirectories, if ident is not NULL
	
	finish.loopfunction( ok = TRUE, bell = bell, show.log = show.log, 
	  ok.console.msg = " -- Done! --\n" ,
	  ok.log.msg = "\n-- Done! --" )
	
	if (start.viewer) {
		imageViewer( dir_ )  
	}
	return(invisible(TRUE))
}
# }}}

# {{{ get.ZITrain
#' Retrieve information from a manual training set and store it in a 'ZITrain' object	
"get.ZITrain" <- function(dir, creator = NULL, desc = NULL, keep_ = FALSE, na.rm = FALSE) {

	# 'dir' must be the base directory of the manual classification
	checkDirExists( dir )

	# Make sure we have .RData files in this dir (otherwise it is perhaps not a
    # training set root dir!
    Dats <- list.files(dir, pattern = "_dat1[.]RData$", full.names = TRUE)
	if (length(Dats) == 0){
		stop( "does not appear to be a ", getTemp("ZIname"), " training set root dir!")
	}

	# list the jpg files (recursively) in the dir
    res <- list.files.ext( dir, extension = "jpg", recursive = TRUE )

	# Check the result...
	if (length(res) < 1 ){
		stop("Error while getting data")
	}

	# Replace "\\" by "/"
	res <- gsub("[\\]", "/", res)

	# Do we eliminate the '_' directory?
	if (!keep_) {
		res <- grep( "^[^_]", res, value = TRUE )
	}

	# 'Id' is the name of the vignettes, minus the extension
	Id <- noext( basename(res ) )

	# 'Path' is the directory path
	Path <- dirname(res)

	# 'Class' is the last directory where the files are located
	Class <- basename(Path)

	# Create a directory (a data frame with: Id, Class)
	df <- data.frame(Id = Id, Class = Class)
	df$Id <- as.character(df$Id)
    nitems <- nrow(df)

	# Read in all the .RData files from the root directory and merge them
    ### TODO: also collect metadata and merge them => make a merge function for ZIDat!!!
	# Get measurement infos
    load(Dats[1])
	Dat <- ZI.sample
	Classes <- class(Dat)
	if (length(Dats) > 1){
		for (i in 2:length(Dats)) {
			load(Dats[i])
			Dat <- rbind(Dat, ZI.sample)
		}
	}
	rownames(Dat) <- 1:nrow(Dat)

	# Create the Id column
	Dat <- cbind(Id = make.Id(Dat), Dat)

	# Merge Dat & df by "Id"
	df <- merge(Dat, df, by = "Id")

	# Issue an error if there is no remaing row in the data frame
	if (nrow(df) == 0){
		stop("No valid item found (both with a vignette and with valid measurement data!")
	}

	# Check that all items have associated measurements
	if (nrow(df) < nitems) {
    	nmiss <- nrow(df) - nitems
		warning(nmiss, " vignettes do not have associated measurement data. They are eliminated (", nrow(df), " items remain in the object)")
	}

	# delete lines which contain NA values v1.2-2
	if (any(is.na(df))){
		print ("NA in table of measurements")
		if (na.rm){
  	  		print("NA are deleted from table of measurements")
  	  		df <- na.omit(df)
		}
  	}
	attr(df, "basedir") <- dir
	attr (df, "path") <- sort(unique(Path))
	if (!is.null(creator)) attr(df, "creator") <- creator
	if (!is.null(desc)) attr(df, "desc") <- desc
	Classes <- c("ZI1Train", "ZITrain", Classes)
	class(df) <- Classes
	return(df)
}
# }}}

# {{{ recode.ZITrain
"recode.ZITrain" <- function(ZITrain, ZIRecode, warn.only = FALSE) {
	
	# check classes
	mustbe(ZITrain, "ZITrain")
	mustbe(ZIRecode, "ZIRecode")
	
	# Check that all levels in ZITrain$Class are represented in ZIRecode
	mustmatch( levels(ZITrain$Class), ZIRecode[ , 1], 
		msg = "Not all levels of ZIRecode match levels of ZITrain" )
	
	# Class column of ZITrain is transformed into a character vector
	clas <- as.character(ZITrain$Class)
	recoded <- clas
	
	# It is then recoded
	for (i in 1:nrow(ZIRecode)) {
		if (ZIRecode[i, 1] != ZIRecode[i, 2]){
			recoded[clas == ZIRecode[i, 1]] <- ZIRecode[i, 2]
		}
	}
	
	# ...and transformed back into a factor
	res <- ZITrain
	res$Class <- as.factor(recoded)
	
	# If a new path is given for these new groups, change it
	path <- attr(ZIRecode, "path")
	### TODO: check its validity here
	if(!is.null(path)){
		attr(res, "path") <- path
	}
	return(res)
}
# }}}

# {{{ make.ZIRecode.level
"make.ZIRecode.level" <- function(ZITrain, level = 1) {
	# check class
	mustbe( ZITrain, "ZITrain")
	
	# Get the "path" attribute
	Path <- attr(ZITrain, "path")
	
	# Split strings on "/"
	Path <- strsplit(Path, "/")
	
	# Functions to get last item, or an item at a given level
	Last <- function(x) x[length(x)]
	Level <- function(x, level = 1) ifelse(length(x) >= level, x[level], x[length(x)])
	res <- data.frame(Class = I(sapply(Path, Last)), Recode = I(sapply(Path, Level, level = level)))
	class(res) <- c("ZIRecode", "data.frame")
	attr(res, "call") <- match.call()
	# We do not need to change the path here: it is still the same one
	return(res)
}
# }}}

# {{{ inimplemented functions
"expand.ZITrain" <- function(ZITrain, ZIDdir, destination) {
	### TODO: make directories and extract vignettes for a classification
	stop("Not implemented yet!")
}

"read.ZITrain" <- function(file) {
    ### TODO: read data from a text file
	stop("Not implemented yet!")
}

"write.ZITrain" <- 	function(ZITrain, file) {
    ### TODO: write data to a text file
	stop("Not implemented yet!")
}

"zip.ZITrain" <- function(dir, zipfile, overwrite = FALSE) {
    ### TODO: compress a classification tree
	stop("Not implemented yet!")
}

"unzip.ZITrain" <- 	function(zipfile, dir, overwrite = FALSE) {
    ### TODO: uncompress a classification tree
	stop("Not implemented yet!")
}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

