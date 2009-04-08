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
"prepare.ZITrain" <-
	function(dir, subdir = "_train", zidfiles, groups.template = c("[Basic]", "[Detailed]", "[Very detailed]"), ident = NULL,
		check.unzip = TRUE, show.log = TRUE, bell = FALSE, start.viewer = FALSE) {
 	# Prepare 'dir\subdir' for a manual classification by expanding all vignettes
	# from a given number of zidfiles to the '_' subdir, and making
	# a template for subdirs
	
	# {{{ Make sure unzip is available
	checkCapable( "unzip" )
	# }}}
	
	# First, check that dir is valid
	if (!file.exists(dir) || !file.info(dir)$isdir) {
		logProcess("is not a valid directory!", dir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# New dir is dir + subdir
	dir <- file.path(dir, subdir)
	# Verify that subdir does not exist or that it is empty
	if (file.exists(dir)) {
		if (!file.info(dir)$isdir || length(list.files(dir)) > 0) {
			logProcess("must be empty. Clean it first!", dir, stop = TRUE, show.log = show.log); return(invisible(FALSE))
		} else {
			dir.create(dir)	# Create the subdir, if it does not exists yet
		}
	}
	# Then, check that all zidfiles exist
	if(!all(file.exists(zidfiles)) || !all(regexpr("[.][zZ][iI][dD]$", zidfiles) > 0)) {
		logProcess("One or more .zid files do not exist or is invalid!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Finally, look for the groups.template
	groups.template <- groups.template[1]
	if (regexpr("^[[].+[]]$", groups.template) > 0) {
		# This should be a template file in the default directory
		groups.template <- paste(sub("^[[](.+)[]]$", "\\1", groups.template), ".zic", sep = "")
		groups.template <- file.path(getTemp("ZIetc"), groups.template) 
	}
	# Now this should be a .zic file directly
	if (!file.exists(groups.template)) {
		logProcess("not found!", groups.template, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }	
	# First line of the file must be "ZI1"
	Line1 <- scan(groups.template, character(), nmax = 1, quiet = TRUE)
	if (Line1 != "ZI1") {
		logProcess("not a ZooImage1 file, or corrupted!", groups.template, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }	
	# Second line must be [path]
	Line2 <- scan(groups.template, character(), skip = 1, nmax = 1, quiet = TRUE)
	if (tolower(Line2) != "[path]") {
		logProcess("not a ZooImage1 .zic file, or corrupted!", groups.template, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }	
	# Do the job...
	cat("Extracting data...\n")
	logProcess("\nExtracting data...")
	zmax <- length(zidfiles)
	for (i in 1:zmax) {
		logProcess("data", zidfiles[i])
		Progress(i, zmax)
		# Unzip data (*.RData files) there
        cmd <- paste('"', ZIpgm("unzip", "misc"), '" -jqq "', zidfiles[i], '" *.RData -d "', dir, '"', sep = "")
		system(cmd, show.output.on.console = TRUE, invisible = TRUE)
	}
	Progress(i + 1, zmax)	# To dismiss the Progress() indication
	# Create '_' subdir and unzip all vignettes there
	dir_ <- file.path(dir, "_")
	if (!dir.create(dir_)) {
		logProcess("error creating subdir '_'!", dir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Do the job...
	cat("Extracting vignettes...\n")
	logProcess("\nExtracting vignettes...")
	zmax <- length(zidfiles)
	for (i in 1:zmax) {
		logProcess("vignettes", zidfiles[i])
		Progress(i, zmax)
		# Unzip vignettes (*.jpg files) there
		cmd <- paste('"', ZIpgm("unzip", "misc"), '" -jqq "', zidfiles[i], '" *.jpg -d "', dir_, '"', sep = "")
		system(cmd, show.output.on.console = TRUE, invisible = TRUE)
	}
	Progress(i + 1, zmax)	# To dismiss the Progress() indication
	# Create the other directories
    Lines <- scan(groups.template, character(), sep = "\n", skip = 2, quiet = TRUE)
	if (length(Lines) < 1) {
 		logProcess("is empty or corrupted!", groups.template, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
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
	  startPgm("ImageViewer", cmdline = paste('"', dir_, '"', sep = ""))
	}
	return(invisible(TRUE))
}
# }}}

# {{{ get.ZITrain
"get.ZITrain" <- function(dir, creator = NULL, desc = NULL, keep_ = FALSE, na.rm = TRUE) {
	# Retrieve information from a manual training set and store it in a 'ZITrain' object
	# 'dir' must be the base directory of the manual classification
	if (!file.exists(dir) || !file.info(dir)$isdir)
		stop("'dir' is not a valid directory!")
	# Make sure we have .RData files in this dir (otherwise it is perhaps not a
    # training set root dir!
    Dats <- list.files(dir, pattern = "_dat1[.]RData$", full.names = TRUE)
	if (length(Dats) == 0)
		stop("'", dir, "' does not appear to be a ", getTemp("ZIname"), " training set root dir!") 
    # Get a list of subdirs vith vignettes
	if (isWin()) {
		# Make sure the directory is written with "\\", not "/"
		Dir <- gsub("[/]", "\\\\", dir)
		if (length(grep("[\\]$", Dir)) == 0) Dir <- paste(Dir, "\\", sep = "")
		cmd <- paste(Sys.getenv("COMSPEC"), " /c dir \"", Dir, "*.jpg\" /B /S", sep = "")
		res <- system(cmd, intern = TRUE, invisible = TRUE)
	} else {
		### TODO: adapt this to other platforms
		stop("This function is not implemented yet on this platform!")
	}
	# Check the result...
	if (length(res) < 1 || length(grep(Dir, res[1], fixed = TRUE)) == 0)
		stop("Error while getting data from ", dir)
	# Eliminate the root directory
	res <- substring(res, nchar(Dir) + 1)
	# Replace "\\" by "/"
	res <- gsub("[\\]", "/", res)
	# Do we eliminate the '_' directory?
	if (!keep_) {
	del <- -grep("^_", res)
	if (length(del) > 0) res <- res[del]
	}
	# 'Id' is the name of the vignettes, minus the extension
	Id <- sub("[.]jpg$", "", basename(res))
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
	if (length(Dats) > 1)
		for (i in 2:length(Dats)) {
			load(Dats[i])
			Dat <- rbind(Dat, ZI.sample)
		}
	rownames(Dat) <- 1:nrow(Dat)
	# Create the Id column
	Dat <- cbind(Id = make.Id(Dat), Dat)
	# Merge Dat & df by "Id"
	df <- merge(Dat, df, by = "Id")
	# Issue an error if there is no remaing row in the data frame
	if (nrow(df) == 0)
		stop("No valid item found (both with a vignette and with valid measurement data!")
	# Check that all items have associated measurements
	if (nrow(df) < nitems) {
    	nmiss <- nrow(df) - nitems
		warning(nmiss, " vignettes do not have associated measurement data. They are eliminated (", nrow(df), " items remain in the object)")
	}
	# delete lines which contain NA values v1.2-2
	if (any(is.na(df))){
  warning ("NA in table of measurements")
  if (na.rm){
    warning("NA are deleted from table of measurements") 
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
	if (!inherits(ZITrain, "ZITrain"))
		stop("ZITrain must be an object of class 'ZITrain'")
	if (!inherits(ZIRecode, "ZIRecode"))
		stop("ZIRecode must be an object of class 'ZIRecode'")
	# Check that all levels in ZITrain$Class are represented in ZIRecode
	if (!all(sort(levels(ZITrain$Class)) == sort(ZIRecode[ , 1]))) {
		if (warn.only) {
			warning("Not all levels of ZIRecode match levels of ZITrain")
		} else {
			stop("Not all levels of ZIRecode match levels of ZITrain")
		}
	}
	# Class column of ZITrain is transformed into a character vector
	clas <- as.character(ZITrain$Class)
	recoded <- clas
	# It is then recoded
	for (i in 1:nrow(ZIRecode)) {
		if (ZIRecode[i, 1] != ZIRecode[i, 2])
			recoded[clas == ZIRecode[i, 1]] <- ZIRecode[i, 2]
	}
	# ...and transformed back into a factor
	res <- ZITrain
	res$Class <- as.factor(recoded)
	# If a new path is given for these new groups, change it
	path <- attr(ZIRecode, "path")
	### TODO: check its validity here
	if(!is.null(path)) attr(res, "path") <- path
	return(res)
}
# }}}

# {{{ make.ZIRecode.level
"make.ZIRecode.level" <- function(ZITrain, level = 1) {
	if (!inherits(ZITrain, "ZITrain"))
		stop("ZITrain must be an object of class 'ZITrain'")
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

