#{{{ Copyright (c) 2004, Ph. Grosjean <phgrosjean@sciviews.org>
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

# {{{ verify.zid
#' check consistancy of a zoo image directory
#' 
#' @param zidir the directory to check
#' @param type must be ZI1 
#' @param check.vignettes do we check vignettes as well
#' @param show.log do we show a log at the end
verify.zid <- function(zidir, type = "ZI1", check.vignettes = TRUE, show.log = TRUE) {
	
	# heck the format of the file
	# This should be a directory containing XXX+YY_dat1.zim files + .jpg files
	if (type != "ZI1") {
		stop("only 'ZI1' is currently supported for 'type'!")
	}
	
	# check the list of _dat1.zim
	dat1files <- list.dat1.zim(zidir)
	if(length(dat1files) == 0) {
		stop( "no '_dat1.zim' file!" )
	}
	
    # Check the content of all these "_dat1.zim" files 
	#     and retrieve the number of items measured
	dat1files <- sort(dat1files)
	# Default to -1 for corrupted dat1 files
	nitems <- sapply( dat1files, function( x ){
		tryCatch( 
			verify.zim( file.path( zidir, x), is.dat1 = TRUE ), 
			zooImageError = function(e ){
				logError( e )
				-1
			} )
	} )
	ok <- all( nitems != -1)
	
	# {{{ check the vignettes
	if (check.vignettes) {
		
        # Check that we have corresponding vignettes (XXX+YY_ZZZ.jpg files)
    	samples <- sub("_dat1[.]zim$", "", dat1files)
		
    	# {{{ Check the content of the directory for .jpg files
    	for (i in 1:length(samples)) {
			
			# {{{ list the jpegs
    		regex <- gsub("[+]", "[+]", samples[i])
    		regex <- gsub("[.]", "[.]", regex)
    		regex2 <-  paste("^", regex, "_[0-9]+[.]jpg$", sep = "")
    		jpgs <- dir(zidir, pattern = regex2)
			# }}}
			
    		# {{{ Get their numbers, sort them, and make sure none are missing
    		n <- nitems[i]
    		if (n < 1) {
				# If impossible to know how many items, just count vignettes
				n <- length(jpgs)	
			}
    		
			# Construct a vector with names of vignettes as they should be
    		chkjpgs <- paste(samples[i], "_", 1:n, ".jpg", sep = "")
    		if (length(jpgs) == 0 && length(chkjpgs) > 0) {
				warning( paste(" no vignettes for", samples[i]) )
				ok <- FALSE
            } else if (length(chkjpgs) != length(jpgs) || !all(sort(chkjpgs) == sort(jpgs))) {
				warning( paste(" mismatch vignettes for", samples[i]) )
				ok <- FALSE 
			}
            
			# }}}
        }
		# }}}
    }
	# }}} 
	
	# {{{ Report results
	if (ok){
		logProcess("OK", zidir, show.log = show.log)
	} else{
		logProcess("Errors found", zidir, show.log = show.log)
	}
	# }}}
	
	# TODO: we should not return ok here ?
	return(invisible(ok))
	
}
# }}}

# {{{ verify.zid.all
"verify.zid.all" <- function(path = ".", samples = NULL, type = "ZI1", 
    check.vignettes = TRUE, show.log = TRUE, bell = FALSE) {
	
	# {{{ Verify all of these directories
	if (type != "ZI1") {
		stop("only 'ZI1' is currently supported for 'type'!")
	}
	# }}}
	
	# {{{ First, switch to that directory
	inidir <- getwd()
	checkDirExists( path )
	setwd(path); on.exit(setwd(inidir))
	path <- "."	# Indicate we are now in the right path
	# }}}
	
	# {{{ Process the list of samples
	if (is.null(samples)) {	# Compute them from path
		d <- dir(path, pattern = "^[^_]")	# All items not starting with '_'
		samples <- unique(d[file.info(d)$isdir])	# Keep only directories
	}
	# }}}
	
	# {{{ If there is no dir, exit now
	if (is.null(samples) || length(samples) == 0){
		stop("There is no directories to verify in ", getwd())
	}
	# }}}
	
	# {{{ Start the process
	smax <- length(samples)
	cat("Verification...\n")
	
	ok <- sapply( samples, function(s){
		tryCatch( verify.zid( s, type = type, check.vignettes = check.vignettes, show.log = FALSE), 
			zooImageError = function(e) -1 )
	} )
	# }}}
	
	# {{{ cleans up
	finish.loopfunction( all( ok ) , bell = bell, show.log = show.log )
	# }}}
}
# }}}

# {{{ make.RData
#' Make a .RData file that collates together data from all the "_dat1.zim" files of a given sample
"make.RData" <- function(zidir, type = "ZI1", replace = FALSE, show.log = TRUE) {

	if (type != "ZI1") {
		stop("only 'ZI1' is currently supported for 'type'!")
	}
	RDataFile <- file.path(zidir, paste(basename(zidir), "_dat1.RData", sep = ""))

	# File already exists
	if (file.exists(RDataFile) && !replace){
		return(invisible(TRUE))
	}

	ok <- TRUE
	dat1files <- list.dat1.zim(zidir)
	if(length(dat1files) == 0) {
		stop("no '_dat1.zim' file!" )
	}
	dat1files <- sort(dat1files)
	fractions <- get.sampleinfo(dat1files, "fraction")

	# Avoid collecting duplicate informations about fractions
	fracdup <- duplicated(fractions)
	results <- lapply( seq.int( 1, length(dat1files) ), function(i){

		dat1path <- file.path(zidir, dat1files[i])
		iszim <- tryCatch( is.zim( dat1path ), zooImageError = function(e){
			logError( e )
			FALSE
		} )
		if( !iszim) return(NULL)

		# Read the header
		Lines <- scan(dat1path, character(), sep = "\t",
			skip = 1, blank.lines.skip = FALSE,
			flush = TRUE, quiet = TRUE, comment.char = "#")
		if (length(Lines) < 1) {
			logProcess("is empty, or is corrupted", dat1files[i]);
			return( NULL )
		}

		# Trim leading and trailing spaces in Lines
		Lines <- trim(Lines)

		# Convert underscore to space
		Lines <- underscore2space(Lines)

		# Determine where the table of measurements starts (it is '[Data]' header)
		endhead <- tail( which( Lines == "[Data]" ), 1)
		if (!is.null(endhead)) {
			Lines <- if (endhead > 1) {
				Lines[ seq.int( 1, endhead - 1) ]
			}
		}

		# Decrypt all lines, that is, split on first occurrence of "=" into 'tag', 'value'
		# and separate into sections
		meta <- if (!fracdup[i] && !is.null(Lines)) {
			parse.ini(Lines, sub("_dat1[.]zim$", "", fractions[i]))
		}

		# Read the table of measurements
		if (!is.null(endhead)) {
			mes <- read.table(dat1path, header = TRUE, sep = "\t",
				dec = ".", as.is = FALSE, skip = endhead + 1,
				comment.char = "#", na.strings = "null")

			# We have several problems here:
			# 1) There is sometimes a column full of NAs at the end.
			#    This is because ImageJ adds an extra tab at the end of the line.

			# [RF] FIXME: this should not be the case anymore because we have more control
			#        of what ImageJ is doing
			if (all(is.na(mes[ , ncol(mes)]))){
				mes <- mes[ , -ncol(mes)]
			}

			# 2) The first column is the 'Item', but i         ts name '!Item' is transformed into 'X.Item'
			# 3) The '%Area' is transformed into 'X.Area'
			Names <- names(mes)
			if (Names[1] == "X.Item") {
				Names[1] <- "Item"
			}
			if ("X.Area" %in% Names){
				Names[Names == "X.Area"] <- "PArea"
			}
			# Invert 'Item' and 'Label'
			mes <- mes[ , c(2, 1, 3:ncol(mes))]
			Names <- Names[c(2, 1, 3:length(Names))]
			names(mes) <- make.names(Names, unique = TRUE)

			Sub     <- meta$Subsample
			Sub$Dil <- 1 / (Sub$SubPart * Sub$CellPart * Sub$Replicates * Sub$VolIni)
			mes$Dil <- rep( Sub$Dil[ Sub$Label == fractions[i] ] , nrow(mes) )

		} else{
			mes <- NULL
		}

		list( meta = meta, mes = mes )
	} )

	notnull.filter <- Negate(is.null)
	results        <- Filter( notnull.filter , results )
	list.allmeta 	<- Filter( notnull.filter, lapply( results, "[[", "meta" ) )
	list.allmes  	<- Filter( notnull.filter, lapply( results, "[[", "mes" ) )
	allmeta 		<- combine( .list = list.allmeta )
	allmes  		<- combine( .list = list.allmes  )
	rownames(allmes) <- 1:nrow(allmes)

	# Calculate an ECD from Area if there is not one yet
	Names <- names(allmes)
	if (!"ECD" %in% Names && "Area" %in% Names) {
		ECD <- ecd(allmes$Area)
		# Place ECD in third position (should be just after 'Label' and 'Item')
		allmes <- data.frame(allmes[, 1:2], "ECD" = ECD,
			allmes[, 3:ncol(allmes)] )
	}

	# Construct a c('ZI1Dat', 'ZIDat', 'data.frame') object containing the data frame
	# and the metadata as attribute
	attr(allmes, "metadata") <- allmeta
	class(allmes) <- c("ZI1Dat", "ZIDat", "data.frame")

	# Save these data in a file
	ZI.sample <- allmes
	save(ZI.sample, file = RDataFile, ascii = FALSE, version = 2, compress = TRUE)
	if (ok){
		ok <- file.exists(RDataFile)
	}
	if (show.log){
		logView()
	}
	return(invisible(ok))
}
# }}} 

# {{{ compress.zid
# Compress one sample as a single .zid zipped file	
"compress.zid" <- function(zidir, type = "ZI1", check = TRUE, 
	check.vignettes = TRUE,
    replace = FALSE, delete.source = replace,
	check.zip = TRUE, show.log = TRUE) {
		
	# check the format
	if (type != "ZI1") {
		stop("only 'ZI1' is currently supported for 'type'!")
	}
	
	# We need to switch to the root of sample dir first for correct path in the zip file
	rootdir <- dirname(zidir)
	inidir <- getwd(); setwd(rootdir); on.exit(setwd(inidir))
	zidir <- basename(zidir) # Use only the latest dir (the "sample dir")
	
	# The .zid file is located in the "root" dir, same name as the "sample dir", with .zid extension
	zidfile <- paste(zidir, ".zid", sep = "")
	if (!replace && file.exists(zidfile)) {
		# It is not advised to delete source without rebuilding the .zid file
		# but it was expressly asked!
		### TODO: verify we have the same files in the .zid and initial dir before deleting files!
		if (delete.source && file.exists(zidir)){
			unlink(zidir, recursive = TRUE)
		}
		return(invisible(TRUE))	# Nothing else to do
	}
	
	# Make sure everything is fine for this directory
	if (check) {
		verify.zid(zidir, type = type, check.vignettes = check.vignettes, show.log = FALSE)
	}
	
	# Make sure the .RData file is created (or refreshed)
	make.RData(zidir, type = type, replace = replace, show.log = FALSE)
	
	# Do compress the directory in the .zip file
	# Copy or move all corresponding files to a .zid zip-compressed file
	invisible( zip( zidfile, zidir, delete.source = delete.source ) )
	
}
# }}}

# {{{ compress.zid.all
# Compress all data in the corresponding directory
"compress.zid.all" <- function(path = ".", samples = NULL, type = "ZI1", check = TRUE,
	check.vignettes = TRUE, replace = FALSE, delete.source = replace,
    show.log = TRUE, bell = FALSE) {
	
	# {{{ check the type 
	#     COMMENT: do we reallyt need to do that since it will be done also 
	#              in the compress.zid function ?
	if (type != "ZI1"){
		stop("only 'ZI1' is currently supported for 'type'!")
	}
	# }}}
	
	# {{{ First, switch to that directory                                       
	inidir <- getwd()
	checkDirExists( path )
	setwd(path)
	on.exit(setwd(inidir))
	path <- "."	# Indicate we are now in the right path
	# }}}
	
	# {{{ Get the list of samples to process
	if (is.null(samples)) {	# Compute them from path
		d <- dir(path, pattern = "^[^_]")	# All items not starting with '_'
		samples <- unique(d[file.info(d)$isdir])	# Keep only directories
	}
	# }}}
	
	# {{{ If there is no dir, exit now
	if (is.null(samples) || length(samples) == 0){
		stop("There is no directories to process in ", getwd())
	}
	# }}}
		
	# {{{ Start the process
	logClear()
	if (check) {
		logProcess("Verification...")
		verify.zid.all(path = path, samples = samples, 
			check.vignettes = check.vignettes, 
			show.log = FALSE, bell = FALSE)
		# COMMENT: the previous version did log this message instead of the one
		# that is generated by verify.zid.all
		# "contains corrupted files, compression not started!"
	}
	# }}}
		
	# {{{ Compress these files
	smax <- length(samples)
	cat("Compression...\n")
	logProcess("\nCompression...")
	ok <- TRUE
	for (s in 1:smax) {
		# Progress should be taken out of here since it is not really related 
		# to the function's job, instead we could throw a condition 
		# from compress.zid when it starts to indicates it has started
		Progress(s, smax)  
		
		tryCatch({  
			compress.zid(samples[s], type = type, check = FALSE, 
				check.vignettes = check.vignettes, 
				replace = replace, delete.source = delete.source, 
				check.zip = FALSE, show.log = FALSE)
			}, zooImageError = function( e ){
				logError( e )
				ok <<- FALSE
			}, zooImageWarning = function(w){
				logWarning( e )
				ok <<- FALSE
			})
	}
	ClearProgress()
	# }}}
	
	# {{{ Possibly clean the whole directory (move .zim files to \_raw
	#     and delete the \_work subdir if everything is fine
	if (ok){
		clean.after.zid(path = path, samples = samples)
	}
    # }}}
	
	# {{{ clean up
	finish.loopfunction( ok = ok, bell = bell, show.log = show.log )
	# }}}
	
}
# }}}

# {{{ clean.after.zid
"clean.after.zid" <- function(path = ".", samples = NULL) {
    
	# {{{ Do we have samples to process
    if (length(samples) == 0){
		return()
	}
	# }}}
	
    # {{{ First, switch to that directory
	inidir <- getwd()
    checkDirExists( path )
	setwd(path)
	on.exit(setwd(inidir))
	# }}}
	
	# {{{ Logging
	cat("Cleaning directory...\n")
	logProcess("\nCleaning directory...")
	# }}}
    
	# {{{ identify paths
	zimfiles   <- list.zim( "." )
	zimsamples <- sub("^(.*)[+].+", "\\1", zimfiles)
	# }}}
	
    # {{{ Keep only those .zim files related to samples
    zimfiles <- zimfiles[zimsamples %in% samples]
	# }}}
	
	# {{{ process
    if (length(zimfiles) > 0) {
        rawdir <- file.path(".", "_raw")
        
		# If the _raw subdirectory does not exists, create it
        if (!file.exists(rawdir)){
			dir.create(rawdir)
		}
		copyto <- file.path(".", "_raw", zimfiles)
        
		# Move these .zim files
        for (i in 1:length(zimfiles)){
			file.rename(zimfiles[i], copyto[i])
		}
		
    }
	# }}}
	
    # {{{ Delete completely the _work subdirectory
    unlink(file.path(".", "_work"), recursive = TRUE)
	# }}}    
}
# }}}

# {{{ uncompress.zid
#' Uncompress a .zid file to get all its content. 
#' Use 'delete.source = TRUE' with caution!
"uncompress.zid" <- function(zidfile, path = dirname(zidfile), delete.source = FALSE, show.log = TRUE) {
	
	# Check if the file provided is a .zid file, and if it exists
	checkFileExists( zidfile, extension = "zid" )
	
	# Uncompress it
	unzip( zidfile, path, delete.source = delete.source )
	
}
# }}}

# {{{ uncompress.zid.all
# Uncompress all .zid files in the 'path.extract' directory
"uncompress.zid.all" <- function(path = ".", zidfiles = list.zid(path),
	path.extract = path, skip.existing.dirs = TRUE, 
	delete.source = FALSE, show.log = TRUE, bell = FALSE) {
	
	# initial checks
	if (is.null(zidfiles) || length(zidfiles) == 0) {
        stop("no .zid files found!" )
	}
	
	# start the process
	logClear()
	ok <- TRUE

	# Check that dirs / files with corresponding names exist in path.extract
	checkdirs  <- file.path(path.extract, noext(zidfiles))
	fileExists <- file.exists(checkdirs) & !file.info(checkdirs)$isdir
	dirExists  <- file.exists(checkdirs) & file.info(checkdirs)$isdir
	
	# If any file not being a dir exist there, stop the process
	if ( any(fileExists) ) {
        stop("one or several files have same name as uncompressed dirs!" )
	}
	
	# Should we eliminate files whose corresponding dirs exist?
	if (skip.existing.dirs && any(dirExists)) {
		cat(sum(dirExists), "file(s) already uncompressed is/are skipped!\n")
        warning( paste( "Skipping already uncompressed file(s):", 
						  paste( zidfiles[dirExists], collapse = ",") )  )
	}
	zidfiles <- zidfiles[!dirExists]
	
	# Decompress the files remaining in the list
	smax <- length(zidfiles)
	if (smax == 0) {
		cat("-- Done! - (nothing to decompress)\n")
		stop("\n -- Done! -- (nothing to decompress)", show.log = show.log)
	}
	
	# Start decompression
	cat("Decompression...\n")
	logProcess("\nDecompression...")
	for (s in 1:smax) {
		Progress(s, smax) 
		uncompress.zid(zidfiles[s], path = path.extract,
			delete.source = delete.source, show.log = FALSE)
	}
	ClearProgress()
	finish.loopfunction( TRUE, bell = bell, show.log = show.log )
}
# }}}

# {{{ zid.extract
"zid.extract" <- function(file, zidfile) {
	tmpd <- tempdir()
	unzip( zidfile, tmpd )
	file.path( tmpd, file )
}
# }}}

# {{{ read.zid
#' Read the .Rdata in a .zid file or corresponding directory
"read.zid" <- function(zidfile) {
	
	# {{{ identify the file and stop if it does not exists
	sample      <- noext(basename(zidfile))
	RdataFile   <- paste(sample, "_dat1.RData", sep ="")
	deletefile  <- FALSE
	checkFileExists( zidfile, message = "%s not found!" )
	# }}}
	
	# {{{ treat different kind of files
	if( !hasExtension( zidfile, "zid" ) ){
		# {{{ Is it a directory?
		if (file.info(zidfile)$isdir) {
			#  Is there a .RData file in this directory?
			rdata <- file.path(zidfile, RdataFile)
			if (!file.exists(rdata)) {
				# Try to create it
				make.RData(zidfile, show.log = FALSE)
				checkFileExists( rdata, message = "Error creating the .RData file")
			}
		} else stop( sprintf( "Unrecognized file: %s", zidfile) )
		# }}}
		
	} else { 
		# {{{ This is a .zid file
		rdata <- file.path(sample, RdataFile)
		rdata <- zid.extract(rdata, zidfile)
		if (rdata == ""){
			stop( sprintf( "Error reading .RData file from %s", zidfile) )
		}
		deletefile <- TRUE
		# }}}
	}
	# }}}
	
	# {{{ Load that file
	ZI.sample <- NULL
	load(rdata)
	# }}}
	
	# {{{ delete the file
	if (deletefile) {
		unlink(rdata)
		# If the directory is empty, delete it also
		datadir <- file.path(tempdir(), sample)
		if (file.exists(datadir) && (length(dir(datadir)) == 0))
			unlink(datadir)
	}
	# }}}
	
	# {{{ set the class 
	if (!inherits(ZI.sample, "ZIDat") && inherits(ZI.sample, "data.frame")){
		class(ZI.sample) <- c("ZI1Dat", "ZIDat", "data.frame")
	}
	# }}}
	
	return(ZI.sample)
}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

# Functions to classify vignettes according to automatic classification
classifVign <- function(zidfile, Zic, log = TRUE){
  Zid <- read.zid(zidfile)
  # Recognition of the zid file
  Rec <- predict(Zic, Zid)
  Gp <- unique(Rec$Ident)
  # Create path for new directories
  if(!is.null(attr(Zic, "path"))){
    # There is a 'path' attribute associated with the classifer
    GpDir <- file.path(dirname(zidfile), "Auto_Classification", attr(Zic, "path"))
  } else {
    # only create classifier without taxonomic relationship
    GpDir <- file.path(dirname(zidfile), "Auto_Classification", unique(Rec$Ident))
  }
  # Create directories for new groups on harddisk
  for(i in 1 : length(GpDir)){
    if(!file.exists(GpDir)[i]){
      dir.create(GpDir[i], showWarnings = TRUE, recursive = TRUE)
    }
  }
  # Extract Vignettes
  uncompress.zid(zidfile)
  # Copy vignettes from zidfile directory to group directories
  Rec$Vign <- make.Id(Rec)
  for(i in 1:nrow(Rec)){
     From <- file.path(file.path(dirname(zidfile), noext(zidfile)), paste(Rec$Vign[i], "jpg", sep = "."))
     To <- file.path(GpDir[grep(as.character(Rec$Ident[i]), GpDir)], paste(Rec$Vign[i], "jpg", sep = "."))
     file.copy(from = From, to = To, overwrite = FALSE)
     file.remove(From)
  }
  # Remove directory
  unlink(file.path(dirname(zidfile), noext(zidfile)), recursive = TRUE)
  if(log){
    print("Your vignettes have been exported into groups in the zid directory")
  }
}

classifVign.all <- function(zidfiles, Zic, log = TRUE){
  for(i in 1 : length(zidfiles)){
    classifVign(zidfile = zidfiles[i], Zic = Zic, log = FALSE)  
    if(log){
      print(paste("zid file", i, "has been extracted" , sep = " "))
    }
  }
  print(paste("Your", length(zidfiles), "zid files", "have been exported into groups in the zid directory" , sep = " "))
}
