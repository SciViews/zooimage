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
	
	# {{{ using the catcher mechanism
	if( is.null( getCatcher() ) ){
		return( catch(match.call()) ) 
	}
	# }}}

	# {{{ check the format of the file
	# This should be a directory containing XXX+YY_dat1.zim files + .jpg files
	if (type != "ZI1") {
	  stop("only 'ZI1' is currently supported for 'type'!")
	}
	# }}}
	
	# {{{ check the list of _dat1.zim
	dat1files <- list.dat1.zim(zidir)
	if(length(dat1files) == 0) {
		stop( sprintf( "%s - has no '_dat1.zim' file!", zidir) )
	}
	if (length(dat1files) == 1 && is.na(dat1files)) {
	 	stop( sprintf( "%s - not found or not a directory!", zidir ) )
	}
	# }}}
	
    # {{{ Check the content of all these "_dat1.zim" files 
	#     and retrieve the number of items measured
	dat1files <- sort(dat1files)
	# Default to -1 for corrupted dat1 files
	nitems <- rep( -1, length(dat1files) )	 
	for (i in 1:length(dat1files)) {
		
		# this might generate an error, which is dealt with
		# in the catcher associated with this function
		res <- verify.zim(file.path(zidir, dat1files[i]), is.dat1 = TRUE)
		if( length( res ) ){
			nitems[i] <- res
		}
		
	}
	ok <- any( nitems == -1 )
	# }}}
	
	# {{{ check the vignettes
	if (check.vignettes) {
		
        # {{{ Check that we have corresponding vignettes (XXX+YY_ZZZ.jpg files)
    	samples <- sub("_dat1[.]zim$", "", dat1files)
		# }}}
		
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
	}
	# }}}
	
	# TODO: we should not return ok here ?
	return(invisible(ok))
	
}
attr( verify.zid, "catcher" ) <- function( call ){
	
	withCallingHandlers( eval( call ), 
		"zooImageError_verify.zim" = function( e ){
			# we get an error from verify.zim, we want to log the error
			# but keep going
			logError( e )			
		} )
	
}
# }}}

# {{{ verify.zid.all
"verify.zid.all" <- function(path = ".", samples = NULL, type = "ZI1", 
    check.vignettes = TRUE, show.log = TRUE, bell = FALSE) {
	
	# {{{ Verify all of these directories
	if (type != "ZI1") stop("only 'ZI1' is currently supported for 'type'!")
	# }}}
	
	# {{{ First, switch to that directory
	inidir <- getwd()
	if (!file.exists(path) || !file.info(path)$isdir)
		stop(path, " does not exist, or it is not a directory!")
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
	if (is.null(samples) || length(samples) == 0)
		stop("There is no directories to verify in ", getwd())
	# }}}
	
	# {{{ Start the process
	ok <- TRUE
	smax <- length(samples)
	cat("Verification...\n")
	for (s in 1:smax) {
		Progress(s, smax)
		withRestarts( withCallingHandlers( {
			verify.zid(samples[s], type = type, check.vignettes = check.vignettes, show.log = FALSE)
		} , zooImageError = function( e ){   # calling handler
			logError( e )               
			                                 #       about the sample being analysed
			invokeRestart( "zooImageError" ) # go to the restart below
		} ), zooImageError = function(e){    # restart
			ok <<- FALSE # should this be depreciated ?
		})  
	}
	# }}}
	
	# {{{ Dismiss the progress 
	Progress (smax + 1, smax) # To dismiss the Progress() indication  
	# }}}
	
	# {{{ cleans up
	finish.loopfunction( ok , bell = bell, show.log = show.log )
	# }}}
}
# }}}

# {{{ make.RData
"make.RData" <-
	function(zidir, type = "ZI1", replace = FALSE, show.log = TRUE) {
	# Make a .RData file that collates together data from all the "_dat1.zim" files of a given sample
	if (type != "ZI1") stop("only 'ZI1' is currently supported for 'type'!")
	RDataFile <- file.path(zidir, paste(basename(zidir), "_dat1.RData", sep = ""))
	if (file.exists(RDataFile) && !replace) return(invisible(TRUE)) # File already exists
	ok <- TRUE
	dat1files <- list.dat1.zim(zidir)
	if(length(dat1files) == 0) {
		logProcess("has no '_dat1.zim' file!", zidir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	if (length(dat1files) == 1 && is.na(dat1files)) {
	 	logProcess("not found or not a directory!", zidir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	dat1files <- sort(dat1files)
	fractions <- get.sampleinfo(dat1files, "fraction")
	# Avoid collecting duplicate informations about fractions
	fracdup <- duplicated(fractions)
	# For each of these files, read content in a variable
	allmes <- NULL
	allmeta <- NULL
	for (i in 1:length(dat1files)) {
		dat1path <- file.path(zidir, dat1files[i])
		
		# TODO; this might generate an error, handle it
		is.zim( dat1path )
		
		# Read the header
		Lines <- scan(dat1path, character(), sep = "\t", skip = 1, blank.lines.skip = FALSE, flush = TRUE, quiet = TRUE, comment.char = "#")
		if (length(Lines) < 1) {
			logProcess("is empty, or is corrupted", dat1files[i]); ok <- FALSE; next }
		# Trim leading and trailing spaces in Lines
		Lines <- trim(Lines)
        # Convert underscore to space
		Lines <- underscore2space(Lines)
		# Determine where the table of measurements starts (it is '[Data]' header)
		endhead <- (1:length(Lines))[Lines == "[Data]"]
		if (length(endhead) == 0) endhead <- NULL else endhead <- endhead[length(endhead)]
		if (!is.null(endhead)) {
			if (endhead > 1) {
				Lines <- Lines[1:(endhead - 1)]
			} else Lines <- NULL
		}
		# Decrypt all lines, that is, split on first occurrence of "=" into 'tag', 'value'
		# and separate into sections
		if (!fracdup[i] && !is.null(Lines)) {
			meta <- parse.ini(Lines, sub("_dat1[.]zim$", "", fractions[i]))
			## Collate all metadata together
			if (i == 1) allmeta <- meta else {
				# Merge metadata
				allmeta <- list.merge(allmeta, meta)
			}
		}
		# Calculate a data frame containing 'dilutions'
		Sub <- allmeta$Subsample
		Sub$Dil <- 1 / (Sub$SubPart * Sub$CellPart * Sub$Replicates * Sub$VolIni)
		# Read the table of measurements
		if (!is.null(endhead)) {
			mes <- read.table(dat1path, header = TRUE, sep = "\t", dec = ".",
				as.is = FALSE, skip = endhead + 1, comment.char = "#")
			# We have several problems here:
			# 1) There is sometimes a column full of NAs at the end.
			#    This is because ImageJ adds an extra tab at the end of the line.
			if (all(is.na(mes[ , ncol(mes)]))) mes <- mes[ , -ncol(mes)]
			# 2) The first column is the 'Item', but i         ts name '!Item' is transformed into 'X.Item'
			# 3) The '%Area' is transformed into 'X.Area'
			Names <- names(mes)
			if (Names[1] == "X.Item") Names[1] <- "Item"
			if ("X.Area" %in% Names) Names[Names == "X.Area"] <- "PArea"
			# Invert 'Item' and 'Label'
			mes <- mes[ , c(2, 1, 3:ncol(mes))]
			Names <- Names[c(2, 1, 3:length(Names))]
			names(mes) <- make.names(Names, unique = TRUE)
			# Add a Dil column at the end with the corresponding dilution
			Dil <- Sub$Dil[Sub$Label == fractions[i]]
			Dil <- rep(Dil, nrow(mes))
			mes <- cbind(mes, Dil)
			# Collate data all together
			if (i == 1) allmes <- mes else {
				if (all(names(allmes) == names(mes))) {
					allmes <- rbind(allmes, mes)	# Faster
				} else {
					allmes <- merge(allmes, mes, all = TRUE)
				}
			}
		}
	}
	rownames(allmes) <- 1:nrow(allmes)
	# Calculate an ECD from Area if there is not one yet
	Names <- names(allmes)
	if (!"ECD" %in% Names && "Area" %in% Names) {
		ECD <- ecd(allmes$Area)
		# Place ECD in third position (should be just after 'Label' and 'Item')
		allmes <- cbind(allmes[, 1:2], ECD, allmes[, 3:ncol(allmes)])
	}
	# Construct a c('ZI1Dat', 'ZIDat', 'data.frame') object containing the data frame
	# and the metadata as attribute
	attr(allmes, "metadata") <- allmeta
	class(allmes) <- c("ZI1Dat", "ZIDat", "data.frame")
	# Save these data in a file
	ZI.sample <- allmes
	save(ZI.sample, file = RDataFile, ascii = FALSE, version = 2, compress = TRUE)
	if (ok) ok <- file.exists(RDataFile)
	if (show.log) logView()
	return(invisible(ok))
}
# }}} 

# {{{ compress.zid
# Compress one sample as a single .zid zipped file	
"compress.zid" <- function(zidir, type = "ZI1", check = TRUE, check.vignettes = TRUE,
    replace = FALSE, delete.source = replace,
	check.zip = TRUE, show.log = TRUE) {
		
	# {{{ check the format
	if (type != "ZI1") stop("only 'ZI1' is currently supported for 'type'!")
	# }}}
	
	# {{{ We need to switch to the root of sample dir first for correct path in the zip file
	rootdir <- dirname(zidir)
	inidir <- setwd(rootdir); on.exit(setwd(inidir))
	zidir <- basename(zidir) # Use only the latest dir (the "sample dir")
	# }}}
	
	# {{{ The .zid file is located in the "root" dir, same name as the "sample dir", with .zid extension
	zidfile <- paste(zidir, ".zid", sep = "")
	if (!replace && file.exists(zidfile)) {
		# It is not advised to delete source without rebuilding the .zid file
		# but it was expressly asked!
		### TODO: verify we have the same files in the .zid and initial dir before deleting files!
		if (delete.source && file.exists(zidir)) unlink(zidir, recursive = TRUE)
		return(invisible(TRUE))	# Nothing else to do
	}
	# }}}
	
	# {{{ This requires the 'zip' program!
	# Make sure it is available
	if (check.zip ) {
		checkZipAvailable( ) # this will stop if zip is not available
	}
	# }}}
	
	# {{{ Make sure everything is fine for this directory
	ok <- TRUE
	if (check) {
		withRestarts( withCallingHandlers( { 
			verify.zid(zidir, type = type, check.vignettes = check.vignettes, show.log = FALSE)
		} , zooImageError = function( e ){
			logError( e )
			invokeRestart( "zooImageError" )
		} ), zooImageError = function( e ){
			ok <<- FALSE
		} )
	}
	if (!ok) {
		return(invisible(FALSE)) 
	}
	# }}}
	
	# {{{ Make sure the .RData file is created (or refreshed)
	# TODO: this needs to be simplified, the calling handlers should be invisible 
	#       here.
	withRestarts( withCallingHandlers( { 
		make.RData(zidir, type = type, replace = replace, show.log = FALSE)
	}, zooImageError = function(e){ # "has no .RData (error while creating it)!"
		logError( e, msg = "has no .RData (error while creating it)!" )
		invokeRestart( "zooImageError" )
	} ), zooImageError = function(e){
		ok <<- FALSE
	} )
	if( !ok ) return( invisible( FALSE ) )
	# }}}
	
	# {{{ Do compress the directory in the .zip file
	#     Copy or move all corresponding files to a .zid zip-compressed file
	invisible( zip( zidfile, zidir, delete.source = delete.source ) )
	# }}}
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
	if (type != "ZI1") stop("only 'ZI1' is currently supported for 'type'!")
	# }}}
	
	# {{{ This requires the 'zip' program!, Make sure it is available
	#     COMMENT : do we really need that
	checkZipAvailable()
	# }}}
	
	# {{{ First, switch to that directory                                       
	inidir <- getwd()
	if (!file.exists(path) || !file.info(path)$isdir)
		stop(path, " does not exist, or it is not a directory!")
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
	if (is.null(samples) || length(samples) == 0)
		stop("There is no directories to process in ", getwd())
	# }}}
		
	# {{{ Start the process
	logClear()
	if (check) {
		logProcess("Verification...")
		verify.zid.all(path = path, samples = samples, check.vignettes = check.vignettes, 
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
		
		# if (!compress.zid(samples[s], type = type, check = FALSE, check.vignettes = check.vignettes, 
        #     replace = replace, delete.source = delete.source, check.zip = FALSE, show.log = FALSE)) {
		# 	ok <- FALSE
		# } else {
		# 	logProcess("OK", samples[s])
		# }
		withRestarts( withCallingHandlers({  
			compress.zid(samples[s], type = type, check = FALSE, check.vignettes = check.vignettes, 
        	     replace = replace, delete.source = delete.source, check.zip = FALSE, show.log = FALSE)
			}, zooImageError = function( e ){
				logError( e )
				invokeRestart( "zooImageError" )
			}, zooImageWarning = function(w){
				logWarning( w )
			}) , zooImageError = function( e){
				ok <<- FALSE
			}) 
		
	}
	Progress (smax + 1, smax)	 # To dismiss the Progress() indication
	# }}}
	
	# {{{ Possibly clean the whole directory (move .zim files to \_raw
	#     and delete the \_work subdir if everything is fine
	if (ok) clean.after.zid(path = path, samples = samples) 
    # }}}
	
	# {{{ clean up
	finish.loopfunction( ok = ok, bell = bell, show.log = show.log )
	# }}}
	
}
# }}}

# {{{ clean.after.zid
"clean.after.zid" <- function(path = ".", samples = NULL) {
    
	# {{{ Do we have samples to process
    if (length(samples) == 0) return()
	# }}}
	
    # {{{ First, switch to that directory
	inidir <- getwd()
    if (!file.exists(path) || !file.info(path)$isdir)
		stop(path, " does not exist, or it is not a directory!")
	setwd(path); on.exit(setwd(inidir))
	# }}}
	
	# {{{ Logging
	cat("Cleaning directory...\n")
	logProcess("\nCleaning directory...")
	# }}}
    
	# {{{ identify paths
	zimfiles <- list.files(path = ".", pattern = "[.][zZ][iI][mM]$")
    zimsamples <- sub("^(.*)[+].+", "\\1", zimfiles)
	# }}}
	
    # {{{ Keep only those .zim files related to samples
    zimfiles <- zimfiles[zimsamples %in% samples]
	# }}}
	
	# {{{ process
    if (length(zimfiles) > 0) {
        rawdir <- file.path(".", "_raw")
        
		# {{{ If the _raw subdirectory does not exists, create it
        if (!file.exists(rawdir)){
			dir.create(rawdir)
		}
		# }}}
		
        copyto <- file.path(".", "_raw", zimfiles)
        
		# {{{ Move these .zim files
        for (i in 1:length(zimfiles)){
			file.rename(zimfiles[i], copyto[i])
		}
		# }}}
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
	
	# {{{ Check if the file provided is a .zid file, and if it exists
	if (!file.exists(zidfile))
		stop(zidfile, " not found!")
	if(length(grep("[.]zid$", zidfile)) == 0)
		stop(file, " is not a .zid file!")
	# }}}
		
	# {{{ Uncompress it
	unzip( zidfile, path, delete.source = delete.source )
	# }}}
}
# }}}

# {{{ uncompress.zid.all
# Uncompress all .zid files in the 'path.extract' directory
"uncompress.zid.all" <-
	function(path = ".", zidfiles = list.files(path = path, pattern = "^.*[.][zZ][iI][dD]"),
	path.extract = path, skip.existing.dirs = TRUE, delete.source = FALSE, show.log = TRUE, bell = FALSE) {
	
	# {{{ initial checks
	if (is.null(zidfiles) || length(zidfiles) == 0) {
        stop("no .zid files found!" )
	}
	# }}}
	
	# {{{ start the process
	logClear()
	ok <- TRUE
	# }}}
	
	# {{{ Check that dirs / files with corresponding names exist in path.extract
    checkdirs <- file.path(path.extract, noext(zidfiles))
	fileExists <- rep(FALSE, length(checkdirs))
	dirExists <- rep(FALSE, length(checkdirs))
	for (i in 1:length(checkdirs)) {
		if (file.exists(checkdirs[i])) {
			if (file.info(checkdirs[i])$isdir) {
				dirExists[i] <- TRUE
			} else {
				fileExists[i] <- TRUE
			}
		}
	}
	# }}}
	
	# {{{ If any file not being a dir exist there, stop the process
	if (any(fileExists)) {
        stop("one or several files have same name as uncompressed dirs!" )
	}
	# }}}
	
	# {{{ Should we eliminate files whose corresponding dirs exist?
	if (skip.existing.dirs && any(dirExists)) {
		cat(sum(dirExists), "file(s) already uncompressed is/are skipped!\n")
        warning( paste( "Skipping already uncompressed file(s):", 
						  paste( zidfiles[dirExists], collapse = ",") )  )
	}
	zidfiles <- zidfiles[!dirExists]
	# }}}
	
	# {{{ Decompress the files remaining in the list
	smax <- length(zidfiles)
	if (smax == 0) {
		cat("-- Done! - (nothing to decompress)\n")
		stop("\n -- Done! -- (nothing to decompress)", show.log = show.log)
	}
	# }}}
	
	# {{{ Start decompression
	cat("Decompression...\n")
	logProcess("\nDecompression...")
	for (s in 1:smax) {
		# TODO: revises this so that this function watches when uncompress.zid
		#       starts and finishes and log the appropriate message using conditions
		Progress(s, smax) 
		uncompress.zid(zidfiles[s], path = path.extract,
		  delete.source = delete.source, show.log = FALSE)

		#< if (!uncompress.zid(zidfiles[s], path = path.extract,
		#< 	delete.source = delete.source, show.log = FALSE)) {
		#< 	ok <- FALSE
		#< } else {
		#< 	logProcess("OK", zidfiles[s])
		#< }
		
	}
	# }}}
	
	# TODO: set the calling handlers in a way that ok is created
	ok <- TRUE
	
	Progress (smax + 1, smax)	 # To dismiss the Progress() indication
	
	finish.loopfunction( ok, bell = bell, show.log = show.log )
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
	sample <- noext(basename(zidfile))
	RdataFile <- paste(sample, "_dat1.RData", sep ="")
	deletefile <- FALSE
	checkFileExists( zidfile, "%s not found!" )
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
				checkFileExists( rdata, "Error creating the .RData file")
			}
		} else stop( sprintf( "Unrecognized file: %s", zidfile) )
		# }}}
		
	} else { 
		# {{{ This is a .zid file
		rdata <- file.path(sample, RdataFile)
		rdata <- zid.extract(rdata, zidfile)
		if (rdata == "")
			stop( sprintf( "Error reading .RData file from %s", zidfile) )
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

