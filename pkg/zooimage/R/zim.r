# {{{ Copyright (c) 2004-2007, Ph. Grosjean <phgrosjean@sciviews.org>
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

# Functions for manipulating .zim files (ZooImage Metadata/measurements)
# These .zim files contain metadata required to analyze plankton images
# and to record the way they were processed. Measurements on each identified
# object can also be appended in a table at the end of this file (in this case,
# the usual extension is '_dat1.zim' to indicate that data processed with
# ZooImage version 1 are present in the file).
#

# {{{ make.zim
# HANDLE: verify.zim might throw an error
# HANDLE: create.zim
"make.zim" <- function(dir = ".", pattern = extensionPattern( "tif" ),
	images = list.files(dir, pattern), show.log = TRUE, bell = FALSE) {
	
	# {{{ check that there are images to process
	if (length(images) < 1) {
		stop("no images to process!" )
	}
	# }}}
	
	# {{{ Name of images is something like SCS.xxxx-xx-xx.SS+Ann.tif
	# We make the same .zim file for all ...+Ann images, so, reduce the list
	zims <- sort(unique(get.sampleinfo(images, type = "fraction", ext = pattern)))
	zims <- file.path(dir, sprintf( "%.zim", zims ) )
	ok <- TRUE
	zmax <- length(zims)
	cat("Making & checking .zim files...\n")
	# }}}
	
	# {{{ Start with a default template
	template <- NULL	
	for (z in 1:zmax) {
		Progress(z, zmax)
		
		#.zim file does not exists... create it
		if (!file.exists(zims[z])) { 	
			logProcess("Creating the file", zims[z])
			create.zim(zims[z], template = template, wait = TRUE)
			
			# Use the previous file as template
			template = zims[z]
		}
		# Verify that the zim is correct
		res <- verify.zim(zims[z])
		if (res != 0) ok <- FALSE
	}
	# }}}
	
	Progress (zmax + 1, zmax)	 # To dismiss the Progress() indication
	
	# {{{ cleans up
	finish.loopfunction( ok, bell = bell, show.log = show.log )
	# }}}
}
# }}}

# {{{ is.zim
# Check if a file is a "(_dat1).zim" file (must start with "ZI1" and have a '.zim' extension)
"is.zim" <- function(zimfile, check.ext = FALSE ) {
	
	# check if the file does not exist or is a directory
	checkFileExists( zimfile, force.file = TRUE, extension = "zim" )
	
	# check the first line
	checkFirstLine( zimfile, message = "File does not appears to be a ZooImage version 1 file, or it is corrupted!" )
	
	#  verything has passed
	invisible( TRUE )
}
# }}}

# {{{ verify.zim
#' Verify a "(_dat1).zim" file
#' 
#' Verify a "(_dat1).zim" file (all required fields + return the number of items in it)
#' If it succeeds, return the number of measured items as numeric
#' Otherwise, an error is generated by stop (see errorHandling for how errors are 
#' thrown and captured using calling handlers)
# TODO: check all functions calling verify.zim since it does not 
#       return the problem anymore but rather calls stop with the 
#       message
# HANDLE: 
# STOP: on extra verify function
# STOP: if file is not zim
# STOP: if file is empty (after first line)
# STOP: missing fields
# STOP: missing process fields
# STOP: missing columns in the table
# STOP: no data
# STOP: unable to read table of measurements
# STOP: no measurements found in the file
"verify.zim" <- function(zimfile, check.ext = FALSE, 
	is.dat1 = hasExtension( zimfile, "_dat1.zim"), check.table = FALSE) {
                                     
	# {{{ Required fields
	# Here are predefined required fields before measurements
	reqfields <- c("[Image]", "Author", "Hardware", "Software",
        "ImageType", "[Fraction]", "Code", "Min", "Max", "[Subsample]",
        "SubPart", "SubMethod", "CellPart", "Replicates", "VolIni",
        "VolPrec")
		
	# Then required fields when measurements are done    
	reqfields2 <- c("[Process]")
	# Finally, required column headers
    reqcols <- c("!Item", "Label", "BX", "BY", "Width", "Height")
	# }}}
	
	# {{{ Determine if there are custom verification rules defined and if they are active
    newRules <- getOption("ZI.zim")
    if (!is.null(newRules) && newRules$active == TRUE) {
        # Should we delegate the whole process to a custom verification function?
		verify.all <- newRules$verify.all
        if (!is.null(verify.all) && inherits(verify.all, "function"))
            return(verify.all(zimfile = zimfile, check.ext = check.ext,
                is.dat1 = is.dat1, chack.table = check.table))
		
		# Should we use additional verification code instead?
		verify <- newRules$verify
        reqfields <- c(reqfield, newRules$zim.required)
        reqfields2 <- c(reqfields2, newRules$dat1.zim.required)
        reqcols <- c(reqcol, newRules$dat1.data.required)
		# 
    } else verify <- NULL
	# }}}

	# {{{ check that it is a zimfile
	is.zim( zimfile, check.ext = check.ext )
	# }}}
	
	# {{{ Run first the extra verification code
	if (!is.null(verify) && inherits(verify, "function")) {
		# we need to grab the error here and call stop from here to maintain 
		# the old API and to allow the custom version of stop to be called
		# with the correct context of the "verify.zim" function
		res <- try( verify(zimfile, check.ext = check.ext, is.dat1 = is.dat1,
            check.table = check.table), silent = TRUE )
		if( inherits( res, "try-error" ) ){
			stop( extractMessage( res ) )  
		} else if( is.character(res) && nchar( res ) > 0 ) { 
			stop( res ) 
		}
    } 
	# }}}

	# {{{ Read the file...
	# Equal sign is used as comment char, in order to read only the field names
    Lines <- scan(zimfile, character(), sep = "\t", skip = 1,
        flush = TRUE, quiet = TRUE, blank.lines.skip = FALSE, 
		comment.char = "=") 
        
	if (length(Lines) < 1){
        stop("File is empty!")
	}
    
	# Trim leading and trailing white spaces
	Lines <- trim(Lines)
	# }}}

	# {{{ Check that all required fields are present for a simple .zim file
    misfields <- reqfields[!(reqfields %in% Lines)]
    if (length(misfields) > 0) {
        stop( paste( "Missing fields:", paste(misfields, collapse = ", ") ) )
    }
	# }}}

	# {{{ Check if this is a _dat1.zim file with measurements
    if ("[Data]" %in% Lines) {
        # {{{ check for missing fields
		misfields2 <- reqfields2[!(reqfields2 %in% Lines)]
        if (length(misfields2) > 0) {
            stop( paste("Missing [Process] fields:", paste(misfields2, collapse = ", ")) )
        }
		# }}}
        
		# {{{ Check for required column headers
		posHeaders <- grep("^\\[Data\\]$", Lines)[1] + 1
        LineHeader <- scan(zimfile, character(), sep = "%", skip = posHeaders,
			nmax = 1, flush = TRUE, quiet = TRUE, comment.char = "=")
        Headers <- trim(strsplit(LineHeader, "\t")[[1]])
        misHeaders <- reqcols[!(reqcols %in% Headers)]
        if (length(misHeaders) > 0) {
            stop( paste("Missing columns in the table:", paste(misHeaders, collapse = ", ")) )
        }
		# }}}

		# {{{ Check that the table can be read 
        if (check.table) {
			# {{{ check the [Data] section
            posMes <- grep("^\\[Data\\]$", Lines)
            if (length(posMes) == 0) {
                stop("Trying to read the table of measurements but no [Data] section found!")
            } else { # The [Data] section is found
				# we try to call read.table, catch the error, and throw it again 
				# from here, because stop might have a different meaning 
				# in the context of the verify.zim function
				# allowing to use the zooImage calling handlers, see errorHandling.R
				# COMMENT: maybe the alternative stop should be revised so that 
				#          it throws the message using the driver that is the 
				#          deapest in the call stack, that way we are 
				#          sure that we get a context and we don't have grab 
				#          errors to rethrow them right away
				#          not sure this will work with namespaces, ...
				Mes <- try(read.table(zimfile, sep = "\t", header = TRUE,
                  skip = posMes + 1), silent = TRUE)
                if (inherits(Mes, "try-error")) {
                  stop( paste( "Unable to read the table of measurements! : ", extractMessage( Mes) ) )
                } else { 	# Successful reading of the table of measurements
                  return(nrow(Mes))	# Return the number of items measured
                }
            } 
			# }}}
        } else { 
			# {{{ Alternative method that does not read the table
			 # We don't read the table, use a different method to get the number of entries in it
            # Read the last entry in Lines and convert it to a numeric value: should be the number of items measured
			nItems <- Lines[length(Lines)]
            if (sub("^[0-9]+$", "", nItems) != ""){
                stop("Impossible to determine the number of items measured!")
			}
            return(as.integer(nItems))
			# }}}
        }
		# }}}
    } else {
		if (is.dat1){
			stop("No measurements found in this file")
		} else {
			return(0)
		}
    }
	# }}}
}
# }}}

# {{{ list.zim, list.dat1.zim
"list.zim" <- function(zidir, ...) {
	list.files.ext( zidir, extension = "zim", ... )
}
"list.dat1.zim" <- function(zidir, ...) {
	list.files.ext( zidir, extension = "_dat1.zim", ... )
}
list.zip <- function( zidir, ... ){
	list.files.ext( zidir, extension = "zip", ... )
}
list.zid <- function( zidir, ... ){
	list.files.ext( zidir, extension = "zid", ... )
}
# }}}

# {{{ extract.zims
# Extract notes from .zip files and place them in .zim files
# STOP: All zip files must be located in the same directory!
# STOP: this is not a valid directory!
# STOP: One or several files not found!
# STOP: sprintf( "%s: is not a valid directory!", path)
# STOP: Done, no file to process!
"extract.zims" <- function(zipdir = ".", zipfiles = list.zip(zipdir),
		path = NULL, replace = FALSE, check.unzip = TRUE, show.log = TRUE, bell = FALSE) {
    
	# {{{ This requires the 'unzip' program!, Make sure it is available
	checkUnzipAvailable( )
	# }}}
	
	# {{{ Make sure all zipfiles are in the same directory
	zipdirs <- dirname(zipfiles)
	if (length(unique(zipdirs)) > 1) {
		stop("All zip files must be located in the same directory!" ) 
	}
	# }}}
	
	# {{{ Check that the dir exists!
	if (!file.exists(zipdir) || !file.info(zipdir)$isdir) {
		stop( sprintf( "%s: this is not a valid directory!", zipdir ) ) 
	}
	# }}}
	
	# {{{ Move to zipdir
    inidir <- getwd()
	setwd(zipdir); on.exit(setwd(inidir))
	zipdir <- getwd()   # That way, if we had ".", it is now expanded
	# }}}
	
	# {{{ Use only basenames for zip files
	zipfiles <- sort(basename(zipfiles))
	# }}}
	
	# {{{ Check that zipfiles exist
	if (!all(file.exists(zipfiles))) {
		stop("One or several files not found!")
	}
	# }}}
	
	# {{{ Look at the path where to place .zim files
	if (is.null(path)) {
		# The rule is the following one:
		# 1) if last subdir is "_raw", then place .zim file up one level
		# 2) else, place them in the same dir as the zip files
		path <- zipdir
		if (tolower(basename(path)) == "_raw"){
			path <- dirname(path)
		}
	} else {    # Look if this is a valid directory
		path <- path[1]
		if (!file.exists(path) || !file.info(path)$isdir) {
			stop( sprintf( "%s: is not a valid directory!", path) ) 
		}
	}
	# }}}
	
	# {{{ Compute the names of .zim files from the names of .zip files
	# Note: use only the fraction, that is, SCS.xxxx-xx-xx.SS+F from SCS.xxxx-xx-xx.SS+Fnn)
	# If there are duplicates, only extract first one
	zimfiles <- sprintf( "%s.zim", 
		get.sampleinfo(zipfiles, "fraction", ext = "\\.[zZ][iI][pP]$") )
	keep <- !duplicated(zimfiles)
	zimfiles <- zimfiles[keep]
	zipfiles <- zipfiles[keep]
	# }}}
	
	# {{{ Make full path for zimfiles
	zimfiles <- file.path(path, zimfiles)
	# }}}
	
	# {{{ If replace == FALSE, eliminate existing .zim files from the list
	if (!replace) {
		keep <- !file.exists(zimfiles)
		zimfiles <- zimfiles[keep]
		zipfiles <- zipfiles[keep]
	}
	# }}}
	
	# {{{ Are there files left
	if (length(zimfiles) == 0) {
		stop("Done, no file to process!" )
	}
	# }}}
	
	# {{{ Extract .zim files, one at a time, and check them
	zmax <- length(zimfiles)
	ok <- rep(TRUE, zmax)
	for (i in 1:zmax) {
		
		# Extract the .zim file from zip comment
		zipnote( zipfiles[i], zimfiles[i] )
		
		# Check that the .zim file is created
		if (!file.exists(zimfiles[i]) || verify.zim(zimfiles[i]) != 0){
			ok[i] <- FALSE
		}
	}
	# }}}
	
	# {{{ cleans up
	finish.loopfunction( ok = all(ok), 
		ok.console.msg = "", nok.console.msg = "", 
		bell = bell, show.log = show.log, 
		ok.log.msg = paste(zmax, ".zim files correctly extracted"), 
		nok.log.msg = paste(sum(!ok), "files not correctly extracted on", zmax) )
	# }}}
}
# }}}

# {{{ refresh.zims
#' Given a list of .zip files and a path where .zim files are located,
#' the function updates comment fields of the .zip files with latest .zim content   
"refresh.zims" <- function(zipdir = ".", 
	zipfiles = list.files.ext(zipdir, "zip" ),
	zimdir = NULL, check.zip = TRUE, check.zim = TRUE, 
	show.log = TRUE, bell = FALSE) {
	
	# {{{ Make sure 'zip' is available
	checkCapable( "zip" )
	# }}}
	
    # {{{ Make sure we have full path for zip files
	if (zipdir == ".") {
		zipdir <- getwd()
	}
	zipfiles <- file.path(zipdir, zipfiles)
	# }}}

    # {{{ Check that zipfiles exist
	if (!all(file.exists(zipfiles))) {
		stop( "One or several .zip files not found!" )
	}
	# }}}
    
	# {{{ Look for the path where .zim files are located
	if (is.null(zimdir)) {
		# The rule is the following one:
		# 1) if last subdir of .zip files is "_raw", then .zim files should be up one level
		# 2) else, look at the same dir
		zimdir <- zipdir
		if (tolower(basename(zimdir)) == "_raw"){
			zimdir <- dirname(zimdir)
		}
	} else {    # Look if this is valid directory
		zimdir <- zimdir[1]
		checkDirExists( zimdir, message = "'%s' is not a valid directory!" )
	}
	# }}}
	
	# {{{ Switch to that dir
	inidir <- getwd()
	setwd(zimdir)
	on.exit(setwd(inidir))
	# }}}
	
	# {{{ Compute the names of zim files from the names of zip files
	# Note: use only the fraction, that is, SCS.xxxx-xx-xx.SS+F from SCS.xxxx-xx-xx.SS+Fnn)
	# If there are duplicates, only extract first one
	zimfiles <- sprintf( "%s.zim", 
		get.sampleinfo(zipfiles, "fraction", ext = extensionPattern("zip") ) )
	
	# Eliminate path for zimfiles
	zimfiles <- basename(zimfiles)
    
	# Keep only existing .zim files
	keep <- file.exists(zimfiles)
	zimfiles <- zimfiles[keep]
	zipfiles <- zipfiles[keep]
	
	# Are there files left?
	if (length(zimfiles) == 0) {
		stop( "Done, no file to update!" )
	}
	# }}}
	
	# {{{ check the zim files using verify.zim if necessary
	logClear()
	ok <- TRUE
	if (check.zim) {
		cat("Verification of .zim files...\n")
		logProcess("Verification of .zim files...")
		ok <- TRUE
		zfiles <- unique(zimfiles)
		zmax <- length(zfiles)
		for (z in 1:zmax) {
        	Progress(z, zmax)
			
			# TODO: this might throw a condition
			verify.zim(zfiles[z])
		}
		Progress (zmax + 1, zmax)	 # To dismiss the Progress() indication
	}
	
	# if (ok) {
		logProcess("\n-- OK, no error found. --")
		cat("-- Done! --\n")		
	# } else {
	# 	logProcess("contains corrupted .zim files, compression not started!", path, stop = TRUE, show.log = show.log); 
	# 	return(invisible(FALSE))
	# }
	# }}}
	
	# {{{ If everything is OK, update comments in the zip files with the content of the .zim files
	imax <- length(zipfiles)
	cat("Update of .zip comments...\n")
	logProcess("\nUpdate of .zip comments...")
	for (i in 1:imax) {
		Progress(i, imax)
		
		# Replace the zip comment with the content of the corresponding .zim file
		zip_addcomments( zipfiles[i] , zimfiles[i], 
			on.success = logProcess( "OK", zipfiles[i] ) )
	
	}
	Progress (imax + 1, imax)	 # To dismiss the Progress() indication
	# }}}
	
	# {{{ cleans up
	finish.loopfunction( ok, bell = bell, show.log = show.log )
	# }}}
}
# }}}

# {{{ create.zim
# TODO; this looks a bit redundant with the edit.zim function
"create.zim" <- function(zimfile = NULL, template = NULL, editor = getOption("ZIEditor"),
	edit = TRUE, wait = FALSE) {
	
	# {{{ Create a .zim file from a template and edit it
	if (is.null(zimfile) || zimfile == "") {
		if (isWin()) {
	        zimfile <- winDialogString("Give a name for the new .zim file:",
				default = "myfile.zim")
		} else {	
			zimfile <- guiDlgInput("Give a name for the new .zim file:",
				"ZIM file creation", default = "myfile.zim")
		}
		if (is.null(zimfile) || length(zimfile) == 0 || zimfile == "")
			return(invisible())
		if ( !hasExtension( zimfile, "zim")){
			zimfile <- sprintf( "%s.zim", zimfile )
		}
	}
	# }}}
	
	# {{{ If the file exists, edit existing version instead
    if (file.exists(zimfile)){
		if (edit) return(edit.zim(zimfile, wait = wait)) else return()
	}
	# }}}
	
	# Look for the template
	if (is.null(template)) {
		template <- template("default.zim") 
	} else{ 
		is.zim(template)
	}
	
	# Copy the template into the new file
	file.copy(template, zimfile)
	
	# Edit this new file
	editor( zimfile, editor = editor )
	
}
# }}}

# {{{ edit.zim
#' Edit a .zim file
"edit.zim" <- function(zimfile, editor = getOption("ZIEditor"), wait = FALSE) {
	if (is.null(zimfile) || zimfile == "") {
		zimfile <- selectFile("Zim")
		if (zimfile == "")
			return(invisible())
	} else {
		is.zim(zimfile)
	}
	editor( zimfile, editor = editor )
}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:
