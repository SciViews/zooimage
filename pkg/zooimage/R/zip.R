# Copyright (c) 2004, Ph. Grosjean <phgrosjean@sciviews.org>
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

#{{{ zip.img
#' Zip a .tif image and embed the corresponding .zim file as comment
#' This requires the 'zip' program!
"zip.img" <- function(imagefile, zimfile = NULL, verify.zimfile = TRUE, replace = FALSE,
		delete.source = TRUE, check.zip = TRUE, show.log = TRUE) {

	#{{{ Initial checks

	#{{{ We need to switch to the root of sample dir first for correct path in the zip file
	imagefile <- imagefile[1]
	inidir    <- getwd()
	setwd(dirname(imagefile)) ; on.exit(setwd(inidir))
	rootdir   <- getwd()
	imagefile <- basename(imagefile)
	#}}}

	# Check if imagefile exists
	checkFileExists( imagefile, message = "%s doesn't exist, or is a directory!",
		force.file = TRUE)

	# Is there an associated .zim file?
	if (is.null(zimfile)){
		sample.info <- get.sampleinfo(imagefile, "fraction", ext = extensionPattern( "tif" ) )
		zimfile <- paste(sample.info, ".zim", sep = "" )
	}

	### TODO: the zim file can be other parts of it , like Sample+A1.zim, instead of Sample+A.zim!
	if (!file.exists(zimfile)) {
        	# Create it from a template
			### TODO
			# Save a copy as LastUsed.zim
			### TODO
			stop( "creation of .zim file not implemented yet!" )
	}

	# Recheck .zim file
	checkFileExists( zimfile,
		message = "%s - doesn't exist or is corrupted!" )
	#}}}

	#{{{ Verify the content of the .zim file
	if (verify.zimfile && verify.zim(zimfile) != 0) {
		stop( sprintf( "%s appears to be corrupted!", zimfile) )
	}
	#}}}

	#{{{ Zip the image in the '_raw' subdir and add the information from the .zim file as comment
	zipfile <- paste(noext(imagefile), ".zip", sep = "")
	zipfile <- file.path(".", "_raw", zipfile)
	# Make sure that "_raw" subdir exists
	force.dir.create( "_raw" )
	#}}}

	#{{{ Copy or move the image to a .zip compressed file
	zip( zipfile, imagefile, comment.file = zimfile, delete.zipfile.first = replace )
	#}}}

	#{{{ Invisibly indicate success
	# Note: the .zim file is never deleted, because it can be used for other purposes!
	return(invisible(TRUE))
	#}}}
}
#}}}

#{{{ zip.img.all
#' Compress all .tif images in the corresponding directory
#' (at least those with an associated .zim file)
"zip.img.all" <- function(path = ".", images = NULL, check = TRUE, replace = FALSE,
	delete.source = replace, show.log = TRUE, bell = FALSE) {

	#{{{ This requires the 'zip' program!
	# Make sure it is available
	checkCapable("zip")
	#}}}

	#{{{ First, switch to that directory
	inidir <- getwd()
	checkDirExists( path )
	setwd(path); on.exit(setwd(inidir))
	path = getwd()	# Indicate we are now in the right path
	#}}}

	#{{{ Get the list of images to process
	if (is.null(images)){	# Compute them from path
		images <- dir(path, pattern = extensionPattern("tif") ) # All .tif files
	}
	#}}}

	#{{{ Make sure there is no path associated
	if(!all(images == basename(images))){
		stop("You cannot provide paths for 'images', just file names")
	}
	#}}}

	#{{{ If there is no images in this dir, exit now
	if (is.null(images) || length(images) == 0){
		stop("There is no images to process in ", getwd())
	}
	#}}}

	#{{{ Look at associated .zim files
	zimfiles <- paste(get.sampleinfo(images, "fraction", ext = extensionPattern("tif") ), ".zim", sep = "")
	keep <- file.exists(zimfiles)
	if (!any(keep)){
		stop("You must create .zim files first (ZooImage Metadata)!")
	}
	if (!all(keep)) {
    	warning(sum(!keep), " on ", length(keep), " images have no .zim file associated and will not be processed!")
		images <- images[keep]
		zimfiles <- zimfiles[keep]
	}
	#}}}

	#{{{ check the zim files
	logClear()
	ok <- TRUE
	if (check) {
		cat("Verification of .zim files...\n")
		logProcess("Verification of .zim files...")
		ok     <- TRUE
		zfiles <- unique(zimfiles)
		zmax   <- length(zfiles)
		oks <- sapply( 1:zmax, function(z){
			Progress(z, zmax)
			tryCatch( {
				verify.zim(zfiles[z])
				TRUE
			} , zooImageError = function(e){
				logError( e )
				FALSE
			} )
		} )
		ok <- all( oks )
		ClearProgress()
	}
	if (ok) {
		logProcess("\n-- OK, no error found. --")
		cat("-- Done! --\n")
	} else {
		stop("contains corrupted .zim files, compression not started!")
	}
	#}}}

	#{{{ If everything is ok compress these files
	imax <- length(images)
	cat("Compression of images...\n")
	logProcess("\nCompression of images...")

	oks <- sapply( 1:imax, function(i){
		Progress(i, imax)
		tryCatch( {
			zip.img(images[i], verify.zimfile = FALSE, replace = replace,
				delete.source = delete.source, check.zip = FALSE, show.log = FALSE)
			logProcess("OK", images[i])
			TRUE
		}, zooImageError = function(e){
			logError(e)
			FALSE
		} )
	} )
	#}}}

	ClearProgress()

	#{{{ Final report
	finish.loopfunction( ok, bell = bell, show.log = show.log )
	#}}}
}
#}}}

#{{{ unzip.img
# use zipnote to extract the comment
"unzip.img" <- 	function (zipfile) {
	# Extract .zim file, .tif file or both from a .zip archive
	zipnote( zipfile )

}
#}}}

#{{{ unzip.img.all
"unzip.img.all" <- function(path = ".", zipfiles = NULL) {

	# {{{ check that unzip is available
	checkUnzipAvailable( )
	# }}}

	# {{{ Extract all .zim, .tif or both from .zip files
	stop("Not implemented yet!")
	# }}}
}
#}}}

# {{{ zip
#' zip the content of the directory into the zipfile
#'
#' zip the content of the directory into the zipfile
#' and delete the directory if needed
zip <- function( zipfile , directory, delete.source = FALSE, comment.file = NULL, delete.zipfile.first = TRUE ){

	# We need the system to be capable of zipping
	checkZipAvailable( )

	# Delete old zip file, if it exists
	if (delete.zipfile.first && file.exists(zipfile)) {
		unlink(zipfile)
	}

	# Test if we need and can add the comment file
	comment <- !is.null(comment.file) && file.exists(comment.file)

	# Build the list of parameters for zip
	zippar <- sprintf( "-rq9%s%s", if(delete.source) "m" else "", if(comment) "z" else "")

	# {{{ create the basic command
	cmd <- sprintf( '"%s" %s "%s" "%s"',
		ZIpgm("zip", "misc"),
		zippar,
		zipfile,
		directory )
	# }}}

	# {{{ call the command
	result <- if( isWin() ){
		# modify the windows command so that the message is piped into the zip command
		if( comment ){
			cmd <- sprintf( '%s /c type "%s" | %s',
				Sys.getenv("COMSPEC"),
				comment.file,
				cmd )
		}
		system(cmd, show.output.on.console = TRUE, invisible = TRUE)
	} else{
		# modify the command if we need and can add the comment file
		if( comment ){
			cmd <- sprintf( '%s < "%s"', cmd, comment.file )
		}
		# send the error stream to the null device
		cmd <- paste( cmd, ' 2> /dev/null' )
		# call the command
		system(cmd)
	}
	# }}}

	checkFileExists( zipfile, message = "Error creating zip file '%s'" )

	invisible( result == 0 )
}
# }}}

# {{{ zip_addcomments
zip_addcomments <- function( zip, comment.file,
	on.failure = stop( sprintf( on.failure.msg , comment.file, zip ) ),
	on.failure.msg = "problem adding comment from '%s' to file '%s' ",
	on.success ){

	checkZipAvailable( )

	cmd <- if( isWin() ){
		sprintf( '%s /c type "%s" | "%s" -z "%s" ',
			Sys.getenv("COMSPEC"), comment.file, zip )
	} else{
		sprintf( 'zip -z "%s" < "%s" ', zip, comment.file )
	}
	res <- system(cmd, show.output.on.console = FALSE, invisible = TRUE, intern = TRUE)
	if( res != 0 ){
		on.failure
	} else if( !missing( on.success ) ){
		on.success
	}
	invisible( res )
}

# }}}

# {{{ unzip
#' unzip a zip file in a directory
#'
#' the function is created differently for R 2.9 (where unzip is available)
#' and other versions of R, where we use a system command
#' this happens at compile time of the package
#' @param zipfile the zip file to extract
#' @param path the path where to extract
#' @param delete.source logical; if TRUE the zipfile is deleted after unzipped
unzip <- if( as.numeric( version$major ) >= 2 && as.numeric( version$minor >= 9) )
	# {{{ version for R > 2.9.0
	function( zipfile, path, delete.source = FALSE){
		utils:::unzip( zipfile, exdir = path, overwrite = TRUE )
		if( delete.source ){
			unlink(zipfile)
		}
	} else # }}}

	# {{{ version for R < 2.9.0
	function( zipfile, path, delete.source = FALSE ){

		# {{{ unzip
		out <- if (isWin()) {
			zip.unpack(zipfile, path)
			TRUE
		} else {
			cmd <- sprintf( 'unzip "%s" -d "%s" 2> /dev/null', zipfile, path )
			out <- system( cmd )
			out == 0
		}
		# }}}

		# {{{ delete the zipfile
		if (delete.source) {
			unlink(zipfile)
		}
		# }}}

		invisible( out )
	}
	# }}}
# }}}

# {{{ zipnote
#' extract the comment from the zipfile
#'
#' comments that are written in the zipfile can be retrieved using
#' the zipnote command, the first lines all start with @ signs, and are not
#' the comment
#' @param zipfile the zip file from which to extract the comment
#' @param outfile if not NULL, indicates the file the comment should be sent to
#' @return the character vector corresponding to the comment. The character
#'         vector is still returned when the outfile is used, but it is returned
#'         invisibly in that case
zipnote <- function( zipfile, outfile = NULL ){

	# {{{ check that the system is zipnote capable
	checkZipnoteAvailable( )
	# }}}

	# {{{ build the command
	cmd <- sprintf( '"%s" "%s" ' , ZIpgm("zipnote", "misc"), zipfile )
	# }}}

	# {{{ call the command and grab the result
	out <- if( isWin( ) ){
		system( cmd, intern = TRUE, show.output.on.console = FALSE,
			invisible = TRUE )
	} else{
		system( cmd, intern = TRUE )
	}
	# }}}

	# {{{ filter out things that are not comments
	out <- out[ !grepl( "^@", out ) ]
	# }}}

	# {{{ write the output to the file if needed and return the result
	if( !is.null( outfile ) ){
		cat( out, file = outfile, sep = "\n" )
		invisible( out )
	} else{
		out
	}
	# }}}
}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:
