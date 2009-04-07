# Copyright (c) 2006, Ph. Grosjean <phgrosjean@sciviews.org>
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


# Specific functions for manipulating .zie files (ZooImage Import/Export)
# These .zie files contain specifications for importing a series of images
# and creating their corresponding .zim files (ZooImage Metadata) automatically.
# Typically, they are created by 'importing' image/data from other software
# or from digitization hardware/software.
# Another version specifies rules to automated exportation of ZooImage results.
# They are all 'ZIE' objects, with respective subclasses 'ZIEimport' and
# 'ZIEexport'.
#

### TODO: check image filename during importation!!!
### TODO: a routine that lists all ZIEimport objects + summary of them.

## Definition of standard import/export classes provided by default with ZooImage
## See also 'FlowCam.r' for an example of such an extension

# {{{ ZIE
# The function that eases creation of a ZIE object
### TODO: add a 'message' entry = message to display at the end of the importation
"ZIE" <- function(title, filter, description, pattern, command, author, version, date,
license, url, depends = "R (>= 2.4.0), zooimage (>= 1.0-0)",
type = c("import", "export")) {
	if (!is.character(title) || !is.character(filter) || !is.character(description) ||
		!is.character(pattern) || !is.character(command) || !is.character(author) ||
		!is.character(version) || !is.character(date) || !is.character(license) ||
		!is.character(url) || !is.character(depends))
		stop("All arguments must be character strings!")
	obj <- list(title = title[1], filter = filter[1], 
		description = paste(description, collapse = "\n"), pattern = pattern[1],
		command = paste(command, collapse = "\n"), author = author[1],
		version = version[1], license = license[1], depends = depends[1])
	class(obj) <- switch(type[1],
		import = c("ZIEimport", "ZIE"),
		export = c("ZIEexport", "ZIE"),
		stop("'type' must be either 'import' or 'export'!"))
	return(obj)
}

"print.ZIE" <-
function(x, ...) {
	Subclass <- class(x)[1]
	cat("A", getTemp("ZIname"), "Import/Export definition object of subclass:", SubClass, "\n")
	cat("\n", x$description, "\n\n")
	cat("Title:  ", x$title, "\n")
	cat("Filter: ", x$filter, "\n")
	cat("Pattern:", x$pattern, "\n")
	cat("Command:", x$command, "\n")
	cat("Author: ", x$author, "\n")
	cat("Version:", x$version, "\n")
	cat("Date:    ", x$date, "\n")
	cat("License:", x$license, "\n")
	cat("Depends:", x$depends, "\n")
	cat("URL:    ", x$url, "\n")
	return(invisible(x))
}
# }}}

# {{{ Various importers
# Import plain .tif files, with manual creation of associated .zim files
ZIEimportTif <- ZIE(title = "Tiff image files (*.tif)",
	filter      = "*.tif",
	description = c("Manual creation of ZooImage Metadata files (.zim)",
				    "given a list of directly usable .tif images",
				    "that is, no conversion required and image names",
				    "already follow the ZooImage convention"),
	pattern     = "\\.[tT][iI][fF]$",
	command     = "make.zim(dir = Dir, pattern = Pattern, images = Files, show.log = TRUE)",
	author      = "Philippe Grosjean (phgrosjean@sciviews.org)",
	version     = "1.1-0",
	date        = "2007-02-20",
	license     = "GPL 2 or above",
	url         = "",
	depends     = "R (>= 2.4.0), zooimage (>= 1.1-0)",
	type        = "import")
 
# Import plain .jpg files, with manual creation of associated .zim files
ZIEimportJpg <- ZIE(title = "Jpeg image files (*.jpg)",
	filter      = "*.jpg",
	description = c("Manual creation of ZooImage Metadata files (.zim)",
				    "given a list of directly usable .jpg images",
				    "that is, no conversion required and image names",
				    "already follow the ZooImage convention"),
	pattern     = "\\.[jJ][pP][gG]$",
	command     = "make.zim(dir = Dir, pattern = Pattern, images = Files, show.log = TRUE)",
	author      = "Philippe Grosjean (phgrosjean@sciviews.org)",
	version     = "1.1-0",
	date        = "2007-02-20",
	license     = "GPL 2 or above",
	url         = "",
	depends     = "R (>= 2.4.0), zooimage (>= 1.1-0)",
	type        = "import")

# Complex import of images (conversion, renaming, etc.) with automatic creation
# of associated .zim files using a .zie file
ZIEimportZie <- ZIE(title = "ZooImage Import Extension (Import_*.zie)",
	filter      = "Import_*.zie",
	description = c("Run a .zie import specification file to convert",
				    "and/or rename images and automatically create",
				    "associated .zim files (ZooImage Metadata)"),
	pattern     = "\\.[zZ][iI][eE]$",
	command     = "make.zie(path = Dir, Filemap = Files[1], check = TRUE, show.log = TRUE))",
	author      = "Philippe Grosjean (phgrosjean@sciviews.org)",
	version     = "1.1-0",
	date        = "2007-02-20",
	license     = "GPL 2 or above",
	url         = "",
	depends     = "R (>= 2.4.0), zooimage (>= 1.1-0)",
	type        = "import")

# Compile a .zie file from TemplateImport.zie and a table.txt and then compute it
ZIEimportTable <- ZIE(title = "Table and ImportTemplate.zie (*.txt)",
	filter      = "*.txt",
	description = c("Create a .zie file by interpretting a table,",
				    "using a template file in the same directory",
				    "and named 'ImportTemplate.zie'. The resulting",
				    ".zie file is then run to make images + metadata"),
	pattern     = "\\.[tT][xX][tT]$",
	command     = "compile.zie(path = Dir, TableFile = Files[1], make.it = TRUE, show.log = TRUE))",
	author      = "Philippe Grosjean (phgrosjean@sciviews.org)",
	version     = "1.1-0",
	date        = "2007-02-20",
	license     = "GPL 2 or above",
	url         = "",
	depends     = "R (>= 2.4.0), zooimage (>= 1.1-0)",
	type        = "import")
# Note: an example import plugin is provided in ./etc/FlowCam.r
# }}}
	
# {{{ make.zie
"make.zie" <- function(path = ".", Filemap = "Import_Table.zie", check = TRUE, replace = FALSE,
	move.to.raw = TRUE, zip.images = "[.][tT][iI][fF]$", show.log = TRUE, bell = FALSE) {
	# Example of use:
	# Import Digicam RAW files (currently, only Canon .CR2 files)
	# and transform them into .pgm file with correct names in _work subdirectory
	# move processed .cr2 files into _raw; create associated .zim files

	# This requires the 'dc_raw' and 'ppmtopgm' programs plus a couple of others!
	# We need 'identify' and 'convert' from ImageMagick 16 bits!
	# Make sure they are available
	if (check) {
		checkCapable( "identify" )
		checkCapable( "convert" )
		checkCapable( "dc_raw" )
		checkCapable( "ppmtopgm" )
		checkCapable( "zip" )
	}
	
	# First, switch to the root directory
	inidir <- getwd()
	checkDirExists( path )
	
	### TODO If last subdir of path is "_raw", then, work with parent dir and do not move files in _raw subdir
	
	setwd(path)
	on.exit(setwd(inidir))
	path = getwd()	# Indicate we are now in the right path
	
	# {{{ Read the Filemap
	cat("Reading Filemap...\n")
	checkFileExists( Filemap, extension = "zie", force.file = TRUE )
	# }}}
	
	# {{{ check first line for ZI1
	if( !checkFirstLine( Filemap ) ){
		stop( 'File does not appear to be a ZooImage version 1 file, or it is corrupted!' )
	}
	# }}}
	
	# {{{ read the file and check it is not empty
	# Note: we don't use comment.char = '#' because we want to read and rewrite those comments!
	Lines <- scan(Filemap, character(), sep = "\t", skip = 1,
		blank.lines.skip = FALSE, flush = TRUE, quiet = TRUE, 
		comment.char = "") 
	if (length(Lines) < 1) {
		stop( 'Empty or corrupted!' )
	}
	# }}}
	
	# {{{ get the position of a section
	getSectionPos <- function( section = "Map", message = "section '[%s]' found" ) {
		rx <- sprintf( "[[]%s[]]", section )
		out <- grep( rx, Lines )
		if( length(out) != 1 ){
			stop( sprintf( message, section ) )
		}
		return( out )
	}
	getSection <- function( section = "Map", to = c("next","end"), message = "The [Map] section is empty!" ){
		to <- match.arg( to )
		start <- getSectionPos( section )[1]
		end <- switch( to, 
			"next" = {
				ends   <- getSectionPos( ".*" )
				ends[ ends > start ][1] - 1
			}, 
			"end" = length(Lines )
		)
		out <- Lines[ seq.int( from = start + 1, to = end ) ]
		if( length( out ) == 0 ){
			stop( message )
		}
		out
	}
	# }}}
	
	# {{{ Get everything before '[Map]' as template data for the .zim file
	posMap <- getSectionPos( "Map", "The file is corrupted: no or duplicated [Map] section found!" )
	# }}}
		
	# {{{ setup the zim data
	zimData <- Lines[1:(posMap - 1)]
	attr(zimData, "Sample") <- NULL	# Currently, there is no sample!
	attr(zimData, "MakeZim") <- FALSE
	# }}}
	
	# {{{ EXtract various properties 
	
	# {{{ property extractor
	property <- function( property = "FilenamePattern", default = "" ){
		rx <- sprintf( "^%s[[:space:]]*=[[:space:]]*(.*)", property )
		if( any( gl <- grepl( rx, Lines ) ) ){
			sub( rx, "\\1", Lines[ gl ][1] )
		} else default
	}
	# }}}
	
	FilePat    <- property( "FilenamePattern" )
	FracPat    <- property( "FractionPattern" )
	SubPat     <- property( "SubsamplePattern" )
	Convert    <- property( "Convert" )
	Return     <- property( "Return" )
	FileExt    <- property( "FileExt" )
	FileC      <- property( "FileC" )
	FileExt2   <- property( "FileExt2", FileExt )
	MoveToWork <- tolower(property( "MoveToWork" ) ) %in% c("true", "yes", "1")
	Exif       <- property( "[<]exif[>]" ) != ""
	attr(zimData, "Exif") <- "" # Nothing yet here

	# Get the [Map] section
	Lines <- getSection( "Map", to = "end", "The [Map] section is empty!" )
	
	logProcess("Reading Filemap... OK!")
	
	# Make sure _raw, and _work subdirectories exists and have write access
	if (!file.exists("_raw")) {
		if (!dir.create("_raw")) {
			logProcess("Impossible to create the '_raw' subdirectory!", stop = TRUE, show.log = show.log); 
			return(invisible(FALSE)) 
		}
	} else {	# Check it is a directory
    	if (!file.info("_raw")$isdir) {
			logProcess("'_raw' exists, but is not a directory!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	}
	if (Convert != "" ||MoveToWork) {	# Only needed if we convert or move  the data!
		if (!file.exists("_work")) {
			if (!dir.create("_work")) {
				logProcess("Impossible to create the '_work' subdirectory!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
		} else {	# Check it is a directory
	    	if (!file.info("_work")$isdir) {
				logProcess("'_work' exists, but is not a directory!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
		}
	}

	# This function constructs image filename using possibly a FilenamePattern
	MakeImageName <- function(x, pattern = FilePat) {
		if (pattern == "") return(x)
		# Do we need to format a number?
		Format <- sub("^.*[<]([1-9]?)[>].*$", "\\1", pattern)
		if (Format != "") x <- formatC(as.integer(x), width = as.integer(Format), flag = "0")
		# Make the replacement according to the pattern
		File <- gsub("[<][1-9]?[>]", x, pattern) # Do we have to use FilePattern?
		return(File)
	}
	### TODO: indicate progression with exact line number in the zie file!
	
	### TODO: allow restarting from a given point!
	# Make sure that all image files are there, and there is no duplicated use of the same image
	cat("Checking all lines in the .zie file for raw images...\n")
	allImages <- character(0)
	for (i in 1:nLines) {
		### TODO: allow restarting from a given point and eliminate previous lines for which there are no images (considered as already processed!)
		Progress(i, nLines)
		if (length(grep("^[-][>]", Lines[i])) == 0) {	# This is not a state change command
			File <- MakeImageName(trim(sub("[=].*$", "", Lines[i])))
			if (!file.exists(File)) {
				logProcess("File not found!", File, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
			if (File %in% allImages) {
				logProcess("Duplicated use of the same file!", File, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
			allImages <- c(allImages, File)		
		}
	}
	Progress (nLines + 1, nLines)	 # To dismiss the Progress() indication
	logProcess("Checking all lines in the .zie file for raw images... OK!")
	cat("...OK!\n")
	# Now that we know all image files are there, process the [Map] section line-by-line	
	logProcess("Processing all lines in the .zie file (import images and make .zim files)...")
	cat("Processing all lines in the .zie file (import images and make .zim files)...\n")
	ok <- TRUE
	
	# This function builds the zim file and check it
	BuildZim <- function(zimData, FracPat, SubPat) {
		# Calculate the name of the zim file
		zimFileName <- paste(Smp, "zim", sep = ".")
		zimFile <- file.path(getwd(), zimFileName)
		
		# If the zim file already exists, skip this
		if (!replace && file.exists(zimFile)) {
			logProcess(paste(".zim file already exists for '", Smp, "'", sep = ""))
			return(TRUE)
		}
		
		# Make necessary replacement in Fraction and Subsample
		Smp <- attr(zimData, "Sample")
		if (is.null(Smp) || Smp == "") return(FALSE)
		zimD <- zimData
		
		# Clear a whole section, starting from its header to the next header
		ClearSection <- function(Data, fromLine) {
			n <- length(Data)
			if (fromLine > n) return(Data)
			# Locate the next header (line starting with "[")
			NextHeader <- grep("^[[]", Data[(fromLine + 1):n])
			if (length(NextHeader) == 0) {
				toLine <- n
			} else {
				toLine <- NextHeader[1] + fromLine - 1
			}
			# Strip out this section
			return(Data[-(fromLine:toLine)])
		}
		
		if (FracPat != "") {
			# This is the header to consider
			if (length(grep(FracPat, Smp)) == 0) {
				logProcess(paste("Sample '", Smp, "' is incompatible\nwith FractionPattern '", FracPat, "'", sep = ""), Smp, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }				
			Frac <- paste("[[]Fraction_", sub(FracPat, "\\1", Smp), "\\]", sep = "")
			posFrac <- grep(Frac, zimD)
			if (length(posFrac) < 1) {
				logProcess(paste("[Fraction] section not found (", Frac, ")!", sep = ""), Smp, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }			
			if (length(posFrac) > 1) {
				logProcess(paste("multiple", Frac, "sections found for this sample!"), Smp, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
			zimD[posFrac] <- "[Fraction]"
			# Strip out all other [Fraction_XXX] sections
			otherFrac <- grep("[[]Fraction_", zimD)
			if (length(otherFrac) > 0) 
				for (i in 1:length(otherFrac)) {
					zimD <- ClearSection(zimD, otherFrac[i])
				}
		}
		if (SubPat != "") {
			# This is the header to consider
			if (length(grep(SubPat, Smp)) == 0) {
				logProcess(paste("Sample '", Smp, "' is incompatible\nwith SubsamplePattern '", SubPat, "'", sep = ""), Smp, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }	
			Sub <- paste("[[]Subsample_", sub(SubPat, "\\1", Smp), "\\]", sep = "")
			posSub <- grep(Sub, zimD)
			if (length(posSub) < 1) {
				logProcess(paste("[Subsample] section not found (", Sub, ")!", sep = ""), Smp, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }			
			if (length(posSub) > 1) {
				logProcess(paste("multiple", Sub, "sections found for this sample!"), Smp, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
			zimD[posSub] <- "[Subsample]"
			# Strip out all other [Subsample_XXX] sections
			otherSub <- grep("[[]Subsample_", zimD)
			if (length(otherSub) > 0) 
				for (i in 1:length(otherSub)) {
					zimD <- ClearSection(zimD, otherSub[i])
				}		
		}
		# Possibly insert Exif data
		if (Exif && !is.null(attr(zimData, "Exif"))) {
		    pos <- grep("^[<]exif[>]", zimD)   # pos is recalculated here, because it may have changed!
		    if (length(pos) > 0) zimD <- c(zimD[1:(pos - 1)], attr(zimData, "Exif"), zimD[(pos+1):length(zimD)])
		}
				
		# Write the zim file
		logProcess(paste("Writing .zim file for sample '", Smp, "'", sep = ""))
		cat(paste(c("ZI1", zimD), collapse = "\n"), file = zimFile)
		return(TRUE)
	}
	
	# This function looks if the line asks for updating zimData and does it (returns TRUE), or it returns FALSE
	UpdateZim <- function(dat, zimData) {
		### TODO: Strip out comments (not done here, because we want to process strings with '#' correctly!
		if (length(grep("^[-][>]", dat)) == 0) return(FALSE)
		# This line starts with "->" => we update zimData
		Key <- sub("^[-][>]([^ =]+).*$", "\\1", dat)
		# Special treatment if Key == "Sample"
		if (Key == "Sample") {
			attr(zimData, "Sample") <- trim(sub("^[^=]+=", "", dat))	# Indicate that we process another sample
			attr(zimData, "MakeZim") <- TRUE # Tell to make the zim file
			attr(zimData, "Exif") <- ""
		} else {	# This is an usual key
			# Replace every line corresponding to this key in zimData
			MatchLines <- grep(paste("^", Key, sep = ""), zimData)
			if (length(MatchLines > 0))
				zimData[MatchLines] <- sub("^[-][>]", "", dat)
		}
		return(zimData)	
	}
	
	# Add or change an entry in [Calibration] section
	SetCalib <- function(Data, Key, Entry) {
		Line <- paste(Key, Entry, sep = "=")
		# Is this key already defined?
		posKey <- grep(paste("^\\s*", Key, "\\s*=", sep = ""), Data)
		# If defined => change it now
		if (length(posKey) > 0) {
			Data[posKey] <- Line
			return(Data)
		}
		# Is the [Calibration] section already defined?
		posCalib <- grep("[[]Calibration\\]", Data)
		if (length(posCalib) > 0) {	# Add this new entry in the [Calibration] section
			Data <- c(Data[1:posCalib[1]], Line, Data[(posCalib[1] + 1): length(Data)])
		} else { 	# Create the [Calibration] section at the end and add this entry inside it
			if (Data[length(Data)] != "")
				Data <- c(Data, "")	# Make sure that the section is separated with a blank
			Data <- c(Data, "[Calibration]", Line)
		}
		return(Data)
	}
	
	BlankField <- NULL  # The name of the blank-field image to use
	for (i in 1:nLines) {
		Progress(i, nLines)
		res <- UpdateZim(Lines[i], zimData)
		if (length(res) == 1 && res == FALSE) {	# This is not a state change command
			File <- MakeImageName(trim(sub("[=].*$", "", Lines[i])))
			# Determine the name of the converted file
			if (Convert != "") {
				if (FileC == "") { # Construct the name of the converted file
					FileConv <- paste(noext(File), FileExt, sep = ".")
				} else {
					# Make sure that previous file is deleted
					unlink(FileC)
					FileConv <- FileC
				}
			} else {
				FileConv <- File
			}
			
			# Determine the final name to give to this converted file, and check if it is a calibration file
			FileConvExt <- tolower(sub("^.*[.]", "", FileConv))
			# Calculate the final name we want for the converted file
			NewFile <- trim(sub("^.*[=]", "", Lines[i]))
			# 1) If this is 'key' or 'key=' (NeWFile == ""), then, the file is not renamed!
			if (NewFile == "") {
				FileConvName <- paste(noext(File), FileExt2, sep = ".")
			# 2) If the new name starts with "_Calib", then, never use the Sample part and add a CalibXX entry in .zim file
			} else if (length(grep("^_Calib", NewFile)) > 0) {
                # If this is a blank-field image, use it for further process
				if (length(grep("^_CalibBF", NewFile)) > 0) {
					FileConvName <- paste(NewFile, FileExt, sep = ".")
					# Remove previous blank-field from root directory (not needed any more!)
					if (!is.null(BlankField)) {
						# Delete blank-field images (.pgm and .img) in the root directory
						unlink(BlankField)
						unlink(paste(noext(BlankField), "img", sep = "."))
					}
					BlankField <- FileConvName
				} else {
				    FileConvName <- paste(NewFile, FileExt2, sep = ".")
				}
				# Add or change the calibration information (_CalibSP01 => CalibSP=_CalibSP01.ext)
				Key <- sub("^_(Calib[A-Z]+).*$", "\\1", NewFile)
				zimData <- SetCalib(zimData, Key, FileConvName)
			# 3) Name is Sample + name + ext
			} else {
				Smp <- attr(zimData, "Sample")
				if (is.null(Smp)) Smp <- ""
				FileConvName <- paste(Smp, NewFile, ".", FileExt2, sep = "")
			} 

			# Possibly read Exif data and place it in the zim file (or check correspondance)
			if (Exif) {
				ExifData <- attr(zimData, "Exif")
				ExifData2 <- readExifRaw(File, check = FALSE)
				if (!is.null(ExifData) && length(ExifData) > 0 && ExifData != "") { # Do a comparison of Exif data
				    compa <- compareExif(ExifData, ExifData2)
				    if (length(compa) > 0) {
						logProcess("Exif seems to be different from the rest!", File)
					}
				} else { # Just set Exif data
				    attr(zimData, "Exif") <- ExifData2
				}
			}
			
			# Possibly write a zim file?
			MakeZim <- attr(zimData, "MakeZim")
			if (!is.null(MakeZim) && MakeZim) {
				if (BuildZim(zimData, FracPat, SubPat)) {
					attr(zimData, "MakeZim") <- FALSE
				} else {
					return(invisible(FALSE))		
				}
			}
			
			# Possibly convert this file
			if (Convert != "") {
				if (zip.images != "" && length(grep(zip.images, FileConvName)) != 0 && length(grep("^_Calib", FileConvName)) == 0)
					finalname <- paste(noext(FileConvName), "zip", sep = ".") else finalname <- FileConvName
				logProcess(paste("Converting image '", File, "' into '", finalname, "'", sep = ""))
				if (replace || !file.exists(FileExt)) { 
					# Create variables Rawbase and Rawnoext
					Rawbase <- File
					Rawnoext <- noext(File)
					# Run the command
					res <- eval(parse(text = Convert))
					if (Return != "" && length(grep(Return, res)) == 0) { # Check that result matches
						ok <- FALSE
						logProcess("Error: result after conversion does not match!", File)
					}
					# Look if the converted file is created
					if (!file.exists(FileConv)) {
						ok <- FALSE
						logProcess("Error: converted file not found!", File)
					}
				}
			} else {
				if (Return != "") logProcess(paste("Processing image '", File, sep = ""))
			}

			# If this is a blank-field, then test it
            if (length(grep("^_CalibBF", NewFile)) > 0) {
				msg <- checkBF(FileConv)
				if (!is.null(msg) && length(msg) > 0 && msg != "") {
					logProcess(paste(c("Warning! Problem(s) detected with blank-field image:", msg), collapse = "\n\t"))  # Report the problem
				}
				# Eliminate dusts and smooth the image with a median filter on a 10 times reduced version of the image
				# We need identify and convert form ImageMagick...16
				Size <- imagemagick_identify( FileConv )
				Size2 <- round(Size/10) # size of the resized image
				imagemagick_convert( FileConv, Size, Size2 )
				
			} else { # make blank-field correction
			    if (!is.null(BlankField)) {
					
					tryCatch( 
						BFcorrection(FileConv, BlankField, deleteBF = FALSE), 
						error=function(e){
							logProcess( extractMessage( e ) , File)
						}
					)
					
					# Delete the uncorrected file
					unlink(FileConv)
					
					# Now, FileConv is the same file, but with a .tif extension
					FileConv <- paste(noext(FileConv), "tif", sep = ".")
					if (!file.exists(FileConv)) {
						ok <- FALSE
						logProcess("Error: blank-field corrected file not found!", File)
					}
			    }
			}
			
			# If this is an optical density calibration, proceed with it
			if (length(grep("^_CalibOD", NewFile)) > 0) {
				Cal <- calibrate(FileConv)
				Msg <- attr(Cal, "msg")
				# Report the problem
				if (!is.null(Msg) && length(Msg) > 0 && Msg != "") {
					logProcess(paste(c("Warning! Problem(s) detected with O.D. calibration image:", attr(Cal, "msg")), collapse = "\n\t"))  
				}
				# Put calibration data in the .zim file
				zimData <- SetCalib(zimData, "WhitePoint", round(Cal[1]))
                zimData <- SetCalib(zimData, "BlackPoint", round(Cal[2]))
			}
			
			### TODO: do the same for the spatial calibration image...
			if (Convert == "") {
				# If a second extention is provided, we need to rename and place the original into _raw subdir
				if (FileExt2 != "" && FileExt2 != FileExt) {
					# Copy the original image (indeed, same image, but with original name)
                	# into _raw
                	RawFile <- file.path(getwd(), "_raw", File)
                	file.copy(File, RawFile)
                	# And rename the original copy
                	# Possibly move it to _work subdirectory
                	if (MoveToWork) FileConvName <- file.path(dirname(FileConvName), "_work", basename(FileConvName))
                	file.rename(File, FileConvName)
                }
            } else { # This image was converted
				# Save the original file in _raw subdir
				RawFile <- file.path(getwd(), "_raw", File)
				file.rename(File, RawFile)

				# Rename the converted file and place it in _work
				WorkFileConv <- file.path(getwd(), "_work", FileConvName)
				# Move it, except if it is a blank-field file, then, copy it!
				if (length(grep("^_CalibBF", FileConvName)) > 0) {
					file.copy(FileConv, WorkFileConv)
                	file.rename(FileConv, FileConvName)
				} else {
					file.copy(FileConv, WorkFileConv)
				}
				if (!file.exists(WorkFileConv)) {
					logProcess("Error moving the converted file into '_work' subdirectory!", File, stop = TRUE, show.log = show.log); return(invisible(FALSE))
				} else {
					# Do we zip the resulting images, using the zim file as zip comment?
					if (length(grep("^_Calib", FileConvName)) == 0) { # Only images, not calib files!
						if (zip.images != "" && length(grep(zip.images, FileConvName)) != 0) {
						curdir <- getwd()
						setwd(file.path(curdir, "_work"))
						zimfile <- paste(attr(zimData, "Sample"), "zim", sep = ".")
						file.copy(file.path(curdir, zimfile), zimfile)
						zipfile <- paste(noext(FileConvName), "zip", sep = ".")
						cmd <- paste(Sys.getenv("COMSPEC"), ' /c type "', zimfile, '" | "', ZIpgm("zip", "misc"), '" -mqz9 "', zipfile, '" "', FileConvName, '"', sep = "")
						system(cmd, show.output.on.console = TRUE, invisible = TRUE)
						# Verify that the .zip file is created
						if (!file.exists(zipfile)) {
							logProcess("Error creating the file!", zipfile, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
							unlink(zimfile)
							setwd(curdir)
						} else {	### TODO: what do we have to do here???? 
					
						}
		    		}
				}
			}
		} else zimData <- res	# Update zimData with value returned by UpdateZim()
	}	
	Progress (nLines + 1, nLines)	 # To dismiss the Progress() indication
	# Possibly remove latest blank-field from root directory (not needed any more!)
	if (!is.null(BlankField)) {
		# Delete blank-field images (.pgm and .img) in the root directory
		unlink(BlankField)
		unlink(paste(noext(BlankField), "img", sep = "."))
	}

	
	if( ok ){
		if (move.to.raw)
			file.rename(Filemap, file.path(getwd(), "_raw", Filemap))
		# There is a bug: a 'fileconv.tif' file is created, delete it for the moment
		unlink("fileconv.tif")
	}
	
	# {{{ cleans up
	finish.loopfunction( ok, bell = bell, show.log = show.log )
	# }}}

}

#Setwd("g:/zooplankton/Madagascar2Macro")	# My example directory
#make.zie(path = ".", Filemap = "Import_Madagascar2Macro.zie")

"compile.zie" <- function(path = ".", Tablefile = "Table.txt",
	Template = "ImportTemplate.zie", Filemap = paste("Import_", noext(Tablefile), ".zie", sep = ""),
	Nrange = c(1, 1000), replace = TRUE, make.it = FALSE, show.log = make.it) {
		
	logProcess("Creating .zie file...")
	
	# First, switch to the root directory
	inidir <- getwd()
	if (!file.exists(path) || !file.info(path)$isdir) {
		logProcess("Path does not exist, or it is not a directory!", path, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	setwd(path)
	on.exit(setwd(inidir))
	path = getwd()	# Indicate we are now in the right path
	
    # Check if needed files exist
    if (!file.exists(Tablefile))
       stop("'", Tablefile, "' not found!")
    if (!file.exists(Template))
       stop("'", Template, "' not found!")
    # Check if the zie file already exists
    if (!replace && file.exists(Filemap))
       stop("'", Filemap, "' already exists and is not replaced (replace = FALSE)!")
    Data <- read.table(Tablefile, header = TRUE, sep = "\t", dec = getDec(), as.is = TRUE)
    # Possibly get Nmin and Nmax from the template file
	Nmin <- Nrange[1]             # Min number of images for each sample
    Nmax <- Nrange[2]             # Max number of images for each sample
    # We start from the template
    file.copy(Template, Filemap, overwrite = TRUE)
    Cat <- function(...) cat(..., sep = "", file = Filemap, append = TRUE)
    Cat("\n")
    Cat("[Map]\n")
    CBF <- -1
    CBFNum <- 0
    COD <- -1
    CODNum <- 0
	CSp <- -1
    CSpNum <- 0
    for (i in 1:nrow(Data)) {
        # Get calibration data
        CalibBF <- Data$CalibBF[i]
        if (!is.na(CalibBF) && !is.null(CalibBF) && CalibBF != "" && CalibBF != CBF) {
           CBFNum <- CBFNum + 1
           text <- paste(CalibBF, sprintf("_CalibBF%3.3d", CBFNum), sep = "=")
           Cat(text, "\n")
           CBF <- CalibBF
        }
        CalibOD <- Data$CalibOD[i]
        if (!is.na(CalibOD) && !is.null(CalibOD) && CalibOD != "" && CalibOD != COD) {
           CODNum <- CODNum + 1
           text <- paste(CalibOD, sprintf("_CalibOD%3.3d", CODNum), sep = "=")
           Cat(text, "\n")
           COD <- CalibOD
        }
        CalibSp <- Data$CalibSP[i]
        if (!is.na(CalibSp) && !is.null(CalibSp) && CalibSp != "" && CalibSp != CSp) {
           CSpNum <- CSpNum + 1
           text <- paste(CalibSp, sprintf("_CalibSP%3.3d", CSpNum), sep = "=")
           Cat(text, "\n")
           CSp <- CalibSp
        }
        num <- Data$Image[i]
        # Calculate list of all images
        num <- gsub(";", ",", num, extended = FALSE)
        num <- gsub("-", ":", num, extended = FALSE)
        num <- paste("c(", num, ")", sep = "")
        num <- eval(parse(text = num))
        # Check if the number is correct
		### TODO: add this in the template file!
		if (length(num) < Nmin || length(num) > Nmax)
           stop("Wrong number of images in 'Image' field for ", Data$Sample[i], "!")
		
		# Update several fileds according to what is deefined in the samples table
		###TODO: add the other fields + define this option
		Fields <- c("Sample", "SubPart", "PixelSize", "VolIni", "VolPrec")
        Cols <- names(Data)
        for (j in 1:length(Fields)) {
            if (Fields[j] %in% Cols) { 
                value <- Data[i, Fields[j]]
                if (!is.null(value) && !is.na(value) && value != "") {
                    text <- paste("->", Fields[j], "=", value, sep = "")
                    Cat(text, "\n")
                }
            }
        }
        #Insert corresponding images
        for (j in 1:length(num)) {
            text <- paste(num[j], "=.", j, sep = "")
            Cat(text, "\n")
        }
    }
	# Do we make it also?
	if (make.it) {
		res <- make.zie(path = path, Filemap = Filemap, check = TRUE, show.log = show.log)
		if (res) { # Everything is fine...
			# Move the table and copy the template to the '_raw' subdirectory too
			file.rename(Tablefile, file.path(path, "_raw", basename(Tablefile)))
			# Move also possibly the .xls equivalent
			Tablexls <- sub("\\.[tT][xX][tT]$", ".xls", Tablefile)
			if (Tablexls != Tablefile && file.exists(Tablexls))
			    file.rename(Tablexls, file.path(path, "_raw", basename(Tablexls)))
			file.rename(Template, file.path(path, "_raw", basename(Template)))
		}
	}
	return(invisible(file.path(path, Filemap)))
}

#Setwd("g:/zooplankton/Madagascar2Macro") # Directory with the example dataset
#compile.zie(Tablefile = "Madagascar2Macro.txt", Nrange = c(2,2))

"readExifRaw" <- function(rawfile, full = FALSE, check = TRUE) {
	# Read most important EXIF data from a Digicam RAW file
	# Make sure dc_raw is available and rawfile exists
	if (check) {
		checkCapable( "dc_raw" )
	}
	if (!file.exists(rawfile))
    	return(paste("'", rawfile, "' not found!", sep = ""))
	filedir <- dirname(rawfile)
	if (filedir != ".") {
		# Temporary change directory to the one where the file is located
		inidir <- getwd()
		setwd(filedir)
		on.exit(setwd(inidir))
		rawfile <- basename(rawfile)
	}
	temp <- "exifdata.txt"
	cmd <- paste('"', ZIpgm("dc_raw", "misc"), '" -i -v "', rawfile, '" > ', temp, sep = "")
	system(cmd, intern = TRUE, invisible = TRUE) 
	if (!file.exists(temp))
		return(paste("Error while reading exif data for '", rawfile, "'", sep = ""))
	res <- scan(temp, character(), sep = "\n", quiet = TRUE)
	if (length(res) < 6)
		return("Error getting EXIF data from '", rawfile, "'")
	# We replace ": " with "="
	res <- sub(": ", "=", res)
	# We replace all spaces by '_' (except for Filename and Timestamp, first two lines!)
	res[-2] <- gsub(" ", "_", res[-2])
	if (full) {
		# Rewrite date time into yyyy-mm-dd hh:mm:ss
		datetime <- sub("^Timestamp=", "", res[2])
		lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
		newdate <- as.character(as.Date(datetime, format = "%a %b %d %H:%M:%S %Y"))
		Sys.setlocale("LC_TIME", lct)
		newtime <- sub("^.* (.*) [0-9]{4}$", "\\1", "Wed Jul 12 09:45:38 2006")
		res[2] <- paste("Timestamp=", newdate, " ", newtime, sep = "")
	} else { # Keep only most important Exif data
	    res <- res[3:7]
	}
	unlink(temp)
	return(res)
}

#Setwd("g:/zooplankton/Madagascar2Macro") # Directory with the example dataset
#(Res <- readExifRaw("Image_0742.CR2"))

"compareExif" <- function(Exif1, Exif2) {
	# Make a comparison of two exif datasets on sensible entries
	dif <- character(0)
	# Need same 'Camera', 'ISO_speed', 'Shutter', 'Aperture', 'Focal_Length'
	### TODO: make it work for larger Exif dataset. Currently requires that the fields are restricted to strict equal data
	if (length(Exif1) != length(Exif2)) {
	    dif <- "Not same size for both Exif data!"
	} else {
	    difpos <- sort(Exif1) != sort(Exif2)
	    if (any(difpos))
			dif <- "Exif data are not identical!"
	}
	return(dif)
}

"isTestFile" <- function(File) {
	# Determine if a given file is a test file (a file with first line being 'ZI1est' and with size < 1000)
	if (file.info(File)$size > 1000) return(FALSE)
	Line1 <- scan(File, character(), nmax = 1, quiet = TRUE)
	return(Line1 == "ZItest")
}

"checkBF" <- function(BFfile) {
	# Check a blank-field image, either in .pgm or in .tif format
	if (!file.exists(BFfile))
		return(paste("Blank-field file '", BFfile, "' not found!", sep = ""))

	# Is it a test file?
	if (isTestFile(BFfile)) return(character(0))    # We behave like if the file was correct!
	
	msg <- character(0)
	filedir <- dirname(BFfile)
	if (filedir != ".") {
		# Temporary change directory to the one where the file is located
		inidir <- getwd()
		setwd(filedir)
		on.exit(setwd(inidir))
		BFfile <- basename(BFfile)
	}
	# The command to use depends on the format of the image (determined on the extension)
	ext <- tolower(rev(strsplit(BFfile, "\\.")[[1]])[1])
	pgmfile <- BFfile
	if (ext == "tif") {
		# First, convert into a .pgm file
		pgmfile <- paste(BFfile, "pgm", sep = ".")
		unlink(pgmfile)
		res <- shell(paste(ZIpgm("tifftopnm", "netpbm") , ' -byrow "', BFfile, '" > "', pgmfile, '"', sep = ""),
			intern = TRUE, invisible = TRUE) 
		if (!file.exists(pgmfile))
			return(paste("Impossible to convert '", BFfile, "' into a .pgm image", sep = ""))
		delfile <- TRUE
		ext <- "pgm"
	} else delfile <- FALSE
	if (ext != "pgm")
		return(paste("Unrecognized image format for '", BFfile, "'", sep = "")) 
	# Create a text file with the statistics of gray levels distribution and read it
	cmd <- paste('"', ZIpgm("pgmhist", "netpbm"), '" "', pgmfile, '"', sep = "")
	res <- system(cmd, intern = TRUE, invisible = TRUE)
	if (delfile) unlink(pgmfile)
	if (length(res) < 100)
		return(paste("Error while getting histogram of '", BFfile, sep = ""))	
	res <- res[-(1:2)]	# Eliminate header
	# Keep only two first columns
	res <- sub("\t.*$", "", sub("\t", " ", res))
	# Transform into a data frame of numerical values
	BF <- as.data.frame(matrix(as.numeric(unlist(strsplit(res, " "))), ncol = 2, byrow = TRUE))
	names(BF) <- c("Gray", "Count")
	# Make sure we work with 16bit images
	if (max(BF$Gray) < 256) {
		msg <- c(msg, "Blank-field seems to be a 8bit image (16bit required)")	
	} else {
		# Look at darkest value with at least 10 points
		BF <- BF[BF$Count >= 10, ]
		darkpart <- min(BF$Count)
		# Eliminate values with low number of points
		BF <- BF[BF$Count >= 100, ]
		# Check range for these values
		rngBF <- range(BF$Gray)
		if (rngBF[2] > 65500)
			msg <- c(msg, "Blank-field is overexposed")
		if (rngBF[2] < 60000)
			msg <- c(msg, "Blank-field is underexposed or contains too dark areas")
		if ((rngBF[2] - rngBF[1]) > 15000)
			msg <- c(msg, "Blank-field is too heterogeneous") 
		if ((rngBF[1] - darkpart) > 15000)
			msg <- c(msg, "Blank-field contains dark zones (dust?)")
	}
	return(msg)
}

#Setwd("g:/zooplankton/Madagascar2Macro") # Directory with the example dataset
#checkBF("test.pgm")
#checkBF("test.tif")

#{{{ calibrate
#' calibrates
#' @examples
#' Setwd("g:/zooplankton/madagascar2macro")
#' calibrate("test.tif")
"calibrate" <- function(ODfile) {
	### TODO: include also a spatial calibration procedure
	#(with a black circle around the center of the image)
	#and check also other characteristics, especially the sharpness

    cal <- c(NA, NA)
	names(cal) <- c("WhitePoint", "BlackPoint")
	msg <- character(0)

	if (!file.exists(ODfile)) {
		msg <- paste("O.D. file '", ODfile, "' not found!", sep = "")
		attr(cal, "msg") <- msg
		return(cal)
	}

	# Is it a test file?
	if (isTestFile(ODfile)) {
		# We behave like if the file was correct and return fake calibration data!
        cal <- c(1000, 50000)
		names(cal) <- c("WhitePoint", "BlackPoint")
		attr(cal, "msg") <- character(0)
		return(cal)
	}

	filedir <- dirname(ODfile)
	if (filedir != ".") {
		# Temporary change directory to the one where the file is located
		inidir <- getwd()
		setwd(filedir)
		on.exit(setwd(inidir))
		ODfile <- basename(ODfile)
	}
	# The command to use depends on the format of the image (determined on the extension)
	ext <- tolower(rev(strsplit(ODfile, "\\.")[[1]])[1])
	pgmfile <- ODfile
	if (ext == "tif") {
		# First, convert into a .pgm file
		pgmfile <- paste(ODfile, "pgm", sep = ".")
		unlink(pgmfile)
		res <- shell(paste(ZIpgm("tifftopnm", "netpbm") , ' -byrow "', ODfile, '" > "', pgmfile, '"', sep = ""),
			intern = TRUE, invisible = TRUE) 
		if (!file.exists(pgmfile))
			return(paste("Impossible to convert '", ODfile, "' into a .pgm image", sep = ""))
		delfile <- TRUE
		ext <- "pgm"
	} else delfile <- FALSE
	if (ext != "pgm")
		return(paste("Unrecognized image format for '", ODfile, "'", sep = "")) 
	# Create a text file with the statistics of gray levels distribution and read it
	cmd <- paste('"', ZIpgm("pgmhist", "netpbm"), '" "', pgmfile, '"', sep = "")
	res <- system(cmd, intern = TRUE, invisible = TRUE)
	if (delfile) unlink(pgmfile)
	if (length(res) < 100)
		return(paste("Error while getting histogram of '", ODfile, sep = ""))	
	res <- res[-(1:2)]	# Eliminate header
	# Keep only two first columns
	res <- sub("\t.*$", "", sub("\t", " ", res))
	# Transform into a data frame of numerical values
	OD <- as.data.frame(matrix(as.numeric(unlist(strsplit(res, " "))), ncol = 2, byrow = TRUE))
	names(OD) <- c("Gray", "Count")
	# Make sure we work with 16bit images
	if (max(OD$Gray) < 256) {
		msg <- c(msg, "O.D. seems to be a 8bit image (16bit required)")	
	} else {
		# Eliminate values with low number of points
		OD <- OD[OD$Count > 100, ]
		# Look at range: should be widespread enough, but without saturation
		rngOD <- range(OD$Gray)
		if (rngOD[2] > 65500)
			msg <- c(msg, "Images are overexposed, or whitepoint is already calibrated")
		if (rngOD[2] < 55000)
			msg <- c(msg, "Images are underexposed")
		# Here, saturation on the left-side of the histogram is not much a problem!
		if (rngOD[2] - rngOD[1] < 40000)
			msg <- c(msg, "Images lack contrast")	
		# We should end up with four segments
		graylev <- OD$Gray
		gap <- (diff(graylev) > 500)
		# If there are not *exactly* four gaps, there is a problem with the image!
		if (sum(gap) != 4) {
			msg <- c(msg, "Impossible to calibrate O.D.: wrong image")
		} else {
			# Get the five peaks, analyze them and get modes for blank, NDx2, NDx4 and NDx8
			peaks <- as.factor(cumsum(c(0, gap)) + 1)
			peaksgray <- split(graylev, peaks)
			names(peaksgray) <- c("Black", "NDx8", "NDx4", "NDx2", "White")
			# These are supposed to be all narrow peaks... check this
			peakspan <- sapply(peaksgray, range)
			peaksrange <- peakspan[2, ] - peakspan[1, ]
			# 1.2-2: width of black peak is much larger for Epson 4990 => be more tolerant for that peak
			if (any(peaksrange > c(20000, rep(5000, 4)))) {
				wrongpeaks <- paste(names(peaksrange)[peaksrange > 5000], collapse = ", ")
				msg <- c(msg, paste("Wrong O.D. image: lack of homogeneity for", wrongpeaks))
			} 
			# Look for the gray levels at the top of the peaks
			peaksheight <- split(OD$Count, peaks)
			names(peaksheight) <- c("Black", "NDx8", "NDx4", "NDx2", "White")
			findmax <- function(x) which.max(lowess(x, f = 0.05, iter = 1)$y)
			peaksval <- sapply(peaksheight, findmax)
			# Get the number of pixels in the white peak
			nbrwhite <- peaksheight$White[peaksval["White"]]
            # Replace the location by the actual gray level
			for (i in 1:5) peaksval[i] <- peaksgray[[i]][peaksval[i]]
			# If the number of pixels for pure white is larger than the white
            # peak found, replace it by pure white (65535)
            nbrpurewhite <- OD[nrow(OD), 2] 
            if (nbrpurewhite > nbrwhite) peaksval["White"] <- 65535   
			# Now, we need to calibrate the black and white points
			WhitePoint <- 65535 - peaksval["White"]
			# Perform a correction for the white point
			peaksval <- peaksval + WhitePoint
			# Transform those gray levels into O.D.
			peaksOD <- log(peaksval) * 65535 / log(65535)
			# Create a data frame with gray levels and corresponding OD for White, NDx2, NDx4 and NDx8
			calib <- data.frame(Gray = peaksOD[5:2], OD = c(0, 0.3, 0.6, 0.9))
			# Fit a line on these data
			calib.lm <- lm(OD ~ Gray, data = calib)
			# Check that calibration line is fine (i.e., the ANOVA should reject H0 at alpha = 5%)
			if (anova(calib.lm)[["Pr(>F)"]][1] > 0.01)
				msg <- c(msg, "Wrong OD calibration: not a straight line relation at alpha level = 0.01")
			# Check also that R squared is at least 0.98
			rsq <- summary(calib.lm)$r.squared
			if (rsq < 0.98)
				msg <- c(msg, paste("Bad OD calibration (R squared = ", formatC(rsq, digits = 3), ")", sep = ""))
			# Check linearity of the relationship by fitting a second order polynome and by looking at the
			# significativity of the x square parameter
			calib2.lm <- lm(OD ~ I(Gray^2) + Gray, data = calib)
			if (summary(calib2.lm)$coefficients["I(Gray^2)", "Pr(>|t|)"] < 0.01)
				msg <- c(msg, "Nonlinear OD calibration at alpha level = 0.01")
			# Calculate the value of the black point to get 0.004 OD per gray level after conversion (see the manual)
			ccoef <- coef(calib.lm)
			BlackPoint <- (1.024 - ccoef[1]) / ccoef[2]
			# Get the calibration data
			cal[1] <- round(WhitePoint)
			cal[2] <- round(BlackPoint)						
		}
	}
	attr(cal, "msg") <- msg
	return(cal)
}
# }}}

#{{{ BFcorrection
#' Make a blank-field correction on File, given a BFfile (blank-field)
#' 
#' both files must be 16bit gray PGM images
#' the resulting file has same name as File, but with a .tif extension instead of .pgm
#' The function returns TRUE in case of success... or an explicit error message
#'
#' @examples 
#' Setwd("g:/zooplankton/madagascar2macro")
#' BFcorrection("_CalibOD03.pgm", "_CalibBF03.pgm")
#' @throws this calls many xite scripts and might generate errors
"BFcorrection" <- function(File, BFfile, deleteBF = TRUE, check = TRUE) {
	
	# {{{ clean up on exit
	on.exit( {
		unlink(imgFile)
		if (deleteBF) {
			unlink(imgBFfile)
		}
	} )
	# }}}
	
	# {{{ check file existence
	checkFileExists( File, "pgm" )
	checkFileExists( BFfile, "pgm", message = "Blank-field file '%s' not found" )
	# }}}
	
	# {{{ check that the various scripts are available
	checkCapable( "pnm2biff" )
	checkCapable( "statistics" )
	checkCapable( "divide" )
	checkCapable( "biff2tiff" )
	# }}}
	
	# {{{ Switch to the directory of File
	filedir <- dirname(File)
	if (filedir != ".") {
		# Temporary change directory to the one where the file is located
		inidir <- getwd()
		setwd(filedir)
		on.exit(setwd(inidir))
		File <- basename(File)
	}
	# }}}
	
	# {{{ Determine the name of the various files
	imgFile <- paste(noext(File), "img", sep = ".")
	imgcorrFile <- paste(noext(File), "coor.img", sep = "")
	tifFile <- paste(noext(File), "tif", sep = ".")
	imgBFfile <- paste(noext(basename(BFfile)), "img", sep = ".")
	if (isWin()) {
		BFfile <- shortPathName(BFfile)
	}
	# }}}

    # {{{ Is File a test file?
	if (isTestFile(File)) {
		# We behave like if the file was corrected, but just copy the content of File into tifFile
		file.copy(File, tifFile)
		
		# Simulate creation of the .img blank-field
		if (!deleteBF){
			file.copy(BFfile, imgBFfile)    
		}
		return(TRUE)
	}
	# }}}

	# {{{ Convert PGM files into BIFF
	xite_pnm2biff( File, imgFile )
	xite_pnm2biff( BFfile, imgBFfile )
	# }}}
	
	# {{{ Get the mean gray level of the blank-field
	meangray <- xite_statistics( imgBFfile )
	# }}}
	
	# {{{ Eliminate the blank field
	res <- xite_divide( meangray, imgFile, imgBFfile, imgcorrFile)
	# }}}
	
	# {{{ make the tiff file
	xite_biff2tiff( imgcorrFile, tifFile )
	# }}}
	
	return(TRUE) # Everything is fine!
}
# }}}

# {{{ RawConvert
#' Convert a RAW file (digital camera) into a pgm file  
#' 
#' @examples
#' Setwd("d:/ZI examples/MacroG16-example")
#' RawConvert("Image_3822.CR2", fake = TRUE)
#' RawConvert("Image_3822.CR2")
#' @todo can we not do this in the the ImageJ plugin directly
"RawConvert" <- function(RawFile, OutputFile = "fileconv.pgm",
	DcRawArgs = "-v -c -4 -q 3 -t 0 -k 0", 
	fake = FALSE, replace = FALSE, check = TRUE) {
	
	# {{{ checks 
	# {{{ Check if the output file already exists
	if (file.exists(OutputFile)) {
		# If we want to replace existing file, delete it, otherwise, we are done
		if (replace) unlink(OutputFile) else return(TRUE)
	}
	# }}}
	
	# {{{ Check if RawFile exists
	if (!file.exists(RawFile)) {
		return( paste("'", RawFile, "' not found", sep = "") )
	}
	# }}}
	# }}}
	
	# {{{ Do a fake convert
	if (fake) { # Create a test file with just ZItest in it
		cat("ZItest\n", file = OutputFile)
		return(TRUE)
	}
	# }}}
	
	# {{{ Do the conversion using dc_raw
	# {{{ check that the system is capable of doing the conversion
	if (check) {
		checkCapable( "dc_raw" )
		checkCapable( "ppmtopgm" )
	}
	# }}}
	
	if( isWin( ) ){
		# {{{ Convert the RAW file into PPM file (48bit color)
		# we have to do it in two steps because windows lack of proper 
		# piping
		cmd <- sprintf( '"%s" %s %s > RAWTEMP.PPM', 
		ZIpgm("dc_raw", "misc"), DcRawArgs, RawFile )
		res <- try(system(cmd, invisible = TRUE), silent = TRUE)
		if (inherits(res, "try-error")) {
			return("Error while converting RAW file ", Rawfile)
		}
		# }}}

		# {{{ Convert from 48bit color to 16bit grayscale
		cmd <- paste(Sys.getenv("COMSPEC"), ' /c type RAWTEMP.PPM | ', ZIpgm("ppmtopgm", "netpbm"), ' > ', OutputFile, sep = "")
		res <- try(system(cmd, invisible = TRUE), silent = TRUE)
		if (inherits(res, "try-error") || res > 0) {
			return("Error while converting color to grayscale for ", Rawfile)
		}
		# Eliminate temporary file
		unlink("RAWTEMP.PPM")
		# }}}
	
	} else{
		
		# {{{ one step conversion (no tempfile)
		cmd <- sprintf( 'dcraw %s %s | ppmtopgm > "%s"' , 
			DcRawArgs, RawFile, OutputFile )
		res <- try(system(cmd ), silent = TRUE)
		if (inherits(res, "try-error") || res > 0) {
			return("Error while converting ", Rawfile)
		}
		# }}}
		
	}
 	# }}}
	
	# Everything was fine
	return(TRUE)
}
# }}}
 
# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

