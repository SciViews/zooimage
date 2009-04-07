# Copyright (c) 2004-2006, Ph. Grosjean <phgrosjean@sciviews.org>
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

# Various utility functions used by ZooImage

# Get the name of one or several variables of a given class
"getVar" <-
	function(class = "data.frame", default = "", multi = FALSE,
	title = paste("Choose a ", class, ":", sep = ""), warn.only = TRUE) {
	# Get one or several variables of a given object class
	(require(utils) || stop("Package 'utils' is required!"))
	varlist <- objects(pos = 1)		# Get objects in .GlobalEnv
	# Filter this list to keep only object inheriting a giving class...
	Filter <- NULL
	for (i in 1:length(varlist)) Filter[i] <- inherits(get(varlist[i]), class)
	varlist <- varlist[Filter]	# Keep only those objects
	if (length(varlist) == 0) {	# No such objects in .GlobalEnv
		if (warn.only) {
			warning("There is no object of class ", paste(class, collapse = " "), " in the user workspace!")
		} else stop("There is no object of class ", paste(class, collapse = ""), " in the user workspace!")
		varsel <- "" 
	} else {
		if (default == "") default <- varlist[1]
		varsel <- select.list(varlist, preselect = default, multiple = multi, title = title)
	}
    return(varsel)		
}

# Get the name of one or several lists with all of their components of a given class
# Note: this is used as a collection in other languages (no such collection in R!)
"getList" <-
	function(class = "data.frame", default = "", multi = FALSE,
	title = paste("Choose a ", class, ":", sep=""), warn.only = TRUE) {
	# Get lists of items of specified class
	(require(utils) || stop("Package 'utils' is required!"))
	varlist <- objects(pos = 1)		# Get objects in .GlobalEnv
	# Filter this list to keep only list objects...
	Filter <- NULL
	for (i in 1:length(varlist)) Filter[i] <- inherits(get(varlist[i]), "list")
	varlist <- varlist[Filter]	# Keep only those objects
	if (length(varlist) == 0) {	# No such objects in .GlobalEnv
		if (warn.only) {
			warning("There is no list objects in the user workspace")
		} else stop("There is no list objects in the user workspace")
		return("") 
	} else {
		# Filter the list objects to keep only those having 'class' objects as items
		Filter <- rep(TRUE, length(varlist))
		for (i in 1:length(varlist)){
			Var <- get(varlist[i])
			for (j in 1:length(Var))
				if (!inherits(Var[[j]], class))
					Filter[i] <- FALSE
		}
		varlist <- varlist[Filter]	# Keep only those objects
		if (length(varlist) == 0) { 	# No such objects in .GlobalEnv
			if (warn.only) {
				warning("There is no list of ", class, " objects in the user workspace")
			} else stop("There is no list of ", class, " objects in the user workspace")
			return("")
		}	
		if (default == "") default <- varlist[1]
		varsel <- select.list(varlist, preselect = default, multiple = multi, title = title)
	}
    return(varsel)		
}

# Select one or several files of a given type
"selectFile" <-
	function(type = c("ZipZid", "ZimZis", "Zip", "Zid", "Zim", "Zis", "Zie"),
		multi = FALSE, quote = TRUE) {
	# Adapt title according to 'multi'
	Type <- type[1]
	if (Type == "ZipZid") Type <- "Zip/Zid"
	if (Type == "ZimZis") Type <- "Zim/Zis"
	if (multi) {
    	title <- paste("Select one or several", Type, "files...")
	} else {
		title <- paste("Select one", Type, "file...")
	}
	res <- switch(type[1],
		ZipZid = choose.files(caption = title, multi = multi,
			filters = c("ZooImage files (*.zip;*.zid)", "*.zip;*.zid")),
		ZimZis = choose.files(caption = title, multi = multi,
			filters = c("ZooImage metadata files (*.zim;*.zis)", "*.zim;*.zis")),
        Zip = choose.files(caption = title, multi = multi,
			filters = c("ZooImage picture files (*.zip)", "*.zip")),
        Zid = choose.files(caption = title, multi = multi,
			filters = c("ZooImage data files (*.zid)", "*.zid")),
        Zim = choose.files(caption = title, multi = multi,
			filters = c("ZooImage metadata files (*.zim)", "*.zim")),
        Zis = choose.files(caption = title, multi = multi,
			filters = c("ZooImage sample files (*.zis)", "*.zis")),
		Zie = choose.files(caption = title, multi = multi,
			filters = c("ZooImage extension files (*.zie)", "*.zie")),
		stop("'Unrecognized 'type'!"))
	if (res != "" && quote) res <- paste('"', res, '"', sep = "")
	return(res)
}

# Get a key in the registry (retrieve ZooImage configuration data)
### TODO: this must be adapted for other platforms!
"getKey" <-
	function(key, default.value = NULL) {
 	# Retrieve a ZooImage key in the registry
	if (!isWin()) return(default.value)
	# Look if the key is defined
	ZIkey <- getTemp("ZIkey")
	if (key %in% tk2reg.values(ZIkey)) {
    	# Get the content of that key
		return(tk2reg.get(ZIkey, key))
	} else return(default.value)
}

# Set a key in the registry (store configuration data for next ZooImage session)
### TODO: this must be adapted for other platforms!
"setKey" <-
	function(key, value, type = "sz") {
	if(!isWin()) return(invisible(FALSE))	# Still must be programmed!
	tk2reg.set(getTemp("ZIkey"), key, value, type = "sz")
	return(invisible(TRUE))
}

# Convert underscores into spaces
"underscore2space" <-
	function(char) {
	# Convert underscores to spaces in strings (underscore is used in calltips
	# in the ZooImage Metadata Editor, because of a bug in this program)
	return(gsub("_", " ", char))
}

# Trim leading and trailing spaces in strings
"trim" <-
	function(char) {
	# Trim leading and trailing white spaces and tabs
	return(sub("\\s+$", "", sub("^\\s+", "", char)))
}

# Get the name of a file, without its extension
"noext" <-
	function(file) {
	# Get basename without extension
	return(sub("\\.[^.]+$", "", basename(file)))
}

# Get information about a sample, given its name
"get.sampleinfo" <-
	function(filename, type = c("sample", "fraction", "image", "scs", "date", "id", "frac", "imgnbr"),
	ext = "_dat1[.]zim$") {
	type <- type[1]
	base <- basename(filename)
	if (ext != "") base <- sub(ext, "", base)
	# filename without extension is supposed to follow the convention: scs.date.id+f[img]
	# with scs.date.id forming an unique sample identifier
	# Note: not all verifications are conducted. So, it sometimes returns a result even if the name does
	# not conform to this specification!
	### TODO: check that the name follows the convention and determine what is facultative, like date, for instance)
	res <- switch(type[1],
		sample = sub("\\+[a-zA-Z][0-9.]+$", "", base),
		fraction = sub("[0-9.]+$", "", base),
		image = base,
		scs = sub("^[^+.]*[+.].+$", "", base),
		date = as.Date(sub("^.*([0-9]{4}-[0-1][0-9]-[0-3][0-9]).*$", "\\1", base)),
		id = sub("^.*\\..*\\.(.*)\\+.*$", "\\1", base),
		frac = sub("^.*\\+([a-zA-Z]).*$", "\\1",base),
		imgnbr = as.numeric(sub("^.*\\+[a-zA-Z]([0-9.]*)$", "\\1", base)),
		stop("'type' must be 'sample', 'fraction', 'image', 'scs', 'date', 'id', 'frac' or 'imgnbr'") )
	### TODO: check results
	return(res)
}

# Calculate equivalence circular diameter (similar to equivalent spherical diameter, but for 2D images)
"ecd" <-
	function(area) {
	return(2 * sqrt(area / pi))
}

# Unique identifiers (Ids) are a combination of Label and Item
"make.Id" <-
	function(df) {
	# Make a list of Ids, combining "Label" and "Item"
	return(paste(df$Label, df$Item, sep = "_"))
}

# Calculate derived variables... default function
"calc.vars" <-
	function(x) {
	# This is the calculation of derived variables
	# Note that you can make your own version of this function for more calculated variables!
	# A small hack to correct some 0 for Minor and Major
	x$Minor[x$Minor == 0] <- 0.000000001
	x$Major[x$Major == 0] <- 0.000000001
	x$Elongation <- x$Major / x$Minor
	x$CentBoxD <- sqrt((x$BX + x$Width/2 - x$X)^2 + (x$BY + x$Height/2 - x$Y)^2)
	x$GrayCentBoxD <- sqrt((x$BX + x$Width/2 - x$XM)^2 + (x$BY + x$Height/2 - x$YM)^2)
	x$CentroidsD <- sqrt((x$X - x$XM)^2 + (x$Y - x$YM)^2)
	x$Range <- x$Max - x$Min
	x$MeanPos <- (x$Max - x$Mean) / x$Range
	x$SDNorm <- x$StdDev / x$Range
	x$CV <- x$StdDev / x$Mean * 100
	x$Area[x$Area == 0] <- 0.000000001
	x$logArea <- log(x$Area)
	x$Perim.[x$Perim. == 0] <- 0.000000001
	x$logPerim. <- log(x$Perim.)
	x$logMajor <- log(x$Major)
	x$logMinor <- log(x$Minor)
	x$Feret[x$Feret == 0] <- 0.000000001
	x$logFeret <- log(x$Feret)
	return(x)
}

# All sample with at least one entry in a given object
"list.samples" <-
	function(obj) {
 	# List all samples represented in a given object
	if (inherits(obj, "ZIDat")) {
    	res <- sort(unique(get.sampleinfo(as.character(obj$Label), type = "sample", ext = "")))
		return(res)
	} else if (inherits(obj, "ZIDesc")) {
		res <- sort(unique(as.character(obj$Label)))
		return(res)
	} else if (inherits(obj, "ZITrain")) {
    	res <- as.character(obj$Id)
		res <- sub("_[0-9]*$", "", res)
		res <- sort(unique(get.sampleinfo(res, type = "sample", ext = "")))
		return(res)
	}
	# Not a recognized object
	stop("'obj' must be a 'ZIDat', 'ZIDesc' or or 'ZITrain' object!")
}

# Parse an ini file (.zim, .zie, etc.) are ini files!
### TODO: manage the case there is no '=' in the data!
"parse.ini" <-
	function(data, label = "1") {
	# Parse an ini file (tag=value => 'tag', 'value') and make a list with different sections
	is.section <- function(str)
		as.logical( length(grep("^\\[.+\\]$", trim(str)) > 0))

	# Get the name of a section
	get.section.name <- function(str)
		sub("^\\[", "", sub("\\]$", "", trim(str)))

	# Transform a vector of characters into a data frame, possibly with type conversion
	vector.convert <- function(vec) {
		as.data.frame(lapply(as.list(vec), type.convert))
	}

	if (is.null(data) || !inherits(data, "character") || length(data) < 1)
		return(character(0))
	# Trim leading and trailing white spaces
	data <- trim(data)
	# Convert underscore to space
	data <- underscore2space(data)
	# Eliminate empty lines
	data <- data[data != ""]
	data <- paste(data, " ", sep = "")
	if (length(data) < 1)
		return(character(0))
	# Substitute the first '=' sign by another separator unlikely to appear in the argument
	data <- sub("=", "&&&&&", data)
	# Split the strings according to this separator
	data <- strsplit(data, "&&&&&")
	# Get a matrix
	data <- t(as.data.frame(data))
	rownames(data) <- NULL
	# Make sure we have a section for the first entries (otherwise, use [.])
	if (!is.section(data[1, 1])) data <- rbind(c("[.]", "[.]"), data)
	Names <- as.vector(trim(data[, 1]))
	Dat <- as.vector(trim(data[, 2]))
	# Determine which is a section header
	Sec <- grep("\\[.+\\]$", Names)
	SecNames <- get.section.name(Names[Sec])
	# Make a vector of sections
	if (length(Sec) == 1) {
		SecNames <- rep(SecNames, length(Names))
	} else {
		SecNames <- rep(SecNames, c(Sec[2:length(Sec)], length(Names) + 1) - Sec)
	}
	# Replace section headers from all vectors
	Names[Sec] <- "Label"
	Dat[Sec] <- label
	names(Dat) <- Names
	# Transform SecNames in a factor
	SecNames <- as.factor(SecNames)
	# Split Dat on sections
	DatSec <- split(Dat, SecNames)
	# for each section, transform the vector in a data frame and possibly convert its content
	DatSec <- lapply(DatSec, vector.convert)
	# Eliminate "Label" if it is ""
	if (label == "") DatSec <- lapply(DatSec, function(x) x[-1])
	return(DatSec)
}

# Merge two lists of data frames
"list.merge" <-
	function(x, y) {
	if (!inherits(x, "list"))
		stop("'x' must be a 'list'!")
	if (!inherits(y, "list"))
		stop("'y' must be a 'list'!")
	xitems <- names(x)
	yitems <- names(y)
	xandy <- xitems[xitems %in% yitems]
	xonly <- xitems[!(xitems %in% xandy)]
	yonly <- yitems[!(yitems %in% xandy)]
	# construct the merged list
	res <- list()
	# First merge common items
	if (length(xandy) > 0) {
		for (i in 1:length(xandy))
			res[[xandy[i]]] <- merge(x[[xandy[i]]], y[[xandy[i]]], all = TRUE)
	}
	# Add xonly items
	if (length(xonly) > 0) {
	 	for (i in 1:length(xonly))
	 		res[[xonly[i]]] <- x[[xonly[i]]]
	}
	# Add yonly items
	if (length(yonly) > 0) {
	 	for (i in 1:length(yonly))
	 		res[[yonly[i]]] <- y[[yonly[i]]]
	}
	return(res)
}

# Add items across two lists (names must be the same)
"list.add" <-
	function(x, y) {
	if (!inherits(x, "list"))
		stop("'x' must be a 'list'!")
	if (!inherits(y, "list"))
		stop("'y' must be a 'list'!")
	if (!all(names(x) == names(y)))
		stop("names of two lists must match!")
	res <- x
	for (i in 1:length(x))
		res[[i]] <- x[[i]] + y[[i]]
	attributes(res) <- attributes(x)
	return(res)	
}

# Internationalization of ZooImage: get messages in other languages
"gettextZI" <-
	function(...) {
	### TODO: internationalization of the package
	#gettext(..., domain = "R-zooimage")
	return(list(...)[[1]])
}

# Display progression of long-running tasks, both on the R console
# and in the ZooImage assistant status bar
"Progress" <-
	function(value, max.value = NULL) {
	# This is my own version of progress() that uses also the Tk window statusbar
    if (!is.numeric(value)) 
        stop("`value' must be numeric!")
    if (is.null(max.value)) {
        max.value <- 100
        percent <- TRUE
    } else percent <- FALSE
    if (!is.numeric(max.value)) 
        stop("`max.value' must be numeric or NULL!")
    erase.only <- (value > max.value)
    Max.Value <- as.character(round(max.value))
    l <- nchar(Max.Value)
    Value <- formatC(round(value), width = l)
    if (percent) {
        backspaces <- paste(rep("\b", l + 14), collapse = "")
        if (erase.only) { 
            message <- ""
        } else {
			message <- paste("Progress: ", Value, "%  ", sep = "")
		}
        cat(backspaces, message, sep = "")
    } else {
        backspaces <- paste(rep("\b", 2 * l + 16), collapse = "")
        if (erase.only) { 
            message <- ""
        } else {
			message <- paste("Progress: ", Value, " on ", Max.Value,
				"  ", sep = "")
		}
        cat(backspaces, message, sep = "")
    }
    if (.Platform$OS.type == "windows") flush.console()
	# Do we need to update the Tk window statusbar also?
    if ("ZIDlgWin" %in% WinNames()) {
    	if (erase.only) { # Not busy any more
			rmTemp("statusBusy")
			tkconfigure(getTemp("statusProg"), value = 0)
			tkconfigure(getTemp("statusText"), text = paste("Ready -", getwd()))
		} else { 	# We are busy - display it
			assignTemp("statusBusy", TRUE)
			# Calculate fraction and show it in the progress bar
			if (!percent) value <- value / max.value * 100
			tkconfigure(getTemp("statusProg"), value = value)
			# Display the progress text also in the statusbar
			tkconfigure(getTemp("statusText"), text = message)
		}
		.Tcl("update idletasks")
	}
    invisible(NULL)
}

# Change the working directory and update the ZooImage assistant status bar
"Setwd" <-
	function(dir) {
	### TODO: this does not work if dir is changed from Rgui menu or from setwd()!
	# My own setwd() function that also updates the Tk window statusbar
	.Internal(setwd(dir))
	# Possibly update the statusbar
	if ("ZIDlgWin" %in% WinNames() && is.null(getTemp("statusBusy"))) {
		tkconfigure(getTemp("statusText"), text = paste("Ready -", getwd()))
		.Tcl("update idletasks")
	}
	# Save the current default directory for future use
	setKey("DefaultDirectory", getwd())
}

"ZIpgm" <- function(pgm, subdir = "misc", ext = "exe") {
	# Get the path of an executable, giving its name and subdirectory
	if (isWin()) {
		pathpgm <- system.file(subdir, "bin", paste(pgm, ext, sep = "."), package = "zooimage")
		if (!file.exists(pathpgm)) return("") else return(shortPathName(pathpgm))		
	} else {	
		# Change nothing: should be directly executable
		if( pgm == "dc_raw" ) {
			pgm <- "dcraw"
		}
		return(pgm)
	}	
}

# ZIpgm("zip")
# ZIpgm("pgmhist", "netpbm")
# ZIpgm("pnm2biff", "xite")

"ZIpgmhelp" <- function(pgm, subdir = "misc") {
	# Show textual help for executables
	if (isWin()) {
		helpfile <- file.path(system.file(subdir, "doc", package = "zooimage"), paste(pgm, "txt", sep = "."))
		if (!file.exists(helpfile))
			stop("No help found for ", pgm)
		file.show(helpfile, title = paste("Help for ", pgm, " [", subdir, "]", sep = ""))		
	} else {
		system(paste("man", pgm), wait = FALSE)
	}	
}

# ZIpgmhelp("zip")
# ZIpgmhelp("pgmhist", "netpbm")
# ZIpgmhelp("pnm2biff", "xite")

"getDec" <- function() {
	Dec <- getKey("OptionInOutDecimalSep", ".")
	DecList <- c(".", ",")
	# It must be either "." or ","!
	if (!Dec %in% DecList) Dec <- "."
	return(Dec)
}

#' transforms a file extension to a pattern for ignore.case matching of the 
#' extension
#' 
#' @param extension extension (with or without the dot at the beginning)
#' @returns a regular expression pattern that can be used
#'          to match files with this extension
#' @examples
#' extensionPattern( "tif" )
extensionPattern <- function( extension = "tif" ){
  extensionLetters <- substring( extension, 1:nchar(extension), 1:nchar(extension) )
  parts <- paste( "[", extensionLetters, casefold( extensionLetters, upper = TRUE ) , "]", sep = "")
  pattern <- paste( parts, collapse = "" ) 
  if( !length( grep( "^\\.", extension)) ){
	pattern <- paste( "[.]", pattern, sep = "" )
  } 
  paste( pattern, "$", sep = "" )
}

#' Get the current call stack
callStack <- function( ){
	calls <- tail( sys.calls(), -1)
	out <- lapply( calls, function(.) {
		out <- try( as.character(.[[1]] ), silent = TRUE )
		if( inherits( out, "try-error" ) ) NULL else out
	} )
	unlist( out ) 
}

#' masking system so that the warnings related to using windows arguments

system <- function (command, intern = FALSE, ignore.stderr = FALSE, wait = TRUE, 
    input = NULL, show.output.on.console = TRUE, minimized = FALSE, 
    invisible = TRUE){ 
		
		call <- match.call( )
		call[[1]] <- base::system
		suppressWarnings( eval( call , envir = parent.frame() ) )
	
}


#' check if a file exists
#' 
#' @param file file to check
#' @param extension if given the file should have this extension
#' @param message message to give when the file is not found
checkFileExists <- function( file, extension, message = "file not found : %s", force.file = FALSE ){
	message <- sprintf( message, file )
	if( !file.exists( file ) ) stop( message ) 
	if( force.file && file.info( file)$isdir ){
		stop( sprintf( 'file "%s" is a directory', file ) )
	}
	if( !missing(extension) && !grepl( extensionPattern(extension), file ) ){
		message <- sprintf( "%s , or not a '%s' file", message, extension )
		stop( message )
	}
	invisible( NULL )
}

#' checks if a directory exists
#'
#' @param dir the directory to check
#' @param message the message to throw into stop if the directory does
#'  not exists or is not a directory
checkDirExists <- function( dir, message = 'Path "%s" does not exist or is not a directory' ){
	message <- sprintf( message, dir )
	if( !file.exists( dir ) || !file.info(path)$isdir ){
		stop( message )
	}
}


#' test if x inherits from class y
`%of%` <- function( x, y ){
	inherits( x, y )
}

