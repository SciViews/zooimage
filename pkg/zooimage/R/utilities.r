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
    
# {{{ warnOrStop
#' warns or stops
warnOrStop <- function( ..., warn.only = get("warn.only", parent.frame() ) ){
	if( is.null(warn.only ) ) warn.only <- TRUE
	msg <- paste( ..., sep = "" )
	if( warn.only ) warning( msg ) else stop( msg )
	invisible( NULL )
}
# }}}

# {{{ getVar
#' Get the name of one or several variables of a given class
"getVar" <- function(class = "data.frame", default = "", multi = FALSE,
	title = paste("Choose a ", class, ":", sep = ""), warn.only = TRUE) {
	
	# Get one or several variables of a given object class
	(require(utils) || stop("Package 'utils' is required!"))
	varlist <- objects(pos = 1)		# Get objects in .GlobalEnv
	
	# Filter this list to keep only object inheriting a giving class...
	Filter <- NULL
	for (i in 1:length(varlist)) {
		Filter[i] <- inherits(get(varlist[i]), class)
	}
	
	# Keep only those objects
	varlist <- varlist[Filter]	
	if (length(varlist) == 0) {	# No such objects in .GlobalEnv
		warnOrStop( "There is no object of class '", paste(class, collapse = " "), "' in the user workspace!" )
		varsel <- "" 
	} else {
		if (default == "") default <- varlist[1]
		varsel <- select.list(varlist, preselect = default, multiple = multi, title = title)
	}
    return(varsel)		
}
# }}}

# {{{ getList
#' Get the name of one or several lists with all of their components of a given class
#' Note: this is used as a collection in other languages (no such collection in R!)
"getList" <- function(class = "data.frame", default = "", multi = FALSE,
	title = paste("Choose a ", class, ":", sep=""), warn.only = TRUE) {
	
	# Get lists of items of specified class
	(require(utils) || stop("Package 'utils' is required!"))
	
	# Get objects in .GlobalEnv
	filter <- function(x) {
		item <- get(x)
		is.list(item) && all( sapply( item, function(y) inherits( y, class ) ) )
	}
	varlist <- Filter( filter , objects(pos = 1) )	
	if( length(varlist) == 0 ){
		warnOrStop( "There is no list of ", class, " objects in the user workspace" )
		return("")
	}
	if (default == ""){
		default <- varlist[1]
	}
	varsel <- select.list(varlist, preselect = default, multiple = multi, title = title)
	return(varsel)		
}
# }}}

# {{{ selectFile
#' Select one or several files of a given type
"selectFile" <- function(
	type = c("ZipZid", "ZimZis", "Zip", "Zid", "Zim", "Zis", "Zie"),
	multi = FALSE, quote = TRUE) {
	
	type <- tryCatch( match.arg( type ), error = function(e){
		stop( "unrecognized type" )
	})
	Type <- switch( type,  "ZipZid" = "Zip/Zid",  "ZimZis" = "Zim/Zis", type )
	
	# Adapt title according to 'multi'
	if (multi) {
    	title <- paste("Select one or several", Type, "files...")
	} else {
		title <- paste("Select one", Type, "file...")
	}
	filters <- switch(type,
		ZipZid 	= c("ZooImage files (*.zip;*.zid)"          , "*.zip;*.zid"),
		ZimZis 	= c("ZooImage metadata files (*.zim;*.zis)" , "*.zim;*.zis"),
		Zip		= c("ZooImage picture files (*.zip)"        , "*.zip"      ),
		Zid		= c("ZooImage data files (*.zid)"           , "*.zid"      ),
		Zim		= c("ZooImage metadata files (*.zim)"       , "*.zim"      ),
		Zis		= c("ZooImage sample files (*.zis)"         , "*.zis"      ),
		Zie		= c("ZooImage extension files (*.zie)"      , "*.zie"      ))
	
	res <- choose.files(caption = title, multi = multi, filters = filters )
	if (res != "" && quote)  {
		res <- paste('"', res, '"', sep = "")
	}
	return(res)
}
# }}}

# {{{ getKey / setKey

# Get a key in the registry (retrieve ZooImage configuration data)
ziKey <- function( key ){
	sprintf( "zooimage-%s", key )
}

"getKey" <- function(key, default.value = NULL) {
 	
	# Retrieve a ZooImage key in the registry
	# TODO: should we use this also for windows ?
	if (!isWin()) {
		return( getTemp( ziKey(key) , default.value) )
	}
	
	# Look if the key is defined
	ZIkey <- getTemp("ZIkey")
	if (key %in% tk2reg.values(ZIkey)) {
    	# Get the content of that key
		return(tk2reg.get(ZIkey, key))
	} else return(default.value)
	
}

# Set a key in the registry (store configuration data for next ZooImage session)
"setKey" <- function(key, value, type = "sz") {
	if(!isWin()) {
		# TODO: should we also use this for windows ?
		assignTemp( ziKey( key), value, TRUE )
	} else{
		tk2reg.set(getTemp("ZIkey"), key, value, type = "sz")
	}
	return(invisible(TRUE))
}
# }}}

# {{{ Text manipulation
#' Convert underscores into spaces
"underscore2space" <- function(char) {
	# Convert underscores to spaces in strings (underscore is used in calltips
	# in the ZooImage Metadata Editor, because of a bug in this program)
	gsub("_", " ", char)
}

#' Trim leading and trailing white spaces and tabs
"trim" <- function(char) {
	sub("\\s+$", "", sub("^\\s+", "", char))
}

#' Get the name of a file, without its extension
"noext" <- function(file) {
	# Get basename without extension
	sub("\\.[^.]+$", "", basename(file))
}

# }}}

# {{{ get.sampleinfo
# Get information about a sample, given its name
"get.sampleinfo" <- function(filename, 
	type = c("sample", "fraction", "image", "scs", "date", "id", "frac", "imgnbr"),
	ext = "_dat1[.]zim$") {
	
	type <- tryCatch( match.arg(type), error = function(e){
		stop("'type' must be 'sample', 'fraction', 'image', 'scs', 'date', 'id', 'frac' or 'imgnbr'")
	} )
	base <- basename(filename)
	if (ext != ""){
		base <- sub(ext, "", base)
	}
	
	# filename without extension is supposed to follow the convention: scs.date.id+f[img]
	# with scs.date.id forming an unique sample identifier
	# Note: not all verifications are conducted. So, it sometimes returns a result even if the name does
	# not conform to this specification!
	### TODO: check that the name follows the convention and determine what is facultative, like date, for instance)
	res <- switch(type,
		sample     = sub("\\+[a-zA-Z][0-9.]+$", "", base),
		fraction   = sub("[0-9.]+$", "", base),
		image      = base,
		scs        = sub("^[^+.]*[+.].+$", "", base),
		date       = as.Date(sub("^.*([0-9]{4}-[0-1][0-9]-[0-3][0-9]).*$", "\\1", base)),
		id         = sub("^.*\\..*\\.(.*)\\+.*$", "\\1", base),
		frac       = sub("^.*\\+([a-zA-Z]).*$", "\\1",base),
		imgnbr     = as.numeric(sub("^.*\\+[a-zA-Z]([0-9.]*)$", "\\1", base)),
		)
	return(res)
}
# }}}

# {{{ ecd
#' Calculate equivalence circular diameter (similar to equivalent spherical diameter, but for 2D images)
"ecd" <- function(area) {
	return(2 * sqrt(area / pi))
}
# }}}

# {{{ make.Id
#' Unique identifiers (Ids) are a combination of Label and Item
"make.Id" <- function(df) {
	# Make a list of Ids, combining "Label" and "Item"
	paste(df$Label, df$Item, sep = "_")
}
# }}}

# {{{ calc.vars
#' Calculate derived variables... default function
"calc.vars" <- function(x) {
	
	# This is the calculation of derived variables
	# Note that you can make your own version of this function for more calculated variables!
	
	# A small hack to correct some 0 for Minor and Major
	hack <- function( x ){
		x[ x == 0 ] <- 0.000000001
	}
	distfun <- function( x, y ){
		sqrt( x^2 + y^2 )
	}
	
	within( x, {
		Minor               <- hack( Minor )
		Major               <- hack( Major ) 
		Elongation          <- Major / Minor
		CentBoxD            <- distfun( BX + Width/2 - X  , BY + Height/2 - Y  )
		GrayCentBoxD        <- distfun( BX + Width/2 - XM , BY + Height/2 - YM )
		CentroidsD          <- distfun( X            - XM , Y             - YM )
		Range               <- Max - Min
		MeanPos             <- (Max - Mean) / Range
		SDNorm              <- StdDev / Range
		CV                  <- StdDev / Mean * 100
		Area                <- hack( Area )
		logArea             <- log(Area)
		Perim.              <- hack( Perim. )
		logPerim.           <- log(Perim.)
		logMajor            <- log(Major)
		logMinor            <- log(Minor)
		Feret               <- hack( Feret )
		logFeret            <- log(Feret)
	} )
}
# }}}

# {{{ list.samples
#' All sample with at least one entry in a given object
"list.samples" <- function(obj) {
 	
	mustbe( obj, c("ZIDat", "ZIDesc","ZITrain") )
	
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
	
}
# }}}

# {{{ parse.ini
#' Parse an ini file (.zim, .zie, etc.) are ini files!
### TODO: manage the case there is no '=' in the data!
"parse.ini" <- function(data, label = "1") {
	# Parse an ini file (tag=value => 'tag', 'value') and make a list with different sections
	
	# is str a section
	is.section <- function(str){
		as.logical( length(grep("^\\[.+\\]$", trim(str)) > 0))
	}

	# Get the name of a section
	get.section.name <- function(str){
		sub("^\\[", "", sub("\\]$", "", trim(str)))
	}

	# Transform a vector of characters into a data frame, possibly with type conversion
	vector.convert <- function(vec) {
		as.data.frame(lapply(as.list(vec), type.convert))
	}

	if (is.null(data) || !inherits(data, "character") || length(data) < 1){
		return(character(0))
	}
	
	# Trim leading and trailing white spaces
	data <- trim(data)
	
	# Convert underscore to space
	data <- underscore2space(data)
	
	# Eliminate empty lines
	data <- data[data != ""]
	data <- paste(data, " ", sep = "")
	if (length(data) < 1){
		return(character(0))
	}
	# Substitute the first '=' sign by another separator unlikely to appear in the argument
	data <- sub("=", "&&&&&", data)
	
	# Split the strings according to this separator
	data <- strsplit(data, "&&&&&")
	
	# Get a matrix
	data <- t(as.data.frame(data))
	rownames(data) <- NULL
	
	# Make sure we have a section for the first entries (otherwise, use [.])
	if (!is.section(data[1, 1])) {
		data <- rbind(c("[.]", "[.]"), data)
	}
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
	if (label == "") {
		DatSec <- lapply(DatSec, function(x) x[-1])
	}
	return(DatSec)
}
# }}}

# {{{ list.merge
#' Merge two lists of data frames
"list.merge" <- function(x, y) {
	
	mustallbe( x, y, class = "list" )
	
	xitems <- names(x)
	yitems <- names(y)
	xandy <- xitems[xitems %in% yitems]
	xonly <- xitems[!(xitems %in% xandy)]
	yonly <- yitems[!(yitems %in% xandy)]
	
	# First merge common items
	if (length(xandy) > 0) {
		res <- lapply( xandy, function(item){
			merge( x[[item]], y[[item]], all = TRUE )
		})
		names( res ) <- xandy
	} else{
		res <- list()
	}
	
	if( length(xonly)>0 ){
		res[ xonly ] <- x[ xonly ]
	}
	if( length(yonly)>0 ){
		res[ yonly ] <- y[ yonly ]
	}
	res
}
# }}}

# {{{ Add items across two lists (names must be the same)
list.add <- function( ..., .list = list(...), FUN = "+"){
	list.reduce( .list= .list, FUN = FUN)
}

list.reduce <- function( ..., .list = list(...), FUN = "+" ){
	.list <- .list[ !sapply( .list, is.null) ]
	if( length(.list) == 1 ) return( .list[[1]] )
	n <- length( .list[[1]] )
	out <- lapply( 1:n, function(i){
		Reduce( FUN, lapply( .list , "[[", i  ) )
	} )
	attributes( out ) <- attributes( .list[[1]] )
	out
}
# }}}

getSample <- function( x, unique = FALSE, must.have, msg){
	res <- sub("[+].*", "", as.character(x))
	if( unique ){
		res <- unique( res )
	}
	if( !missing(must.have) ){
		if( ! all( must.have %in% res ) ){
			if( missing(msg) ){
				msg <- sprintf( "sample '%s' not in ZIDat", paste(must.have, sep = ",") )
			}
			stop( msg )
		}
	}
	res
}


# {{{ Internationalization of ZooImage: get messages in other languages
"gettextZI" <- function(...) {
	### TODO: internationalization of the package
	#gettext(..., domain = "R-zooimage")
	return(list(...)[[1]])
}
# }}}

# {{{ Progress
#' Display progression of long-running tasks, both on the R console
#' and in the ZooImage assistant status bar
"Progress" <- function(value, max.value = NULL) {
	# This is my own version of progress() that uses also the Tk window statusbar
    if (!is.numeric(value)){
        stop("`value' must be numeric!")
	}
    if (is.null(max.value)) {
        max.value <- 100
        percent <- TRUE
    } else percent <- FALSE
    if (!is.numeric(max.value)){
        stop("`max.value' must be numeric or NULL!")
	}
    erase.only <- (value > max.value)
    Max.Value <- as.character(round(max.value))
    l <- nchar(Max.Value)
    Value <- formatC(round(value), width = l)
    if (percent) {
        backspaces <- backspaces( l + 14 )
		if (erase.only) { 
            message <- ""
        } else {
			message <- paste("Progress: ", Value, "%  ", sep = "")
		}
        cat(backspaces, message, sep = "")
    } else {
        backspaces <- backspaces( 2 * l + 16)
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
ClearProgress <- function( ){
	cat(backspaces(), "", sep = "")
	if ("ZIDlgWin" %in% WinNames()) {
		rmTemp("statusBusy")
		tkconfigure( getTemp("statusProg") , value = 0)
		tkconfigure( getTemp("statusText") , text = paste("Ready -", getwd()))
	}
	invisible( NULL )
}
backspaces <- function( n = getOption("width") ){
	paste( rep("\b",  ), collapse = "" )
}
# }}}

# {{{ Setwd
#' Change the working directory and update the ZooImage assistant status bar
"Setwd" <- function(dir) {
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
# }}}

# {{{ ZIpgm
#' Get the path of an executable, giving its name and subdirectory
#' @examples 
#' ZIpgm("zip")
#' ZIpgm("pgmhist", "netpbm")
#' ZIpgm("pnm2biff", "xite")
"ZIpgm" <- function(pgm, subdir = "misc", ext = "exe") {
	
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
# }}}

# {{{ ZIpgmhelp
#' Show textual help for executables
#' @examples
#' ZIpgmhelp("zip")
#' ZIpgmhelp("pgmhist", "netpbm")
#' ZIpgmhelp("pnm2biff", "xite")
"ZIpgmhelp" <- function(pgm, subdir = "misc") {
	if (isWin()) {
		helpfile <- file.path(system.file(subdir, "doc", package = "zooimage"), paste(pgm, "txt", sep = "."))
		if (!file.exists(helpfile))
			stop("No help found for ", pgm)
		file.show(helpfile, title = paste("Help for ", pgm, " [", subdir, "]", sep = ""))		
	} else {
		system(paste("man", pgm), wait = FALSE)
	}	
}
# }}}

# {{{ getDec
"getDec" <- function() {
	Dec <- getKey("OptionInOutDecimalSep", ".")
	DecList <- c(".", ",")
	# It must be either "." or ","!
	if (!Dec %in% DecList) Dec <- "."
	return(Dec)
}
# }}}


#' Get the current call stack
callStack <- function( ){
	calls <- sys.calls()
	out <- lapply( calls, function(.) {
		out <- try( as.character(.[[1]] ), silent = TRUE )
		if( inherits( out, "try-error" ) ) NULL else out
	} )
	out <- unlist( out[ !sapply( out, is.null ) ] )
	out
}

#' masking system so that the warnings related to using windows arguments
system <- function (command, intern = FALSE, ignore.stderr = FALSE, wait = TRUE, 
    input = NULL, show.output.on.console = TRUE, minimized = FALSE, 
    invisible = TRUE){ 
		
		call <- match.call( )
		call[[1]] <- base::system
		suppressWarnings( eval( call , envir = parent.frame() ) )
	
}

# {{{ File utilities
#' checks if the file has the extension
hasExtension <- function( file, extension = "zip", pattern = extensionPattern(extension ) ){
	grepl( pattern, file )
}

#' list files with given extension
#' 
#' @param dir directory to list files
#' @param extension file extension to accept. This will be 
#' modified by extensionPattern so that the test is case independent
list.files.ext <- function( dir, extension = "zip", pattern = extensionPattern(extension), ... ){
	checkDirExists( dir )
	out <- list.files( dir, pattern = pattern , ... )
	out
}

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


#' transforms a file extension to a pattern for ignore.case matching of the 
#' extension
#' 
#' @param extension extension (with or without the dot at the beginning)
#' @returns a regular expression pattern that can be used
#'          to match files with this extension
#' @examples
#' extensionPattern( "tif" )
extensionPattern <- function( extension = "tif", add.dot = !grepl("[.]", extension) ){
  extensionLetters <- substring( extension, 1:nchar(extension), 1:nchar(extension) )
  parts <- ifelse( extensionLetters %in% c(letters, LETTERS ), 
  	paste( "[", extensionLetters, casefold( extensionLetters, upper = TRUE ) , "]", sep = ""), 
	extensionLetters )
  pattern <- paste( parts, collapse = "" ) 
  if( add.dot ){
	pattern <- paste( ".", pattern, sep = "" )
  }
  pattern <- gsub( "[.]", "[.]", pattern )
  paste( pattern, "$", sep = "" )
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
		message <- sprintf( "'%s' is not a '%s' file", file, extension )
		stop( message )
	}
	invisible( NULL )
}


checkAllFileExist <- function( files, extension){
	if( !all( file.exists( files ) ) ){
		stop( "one or more file does not exist" )
	}
	if( !missing(extension) && ! all( hasExtension( files, extension ) ) ){
		stop( "one or more files have wrong extension" )
	}
}

#' checks if a directory exists
#'
#' @param dir the directory to check
#' @param message the message to throw into stop if the directory does
#'  not exists or is not a directory
checkDirExists <- function( dir, message = 'Path "%s" does not exist or is not a directory' ){
	message <- sprintf( message, dir )
	if( !file.exists( dir ) || !file.info(dir)$isdir ){
		stop( message )
	}
}

checkEmptyDir <- function( dir, message = "not empty" ){
	
	if( file.exists( dir ) ){
		if( length( list.files( dir, all.files = TRUE ) > 0 ) ){
			stop( message )
		}
	} else{
		force.dir.create( dir )
	}
	
}


#' force creation of a directory
#'
#' First, if the path exists but is not a directory, this stops.
#' Then, if it did not exist, it calls dir.create to attempt to create it
#' If the creation was not sucessful, it stops
#' 
#' @param path the path of the directory to create
force.dir.create <- function( path, ... ){
	
	if( file.exists( path ) && !file.info(path)$isdir ){
		stop ( "not a directory" )
	}
	out <- dir.create( path, ... )
	if( !out ){
		stop( "could not create directory" )
	}
	out
}

#' checks the first line of a file against some expected content
checkFirstLine <- function( file, expected = "ZI1", 
	message = 'file "%s" is not a valid ZooImage version 1 file', stop = FALSE ){
	Line1 <- scan(file, character(), nmax = 1, quiet = TRUE)
	res <- Line1 == expected
	if( !res && stop){
		message <- sprintf( message, file )
		stop( message )
	}
	invisible( res )
}

list.dir <- function( dir, ... ){
	out <- list.files( dir )
	out[ file.info( file.path( dir, basename(out) ) )$isdir ]
}


# }}}

# {{{ binary operators
#' test if x inherits from class y
`%of%` <- function( x, y ){
	inherits( x, y )
}
# }}}


mustbe <- function( x, class, msg ){
	if( !any( sapply( class, function( cl ) inherits( x, cl) ) ) )
	if( length(class) == 1){
		if( missing(msg) ) {
			msg <- sprintf( "'%s' must be a '%s' object" , deparse( substitute(x)) , as.character(class) )
		}
		stop( msg )
	} else{
		if( missing(msg) ){
			msg <- paste( "'%s' must be of one of these classes: ", deparse( substitute(x)), paste( class, collapse = ", "), sep = "" )
		}
		stop( msg )
	}
}

mustallbe <- function( ..., .list = list(...), class, msg ){
	invisible( lapply( .list, mustbe, class = class, msg = msg) )
}

mustmatch <- function( x, y, msg ){
	if( !all( sort( x )  == sort( y ) ) ){
		if( missing(msg) ) msg <- sprintf( "'%s' and '%s' must match", deparse(substitute(x)), deparse(substitute(y)) )
		stop( msg )
	}
	invisible( NULL )
}

mustallmatch <- function( ..., .list = list(...), msg = "all must match" ){
	n <- length(.list)
	if( n==0 || n == 1 ) {
		stop("need at list 2 elements")
	}
	first <- .list[[1]]
	for( i in 2:n){
		mustmatch( first, .list[[i]], msg = msg )
	}
	invisible( NULL )
}

mustcontain <- function( container, element, msg ){
	if( ! all(element %in% container) ){
		if( missing(msg) ){
			msg <- sprintf( "'%s' must contain '%s'", deparse( substitute( container)), deparse(substitute(element)) )
			stop( msg )
		}
	}
}

mustbeString <- function( x, length){
	if( !is.character( x ) ){
		stop( sprintf( "%s must be a character string", deparse( substitute(x)) ) )
	}
	if( !missing(length) && !length(x) == length ){
		stop( sprintf( "%s must be a character string of length %d", deparse( substitute(x)), length ) )
	}
}


# a version that stops
require <- function( ... ){
	withCallingHandlers( base:::require(...), 
		warning = function( e ){
			base:::stop( e )
		} )
}

if( !isWin() ){
	# choose.files is only available on windows, so we fall 
	# back on tcl-tk equivalent function
	choose.files <- function( default = "", caption = "Select files",
	     multi = TRUE, filters = Filters,
		 index = nrow(Filters) ){
		
		call <- match.call( )
		call[[1]] <- as.name( "tk_choose.files")
		eval( call, envir = parent.frame() )	
	}
}


#' get a template file from the "ZITemplate" option
template <- function( file = "default.zim", dir = getOption("ZITemplates") ){
	f <- file.path( dir, file )
	checkFileExists( f, "template file '%s' does not exist" )
	f
}




# {{{ finish.loopfunction
#' Called at the looping function (*.all)
#' 
#' @param ok logical; TRUE if there was a problem
#' @param ok.console.msg the message to write to the console if ok is TRUE
#' @param ok.log.msg the message to write to the log file if ok is TRUE
#' @param nok.console.msg the message to write to the console is ok is FALSE
#' @param nok.log.msg the message to write to the log when ok is FALSE
#' @param show.log logical; if TRUE the log file is shown at the end
#' @param show.console logical; if TRUE messages are written to the console
#' @return ok, invisibly
finish.loopfunction <- function(
	ok = TRUE, 
	ok.console.msg     	 = "-- Done! --\n" ,
	ok.log.msg 	 = "\n-- OK, no error found. --", 
	nok.console.msg    	 = " -- Done! [ERROR(S) FOUND] --\n", 
	nok.log.msg  = "-- Error(s)! --", 
	bell = TRUE,
	show.log = FALSE, 
	show.console = TRUE){

	# {{{ \a rings the bell on most platforms!
	Bell <- if (bell) "\a" 
	# }}}
	
	# {{{ dispatch
	if (ok) {
		logProcess( ok.log.msg )
		if( show.console ) cat(Bell, ok.console.msg , sep = "" )
	} else {
		logProcess( nok.log.msg)
		if( show.console ) cat(Bell, nok.console.msg, sep = "" )
	}
	# }}}
	
	# {{{ show the log if needed
	if (show.log) logView()
	# }}}
	
	invisible( ok )
}
# }}}

#' import grepl from the future 2.9.0 version
grepl <- if( as.numeric( version$major ) >= 2 && as.numeric( version$minor >= 9) )
	base:::grepl else function (pattern, x, ignore.case = FALSE, extended = TRUE, perl = FALSE,
	    fixed = FALSE, useBytes = FALSE) {
	    index <- grep( pattern, x, ignore.case = ignore.case, 
			extended = extended, perl = perl, fixed = fixed, useBytes = useBytes )
		if( length( index ) == 0 ) return( rep( FALSE, length( x ) ) )
		replace( rep( FALSE, length(x) ), index, TRUE )
	} 

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:
