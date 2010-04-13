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

# Masking system so that the warnings related to using windows arguments
# system <- function (command, intern = FALSE, ignore.stderr = FALSE, wait = TRUE, 
# input = NULL, show.output.on.console = TRUE, minimized = FALSE, 
# invisible = TRUE){
# {		
#  	call <- match.call()
#  	call[[1]] <- base::system
#  	suppressWarnings(eval(call, envir = parent.frame()))
# }

# Various utility functions used by ZooImage
# Get the name of one or several variables of a given class
"getVar" <- function (class = "data.frame", default = "", multi = FALSE,
title = paste("Choose a ", class, ":", sep = ""), warn.only = TRUE)
{	
	# Get one or several variables of a given object class
	varlist <- objects(pos = 1)	# Get objects in .GlobalEnv
	
	# Filter this list to keep only object inheriting a giving class...
	Filter <- NULL
	for (i in 1:length(varlist))
		Filter[i] <- inherits(get(varlist[i]), class)
	
	# Keep only those objects
	varlist <- varlist[Filter]	
	if (length(varlist) == 0) {	# No such objects in .GlobalEnv
		msg <- paste("There is no object of class '",
			paste(class, collapse = " "), "' in the user workspace!", sep = "")
		if (isTRUE(warn.only)) warning(msg) else stop(msg)
		varsel <- "" 
	} else {
		if (default == "") default <- varlist[1]
		varsel <- select.list(varlist, preselect = default, multiple = multi,
			title = title)
	}
    return(varsel)		
}

# Get the name of one or more lists with their components of a given class
# Note: this is used as a collection in other languages
# (there is no such collection in R, so, we use a list here!)
"getList" <- function (class = "data.frame", default = "", multi = FALSE,
title = paste("Choose a list (of ", class, "s):", sep = ""), warn.only = TRUE)
{	
	# Get objects in .GlobalEnv
	filter <- function(x) {
		item <- get(x)
		is.list(item) && all(sapply(item, function(y) inherits(y, class)))
	}
	varlist <- Filter(filter, objects(pos = 1))	
	if (length(varlist) == 0) {
		msg <- paste("There is no list of '", class,
			"' objects in the user workspace", sep = "")
		if (isTRUE(warn.only)) warning(msg) else stop(msg)
		return("")
	}
	if (default == "") default <- varlist[1]
	varsel <- select.list(varlist, preselect = default, multiple = multi,
		title = title)
	return(varsel)		
}

# Select one or several files of a given type
"selectFile" <- function (
type = c("ZipZid", "ZimZis", "LstZid", "Zip", "Zid", "Zim", "Zis", "Zie"),
multi = FALSE, quote = TRUE)
{	
	type <- tryCatch(match.arg(type), error = function (e) {
		stop("unrecognized type")
	})
	Type <- switch(type, "ZipZid" = "Zip/Zid", "ZimZis" = "Zim/Zis",
		"LstZid" = "Lst/Zid", type)
	
	# Adapt title according to 'multi'
	if (isTRUE(multi)) {
    	title <- paste("Select one or several", Type, "files...")
	} else {
		title <- paste("Select one", Type, "file...")
	}
	
	#if (!isWin()) {
		filters <- switch(type,
			ZipZid 	= c("ZooImage files"          , ".zip",
						"ZooImage files"          , ".zid"      ),
			ZimZis 	= c("ZooImage metadata files" , ".zim",
						"ZooImage metadata files" , ".zis"      ),
			LstZid  = c("FlowCAM list files"      , ".lst",
						"ZooImage files"          , ".zid"      ),
			Zip		= c("ZooImage picture files"  , ".zip"      ),
			Zid		= c("ZooImage data files"     , ".zid"      ),
			Zim		= c("ZooImage metadata files" , ".zim"      ),
			Zis		= c("ZooImage sample files"   , ".zis"      ),
			Zie		= c("ZooImage extension files", ".zie"      ))
		filters <- matrix(filters, ncol = 2, byrow = TRUE)
		res <- tk_choose.files(caption = title, multi = multi, filters = filters)
	#} else { # Old treatment using Windows-only function
	#	filters <- switch(type,
	#		ZipZid 	= c("ZooImage files (*.zip;*.zid)"          , "*.zip;*.zid"),
	#		ZimZis 	= c("ZooImage metadata files (*.zim;*.zis)" , "*.zim;*.zis"),
	#		Zip		= c("ZooImage picture files (*.zip)"        , "*.zip"      ),
	#		Zid		= c("ZooImage data files (*.zid)"           , "*.zid"      ),
	#		Zim		= c("ZooImage metadata files (*.zim)"       , "*.zim"      ),
	#		Zis		= c("ZooImage sample files (*.zis)"         , "*.zis"      ),
	#		Zie		= c("ZooImage extension files (*.zie)"      , "*.zie"      ))
	#	filters <- matrix(filters, ncol = 2, byrow = TRUE)
	#	res <- choose.files(caption = title, multi = multi, filters = filters)
	#}
	
	if (length(res) && res != "" && quote)
		res <- paste('"', res, '"', sep = "")
	return(res)
}

# Get a key (permanent configuration data, from the registry if under Windows)
"getKey" <- function (key, default.value = NULL)
{ 	
	# Retrieve a ZooImage key in the registry
	# TODO: should we use this also for windows ?
	if (!isWin()) {
		return(getTemp(sprintf("zooimage-%s", key), default.value))
	}
	
	# Look if the key is defined
	ZIkey <- getTemp("ZIkey")
	if (key %in% tk2reg.values(ZIkey)) {
    	# Get the content of that key
		return(tk2reg.get(ZIkey, key))
	} else return(default.value)
	
}

# Set a key permanently (in the registry, if under Windows)
"setKey" <- function (key, value, type = "sz")
{
	if(!isWin()) {
		# TODO: should we also use this for windows ?
		assignTemp(sprintf("zooimage-%s", key), value, TRUE )
	} else{
		tk2reg.set(getTemp("ZIkey"), key, value, type = "sz")
	}
	return(invisible(TRUE))
}

# Convert underscores into spaces
"underscore2space" <- function (char)
	return(gsub("_", " ", char))

# Trim leading and trailing white spaces and tabs
"trim" <- function (char)
	return(sub("\\s+$", "", sub("^\\s+", "", char)))

# Get the name of a file, without its extension
"noext" <- function (file)
	return(sub("\\.[^.]+$", "", basename(file)))

# Get information about a sample, given its name
"get.sampleinfo" <- function (filename,  type = c("sample", "fraction", "image",
"scs", "date", "id", "frac", "imgnbr"), ext = "_dat1[.]zim$")
{	
	type <- tryCatch( match.arg(type), error = function (e) {
		stop("'type' must be 'sample', 'fraction', 'image', 'scs', 'date', 'id',
		'frac' or 'imgnbr'")
	})
	base <- basename(filename)
	if (ext != "") base <- sub(ext, "", base)
	
	# Filename without extension is supposed to follow the convention:
	# scs.date.id+f[img] with scs.date.id forming an unique sample identifier
	# Note: not all verifications are conducted. So, it sometimes returns a
	# result even if the name does not conform to this specification!
	### TODO: check that the name follows the convention and determine what is
	#         optional, like date, for instance)
	res <- switch(type,
		sample   = sub("\\+[a-zA-Z][0-9.]+$", "", base),
		fraction = sub("[0-9.]+$", "", base),
		image    = base,
		scs      = sub("[+.].+$", "", base),
		date     = as.Date(sub("^.*([0-9]{4}-[0-1][0-9]-[0-3][0-9]).*$", "\\1",
			base)),
		id       = sub("^.*\\..*\\.(.*)\\+.*$", "\\1", base),
		frac     = sub("^.*\\+([a-zA-Z]).*$", "\\1",base),
		imgnbr   = as.numeric(sub("^.*\\+[a-zA-Z]([0-9.]*)$", "\\1", base)),
	)
	return(res)
}

# Calculate equivalent circular diameter (similar to equivalent spherical
# diameter, but for 2D images)
"ecd" <- function (area)
	return(2 * sqrt(area / pi))

# Unique identifiers (Ids) are a combination of Label and Item
"make.Id" <- function (df)
	paste(df$Label, df$Item, sep = "_")

# Calculate derived variables... default function
"calc.vars" <- function (x)
{	
	# This is the calculation of derived variables
	# Note that you can make your own version of this function for more
	# calculated variables!
	
	# A small hack to correct some 0 (which can be problematic in further calcs)
	noZero <- function (x)
		x[x == 0] <- 0.000000001
	# Euclidean distance between two points
	distance <- function (x, y)
		sqrt(x^2 + y^2)
	
	x$Minor <- noZero(x$Minor)
	x$Major <- noZero(x$Major) 
	x$Elongation <- x$Major / x$Minor
	x$CentBoxD <- distance(x$BX + x$Width/2 - x$X, x$BY + x$Height/2 - x$Y)
	x$GrayCentBoxD <- distance(x$BX + x$Width/2 - x$XM, x$BY + x$Height/2 - x$YM)
	x$CentroidsD <- distance(x$X - x$XM, x$Y - x$YM)
	x$Range <- x$Max - x$Min
	x$MeanPos <- (x$Max - x$Mean) / x$Range
	x$SDNorm <- x$StdDev / x$Range
	x$CV <- x$StdDev / x$Mean * 100
	x$Area <- noZero(x$Area)
	x$logArea <- log(x$Area)
	x$Perim. <- noZero(x$Perim.)
	x$logPerim. <- log(x$Perim.)
	x$logMajor <- log(x$Major)
	x$logMinor <- log(x$Minor)
	x$Feret <- noZero(x$Feret)
	x$logFeret <- log(x$Feret)

	return(x)
}

# All sample with at least one entry in a given object
"list.samples" <- function (obj)
{ 	
	mustbe(obj, c("ZIDat", "ZIDesc","ZITrain"))
	
	# List all samples represented in a given object
	if (inherits(obj, "ZIDat")) {
    	res <- sort(unique(get.sampleinfo(as.character(obj$Label),
			type = "sample", ext = "")))
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

# Parse an ini file (.zim, .zie, etc. are .ini files!)
### TODO: manage the case there is no '=' in the data!
"parse.ini" <- function (data, label = "1")
{
	# Parse an ini file (tag=value => 'tag', 'value')
	# and make a list with different sections
	
	# is str a section
	is.section <- function (str)
		as.logical(length(grep("^\\[.+\\]$", trim(str)) > 0))

	# Get the name of a section
	get.section.name <- function (str)
		sub("^\\[", "", sub("\\]$", "", trim(str)))

	# Transform a vector of characters into a data frame,
	# possibly with type conversion
	vector.convert <- function (vec)
		as.data.frame(lapply(as.list(vec), type.convert))

	if (is.null(data) || !inherits(data, "character") || length(data) < 1)
		return(character(0))
	
	# Trim leading and trailing white spaces
	data <- trim(data)
	
	# Convert underscore to space
	data <- underscore2space(data)
	
	# Eliminate empty lines
	data <- data[data != ""]
	data <- paste(data, " ", sep = "")
	if (length(data) < 1) return(character(0))
	# Substitute the first '=' sign by another separator unlikely to appear in
	# the argument
	data <- sub("=", "&&&&&", data)
	
	# Split the strings according to this separator
	data <- strsplit(data, "&&&&&")
	
	# Get a matrix
	data <- t(as.data.frame(data))
	rownames(data) <- NULL
	
	# Make sure we have a section for the first entries (otherwise, use [.])
	if (!is.section(data[1, 1]))
		data <- rbind(c("[.]", "[.]"), data)
	Names <- as.vector(trim(data[, 1]))
	Dat <- as.vector(trim(data[, 2]))
	
	# Determine which is a section header
	Sec <- grep("\\[.+\\]$", Names)
	SecNames <- get.section.name(Names[Sec])
	
	# Make a vector of sections
	if (length(Sec) == 1) {
		SecNames <- rep(SecNames, length(Names))
	} else {
		SecNames <- rep(SecNames, c(Sec[2:length(Sec)],
			length(Names) + 1) - Sec)
	}
	
	# Replace section headers from all vectors
	Names[Sec] <- "Label"
	Dat[Sec] <- label
	names(Dat) <- Names
	
	# Transform SecNames in a factor
	SecNames <- as.factor(SecNames)
	
	# Split Dat on sections
	DatSec <- split(Dat, SecNames)
	
	# For each section, transform the vector in a data frame and possibly
	# convert its content
	DatSec <- lapply(DatSec, vector.convert)
	
	# Eliminate "Label" if it is ""
	if (label == "") DatSec <- lapply(DatSec, function(x) x[-1])
	return(DatSec)
}

# Merge two lists of data frames
"list.merge" <- function (x, y)
{	
	mustallbe(x, y, class = "list")
	
	xitems <- names(x)
	yitems <- names(y)
	xandy <- xitems[xitems %in% yitems]
	xonly <- xitems[!(xitems %in% xandy)]
	yonly <- yitems[!(yitems %in% xandy)]
	
	# First merge common items
	if (length(xandy) > 0) {
		res <- lapply(xandy, function (item) {
			merge(x[[item]], y[[item]], all = TRUE)
		})
		names(res) <- xandy
	} else {
		res <- list()
	}
	
	if (length(xonly) > 0) res[xonly] <- x[xonly]
	if (length(yonly) > 0) res[yonly] <- y[yonly]
	return(res)
}

"combine" <- function (..., .list = list(...))
{
	force(.list)
	mergefun <- function (x, y) {
		if (all(sort(names(x)) == sort(names(y)))) {
			rbind(x, y)
		} else {
			merge(x, y, all = TRUE)
		}
	}
	Reduce(mergefun, .list)
}

# Add items across two lists (names must be the same)
"list.add" <- function (..., .list = list(...))
	list.reduce(.list= .list, FUN = "+")

"list.reduce" <- function (..., .list = list(...), FUN = "+" )
{
	.list <- Filter(Negate(is.null), .list)
	if (length(.list) == 1) return(.list[[1]])
	n <- length(.list[[1]])
	out <- lapply(1:n, function (i) {
		Reduce(FUN, lapply(.list , "[[", i))
	})
	attributes(out) <- attributes(.list[[1]])
	return(out)
}

# Internationalization of ZooImage: get messages in other languages
"gettextZI" <- function (...)
{
	### TODO: internationalization of the package
	#gettext(..., domain = "R-zooimage")
	return(list(...)[[1]])
}

# Display progression of long-running tasks, both on the R console
# and in the ZooImage assistant status bar
"Progress" <- function (value, max.value = NULL)
{
	# My own version of progress() that also uses the Tk window statusbar
    if (!is.numeric(value))
        stop("'value' must be numeric!")
    if (is.null(max.value)) {
        max.value <- 100
        percent <- TRUE
    } else percent <- FALSE

    if (!is.numeric(max.value))
        stop("'max.value' must be numeric or NULL!")
	if (value > max.value)
		stop( "use ClearProgress instead")
    
	Max.Value <- as.character(round(max.value))
    l <- nchar(Max.Value)
    Value <- formatC(round(value), width = l)
    if (percent) {
        backspaces <- backspaces(l + 14)
		message <- paste("Progress: ", Value, "%  ", sep = "")
		cat(backspaces, message, sep = "")
    } else {
        backspaces <- backspaces(2 * l + 16)
        message <- paste("Progress: ", Value, " on ", Max.Value, "  ", sep = "")
		cat(backspaces, message, sep = "")
    }
	flush.console()
	
	# Do we need to update the Tk window statusbar also?
    if ("ZIDlgWin" %in% WinNames()) {
    	assignTemp("statusBusy", TRUE)
		# Calculate fraction and show it in the progress bar
		if (!percent) value <- value / max.value * 100
		tkconfigure(getTemp("statusProg"), value = value)
		# Display the progress text also in the statusbar
		tkconfigure(getTemp("statusText"), text = message)
		.Tcl("update idletasks")
	}
    invisible(NULL)
}

"ClearProgress" <- function ()
{
	cat(backspaces(), "", sep = "")
	if ("ZIDlgWin" %in% WinNames()) {
		rmTemp("statusBusy")
		tkconfigure(getTemp("statusProg") , value = 0)
		tkconfigure(getTemp("statusText") , text = paste("Ready -", getwd()))
	}
	return(invisible(NULL))
}

# Change the working directory and update the ZooImage assistant status bar
"setwd" <- function (dir)
{
	# My own setwd() function that also updates the Tk window statusbar
	if (!is.character(dir)) return(invisible(NULL))
	base::setwd(dir)
	
	# Possibly update the statusbar
	if ("ZIDlgWin" %in% WinNames() && is.null(getTemp("statusBusy"))) {
		tkconfigure(getTemp("statusText"), text = paste("Ready -", getwd()))
		.Tcl("update idletasks")
	}
	
	# Save the current default directory for future use
	setKey("DefaultDirectory", getwd())
}

# Get the path of an executable, giving its name and subdirectory
# ex.: ZIpgm("zip"), ZIpgm("pgmhist", "netpbm"), ZIpgm("pnm2biff", "xite")
"ZIpgm" <- function (pgm, subdir = "misc", ext = "exe")
{	
	if (isWin()) {
		pathpgm <- system.file(subdir, "bin", paste(pgm, ext, sep = "."),
			package = "zooimage")
		if (!file.exists(pathpgm)) return("") else
			return(shortPathName(pathpgm))
	} else {	
		# Change nothing: should be directly executable
		if (pgm == "dc_raw") pgm <- "dcraw"
		return(pgm)
	}	
}

# Show textual help for executables
# ex.: ZIpgmhelp("zip"), ZIpgmhelp("pgmhist", "netpbm")
"ZIpgmhelp" <- function (pgm, subdir = "misc")
{
	# TODO: would it not be better to use the same thing on all platforms
	#       (the doc directory)
	if (isWin()) {
		helpfile <- file.path(system.file(subdir, "doc", package = "zooimage"),
			paste(pgm, "txt", sep = "."))
		if (!file.exists(helpfile))
			stop("No help found for ", pgm)
		file.show(helpfile, title = paste("Help for ", pgm, " [", subdir, "]",
			sep = ""))		
	} else {
		system(paste("man", pgm), wait = FALSE)
	}	
}

"getDec" <- function ()
{
	Dec <- getKey("OptionInOutDecimalSep", ".")
	DecList <- c(".", ",")
	# It must be either "." or ","!
	if (!Dec %in% DecList) Dec <- "."
	return(Dec)
}
