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

"read.description" <-
	function(zisfile = "Description.zis") {

    ### TODO: a print function for ZIDesc object.

	# Read a *.zis file, and construct a 'ZIDesc' object
	deleteExtraRows <- function(df) {
		Names <- names(df)
		Del <- grep("^X[.][0-9]+$", Names)
		if (length(Del) > 0) Names <- Names[-Del]
		if ("X" %in% Names && all(is.na(df$X))) Names <- Names[Names != "X"]
		return(df[ , Names])
	}

	if (!file.exists(zisfile) || file.info(zisfile)$isdir)
		stop(zisfile, " not found, or not a file!")
	if (length(grep("[.]zis$", tolower(zisfile))) == 0)
		stop(zisfile, " is not a ZooImage sample description file (*.zis)!")
	# check first line for ZI1
	Line1 <- scan(zisfile, character(), nmax = 1, quiet = TRUE)
	if (Line1 != "ZI1")
		stop(zisfile, " does not appear to be ZooImage version 1 file, or it is corrupted!")
	Lines <- scan(zisfile, character(), sep = "\t", skip = 1,
		blank.lines.skip = FALSE, flush = TRUE, quiet = TRUE, comment.char = "#")
	if (length(Lines) < 1)
		stop("The file is empty or corrupted!")
	# Determine the position of the various tables (Series, Cruises, Stations & Samples)
	posSeries <- grep("[[]Series[]]", Lines) + 1
	if (length(posSeries) == 0) stop("[Series] section not found!")
	posCruises <- grep("[[]Cruises[]]", Lines) + 1
	if (length(posCruises) == 0) stop("[Cruises] section not found!")
	posStations <- grep("[[]Stations[]]", Lines) + 1
	if (length(posStations) == 0) stop("[Stations] section not found!")
	posSamples <- grep("[[]Samples[]]", Lines) + 1
	if (length(posSamples) == 0) stop("[Samples] section not found!")
	# Parse all the text before those tables
	data <- Lines[1:(posSeries - 2)]
	Desc <- parse.ini(data, "")
	# Parse these tables
	Series <- data.frame(); Cruises <- data.frame(); Stations <- data.frame()
	Nrows <- posCruises - posSeries - 3
	if (Nrows > 0) {
		Series <- read.table(zisfile, sep = "\t", header = TRUE, skip = posSeries,
			dec = getDec(), blank.lines.skip = FALSE, nrows = Nrows)
		Series <- deleteExtraRows(Series)
		Names <- names(Series)
		if (Names[1] == "X.Code") { Names[1] <- "Code"; names(Series) <- Names }
	}
	Nrows <- posStations - posCruises - 3
	if (Nrows > 0) {
		Cruises <- read.table(zisfile, sep = "\t", header = TRUE, skip = posCruises,
			dec = getDec(), blank.lines.skip = FALSE, nrows = Nrows)
		Cruises <- deleteExtraRows(Cruises)
		Names <- names(Cruises)
		if (Names[1] == "X.Code") { Names[1] <- "Code"; names(Cruises) <- Names }
		# Convert 'Start' and 'End' in dates
		Stations$Start <- as.Date(Stations$Start)
		Stations$End <- as.Date(Stations$End)
	}
	Nrows <- posSamples - posStations - 3
	if (Nrows > 0) {
		Stations <- read.table(zisfile, sep = "\t", header = TRUE, skip = posStations,
			dec = getDec(), blank.lines.skip = FALSE, nrows = Nrows)
		Stations <- deleteExtraRows(Stations)
		Names <- names(Stations)
		if (Names[1] == "X.Code") { Names[1] <- "Code"; names(Stations) <- Names }
		# Convert 'Start' and 'End' in dates
		Stations$Start <- as.Date(Stations$Start)
		Stations$End <- as.Date(Stations$End)
	}
	Samples <- read.table(zisfile, sep = "\t", dec = getDec(),header = TRUE,
		skip = posSamples)
	Samples <- deleteExtraRows(Samples)
	Names <- names(Samples)
	if (Names[1] == "X.Label") { Names[1] <- "Label"; names(Samples) <- Names }
	# Convert 'Date' into a Date
	Samples$Date <- as.Date(Samples$Date)
	# Combine all this in a data frame + metadata
	Meta <- list(Desc = Desc$Description, Series = Series, Cruises = Cruises, Stations = Stations)
	attr(Samples, "metadata") <- Meta
	# This is a "ZIDesc" object
	class(Samples) <- c("ZIDesc", "data.frame")
	return(Samples)
}

"createZis" <-
	function(zisfile = NULL, template = NULL, editor = getOption("ZIEditor"),
	wait = FALSE) {
	# Create a .zis file from a template and edit it
	
    (require(svDialogs) || stop("Package 'svDialogs' from 'SciViews' bundle is required. Please, install it first!"))

	if (is.null(zisfile) || zisfile == "") {
		if (isWin()) {
	        zisfile <- winDialogString("Give a name for the new .zis file:",
				default = "Description.zis")
		} else {	
			zisfile <- guiDlgInput("Give a name for the new .zis file:",
				"ZIS file creation", default = "Description.zis")
		}
		if (is.null(zisfile) || length(zisfile) == 0 || zisfile == "")
			return(invisible())
		if (regexpr("[.][zZ][iI][sS]$", zisfile) < 0)
			zisfile <- paste(zisfile, ".zis", sep = "")
	}
    # If the file already exists, edit current version
	if (file.exists(zisfile)) return(editZis(zisfile, wait = wait))
	Ed <- getOption("ZIEditor")
	if (is.null(Ed) || !file.exists(Ed))
		stop("The metadata editor is not defined or not found!")
	# Look for the template
	if (is.null(template)) {
		# Look for the default template
		Edpath <- dirname(Ed)
		template <- file.path(Edpath, "templates", "Description.zis") 
	}
	if (!file.exists(template))
		stop("Template file '", template, "' not found!")
	if (regexpr("[.][zZ][iI][sS]$", template) < 0)    
		stop("Template '", template, "' is not a valid '.zis' file!")
	# Copy the template into the new file
	file.copy(template, zisfile)
	# Edit this new file
	startPgm("ZIEditor", cmdline = paste("\"", zisfile, "\"", sep = ""), wait = wait)
	return(zisfile)
}

"editZis" <-
	function(zisfile, editor = getOption("ZIEditor"), wait = FALSE) {
	# Edit a .zis file
    if (is.null(zisfile) || zisfile == "") {
		zisfile <- selectFile("Zis")
		if (zisfile == "")
			return(invisible())
	} else {
		if (regexpr("[.][zZ][iI][sS]$", zisfile) < 0)
			stop("This is not a '.zis' file!")
		if (!file.exists(zisfile))
			stop("the file '", zisfile, "' is not found!")
	}
	Ed <- getOption("ZIEditor")
	if (is.null(Ed) || !file.exists(Ed))
		stop("The metadata editor is not defined or not found!")
	# Everything is fine, open the document for editing...
	startPgm("ZIEditor", cmdline = zisfile, wait = wait)
	return(zisfile)
}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

