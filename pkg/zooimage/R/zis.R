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

"read.description" <- function (zisfile = "Description.zis", 
expected.sections = c("Description", "Series", "Cruises", "Stations",
"Samples"))
{
    checkFileExists(zisfile, extension = "zis", force.file = TRUE)
	checkFirstLine(zisfile)
	rl <- readLines(zisfile)
	if (!length(rl) > 1)
		stop("The file is empty or corrupted!")
	positions <- grep("^[[].*[]]", rl)
	names <- sub("^[[](.*)[]]", "\\1", rl[positions])
	if (!all(expected.sections %in% names))
		stop("Incorrect zis file, does not have all expected sections")
	start <- positions + 1
	end <- c(tail(positions, -1) - 2, length(rl))
	data <- lapply(1:length(start), function (i) {
		if (names[i] == "Description") {
			rx <- "^(.*?)=(.*)$"
			txt <- rl[start[i] : end[i]] 
			variables <- sub(rx, "\\1", txt)
			values <- sub(rx, "\\2", txt)
			out <- data.frame(matrix(values, nr = 1))
			names(out) <- variables
		} else {
			con <- textConnection(rl[start[i] : end[i]])
			out <- read.table(con, 
				sep = "\t", header = TRUE,
				dec = getDec(), blank.lines.skip = FALSE)
			close(con)
			
			names(out)[1] <- sub("^X\\.", "", names(out)[1])
			out <- out[, !grepl("^X\\.[0-9]+", names(out))]
		}
		return(out)
	})
	names(data) <- names
	Samples <- data[["Samples"]]
	Samples$Date <- as.Date(Samples$Date)
	Series <- data[["Series"]]
	Cruises <- data[["Cruises"]]
	Cruises$Start <- as.Date(Cruises$Start)
	Cruises$End <- as.Date(Cruises$End)
	Stations <- data[["Stations"]]
	Stations$Start <- as.Date(Stations$Start)
	Stations$End <- as.Date(Stations$End)
	Description <- data[["Description"]]
	
	# Combine all this in a data frame + metadata
	structure(Samples, 
		metadata =  list(Desc = Description, Series = Series, Cruises = Cruises,
			Stations = Stations),
		class = c("ZIDesc", "data.frame"))
}

# Create a .zis file from a template and edit it
"createZis" <- function (zisfile = NULL, template = NULL,
editor = getOption("ZIEditor"), wait = FALSE)
{	
	# Use a ui to get the file name
	if (is.null(zisfile) || zisfile == "") {
		zisfile <- dialogString("Give a name for the new .zis file:",
			"ZIS file creation", default = "Description.zis")
		if (is.null(zisfile) || length(zisfile) == 0 || zisfile == "")
			return(invisible())
		if (!hasExtension(zisfile, "zis"))
			zisfile <- paste(zisfile, ".zis", sep = "")
	}
	
    # If the file already exists, edit current version
	if (file.exists(zisfile))
		return(editZis(zisfile, wait = wait))
	
	# Look for the template
	if (is.null(template)) {
		template <- template("Description.zis")
	} else {
		checkFileExists(template, "template '%s' not found", extension = "zis")
	}
	# Copy the template into the new file
	file.copy(template, zisfile)
	
	# Edit this new file
	editor(zisfile, editor = editor)
}

"editZis" <- function (zisfile, editor = getOption("ZIEditor"), wait = FALSE)
{
	# Edit a .zis file
    if (is.null(zisfile) || zisfile == "") {
		zisfile <- selectFile("Zis")
		if (zisfile == "")
			return(invisible())
	} else {
		checkFileExists(zisfile, message = "the file '%s' is not found!",
			extension = "zis")
	}
	editor(zisfile, editor = editor)
	return(zisfile)
}
