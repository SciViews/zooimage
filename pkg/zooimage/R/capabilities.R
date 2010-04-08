# Copyright (c) 2009, Ph. Grosjean <phgrosjean@sciviews.org>
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

ZOOIMAGEENV <- new.env()

checkCapable <- function (cap)
	if (cap %in% names(capabilities))
		capabilities[[cap]]()

capabilities <- list(
	"zip"        = checkZipAvailable, 
	"unzip"      = checkUnzipAvailable, 
	"zipnote"    = checkZipnoteAvailable,
	"identify"   = checkIdentifyAvailable,
	"convert"    = checkConvertAvailable,
	"ppmtopgm"   = checkPpmtopgmAvailable, 
	"dc_raw"     = checkDcRawAvailable, 
	"pnm2biff"   = checkAvailable_pnm2biff, 
	"divide"     = checkAvailable_divide, 
	"statistics" = checkAvailable_statistics, 
	"biff2tiff"  = checkAvailable_biff2tiff, 
	"java"       = checkAvailable_java
) 

# Various check*Capability functions
# Utility that checks if the zip program is available
checkZipAvailable <- function ()
{
	checkCapabilityAvailable("zip", 
		sprintf('"%s" -h %s', ZIpgm("zip", "misc"),
		if (!isWin()) " > /dev/null" else ""),
		"zip - program from Info-Zip not found!")
}

checkUnzipAvailable <- function ()
{
	checkCapabilityAvailable("unzip", 
		sprintf('"%s" -h %s', ZIpgm("unzip", "misc"),
		if (!isWin()) " > /dev/null" else ""), 
		"unzip - program from Info-Zip not found!")
}

checkZipnoteAvailable <- function ()
{
	checkCapabilityAvailable("zipnote", 
		sprintf('"%s" -h %s', ZIpgm("zipnote", "misc"),
		if(!isWin()) " > /dev/null" else ""), 
		"zipnote - program from Info-Zip not found!")
}

checkIdentifyAvailable <- function ()
{
	checkCapabilityAvailable("identify", 
		sprintf('"%s" -version ', ZIpgm("identify", "imagemagick")), 
		"program not found! Install ImageMagick 16 bit!")
}

checkConvertAvailable <- function ()
{
	checkCapabilityAvailable("convert", 
		sprintf('"%s" -version ', ZIpgm("convert", "imagemagick")), 
		"program not found! Install ImageMagick 16 bit!")
}

checkPpmtopgmAvailable <- function ()
{
	checkCapabilityAvailable("ppmtopgm", 
		sprintf('"%s" -help ', ZIpgm("ppmtopgm", "netpbm")), 
		"ppmtopgm: program not found! Please, install it!")
}

checkDcRawAvailable <- function ()
{
	checkCapabilityAvailable("dc_raw", 
		sprintf('"%s" -help ', ZIpgm("dc_raw", "misc")), 
		"dc_raw: program not found! Please, install it!")
}

checkAvailable_pnm2biff <- function ()
{
	checkCapabilityAvailable("pnm2biff", 
		sprintf('"%s" -version ', ZIpgm("pnm2biff", "xite")), 
		"pnm2biff: program not found! Please, install xite!")
}

checkAvailable_divide <- function ()
{
	checkCapabilityAvailable("divide", 
		sprintf('"%s" -version ', ZIpgm("divide", "xite")), 
		"divide: program not found! Please, install xite!")
}

checkAvailable_statistics <- function ()
{
	checkCapabilityAvailable("statistics", 
		sprintf('"%s" -version ', ZIpgm("statistics", "xite")), 
		"statistics: program not found! Please, install xite!")
}

checkAvailable_biff2tiff <- function ()
{
	checkCapabilityAvailable("biff2tiff", 
		sprintf('"%s" -version ', ZIpgm("biff2tiff", "xite")), 
		"biff2tiff: program not found! Please, install xite!")
}

checkAvailable_java <- function ()
{
	checkCapabilityAvailable("java", 
		'java -version ', 
		"java: program not found! Please, install it!")
}

checkCapabilityAvailable <- function (cap, cmd, msg)
{
  program <- cap
	if (program == "dc_raw" && !isWin()) program <- "dcraw"
	
	# Function called when zip is not available
	stopHere <- function () stop(msg)
	
	# Check if we don't already know about that
	zipCap <- getZooImageCapability(cap)
	if (!is.null(zipCap)) {
		if (!isTRUE(zipCap)) {
			stopHere() 
		} else {
			return(invisible(NULL))
		}
	}
	
	# [RF,20090219] the invisible flag gives a warning outside of windows
	#               and we do not want this warning to be captured by our
	#               error trapping
	ok <- if (isWin()) {
	  	system(cmd, invisible = TRUE) == 0
	} else {
		length(system(sprintf(" which %s 2> /dev/null" , program),
			intern = TRUE)) > 0
	}
				
	# Cache the result for next time, so that we don't have to check again
	arguments <- list(cap = ok)
	names(arguments) <- cap
	zooImageCapabilities(arguments)
	if (!ok) stopHere()
} 

getZooImageCapability <- function (cap = "zip")
	ZOOIMAGEENV[[cap]]

zooImageCapabilities <- function (...)
{
	dots <- list(...)
	if (length(dots) == 1 && is.list(dots[[1]]))
		dots <- dots[[1]]
	snapshot <- structure(as.list(ZOOIMAGEENV), class = "zooimagecapabilities")
  
	if (length(dots)) {
		# Checking that dots have names
		if (is.null(names(dots)) || any(names(dots) == ""))
			stop("capabilities must have names")
  	 
		# Checking that each capability is a logicial of length one
		check <- function (x)
			is.logical(x) && length(x) == 1
	
		if (any(!sapply(dots, check)))
			stop("capability are logicals of length one")
  	
		# Store the capability in the ZOOIMAGEENV environment
		for (cap in names(dots))
			ZOOIMAGEENV[[cap]] <- dots[[cap]] 
	}
	return(snapshot) 
}
