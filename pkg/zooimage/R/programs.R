## Copyright (c) 2009-2012, Ph. Grosjean <phgrosjean@sciviews.org>
##
## This file is part of ZooImage
## 
## ZooImage is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
## 
## ZooImage is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with ZooImage.  If not, see <http://www.gnu.org/licenses/>.

## Program dispatcher 
program <- function (prog, args, ..., dir)
{	
	prog <- ZIpgm(prog, dir)
	cmd <- paste(prog, sprintf(args, ...), sep = " ") 
	if (isWin()) {
		system(cmd, intern = TRUE, invisible = TRUE)
	} else {
		system(cmd, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
	}
}

## Xite scripts 
xite <- function (prog, args, ...)
	program(prog, args, ..., dir = "xite")

xite_pnm2biff <- function (input, output)
{
	if (!file.exists(output)) {
		xite("pnm2biff", '"%s" "%s"', input, output)
		checkFileExists(output,
			message = 'error converting file "%s" to BIFF format')
	}
}

xite_statistics <- function (file)
{	
	out <- as.numeric(xite( "statistics", '-m "%s" ', file)) 
	if (is.na(out)) {
		stop("Unable to get mean gray value from the blank-field image!")
	}
	return(out)
}

xite_divide <- function (meangray, image, bf, cor)
{	
	out <- xite("divide", ' -s "%s" "%s" "%s"', image, bf, cor)
	checkFileExists(cor, message = "Error while correcting blank-field for %s")
}

xite_biff2tiff <- function (cor, tif)
{
	out <- xite("biff2tiff", ' "%s" "%s"', cor, tif)
	checkFileExists(tif,
		message = "Error while converting corrected image to TIFF format!")
}

## Imagemagick scripts
imagemagick <- function (prog, args, ...)
	program(prog, args, ..., dir = "imagemagick")

imagemagick_identify <- function (file)
{
	size <- imagemagick("identify", '  -format "%s" %s', '%w %h', file)
	size <- as.numeric(strsplit(size, " ")[[1]])
	if (is.na(size) || is.null(size) || length(size) != 2 || size[1] < 100 ||
		size[2] < 100)
		stop("Error while getting image size with 'identify'", file) 
	return(size)
}

imagemagick_convert <- function (file, size1, size2)
	imagemagick("convert", ' "%s" -resize %dx%d -median 2.0 -resize %dx%d! "%s"',
		file, size2[1], size2[2], size1[1], size1[2], file)

misc <- function (prog, args, ...)
	program(prog, args, ..., dir = "misc")

misc_dcraw <- function (file, arguments, output)
{
	## TODO: look for dc_raw differently!
	#checkCapable("dc_raw")
	out <- try(misc("dc_raw", '"%s" %s > "%s" ', file, args, output),
		silent = TRUE)
	if (inherits(out, "try-error"))
		stop(sprintf("error converting '%s' with dc_raw", file))
	return(out)
}

## netpbm scripts
netpbm <- function (prog, args, ...)
	program(prog, args, ..., dir = "netpbm")

netpbm_tifftopnm <- function (input, output)
{
	unlink(output)
	res <- netpbm("tifftopnm", ' -byrow "%s" > "%s" ', input, output) 
	checkFileExists(output, message = "Impossible to convert into .pgm image")
	return(res)
}

netpbm_pgmhist <- function (file, delete = TRUE)
{
	## Create a text file with the gray levels distribution and read it
	res <- netpbm("pgmhist", ' "%s" ', file)
	if (delete) unlink(file)
	if (length(res) < 100)
		stop(sprintf("Error while getting histogram of '%s' ", file))
	res <- res[-(1:2)]	# Eliminate header
	
	## Keep only the two first columns
	res <- sub("\t.*$", "", sub("\t", " ", res))
	
	## Transform into a data frame of numerical values
	BF <- as.data.frame(matrix(as.numeric(unlist(strsplit(res, " "))),
		ncol = 2, byrow = TRUE))
	names(BF) <- c("Gray", "Count")
	return(BF)
}

netpbm_ppmtopgm <- function (ppm, pgm)
{	
	cmd <- if (isWin()) {
		sprintf('%s /c type "%s" | %s > %s', Sys.getenv("COMSPEC"), ppm, 
			ZIpgm("ppmtopgm", "netpbm"), pgm)
	} else {
		sprintf('ppmtopgm < "%s" > "%s" ', ppm, pgm)
	}
	if (isWin()) {
		res <- try(system(cmd, invisible = TRUE), silent = TRUE)
	} else {
		res <- try(system(cmd, wait = FALSE, ignore.stdout = TRUE,
			ignore.stderr = TRUE), silent = TRUE)
	}
	checkFileExists(pgm, message = "problem converting to '%s' using ppmtopgm")
	unlink(ppm) 
	return(res)
}

imageViewer <- function (dir = getwd())
{
	if (isWin()) {
		startPgm("ImageViewer", sprintf('"%s"',
			tools:::file_path_as_absolute(dir)))
	} else if (isMac()) {
		cmd <- sprintf('/Applications/Utilities/XnViewMP.app/Contents/MacOS/xnview "%s"',
			dir)
	} else {
		## TODO: maybe we should not rely on nautilus
		cmd <- sprintf('nautilus --geometry 600x600 "%s"', dir) 
		system(cmd, wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
	}
}
