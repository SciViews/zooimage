## Copyright (c) 2004-2012, Ph. Grosjean <phgrosjean@sciviews.org>
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

## Zip a .tif image and embed the corresponding .zim file as comment
## This requires the 'zip' program!
zipImg <- function (imagefile, zimfile = NULL, verify.zimfile = TRUE,
replace = FALSE, delete.source = TRUE, check.zip = TRUE, show.log = TRUE)
{
	## We need to switch to the root of sample dir for correct path in the zip file
	imagefile <- imagefile[1]
	inidir <- getwd()
	setwd(dirname(imagefile))
	on.exit(setwd(inidir))
	rootdir <- getwd()
	imagefile <- basename(imagefile)

	## Check if imagefile exists
	if (!checkFileExists(imagefile, message = "%s doesn't exist, or is a directory!",
		force.file = TRUE))
		return(invisible(FALSE))

	## Is there an associated .zim file?
	if (is.null(zimfile)) {
		sample.info <- sampleInfo(imagefile, "fraction",
			ext = extensionPattern("tif"))
		zimfile <- paste(sample.info, ".zim", sep = "")
	}

	### TODO: the zim file can be other parts of it , like Sample+A1.zim,
	###       instead of Sample+A.zim!
	if (!file.exists(zimfile))
		stop("creation of .zim file not implemented yet!")

	## Recheck .zim file
	if (!checkFileExists(zimfile, message = "%s - doesn't exist or is corrupted!"))
		return(invisible(FALSE))

	## Verify the content of the .zim file
	if (verify.zimfile && zimVerify(zimfile) != 0)
		stop(sprintf("%s appears to be corrupted!", zimfile))

	## Zip the image in the '_raw' subdir and add the information from the .zim
	## file as comment
	zipfile <- paste(noExtension(imagefile), ".zip", sep = "")
	zipfile <- file.path(".", "_raw", zipfile)
	## Make sure that "_raw" subdir exists
	if (!forceDirCreate("_raw")) return(invisible(FALSE))

	## Copy or move the image to a .zip compressed file
	## TODO: how to include the comment in the zip file with the standard R zip() function?
	#zip(zipfile, imagefile, comment.file = zimfile,
	#	delete.zipfile.first = replace)
	zip(zipfile, imagefile)

	## Invisibly indicate success
	## Note: the .zim file is never deleted, because it can be used for other
	## purposes!
	return(invisible(TRUE))
}

## Compress all .tif images in the corresponding directory
## (at least those with an associated .zim file)
zipImgAll <- function (path = ".", images = NULL, check = TRUE,
replace = FALSE, delete.source = replace, show.log = TRUE, bell = FALSE)
{
	## This requires the 'zip' program!
	## Make sure it is available
	checkCapable("zip")

	## First, switch to that directory
	inidir <- getwd()
	checkDirExists(path)
	setwd(path)
	on.exit(setwd(inidir))
	path <- getwd()	# Indicate we are now in the right path

	## Get the list of images to process
	if (is.null(images))	# Compute them from path
		images <- dir(path, pattern = extensionPattern("tif")) # All .tif files

	## Make sure there is no path associated
	if (!all(images == basename(images)))
		stop("You cannot provide paths for 'images', just file names")

	## If there is no images in this dir, exit now
	if (is.null(images) || length(images) == 0)
		stop("There is no images to process in ", getwd())

	## Look at associated .zim files
	zimfiles <- paste(sampleInfo(images, "fraction",
		ext = extensionPattern("tif") ), ".zim", sep = "")
	keep <- file.exists(zimfiles)
	if (!any(keep))
		stop("You must create .zim files first (ZooImage Metadata)!")
	if (!all(keep)) {
    	warning(sum(!keep), " on ", length(keep),
			" images have no .zim file associated and will not be processed!")
		images <- images[keep]
		zimfiles <- zimfiles[keep]
	}

	## Check the zim files
	logClear()
	ok <- TRUE
	if (check) {
		cat("Verification of .zim files...\n")
		logProcess("Verification of .zim files...")
		ok <- TRUE
		zfiles <- unique(zimfiles)
		zmax <- length(zfiles)
		oks <- sapply( 1:zmax, function (z) {
			Progress(z, zmax)
			tryCatch({
				zimVerify(zfiles[z])
				return(TRUE)
			}, zooImageError = function (e) {
				logError(e)
				return(FALSE)
			})
		})
		ok <- all(oks)
		clearProgress()
	}
	if (ok) {
		logProcess("\n-- OK, no error found. --")
		cat("-- Done! --\n")
	} else {
		stop("contains corrupted .zim files, compression not started!")
	}

	## If everything is ok compress these files
	imax <- length(images)
	cat("Compression of images...\n")
	logProcess("\nCompression of images...")

	oks <- sapply(1:imax, function (i) {
		Progress(i, imax)
		tryCatch({
			zipImg(images[i], verify.zimfile = FALSE, replace = replace,
				delete.source = delete.source, check.zip = FALSE,
				show.log = FALSE)
			logProcess("OK", images[i])
			return(TRUE)
		}, zooImageError = function (e) {
			logError(e)
			return(FALSE)
		})
	})

	clearProgress()

	## Final report
	finishLoop(ok, bell = bell, show.log = show.log)
}

## Use zipnote to extract the comment
unzipImg <- function (zipfile)
{
	# Extract .zim file, .tif file or both from a .zip archive
	zipNote(zipfile)
}

unzipImgAll <- function (path = ".", zipfiles = NULL)
{
	## Check that unzip is available
	checkUnzipAvailable()

	## Extract all .zim, .tif or both from .zip files
	stop("Not implemented yet!")
}
