"zip.img" <-
	function(imagefile, zimfile = NULL, verify.zimfile = TRUE, replace = FALSE,
		delete.source = TRUE, check.zip = TRUE, show.log = TRUE) {
 	# Zip a .tif image and embed the corresponding .zim file as comment
	# This requires the 'zip' program!
	# Make sure it is available
	cmd <- paste('"', ZIpgm("zip", "misc"), '" -h', sep = "")
	if (check.zip && (system(cmd, invisible = TRUE) != 0)) {
		logProcess("program from Info-Zip not found!", "zip", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# We need to switch to the root of sample dir first for correct path in the zip file
	imagefile <- imagefile[1]
	inidir <- getwd()
	setwd(dirname(imagefile))
	on.exit(setwd(inidir))
	rootdir <- getwd() 
	imagefile <- basename(imagefile)
	# Check if imagefile exists
	if (!file.exists(imagefile) || file.info(imagefile)$isdir) {
		logProcess("doesn't exist, or is a directory!", imagefile, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Is there an associated .zim file?
	if (is.null(zimfile))
		zimfile <- paste(get.sampleinfo(imagefile, "fraction", ext = "[.][tT][iI][fF]$"), ".zim", sep = "")	
	### TODO: the zim file can be other parts of it , like Sample+A1.zim, instead of Sample+A.zim!
	if (!file.exists(zimfile)) {
        	# Create it from a template
			### TODO
			# Save a copy as LastUsed.zim
			### TODO
			stop("creation of .zim file not implemented yet!")
	}
	# Recheck .zim file
	if (!file.exists(zimfile)) {
		logProcess("doesn't exist or is corrupted!", zimfile, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Verify the content of the .zim file
	if (verify.zimfile && verify.zim(zimfile) != 0) {
		logProcess("appears to be corrupted!", zimfile, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Zip the image in the '_raw' subdir and add the information from the .zim file as comment
	zipfile <- paste(noext(imagefile), ".zip", sep = "")
	zipfile <- file.path(".", "_raw", zipfile)
	# Make sure that "_raw" subdir exists
	if (!file.exists("_raw")) {
		if (!dir.create("_raw")) {
			logProcess("Impossible to create the '_raw' subdirectory!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	} else {	# Check it is a directory
    	if (!file.info("_raw")$isdir) {
			logProcess("'_raw' exists, but is not a directory!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	}
	# Delete old .zip file, if it exists (replace = TRUE), or exit
	if (file.exists(zipfile))
		if (replace) unlink(zipfile) else return(invisible(TRUE))
	# Copy or move the image to a .zip compressed file
	if (delete.source) zippar <- "-mqz9 " else zippar <- "-qz9 "
	# command is: type %~n1.zim | %~dp0zip -mqz9 _raw/%~n1.zip %1 
	cmd <- paste(Sys.getenv("COMSPEC"), ' /c type "', zimfile, '" | "', ZIpgm("zip", "misc"), '" ', zippar, '"', zipfile, '" "', imagefile, '"', sep = "")
	system(cmd, show.output.on.console = TRUE, invisible = TRUE)
	# Verify that the .zip file is created
	if (!file.exists(zipfile)) { 
		logProcess("Error creating the file!", zipfile, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Note: the .zim file is never deleted, because it can be used for other purposes!
	# Invisibly indicate success
	return(invisible(TRUE))
}

"zip.img.all" <-
	function(path = ".", images = NULL, check = TRUE, replace = FALSE,
	delete.source = replace, show.log = TRUE, bell = FALSE) {
	# Compress all .tif images in the corresponding directory (at least those with an associated .zim file)
	# This requires the 'zip' program!
	# Make sure it is available
	cmd <- paste('"', ZIpgm("zip", "misc"), '" -h', sep = "")
	if ((system(cmd, invisible = TRUE) != 0)) {
		logProcess("program from Info-Zip not found!", "zip", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# First, switch to that directory
	inidir <- getwd()
	if (!file.exists(path) || !file.info(path)$isdir)
		stop(path, " does not exist, or it is not a directory!")
	setwd(path)
	on.exit(setwd(inidir))
	path = getwd()	# Indicate we are now in the right path
	# Get the list of images to process
	if (is.null(images))	# Compute them from path
		images <- dir(path, pattern = "[.][tT][iI][fF]$")	# All .tif files
	# Make sure there is no path associated
	if (!all(images == basename(images)))
		stop("You cannot provide paths for 'images', just file names")
	# If there is no images in this dir, exit now
	if (is.null(images) || length(images) == 0)
		stop("There is no images to process in ", getwd())
	# Look at associated .zim files
	zimfiles <- paste(get.sampleinfo(images, "fraction", ext = "[.][tT][iI][fF]$"), ".zim", sep = "")
	keep <- file.exists(zimfiles)
	if (!any(keep))
		stop("You must create .zim files first (ZooImage Metadata)!")
	if (!all(keep)) {
    	warning(sum(!keep), " on ", length(keep), " images have no .zim file associated and will not be processed!")
		images <- images[keep]
		zimfiles <- zimfiles[keep]
	}
	# Start the process
	logClear()
	ok <- TRUE
	if (check) {
		cat("Verification of .zim files...\n")
		logProcess("Verification of .zim files...")
		ok <- TRUE
		zfiles <- unique(zimfiles)
		zmax <- length(zfiles)
		for (z in 1:zmax) {
        	Progress(z, zmax)
			res <- verify.zim(zfiles[z])
			if (res != 0) { # Error
            	logProcess(res, zfiles[z])
				ok <- FALSE
			}
		}
		Progress (zmax + 1, zmax)	 # To dismiss the progress() indication
	}
	if (ok) {
		logProcess("\n-- OK, no error found. --")
		cat("-- Done! --\n")		
	} else {
		logProcess("contains corrupted .zim files, compression not started!", path, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# If everything is fileompress these files
	imax <- length(images)
	cat("Compression of images...\n")
	logProcess("\nCompression of images...")
	for (i in 1:imax) {
		Progress(i, imax)
		if (!zip.img(images[i], verify.zimfile = FALSE, replace = replace,
			delete.source = delete.source, check.zip = FALSE, show.log = FALSE)) {
			ok <- FALSE
		} else {
			logProcess("OK", images[i])
		}
	}
	Progress (imax + 1, imax)	 # To dismiss the Progress() indication
	if (bell) Bell <- "\a" else Bell <- ""   # \a rings the bell on most platforms!
	if (ok) {
		logProcess("\n-- OK, no error found. --")
		cat(Bell, "-- Done! --\n")
	} else {
		logProcess("-- Error(s)! --")
		cat(Bell, " -- Done! [ERROR(S) FOUND] --\n")
	}
	if (show.log) logView()
	return(invisible(ok))
}

"unzip.img" <-
	function(zipfile) {
 	# Extract .zim file, .tif file or both from a .zip archive
	stop("Not implemented yet!")
}

"unzip.img.all" <-
	function(path = ".", zipfiles = NULL) {
 	# Extract all .zim, .tif or both from .zip files
	stop("Not implemented yet!")
}
