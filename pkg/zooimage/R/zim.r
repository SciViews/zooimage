# Functions for manipulating .zim files (ZooImage Metadata/measurements)
# These .zim files contain metadata required to analyze plankton images
# and to record the way they were processed. Measurements on each identified
# object can also be appended in a table at the end of this file (in this case,
# the usual extension is '_dat1.zim' to indicate that data processed with
# ZooImage version 1 are present in the file).
#
# Copyright (c) 2004-2007, Ph. Grosjean <phgrosjean@sciviews.org>

"make.zim" <-
	function(dir = ".", pattern = "\\.[tT][iI][fF]$",
	images = list.files(dir, pattern), show.log = TRUE, bell = FALSE) {
	if (length(images) < 1) {
		logProcess("no images to process!", show.log = show.log); return(invisible(FALSE)) }
	# Name of images is something like SCS.xxxx-xx-xx.SS+Ann.tif
	# We make the same .zim file for all ...+Ann images, so, reduce the list
	zims <- sort(unique(get.sampleinfo(images, type = "fraction", ext = pattern)))
	zims <- file.path(dir, paste(zims, "zim", sep = "."))
	ok <- TRUE
	zmax <- length(zims)
	cat("Making & checking .zim files...\n")
	template <- NULL	# Start with a default template
	for (z in 1:zmax) {
		Progress(z, zmax)
		if (!file.exists(zims[z])) { 	#.zim file does not exists... create it
			logProcess("Creating the file", zims[z])
			create.zim(zims[z], template = template, wait = TRUE)
			# Use the previous file as template
			template = zims[z]
		}
		# Verify that the zim is correct
		res <- verify.zim(zims[z])
		if (!res == 0) ok <- FALSE
	}
	Progress (zmax + 1, zmax)	 # To dismiss the Progress() indication
	if (bell) Bell <- "\a" else Bell <- ""   # \a rings the bell on most platforms!
	if (ok) {
		logProcess("\n-- OK, no error found. --")
		cat(Bell, "-- Done! --\n")
	} else {
		logProcess("\n-- One or several errors found! --")
		cat(Bell, " -- Done! [ERROR(S) FOUND] --\n")
	}
	if (show.log) logView()
	return(invisible(ok))
}

"is.zim" <-
	function(zimfile, check.ext = FALSE) {
	# Check if a file is a "(_dat1).zim" file (must start with "ZI1" and have a '.zim' extension)
	if (!file.exists(zimfile) || file.info(zimfile)$isdir)
		return(FALSE)
	if (check.ext && length(grep("\\.zim$", tolower(zimfile))) != 1)
		return(FALSE)
	Line1 <- scan(zimfile, character(), nmax = 1, quiet = TRUE)
	return(Line1 == "ZI1")
}

"verify.zim" <-
	function(zimfile, check.ext = FALSE, is.dat1 = FALSE, check.table = FALSE) {
    # Verify a "(_dat1).zim" file (all required fields + return the number of items in it)
	# If it succeeds, return the number of measured items as numeric
	# Otherwise, return an explicit error message as character
	
	# Here are predefined required fields before measurements
	reqfields <- c("[Image]", "Author", "Hardware", "Software",
        "ImageType", "[Fraction]", "Code", "Min", "Max", "[Subsample]",
        "SubPart", "SubMethod", "CellPart", "Replicates", "VolIni",
        "VolPrec")
	# Then required fields when measurements are done    
	reqfields2 <- c("[Process]")
	# Finally, required column headers
    reqcols <- c("!Item", "Label", "BX", "BY", "Width", "Height")
	
	# Determine if there are custom verification rules defined and if they are active
    newRules <- getOption("ZI.zim")
    if (!is.null(newRules) && newRules$active == TRUE) {
        # Should we delegate the whole process to a custom verification function?
		verify.all <- newRules$verify.all
        if (!is.null(verify.all) && inherits(verify.all, "function"))
            return(verify.all(zimfile = zimfile, check.ext = check.ext,
                is.dat1 = is.dat1, chack.table = check.table))
        # Should we use additional verification code instead?
		verify <- newRules$verify
        reqfields <- c(reqfield, newRules$zim.required)
        reqfields2 <- c(reqfields2, newRules$dat1.zim.required)
        reqcols <- c(reqcol, newRules$dat1.data.required)
    } else verify <- NULL

    if (!file.exists(zimfile) || file.info(zimfile)$isdir)
        return("File not found!")
    # Verify that the file extension is .zim
	if (check.ext && length(grep("\\.zim$", tolower(zimfile))) != 1)
        return("File extension is incorrect (not '.zim')")
    # Verify it is a zimfile
	Line1 <- scan(zimfile, character(), nmax = 1, quiet = TRUE)
    if (Line1 != "ZI1")
        return("File does not appears to be a ZooImage version 1 file, or it is corrupted!")
    
	# Run first the extra verification code
	if (!is.null(verify) && inherits(verify, "function")) {
        res <- verify(zimfile, check.ext = check.ext, is.dat1 = is.dat1,
            check.table = check.table)
    } else res <- ""	# No problems!

	# Read the file...
    Lines <- scan(zimfile, character(), sep = "\t", skip = 1,
        flush = TRUE, quiet = TRUE, blank.lines.skip = FALSE, comment.char = "=")
        # Equal sign is used as comment char, in order to read only the field names
    if (length(Lines) < 1)
        return("File is empty!")
    # Trim leading and trailing white spaces
	Lines <- trim(Lines)

	# Check that all required fields are present for a simple .zim file
    misfields <- reqfields[!(reqfields %in% Lines)]
    if (length(misfields) > 0) {
        res2 <- paste("Missing fields:", paste(misfields, collapse = ", "))
        if (res == "") return(res2) else return(paste(res, res2, collapse = "\n"))
    }

	# Check if this is a _dat1.zim file with measurements
    if ("[Data]" %in% Lines) {
        misfields2 <- reqfields2[!(reqfields2 %in% Lines)]
        if (length(misfields2) > 0) {
            res2 <- paste("Missing [Process] fields:", paste(misfields2,
                collapse = ", "))
            if (res == "") return(res2) else return(paste(res, res2, collapse = "\n"))
        }
        
		# Check for required column headers
		posHeaders <- grep("^\\[Data\\]$", Lines)[1] + 1
        LineHeader <- scan(zimfile, character(), sep = "%", skip = posHeaders,
        nmax = 1, flush = TRUE, quiet = TRUE, comment.char = "=")
        Headers <- trim(strsplit(LineHeader, "\t")[[1]])
        misHeaders <- reqcols[!(reqcols %in% Headers)]
        if (length(misHeaders) > 0) {
            res2 <- paste("Missing columns in the table:", paste(misHeaders,
                collapse = ", "))
            if (res == "") return(res2) else return(paste(res, res2, collapse = "\n"))
        }

		# Check that the table can be read 
        if (check.table) {
            posMes <- grep("^\\[Data\\]$", Lines)
            if (length(posMes) == 0) {
                return("Trying to read the table of measurements but no [Data] section found!")
            } else { # The [Data] section is found
                Mes <- try(read.table(zimfile, sep = "\t", header = TRUE,
                  skip = posMes + 1), silent = TRUE)
                if (inherits(Mes, "try-error")) {
                  return("Unable to read the table of measurements!")
                } else { 	# Successful reading of the table of measurements
                  return(nrow(Mes))	# Return the number of items measured
                }
            }
        } else { # We don't read the table, use a different method to get the number of entries in it
            # Read the last entry in Lines and convert it to a numeric value: should be the number of items measured
			nItems <- Lines[length(Lines)]
            if (sub("^[0-9]+$", "", nItems) != "")
                return("Impossible to determine the number of items measured!")
            return(as.numeric(nItems))
        }
    } else {
        if (is.dat1) return("No measurements found in this file")
        else return(0)
    }
}

"list.zim" <-
	function(zidir) {
	# Is this a directory and does it exists?
	if (!file.exists(zidir) || !file.info(zidir)$isdir) return(NA)
	# Get a list of all ".zim" files in this directory
	res <- dir(zidir, pattern = "\\.[zZ][iI][mM]$")
	return(res)
}

"list.dat1.zim" <-
	function(zidir) {
	# Is this a directory and does it exists?
	if (!file.exists(zidir) || !file.info(zidir)$isdir) return(NA)
	# Get a list of all "_dat1.zim" files in this directory
	res <- dir(zidir, pattern = "_[dD][aA][tT]1\\.[zZ][iI][mM]$")
	return(res)
}

"extract.zims" <-
	function(zipdir = ".", zipfiles = list.files(zipdir, pattern = "\\.[zZ][iI][pP]$"),
		path = NULL, replace = FALSE, check.unzip = TRUE, show.log = TRUE, bell = FALSE) {
    # Extract notes from .zip files and place them in .zim files
	# This requires the 'unzip' program!
	# Make sure it is available
	cmd <- paste('"', ZIpgm("unzip", "misc"), '" -h', sep = "")
	if (check.unzip && (system(cmd, invisible = TRUE) != 0)) {
		logProcess("'unzip' program from Info-Zip not found!", "zip", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
    # Make sure all zipfiles are in the same directory
	zipdirs <- dirname(zipfiles)
	if (length(unique(zipdirs)) > 1) {
		logProcess("All zip files must be located in the same directory!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Move to zipdir
    inidir <- getwd()
	# Check that the dir exists!
	if (!file.exists(zipdir) || !file.info(zipdir)$isdir) {
		logProcess("this is not a valid directory!", zipdir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	setwd(zipdir)
	on.exit(setwd(inidir))
	zipdir <- getwd()   # That way, if we had ".", it is now expanded
	# Use only basenames for zip files
	zipfiles <- sort(basename(zipfiles))
	# Check that zipfiles exist
	if (!all(file.exists(zipfiles))) {
		logProcess("One or several files not found!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Look at the path where to place .zim files
	if (is.null(path)) {
		# The rule is the following one:
		# 1) if last subdir is "_raw", then place .zim file up one level
		# 2) else, place them in the same dir as the zip files
		path <- zipdir
		if (tolower(basename(path)) == "_raw")
			path <- dirname(path)
	} else {    # Look if this is a valid directory
		path <- path[1]
		if (!file.exists(path) || !file.info(path)$isdir) {
			logProcess("is not a valid directory!", path, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	}
	# Compute the names of .zim files from the names of .zip files
	# Note: use only the fraction, that is, SCS.xxxx-xx-xx.SS+F from SCS.xxxx-xx-xx.SS+Fnn)
	# If there are duplicates, only extract first one
	zimfiles <- paste(get.sampleinfo(zipfiles, "fraction", ext = "\\.[zZ][iI][pP]$"), ".zim", sep = "")
	keep <- !duplicated(zimfiles)
	zimfiles <- zimfiles[keep]
	zipfiles <- zipfiles[keep]
    # Make full path for zimfiles
	zimfiles <- file.path(path, zimfiles)
	# If replace == FALSE, eliminate existing .zim files from the list
	if (!replace) {
		keep <- !file.exists(zimfiles)
		zimfiles <- zimfiles[keep]
		zipfiles <- zipfiles[keep]
	}
	# Are there files left
	if (length(zimfiles) == 0) {
		logProcess("Done, no file to process!", show.log = show.log); return(invisible(FALSE)) }
	# Extract .zim files, one at a time, and check them
	zmax <- length(zimfiles)
	ok <- rep(TRUE, zmax)
	for (i in 1:zmax) {
		# Make sure that old versions are eliminated
		if (file.exists(zimfiles[i])) unlink(zimfiles[i])
		# Extract the .zim file from zip comment
		cmd <- paste('"', ZIpgm("unzip", "misc"), '" -pz "', zipfiles[i], '"', sep = "")
		zimdat <- system(cmd, intern = TRUE, show.output.on.console = FALSE, invisible = TRUE)
		cat(paste(paste(zimdat, collapse = "\n"), "\n", sep = ""), file = zimfiles[i])
		# Check that the .zim file is created
		if (!file.exists(zimfiles[i]) || verify.zim(zimfiles[i]) != 0) ok[i] <- FALSE
	}
	# Check the results
	if (all(ok)) {
        logProcess(paste(zmax, ".zim files correctly extracted"), show.log = FALSE)
	} else { # One or several errors
        logProcess(paste(sum(!ok), "files not correctly extracted on", zmax), show.log = FALSE)
	}
	# Display the log file, if required
    if (show.log) logView()
    if (bell) cat("\a")   # \a rings the bell on most platforms!
	# Invisibly indicate success (if no errors)
	return(invisible(all(ok)))
}

"refresh.zims" <-
	function(zipdir = ".", zipfiles = list.files(zipdir, pattern = "\\.[zZ][iI][pP]$"),
		zimdir = NULL, check.zip = TRUE, check.zim = TRUE, show.log = TRUE, bell = FALSE) {
	# Given a list of .zip files and a path where .zim files are located,
	# the function updates comment fields of the .zip files with latest .zim content
    # This requires the 'zip' program!
	# Make sure it is available
	cmd <- paste('"', ZIpgm("zip", "misc"), '" -h', sep = "")
	if (check.zip && (system(cmd, invisible = TRUE) != 0)) {
		logProcess("'zip' program from Info-Zip not found!", "zip", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
    # Make sure we have full path for zip files
	if (zipdir == ".") zipdir <- getwd()
	zipfiles <- file.path(zipdir, zipfiles)
    # Check that zipfiles exist
	if (!all(file.exists(zipfiles))) {
		logProcess("One or several .zip files not found!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
    # Look for the path where .zim files are located
	if (is.null(zimdir)) {
		# The rule is the following one:
		# 1) if last subdir of .zip files is "_raw", then .zim files should be up one level
		# 2) else, look at the same dir
		zimdir <- zipdir
		if (tolower(basename(zimdir)) == "_raw")
			zimdir <- dirname(zimdir)
	} else {    # Look if this is valid directory
		zimdir <- zimdir[1]
		if (!file.exists(zimdir) || !file.info(zimdir)$isdir) {
			logProcess("is not a valid directory!", zimdir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	}
	# Switch to that dir
	inidir <- getwd()
	setwd(zimdir)
	on.exit(setwd(inidir))
	# Compute the names of zim files from the names of zip files
	# Note: use only the fraction, that is, SCS.xxxx-xx-xx.SS+F from SCS.xxxx-xx-xx.SS+Fnn)
	# If there are duplicates, only extract first one
	zimfiles <- paste(get.sampleinfo(zipfiles, "fraction", ext = "\\.[zZ][iI][pP]$"), ".zim", sep = "")
	# Eliminate path for zimfiles
	zimfiles <- basename(zimfiles)
    # Keep only existing .zim files
	keep <- file.exists(zimfiles)
	zimfiles <- zimfiles[keep]
	zipfiles <- zipfiles[keep]
	# Are there files left?
	if (length(zimfiles) == 0) {
		logProcess("Done, no file to update!", show.log = show.log); return(invisible(FALSE)) }
	# Start the process
	logClear()
	ok <- TRUE
	if (check.zim) {
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
		Progress (zmax + 1, zmax)	 # To dismiss the Progress() indication
	}
	if (ok) {
		logProcess("\n-- OK, no error found. --")
		cat("-- Done! --\n")		
	} else {
		logProcess("contains corrupted .zim files, compression not started!", path, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# If everything is OK, update comments in the zip files with the content of the .zim files
	imax <- length(zipfiles)
	cat("Update of .zip comments...\n")
	logProcess("\nUpdate of .zip comments...")
	for (i in 1:imax) {
		Progress(i, imax)
		# Replace the zip comment with the content of the corresponding .zim file
		cmd <- paste(Sys.getenv("COMSPEC"), ' /c type "', zimfiles[i], '" | "',ZIpgm("zip", "misc") , '" -z "', zipfiles[i], '"', sep = "")
		res <- system(cmd, show.output.on.console = FALSE, invisible = TRUE)
		# Check that the command was correctly processed
		if (res != 0) ok <- FALSE else logProcess("OK", zipfiles[i])
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

"create.zim" <-
	function(zimfile = NULL, template = NULL, editor = getOption("ZIEditor"),
	edit = TRUE, wait = FALSE) {
	# Create a .zim file from a template and edit it
	
    (require(svDialogs) || stop("Package 'svDialogs' from 'SciViews' bundle is required. Please, install it first!"))

	if (is.null(zimfile) || zimfile == "") {
		if (isWin()) {
	        zimfile <- winDialogString("Give a name for the new .zim file:",
				default = "myfile.zim")
		} else {	
			zimfile <- guiDlgInput("Give a name for the new .zim file:",
				"ZIM file creation", default = "myfile.zim")
		}
		if (is.null(zimfile) || length(zimfile) == 0 || zimfile == "")
			return(invisible())
		if (regexpr("\\.[zZ][iI][mM]$", zimfile) < 0)
			zimfile <- paste(zimfile, "zim", sep = ".")
	}
	# If the file exists, edit existing version instead
    if (file.exists(zimfile))
		if (edit) return(edit.zim(zimfile, wait = wait)) else return()
	Ed <- getOption("ZIEditor")
	if (edit && (is.null(Ed) || !file.exists(Ed)))
		stop("The metadata editor is not defined or not found!")
	# Look for the template
	if (is.null(template)) {
		# Look for the default template
		Edpath <- dirname(Ed)
		template <- file.path(Edpath, "templates", "default.zim") 
	}
	if (!file.exists(template))
		stop("Template file '", template, "' not found!")
    if (!is.zim(template))
		stop("Template '", template, "' is not a valid '.zim' file!")
	# Copy the template into the new file
	file.copy(template, zimfile)
	# Edit this new file
	if (edit) startPgm("ZIEditor", cmdline = zimfile, wait = wait)
}

"edit.zim" <-
	function(zimfile, editor = getOption("ZIEditor"), wait = FALSE) {
	# Edit a .zim file
    if (is.null(zimfile) || zimfile == "") {
		zimfile <- selectFile("Zim")
		if (zimfile == "")
			return(invisible())
	} else {
		if (!file.exists(zimfile))
			stop("the file '", zimfile, "' is not found!")
		if (!is.zim(zimfile))
			stop("This is not a valid '.zim' file!")
	}
	Ed <- getOption("ZIEditor")
	if (is.null(Ed) || !file.exists(Ed))
		stop("The metadata editor is not defined or not found!")
	# Everything is fine, open the document for editing...
	startPgm("ZIEditor", cmdline = zimfile, wait = wait)
}