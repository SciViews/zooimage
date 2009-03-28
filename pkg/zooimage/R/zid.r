"verify.zid" <-
	function(zidir, type = "ZI1", check.vignettes = TRUE, show.log = TRUE) {
	# This should be a directory containing XXX+YY_dat1.zim files + .jpg files
	if (type != "ZI1") stop("only 'ZI1' is currently supported for 'type'!")
	ok <- TRUE
	dat1files <- list.dat1.zim(zidir)
	if(length(dat1files) == 0) {
		logProcess("has no '_dat1.zim' file!", zidir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	if (length(dat1files) == 1 && is.na(dat1files)) {
	 	logProcess("not found or not a directory!", zidir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
    dat1files <- sort(dat1files)
	# Check the content of all these "_dat1.zim" files, and retrieve the number of items measured
	nitems <- rep(-1, length(dat1files))	# Default to -1 for corrupted dat1 files
	for (i in 1:length(dat1files)) {
		n <- verify.zim(file.path(zidir, dat1files[i]), is.dat1 = TRUE)
		if (is.character(n)) { 	# An error occurred
			logProcess(paste("error (", n, ")", sep = ""), dat1files[i]); ok <- FALSE
		} else nitems[i] <- n
	}
	if (check.vignettes) {
        # Check that we have corresponding vignettes (XXX+YY_ZZZ.jpg files)
    	samples <- sub("_dat1[.]zim$", "", dat1files)
    	# Check the content of the directory for .jpg files
    	for (i in 1:length(samples)) {
    		regex <- gsub("[+]", "[+]", samples[i])
    		regex <- gsub("[.]", "[.]", regex)
    		regex2 <-  paste("^", regex, "_[0-9]+[.]jpg$", sep = "")
    		jpgs <- dir(zidir, pattern = regex2)
    		# Get their numbers, sort them, and make sure none are missing
    		n <- nitems[i]
    		if (n < 1) n <- length(jpgs)	# If impossible to know how many items, just count vignettes
    		# Construct a vector with names of vignettes as they should be
    		chkjpgs <- paste(samples[i], "_", 1:n, ".jpg", sep = "")
    		if (length(jpgs) == 0 && length(chkjpgs) > 0) {
    		  logProcess(paste(" no vignettes for", samples[i]), zidir)
              ok <- FALSE
            } else {
                if (length(chkjpgs) != length(jpgs) || !all(sort(chkjpgs) == sort(jpgs))) {
  			       logProcess(paste(" mismatch vignettes for", samples[i]), zidir); ok <- FALSE }
            }
        }	
    }
	# Report results
	if (ok) logProcess("OK", zidir)
	if (show.log) logView()
	return(invisible(ok))
}

"verify.zid.all" <-
	function(path = ".", samples = NULL, type = "ZI1", 
    check.vignettes = TRUE, show.log = TRUE, bell = FALSE) {
	# Verify all of these directories
	if (type != "ZI1") stop("only 'ZI1' is currently supported for 'type'!")
	# First, switch to that directory
	inidir <- getwd()
	if (!file.exists(path) || !file.info(path)$isdir)
		stop(path, " does not exist, or it is not a directory!")
	setwd(path)
	on.exit(setwd(inidir))
	path = "."	# Indicate we are now in the right path
	# Process the list of samples
	if (is.null(samples)) {	# Compute them from path
		d <- dir(path, pattern = "^[^_]")	# All items not starting with '_'
		samples <- unique(d[file.info(d)$isdir])	# Keep only directories
	}
	# If there is no dir, exit now
	if (is.null(samples) || length(samples) == 0)
		stop("There is no directories to verify in ", getwd())
	# Start the process
	ok <- TRUE
	smax <- length(samples)
	cat("Verification...\n")
	for (s in 1:smax) {
		Progress(s, smax)
		if (!verify.zid(samples[s], type = type, check.vignettes = check.vignettes,
            show.log = FALSE)) ok <- FALSE
	}
	Progress (smax + 1, smax)	 # To dismiss the Progress() indication
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

"make.RData" <-
	function(zidir, type = "ZI1", replace = FALSE, show.log = TRUE) {
	# Make a .RData file that collates together data from all the "_dat1.zim" files of a given sample
	if (type != "ZI1") stop("only 'ZI1' is currently supported for 'type'!")
	RDataFile <- file.path(zidir, paste(basename(zidir), "_dat1.RData", sep = ""))
	if (file.exists(RDataFile) && !replace) return(invisible(TRUE)) # File already exists
	ok <- TRUE
	dat1files <- list.dat1.zim(zidir)
	if(length(dat1files) == 0) {
		logProcess("has no '_dat1.zim' file!", zidir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	if (length(dat1files) == 1 && is.na(dat1files)) {
	 	logProcess("not found or not a directory!", zidir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	dat1files <- sort(dat1files)
	fractions <- get.sampleinfo(dat1files, "fraction")
	# Avoid collecting duplicate informations about fractions
	fracdup <- duplicated(fractions)
	# For each of these files, read content in a variable
	allmes <- NULL
	allmeta <- NULL
	for (i in 1:length(dat1files)) {
		dat1path <- file.path(zidir, dat1files[i])
		if (!is.zim(dat1path)) {
			logProcess("is not a ZI1 file, or is corrupted", dat1files[i]); ok <- FALSE; next }
		# Read the header
		Lines <- scan(dat1path, character(), sep = "\t", skip = 1, blank.lines.skip = FALSE, flush = TRUE, quiet = TRUE, comment.char = "#")
		if (length(Lines) < 1) {
			logProcess("is empty, or is corrupted", dat1files[i]); ok <- FALSE; next }
		# Trim leading and trailing spaces in Lines
		Lines <- trim(Lines)
        # Convert underscore to space
		Lines <- underscore2space(Lines)
		# Determine where the table of measurements starts (it is '[Data]' header)
		endhead <- (1:length(Lines))[Lines == "[Data]"]
		if (length(endhead) == 0) endhead <- NULL else endhead <- endhead[length(endhead)]
		if (!is.null(endhead)) {
			if (endhead > 1) {
				Lines <- Lines[1:(endhead - 1)]
			} else Lines <- NULL
		}
		# Decrypt all lines, that is, split on first occurrence of "=" into 'tag', 'value'
		# and separate into sections
		if (!fracdup[i] && !is.null(Lines)) {
			meta <- parse.ini(Lines, sub("_dat1[.]zim$", "", fractions[i]))
			## Collate all metadata together
			if (i == 1) allmeta <- meta else {
				# Merge metadata
				allmeta <- list.merge(allmeta, meta)
			}
		}
		# Calculate a data frame containing 'dilutions'
		Sub <- allmeta$Subsample
		Sub$Dil <- 1 / (Sub$SubPart * Sub$CellPart * Sub$Replicates * Sub$VolIni)
		# Read the table of measurements
		# 'null' values transformed as NA values in table Denis Kevin v 1.2-2
		if (!is.null(endhead)) {
			mes <- read.table(dat1path, header = TRUE, sep = "\t", dec = ".",
				as.is = FALSE, skip = endhead + 1, comment.char = "#", na.strings = "null")
			# We have several problems here:
			# 1) There is sometimes a column full of NAs at the end.
			#    This is because ImageJ adds an extra tab at the end of the line.
			if (all(is.na(mes[ , ncol(mes)]))) mes <- mes[ , -ncol(mes)]
			# 2) The first column is the 'Item', but i         ts name '!Item' is transformed into 'X.Item'
			# 3) The '%Area' is transformed into 'X.Area'
			Names <- names(mes)
			if (Names[1] == "X.Item") Names[1] <- "Item"
			if ("X.Area" %in% Names) Names[Names == "X.Area"] <- "PArea"
			# Invert 'Item' and 'Label'
			mes <- mes[ , c(2, 1, 3:ncol(mes))]
			Names <- Names[c(2, 1, 3:length(Names))]
			names(mes) <- make.names(Names, unique = TRUE)
			# Add a Dil column at the end with the corresponding dilution
			Dil <- Sub$Dil[Sub$Label == fractions[i]]
			Dil <- rep(Dil, nrow(mes))
			mes <- cbind(mes, Dil)
			# Collate data all together
			if (i == 1) allmes <- mes else {
				if (all(names(allmes) == names(mes))) {
					allmes <- rbind(allmes, mes)	# Faster
				} else {
					allmes <- merge(allmes, mes, all = TRUE)
				}
			}
		}
	}
	rownames(allmes) <- 1:nrow(allmes)
	# Calculate an ECD from Area if there is not one yet
	Names <- names(allmes)
	if (!"ECD" %in% Names && "Area" %in% Names) {
		ECD <- ecd(allmes$Area)
		# Place ECD in third position (should be just after 'Label' and 'Item')
		allmes <- cbind(allmes[, 1:2], ECD, allmes[, 3:ncol(allmes)])
	}
	# Construct a c('ZI1Dat', 'ZIDat', 'data.frame') object containing the data frame
	# and the metadata as attribute
	attr(allmes, "metadata") <- allmeta
	class(allmes) <- c("ZI1Dat", "ZIDat", "data.frame")
	# Save these data in a file
	ZI.sample <- allmes
	save(ZI.sample, file = RDataFile, ascii = FALSE, version = 2, compress = TRUE)
	if (ok) ok <- file.exists(RDataFile)
	if (show.log) logView()
	return(invisible(ok))
}

"compress.zid" <-
	function(zidir, type = "ZI1", check = TRUE, check.vignettes = TRUE,
    replace = FALSE, delete.source = replace,
	check.zip = TRUE, show.log = TRUE) {
	# Compress one sample as a single .zid zipped file
	if (type != "ZI1") stop("only 'ZI1' is currently supported for 'type'!")
	# We need to switch to the root of sample dir first for correct path in the zip file
	rootdir <- dirname(zidir)
	inidir <- getwd()
	setwd(rootdir)
	on.exit(setwd(inidir))
	zidir <- basename(zidir) # Use only the latest dir (the "sample dir")
	# The .zid file is located in the "root" dir, same name as the "sample dir", with .zid extension
	zidfile <- paste(zidir, ".zid", sep = "")
	if (!replace && file.exists(zidfile)) {
		# It is not advised to delete source without rebuilding the .zid file
		# but it was expressly asked!
		### TODO: verify we have the same files in the .zid and initial dir before deleting files!
		if (delete.source && file.exists(zidir)) unlink(zidir, recursive = TRUE)
		return(invisible(TRUE))	# Nothing else to do
	}
	# This requires the 'zip' program!
	# Make sure it is available
	cmd <- paste('"', ZIpgm("zip", "misc"), '" -h', sep = "")
	if (check.zip && (system(cmd, invisible = TRUE) != 0)) {
		logProcess("program from Info-Zip not found!", "zip", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Make sure everything is fine for this directory
	ok <- TRUE
	if (check) ok <- verify.zid(zidir, type = type, check.vignettes = check.vignettes,
        show.log = FALSE)
	if (!ok) {
		logProcess("appears to be corrupted!", zidir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Make sure the .RData file is created (or refreshed)
	if (!make.RData(zidir, type = type, replace = replace, show.log = FALSE)) {
		logProcess("has no .RData (error while creating it)!", zidir, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Do compress the directory in the .zip file
	# Delete old .zid file, if it exists
	if (file.exists(zidfile)) unlink(zidfile)
	# Copy or move all corresponding files to a .zid zip-compressed file
	if (delete.source) zippar <- "-mrq9 " else zippar <- "-rq9 "
	cmd <- paste('"', ZIpgm("zip", "misc"), '" ', zippar, '"', zidir, '.zid" "', zidir, '"', sep = "")
	system(cmd, show.output.on.console = TRUE, invisible = TRUE)
	# Invisibly indicate success (if file exists)
	return(invisible(file.exists(zidfile)))
}

"compress.zid.all" <-
	function(path = ".", samples = NULL, type = "ZI1", check = TRUE,
	check.vignettes = TRUE, replace = FALSE, delete.source = replace,
    show.log = TRUE, bell = FALSE) {
	# Compress all data in the corresponding directory
	if (type != "ZI1") stop("only 'ZI1' is currently supported for 'type'!")
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
	path = "."	# Indicate we are now in the right path
	# Get the list of samples to process
	if (is.null(samples)) {	# Compute them from path
		d <- dir(path, pattern = "^[^_]")	# All items not starting with '_'
		samples <- unique(d[file.info(d)$isdir])	# Keep only directories
	}
	# If there is no dir, exit now
	if (is.null(samples) || length(samples) == 0)
		stop("There is no directories to process in ", getwd())
	# Start the process
	logClear()
	ok <- TRUE
	if (check) {
		logProcess("Verification...")
		ok <- verify.zid.all(path = path, samples = samples, check.vignettes = check.vignettes, 
            show.log = FALSE, bell = FALSE)
	}
	if (!ok) {
		logProcess("contains corrupted files, compression not started!", path, stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Compress these files
	smax <- length(samples)
	cat("Compression...\n")
	logProcess("\nCompression...")
	for (s in 1:smax) {
		Progress(s, smax)
		if (!compress.zid(samples[s], type = type, check = FALSE, check.vignettes = check.vignettes, 
            replace = replace, delete.source = delete.source, check.zip = FALSE, show.log = FALSE)) {
			ok <- FALSE
		} else {
			logProcess("OK", samples[s])
		}
	}
	Progress (smax + 1, smax)	 # To dismiss the Progress() indication
	# Possibly clean the whole directory (move .zim files to \_raw
	# and delete the \_work subdir if everything is fine
	if (ok) clean.after.zid(path = path, samples = samples) 
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

"clean.after.zid" <-
    function(path = ".", samples = NULL) {
    # Do we have samples to process
    if (length(samples) == 0) return()
    # First, switch to that directory
	inidir <- getwd()
    if (!file.exists(path) || !file.info(path)$isdir)
		stop(path, " does not exist, or it is not a directory!")
	setwd(path)
	on.exit(setwd(inidir))
	cat("Cleaning directory...\n")
	logProcess("\nCleaning directory...")
    zimfiles <- list.files(path = ".", pattern = "[.][zZ][iI][mM]$")
    zimsamples <- sub("^(.*)[+].+", "\\1", zimfiles)
    # Keep only those .zim files related tyo samples
    zimfiles <- zimfiles[zimsamples %in% samples]
    if (length(zimfiles) > 0) {
        rawdir <- file.path(".", "_raw")
        # If the _raw subdirectory does not exists, create it
        if (!file.exists(rawdir)) dir.create(rawdir) 
        copyto <- file.path(".", "_raw", zimfiles)
        # Move these .zim files
        for (i in 1:length(zimfiles)) file.rename(zimfiles[i], copyto[i])
    }
    # Delete completely the _work subdirectory
    unlink(file.path(".", "_work"), recursive = TRUE)    
}

"uncompress.zid" <-
	function(zidfile, path = dirname(zidfile), delete.source = FALSE, show.log = TRUE) {
	# Uncompress a .zid file to get all its content. Use 'delete.source = TRUE' with caution!
	# Check if the file provided is a .zid file, and if it exists
	if (!file.exists(zidfile))
		stop(zidfile, " not found!")
	if(length(grep("[.]zid$", zidfile)) == 0)
		stop(file, " is not a .zid file!")
	# Uncompress it
	if (.Platform$OS.type == "windows") {
		zip.unpack(zidfile, path)
		if (delete.source) unlink(zidfile)
		return(TRUE)
	} else {
		# Use 'unzip zipfile -d dest' on other platforms than Windows
		stop("This function is not implemented yet on this platform!")
	}
}

"uncompress.zid.all" <-
	function(path = ".", zidfiles = list.files(path = path, pattern = "^.*[.][zZ][iI][dD]"),
	path.extract = path, skip.existing.dirs = TRUE, delete.source = FALSE, show.log = TRUE, bell = FALSE) {
	# Uncompress all .zid files in the 'path.extract' directory
	if (is.null(zidfiles) || length(zidfiles) == 0) {
        logProcess("no .zid files found!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	logClear()
	ok <- TRUE
	# Check that dirs / files with corresponding names exist in path.extract
    checkdirs <- file.path(path.extract, noext(zidfiles))
	fileExists <- rep(FALSE, length(checkdirs))
	dirExists <- rep(FALSE, length(checkdirs))
	for (i in 1:length(checkdirs)) {
		if (file.exists(checkdirs[i])) {
			if (file.info(checkdirs[i])$isdir) {
				dirExists[i] <- TRUE
			} else {
				fileExists[i] <- TRUE
			}
		}
	}
	# If any file not being a dir exist there, stop the process
	if (any(fileExists)) {
        logProcess("one or several files have same name as uncompressed dirs!", stop = TRUE, show.log = show.log); return(invisible(FALSE)) }
	# Should we eliminate files whose corresponding dirs exist?
	if (skip.existing.dirs && any(dirExists)) {
		cat(sum(dirExists), "file(s) already uncompressed is/are skipped!\n")
        logProcess("Skipping already uncompressed file(s):")
        logProcess(zidfiles[dirExists])
	}
	zidfiles <- zidfiles[!dirExists]
	# Decompress the files remaining in the list
	smax <- length(zidfiles)
	if (smax == 0) {
		cat("-- Done! - (nothing to decompress)\n")
		logProcess("\n -- Done! -- (nothing to decompress)", show.log = show.log)
		return(invisible(FALSE))
	}
	# Start decompression
	cat("Decompression...\n")
	logProcess("\nDecompression...")
	for (s in 1:smax) {
		Progress(s, smax)
		if (!uncompress.zid(zidfiles[s], path = path.extract,
			delete.source = delete.source, show.log = FALSE)) {
			ok <- FALSE
		} else {
			logProcess("OK", zidfiles[s])
		}
	}
	Progress (smax + 1, smax)	 # To dismiss the Progress() indication
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

"zid.extract" <- function(file, zidfile, dir = tempdir()) {
	if ((unzip <- getOption("unzip")) != "internal") {
		if (!system(paste(unzip, " -oq \"", zidfile, "\" \"", file, "\" -d ", dir, sep = ""), invisible = TRUE))
			file <- file.path(dir, file) else file <- ""
	} else {
		rc <- .Internal(int.unzip(zidfile, file, dir))
		if (rc == 0) file <- file.path(dir, file) else file <- ""
	}
	return(file)
}

"read.zid" <- function(zidfile) {
	# Read the .Rdata in a .zid file or corresponding directory
	sample <- noext(basename(zidfile))
	RdataFile <- paste(sample, "_dat1.RData", sep ="")
	deletefile <- FALSE
	if (!file.exists(zidfile))
		stop(zidfile, " not found!")
	if(length(grep("[.][zZ][iI][dD]$", zidfile)) == 0) {
		# Is it a directory?
		if (file.info(zidfile)$isdir) {
			# Is there a .RData file in this directory?
			rdata <- file.path(zidfile, RdataFile)
			if (!file.exists(rdata)) {
				# Try to create it
				make.RData(zidfile, show.log = FALSE)
				if (!file.exists(rdata))
					stop("Error creating the .RData file")
			}
		} else stop("Unrecognized file", zidfile)
	} else { # This is a .zid file
		rdata <- file.path(sample, RdataFile)
		rdata <- zid.extract(rdata, zidfile)
		if (rdata == "")
			stop("Error reading .RData file from", zidfile)
		deletefile <- TRUE
	}
	# Load that file
	ZI.sample <- NULL
	load(rdata)
	if (deletefile) {
		unlink(rdata)
		# If the directory is empty, delete it also
		datadir <- file.path(tempdir(), sample)
		if (file.exists(datadir) && (length(dir(datadir)) == 0))
			unlink(datadir)
	}
	if (!inherits(ZI.sample, "ZIDat") && inherits(ZI.sample, "data.frame"))
		class(ZI.sample) <- c("ZI1Dat", "ZIDat", "data.frame")
	return(ZI.sample)
}

# reprocess Rdata in zid files v 1.2-2 by Denis Kevin
NewRdata <- function(path = "D", replace = TRUE){
  # list of zid files to reporcess
  zid <- list.files(path = path, pattern = "^.*[.][zZ][iI][dD]")
    if(is.null(zid)) stop("no zid files in the directory") # of no zid files
  # path of zid files
  path.zid <- paste(path, zid, sep = "/")
  # loop to analyze zid files one by one
  for (i in 1 : length(zid)){
    # extract zid in 'path' directory
    uncompress.zid(path.zid[i])
    # calculate new Rdata
    path.sample <- sub("[.][zZ][iI][dD]","",path.zid[i])
    make.RData(path.sample, replace = replace)
    # compress new zid file
    compress.zid(path.sample, replace = replace)
    }
}
