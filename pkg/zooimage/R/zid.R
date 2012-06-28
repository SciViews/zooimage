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

## Check consistency of a zoo image directory
## zidir: the directory to check
## type: must be ZI1, ZI2 or ZI3 
## check.vignettes: do we check vignettes as well
## show.log: do we show a log at the end
zidVerify <- function (zidir, type = c("ZI1", "ZI2", "ZI3"),
check.vignettes = TRUE, show.log = TRUE)
{	
	## Check the format of the file
	## This should be a directory containing XXX+YY_dat1.zim files
	## + .jpg or .png files (vignettes)
	if (any(!type %in% c("ZI1", "ZI2", "ZI3")))
		stop("only 'ZI1', 'ZI2' or 'ZI3' are currently supported for 'type'")
	
	## Check the list of _dat1.zim
	dat1files <- zimDatList(zidir)
	if (length(dat1files) == 0)
		stop("no '_dat1.zim' file!")
	
    ## Check the content of all these "_dat1.zim" files 
	## and retrieve the number of items measured
	dat1files <- sort(dat1files)
	## Default to -1 for corrupted dat1 files
	nitems <- sapply(dat1files, function(x) {
		tryCatch(zimVerify(file.path(zidir, x), is.dat1 = TRUE ), 
			zooImageError = function (e) {
				logError(e)
				return(-1)
		})
	})
	ok <- all(nitems != -1)
	
	## Check the vignettes
	if (check.vignettes) {
        ## Check that we have corresponding vignettes (XXX+YY_ZZZ.jpg/png files)
    	samples <- sub("_dat1[.]zim$", "", dat1files)
		
    	## Check the content of the directory for .jpg or .png files
    	for (i in 1:length(samples)) {		
			## List the jpegs
    		regex <- gsub("[+]", "[+]", samples[i])
    		regex <- gsub("[.]", "[.]", regex)
    		regex2 <-  paste("^", regex, "_[0-9]+[.]jpg$", sep = "")
			vigstype <- "jpg"
    		vigs <- dir(zidir, pattern = regex2)
			if (!length(vigs)) { # Try also for .png vignettes
				regex2 <-  paste("^", regex, "_[0-9]+[.]png$", sep = "")
				vigstype <- "png"
				vigs <- dir(zidir, pattern = regex2)
			}
			
    		## Get their numbers, sort them, and make sure none is missing
    		n <- nitems[i]
			## If impossible to know how many items, just count vignettes
    		if (n < 1) n <- length(vigs)
    		
			## Construct a vector with names of vignettes as they should be
    		chkvigs <- paste(samples[i], "_", 1:n, ".", vigstype, sep = "")
    		if (length(vigs) == 0 && length(chkvigs) > 0) {
				warning(paste(" no vignettes for", samples[i]))
				ok <- FALSE
            } else if (length(chkvigs) != length(vigs) ||
				!all(sort(chkvigs) == sort(vigs))) {
				warning(paste(" mismatch vignettes for", samples[i]))
				ok <- FALSE 
			}
        }
    } 
	
	## Report results
	if (ok) {
		logProcess("OK", zidir, show.log = show.log)
	} else {
		logProcess("Errors found", zidir, show.log = show.log)
	}
	
	## TODO: we should not return ok here?
	return(invisible(ok))
}

zidVerifyAll <- function (path = ".", samples = NULL,
type = c("ZI1", "ZI2", "ZI3"), check.vignettes = TRUE, show.log = TRUE,
bell = FALSE)
{	
	## Verify all of these directories
	if (any(!type %in% c("ZI1", "ZI2", "ZI3")))
		stop("only 'ZI1', 'ZI2' or 'ZI3' are currently supported for 'type'")
	
	## First, switch to that directory
	inidir <- getwd()
	checkDirExists(path)
	on.exit(setwd(inidir))
	setwd(path)
	path <- "."	# Indicate we are now in the right path
	
	## Process the list of samples
	if (is.null(samples)) {	# Compute them from path
		d <- dir(path, pattern = "^[^_]")	# All items not starting with '_'
		samples <- unique(d[file.info(d)$isdir])	# Keep only directories
	}
	
	## If there is no dir, exit now
	if (is.null(samples) || length(samples) == 0)
		stop("There is no directories to verify in ", getwd())
	
	## Start the process
	smax <- length(samples)
	logClear()
	cat("Verification...\n")
	logProcess("\nVerification...")
	
	ok <- sapply(samples, function(s) {
		tryCatch(zidVerify(s, type = type,
			check.vignettes = check.vignettes, show.log = FALSE), 
			zooImageError = function (e) return(-1))
	})
	
	## Clean up
	finishLoop(all(ok), bell = bell, show.log = show.log)
}

## Compress one sample as a single .zid zipped file	
zidCompress <- function (zidir, type = c("ZI1", "ZI2", "ZI3"), check = TRUE,
check.vignettes = TRUE, replace = FALSE, delete.source = replace,
check.zip = TRUE, show.log = TRUE)
{		
	## Check the format
	if (any(!type %in% c("ZI1", "ZI2", "ZI3")))
		stop("only 'ZI1', 'ZI2' or 'ZI3' are currently supported for 'type'")
	
	## We need to switch to the root of sample dir first for correct path
	## in the zip file
	rootdir <- dirname(zidir)
	inidir <- getwd()
	on.exit(setwd(inidir))
	setwd(rootdir)
	zidir <- basename(zidir) # Use only the latest dir (the "sample dir")
	
	## The .zid file is located in the "root" dir, same name as the
	## "sample dir", with .zid extension
	zidfile <- paste(zidir, ".zid", sep = "")
	if (!replace && file.exists(zidfile)) {
		## It is not advised to delete source without rebuilding the .zid file
		## but it was expressly asked!
		### TODO: verify we have the same files in the .zid and initial dir
		## before deleting files!
		if (delete.source && file.exists(zidir))
			unlink(zidir, recursive = TRUE)
		return(invisible(TRUE))	# Nothing else to do
	}
	
	## Make sure everything is fine for this directory
	if (check)
		zidVerify(zidir, type = type, check.vignettes = check.vignettes,
			show.log = FALSE)
	
	## Make sure the .RData file is created (or refreshed)
	zidDatMake(zidir, type = type, replace = replace, show.log = FALSE)
	
	## Do compress the directory in the .zip file
	## Copy or move all corresponding files to a .zid zip-compressed file
	invisible(zip(zidfile, zidir, delete.source = delete.source))
	## TODO: log results...
}

## Compress all data in the corresponding directory
zidCompressAll <- function (path = ".", samples = NULL,
type = c("ZI1", "ZI2", "ZI3"), check = TRUE, check.vignettes = TRUE,
replace = FALSE, delete.source = replace, show.log = TRUE, bell = FALSE)
{ 
	if (any(!type %in% c("ZI1", "ZI2", "ZI3")))
		stop("only 'ZI1', 'ZI2' or 'ZI3' are currently supported for 'type'")
	
	## First, switch to that directory                                       
	inidir <- getwd()
	checkDirExists(path)
	on.exit(setwd(inidir))
	setwd(path)
	path <- "."	# Indicate we are now in the right path
	
	## Get the list of samples to process
	if (is.null(samples)) {	# Compute them from path
		d <- dir(path, pattern = "^[^_]")	# All items not starting with '_'
		samples <- unique(d[file.info(d)$isdir])	# Keep only directories
	}
	
	## If there is no dir, exit now
	if (is.null(samples) || length(samples) == 0)
		stop("There is no directories to process in ", getwd())
		
	## Start the process
	logClear()
	if (isTRUE(check)) {
		zidVerifyAll(path = path, samples = samples, 
			check.vignettes = check.vignettes, show.log = FALSE, bell = FALSE)
		## COMMENT: the previous version did log this message instead of the one
		## that is generated by zidVerifyAll
		## "contains corrupted files, compression not started!"
	}
		
	## Compress these files
	smax <- length(samples)
	cat("Compression...\n")
	logProcess("\nCompression...")
	ok <- TRUE
	for (s in 1:smax) {
		## Progress should be taken out of here since it is not really related 
		## to the function's job, instead we could throw a condition 
		## from zidCompress when it starts to indicates it has started
		Progress(s, smax)  
		
		tryCatch({  
			zidCompress(samples[s], type = type, check = FALSE, 
				check.vignettes = check.vignettes, replace = replace,
				delete.source = delete.source, check.zip = FALSE,
				show.log = FALSE)
			}, zooImageError = function (e) {
				logError (e)
				ok <<- FALSE
			}, zooImageWarning = function (w) {
				logWarning(w)
				ok <<- FALSE
			})
	}
	clearProgress()
	
	## Possibly clean the whole directory (move .zim files to \_raw
	## and delete the \_work subdir if everything is fine
	if (ok) zidClean(path = path, samples = samples)
	
	## Clean up
	finishLoop(ok = ok, bell = bell, show.log = show.log)
}

## Clean Zid (eliminate the _work subdirectory and move initial data to _raw)
zidClean <- function (path = ".", samples = NULL)
{
	## Do we have samples to process
    if (length(samples) == 0) return()
	
    ## First, switch to that directory
	inidir <- getwd()
    checkDirExists(path)
	on.exit(setwd(inidir))
	setwd(path)
	
	## Logging
	cat("Cleaning directory...\n")
	logProcess("\nCleaning directory...")
    
	## Identify paths
	zimfiles   <- zimList( "." )
	zimsamples <- sub("^(.*)[+].+", "\\1", zimfiles)
	
    ## Keep only those .zim files related to samples
    zimfiles <- zimfiles[zimsamples %in% samples]
	
	## Process
    if (length(zimfiles) > 0) {
        rawdir <- file.path(".", "_raw")
        
		## If the _raw subdirectory does not exists, create it
        if (!file.exists(rawdir)) dir.create(rawdir)
		copyto <- file.path(".", "_raw", zimfiles)
        
		## Move these .zim files
        for (i in 1:length(zimfiles))
			file.rename(zimfiles[i], copyto[i])
    }
	
    ## Delete completely the _work subdirectory
    unlink(file.path(".", "_work"), recursive = TRUE)    
}

## Uncompress a .zid file to get all its content. 
## Use 'delete.source = TRUE' with caution!
zidUncompress <- function (zidfile, path = dirname(zidfile),
delete.source = FALSE, show.log = TRUE)
{	
	## Check if the file provided is a .zid file, and if it exists
	checkFileExists(zidfile, extension = "zid")
	
	## Uncompress it
	unzip(zidfile, path, delete.source = delete.source)
}

## Uncompress all .zid files in the 'path.extract' directory
zidUncompressAll <- function (path = ".", zidfiles = zidList(path),
path.extract = path, skip.existing.dirs = TRUE, delete.source = FALSE,
show.log = TRUE, bell = FALSE)
{	
	## Initial checks
	if (is.null(zidfiles) || length(zidfiles) == 0)
        stop("no .zid files!")
	
	## start the process
	logClear()
	ok <- TRUE

	## Check that dirs / files with corresponding names exist in path.extract
	checkdirs  <- file.path(path.extract, noExt(zidfiles))
	fileExists <- file.exists(checkdirs) & !file.info(checkdirs)$isdir
	dirExists  <- file.exists(checkdirs) & file.info(checkdirs)$isdir
	
	## If any file not being a dir exist there, stop the process
	if (any(fileExists)) 
        stop("one or several files have same name as uncompressed dirs!")
	
	## Should we eliminate files whose corresponding dirs exist?
	if (skip.existing.dirs && any(dirExists)) {
		cat(sum(dirExists), "file(s) already uncompressed skipped!\n")
        warning(paste("Skipping already uncompressed file(s):", 
			paste(zidfiles[dirExists], collapse = ",")))
	}
	zidfiles <- zidfiles[!dirExists]
	
	## Decompress the files remaining in the list
	smax <- length(zidfiles)
	if (smax == 0) {
		cat("-- Done! - (nothing to decompress)\n")
		stop("\n -- Done! -- (nothing to decompress)", show.log = show.log)
	}
	
	## Start decompression
	cat("Decompression...\n")
	logProcess("\nDecompression...")
	for (s in 1:smax) {
		Progress(s, smax) 
		zidUncompress(zidfiles[s], path = path.extract,
			delete.source = delete.source, show.log = FALSE)
	}
	clearProgress()
	finishLoop(TRUE, bell = bell, show.log = show.log)
}

zidExtract <- function (file, zidfile)
{
	tmpd <- tempdir()
	unzip(zidfile, tmpd)
	file.path(tmpd, file)
}

## Make a .RData file that collates together data from all the "_dat1.zim" files
## of a given sample
zidDatMake <- function (zidir, type = "ZI3", replace = FALSE, show.log = TRUE) 
{
    if (any(!type %in% c("ZI1", "ZI2", "ZI3")))
		stop("only 'ZI1', 'ZI2' or 'ZI3' are currently supported for 'type'")
		
    RDataFile <- file.path(zidir, paste(basename(zidir), "_dat1.RData", 
        sep = ""))
    
    ## File already exists
    if (file.exists(RDataFile) && !replace) 
        return(invisible(TRUE))
    ok <- TRUE
    dat1files <- zimDatList(zidir)

    ## Create _dat1.zim file if it is missing (for FlowCAM data)
    if (length(dat1files) == 0) {
        ## Try to create them
        SmpDir <- dirname(zidir)
        ZimFile <- file.path(SmpDir, paste(basename(zidir), ".zim", sep = ""))
        zimDatMake(ZimFile)
        dat1files <- zimDatList(zidir)
        if (length(dat1files) == 0)
            stop("no '_dat1.zim' file!")
    }
    
    dat1files <- sort(dat1files)
    fractions <- sampleInfo(dat1files, "fraction")

    ## Avoid collecting duplicate informations about fractions
    fracdup <- duplicated(fractions)
    results <- lapply(seq.int(1, length(dat1files)), function (i) {
        dat1path <- file.path(zidir, dat1files[i])
        iszim <- tryCatch(is.zim(dat1path), zooImageError = function (e) {
            logError(e)
            return(FALSE)
        })
        if (!iszim) return(NULL)
        
        ## Read the header
        Lines <- scan(dat1path, character(), sep = "\t", skip = 1, 
            blank.lines.skip = FALSE, flush = TRUE, quiet = TRUE, 
            comment.char = "#")
        if (length(Lines) < 1) {
            logProcess("is empty, or is corrupted", dat1files[i])
            return(NULL)
        }
        
        ## Trim leading and trailing spaces in Lines
        Lines <- trimString(Lines)
        
        ## Convert underscore to space
        Lines <- underscoreToSpace(Lines)
        
        ## Determine start of the measurements table (it is '[Data]' header)
        endhead <- tail(which(Lines == "[Data]"), 1)
        if (!is.null(endhead) && endhead > 1) 
            Lines <- Lines[seq.int(1, endhead - 1)]
        
        ## Decrypt all lines, that is, split on first occurrence
		## of "=" into 'tag', 'value' and separate into sections
        if (!is.null(Lines))
            meta <- parseIni(Lines, sub("_dat1[.]zim$", "", fractions[i]))

        if (!is.null(endhead)) {
            mes <- read.table(dat1path, header = TRUE, sep = "\t", 
                dec = ".", as.is = FALSE, skip = endhead + 1, 
                comment.char = "#", na.strings = "null")
            ## We have several problems here:
            ## 1) There is sometimes a column full of NAs at the end.
            ##    This is because ImageJ adds an extra tab at the end of the line.
            ## [RF] FIXME: this should not be the case anymore because we have
            ## more control on what ImageJ is doing
			## [PhG] We keep this here anyway for old datasets!
            if (all(is.na(mes[, ncol(mes)]))) 
                mes <- mes[, -ncol(mes)]
            ## 2) The first column is the 'Item', but its name '!Item' is
            ##    transformed into 'X.Item'
            ## 3) The '%Area' is transformed into 'X.Area'
            Names <- names(mes)
            if (Names[1] == "X.Item") Names[1] <- "Item"
            if ("X.Area" %in% Names) Names[Names == "X.Area"] <- "PArea"
            ## Invert 'Item' and 'Label'
            mes <- mes[, c(2, 1, 3:ncol(mes))]
            Names <- Names[c(2, 1, 3:length(Names))]
            names(mes) <- make.names(Names, unique = TRUE)
            Sub <- meta$Subsample
            Sub$Dil <- 1/(Sub$SubPart * Sub$CellPart * Sub$Replicates * 
                Sub$VolIni)
            mes$Dil <- rep(Sub$Dil[Sub$Label == fractions[i]], nrow(mes))
        } else {
            mes <- NULL
        }
        list(meta = meta, mes = mes)
    })
    notnull.filter <- Negate(is.null)
    results <- Filter(notnull.filter, results)
    list.allmeta <- Filter(notnull.filter, lapply(results, "[[", "meta"))
    list.allmes <- Filter(notnull.filter, lapply(results, "[[", "mes"))
    
    combine <- function (.list) {
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

## Bug combine(list.allmeta) doesn't work!
##    allmeta <- combine(list.allmeta)
    list.allmeta <- list.allmeta[!fracdup] # only the levels of not duplicated metadata
    lmeta <- length(list.allmeta[])
    if (lmeta == 1) {
        allmeta <- combine(list.allmeta)
    } else {
        allmeta <- NULL
        for (i in 1:(lmeta - 1))
            allmeta <- listMerge(list.allmeta[[i]], list.allmeta[[i + 1]])
    }
## Bug combine(list.allmeta) doesn't work!

    allmes <- combine(list.allmes)
    rownames(allmes) <- 1:nrow(allmes)
    Names <- names(allmes)
    ## Calculate an ECD from Area if there is not one yet
    if (!"ECD" %in% Names && "Area" %in% Names) {
        ECD <- ecd(allmes$Area)
        allmes <- data.frame(allmes[, 1:2], ECD = ECD, allmes[, 3:ncol(allmes)])
    }
    attr(allmes, "metadata") <- allmeta
    class(allmes) <- c("ZI1Dat", "ZIDat", "data.frame")
    ZI.sample <- allmes
    save(ZI.sample, file = RDataFile, ascii = FALSE, version = 2,
		compress = TRUE)
    if (ok) ok <- file.exists(RDataFile)
    if (show.log) logView()
    return(invisible(ok))
}

## Read the .Rdata in a .zid file or corresponding directory
zidDatRead <- function (zidfile)
{	
	## Identify the file and stop if it does not exists
	sample <- noExt(basename(zidfile))
	RdataFile <- paste(sample, "_dat1.RData", sep = "")
	deletefile <- FALSE
	checkFileExists(zidfile, message = "%s not found!")
	
	## Treat different kind of files
	if (!hasExtension(zidfile, "zid")) {
		# Is it a directory?
		if (file.info(zidfile)$isdir) {
			# Is there a .RData file in this directory?
			rdata <- file.path(zidfile, RdataFile)
			if (!file.exists(rdata)) {
				# Try to create it
				zidDatMake(zidfile, show.log = FALSE)
				checkFileExists(rdata,
					message = "Error creating the .RData file")
			}
		} else stop(sprintf("Unrecognized file: %s", zidfile))
	} else { 
		# This is a .zid file
		rdata <- file.path(sample, RdataFile)
		rdata <- zidExtract(rdata, zidfile)
		if (rdata == "")
			stop(sprintf("Error reading .RData file from %s", zidfile))
		deletefile <- TRUE
	}
	
	## Load that file
	ZI.sample <- NULL
	load(rdata)
	
	## Delete the file
	if (deletefile) {
		unlink(rdata)
		# If the directory is empty, delete it also
		datadir <- file.path(tempdir(), sample)
		if (file.exists(datadir) && (length(dir(datadir)) == 0))
			unlink(datadir)
	}
	
	## Set the class 
	if (!inherits(ZI.sample, "ZIDat") && inherits(ZI.sample, "data.frame"))
		class(ZI.sample) <- c("ZI1Dat", "ZIDat", "data.frame")
	return(ZI.sample)
}
