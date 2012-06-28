## Copyright (c) 2012, Ph. Grosjean <phgrosjean@sciviews.org> & K. Denis
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

## Make a ZooImage database file for one sample
zidbMake <- function (zidir, zidbfile = paste0(zidir, ".zidb"), type = "ZI3",
check = TRUE, check.vignettes = TRUE, replace = FALSE,
delete.source = replace, show.log = TRUE)
{		
	## Check the format
	if (type != "ZI3")
		stop("only 'ZI3' is currently supported for 'type'")
	
	if (!isTRUE(replace) && file.exists(zidbfile)) {
		## Nothing to do... the file already exists
		if (isTRUE(delete.source) && file.exists(zidir) && file.info(zidir)$isdir)
			unlink(zidir, recursive = TRUE)
		return(invisible(TRUE))	# Nothing else to do
	}
	
	## Make sure everything is fine for this directory
	if (isTRUE(check))
		zidVerify(zidir, type = type, check.vignettes = check.vignettes,
			show.log = FALSE)
	
	## Make sure the .RData file is created (or refreshed)
	zidDatMake(zidir, type = type, replace = replace, show.log = FALSE)
	
    ## List all vignettes
    Vigs <- dir(zidir, pattern = "\\.jpg$", full.names = TRUE)
	## May be the vignettes are in .png format...
	if (!length(Vigs)) {
		Vigs <- dir(zidir, pattern = "\\.png$", full.names = TRUE)
		VigType <- "png"
	} else VigType <- "jpg"
	if (!length(Vigs)) {
		## TODO: log the error!
		return(invisible(FALSE))
	}
    ## List all .zim files
    Zims <- dir(zidir, pattern = "\\.zim$", full.names = TRUE)
	if (!length(Zims))
# log!	stop("No .zim files found!")
    
## TODO: read this from the root dir!	
#	## list zis file
#    Zis <- dir(zidir, pattern = ".zis$", full.names = TRUE)
    
    ## Create the .zidb file: put all vignettes there, plus the .RData file
# log    cat("Creating the ZIDB file...\n")
    filehashOption(defaultType = "DB1")
    unlink(zidbfile)
    dbCreate(zidbfile)
    db <- dbInit(zidbfile)

    ## Indicate which zooimage version and which image type we use
    dbInsert(db, ".ZI", 3)
    dbInsert(db, ".ImageType", VigType)

    ## Read each vignette in turn and add it to the database
# log!    cat("Adding vignettes to ZIDB file...\n")
    VigExt <- paste0("\\.", VigType, "$")
	for (i in 1:length(Vigs)) {
    	Vig <- Vigs[i]
    	VigName <- sub(VigExt, "", basename(Vig))
    	VigSize <- file.info(Vig)$size
# log!    	if (is.na(VigSize)) stop("File '", Vig, "' not found, or of null length")
    	dbInsert(db, VigName, readBin(Vig, "raw", VigSize + 100))
    }
    
    ## Add .zim files to db
# log!    cat("Adding zim files to ZIDB file...\n")
    for (i in 1:length(Zims)) {
    	Zim <- Zims[i]
    	ZimName <- sub("\\.zim$", "", basename(Zim))
    	ZimSize <- file.info(Zim)$size
# log!    	if (is.na(ZimSize)) stop("File '", Zim, "' not found or of null length")
    	dbInsert(db, ZimName, readBin(Zim, "raw", ZimSize + 100))
    }

    ## Add zis file to db
#    if (length(Zis) != 0) {
#log!        cat("Adding sample data to .zidb file...\n")
#        zisname <- basename(Zis)
#       	zisSize <- file.info(Zis)$size
#       	if (is.na(zisSize)) stop("File ", zis, " not found")
#       	dbInsert(db, zisname, readBin(Zis, "raw", zisSize + 100))
#   	}

    ## Add the data frame with all data and metadata to the file
# log!    cat("Adding data to ZIDB file...\n")
    zidat <- file.path(zidir, paste0(basename(zidir), "_dat1.RData"))
    obj <- load(zidat)
# log!    if (length(obj) != 1) stop("Error loading ", zidat)
    dbInsert(db, ".Data", get(obj))

	## Do we delete sources?
    if (isTRUE(delete.source))
        unlink(zidir, recursive = TRUE)
		
	return(invisible(TRUE))
}

## Make all .zidb files for data in the corresponding directory
zidbMakeAll <- function (path = ".", samples = NULL,
type = "ZI3", check = TRUE, check.vignettes = TRUE,
replace = FALSE, delete.source = replace, show.log = TRUE, bell = FALSE)
{ 
	if (type != "ZI3")
		stop("only 'ZI3' is currently supported for 'type'")
	
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
		## from zidbMakeAll when it starts to indicates it has started
		Progress(s, smax)  
		
		tryCatch({  
			zidbMake(samples[s], type = type, check = FALSE, 
				check.vignettes = check.vignettes, replace = replace,
				delete.source = delete.source, show.log = FALSE)
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

## Convert .zid file to .zidb file
## TODO: add replace = FALSE, delete.source = replace, show.log = TRUE, bell = FALSE
zidToZidb <- function (zidfile)
{
    ZidDir <- sub("\\.zid$", "", zidfile)
    IniDir <- dirname(zidfile)
    ## Unzip the file...
# log!    cat("Unzipping ZID file '", basename(zidfile), "' ...\n", sep = "")
    zooimage:::unzip(zidfile, path = IniDir)
    zidbMake(zidir = ZidDir, delete.source = TRUE)
}

## Convert all .zid files to .zidb files
## TODO: add replace = FALSE, delete.source = replace, show.log = TRUE, bell = FALSE
zidToZidbAll <- function (zidfiles)
{
    for (zidfile in zidfiles) {
# log!        cat("Converting zid file: ", i, " on ", Tot, "\n", sep = "")
        zidToZidb(zidfile)
    }
}

## Convert a .zidb file to a .zid file
## TODO: add replace = FALSE, delete.source = replace, show.log = TRUE, bell = FALSE
zidbToZid <- function (zidbfile)
{
    ZidbDir <- sub("\\.zidb$", "", zidbfile)
    ## Create the directory to extract data
    dir.create(ZidbDir)
    ## Link database to objects in memory
    Zidb <- zidbLink(zidbfile)
    ## All files in Zidb
    AllFiles <- ls(Zidb) # List only variables not starting with . => zim + vignettes
#    ## zis file
#    ZisFile <- grep(".zis", AllFiles)
#    if (length(ZisFile)) {
#        Zisname <- AllFiles[ZisFile]
#        writeBin(Zidb[[Zisname]], con = file.path(ZidbDir, Zisname))
#    }

    # .zim files
    isZimFile <- grep("_dat1$", AllFiles)
    ZimNames <- AllFiles[isZimFile]
# log!    cat("Extracting Zim files\n")
    for (ZimName in ZimNames)
        writeBin(Zidb[[ZimName]], con = file.path(ZidbDir, paste0(ZimName, ".zim")))
    
	## Vignettes
    VignNames <- AllFiles[!isZimFile]
    cat("Extracting vignettes\n")
    for(i in 1 : length(VignNames)){
        writeBin(Zidb[[VignNames[i]]], con = file.path(ZidbDir, paste(VignNames[i], ".jpg", sep = "")))
    }
    # Rdata
    ZI.sample <- Zidb$.DATA
    cat("Extracting Rdata file\n")
    save(ZI.sample, file = file.path(ZidbDir, paste(sub(".zidb", "", basename(zidbfile)), "_dat1.RData", sep = "")))    
    # Create zid file
    cat("Compressing zid file\n")
    zidCompress(zidir = ZidbDir, delete.source = TRUE)
}

# Convert .zidb files to .zid files
## TODO: use path!
## TODO: add replace = FALSE, delete.source = replace, show.log = TRUE, bell = FALSE
zidbToZidAll <- function (zidbfiles)
{
## TODO: rework this!!!
    n <- length(zidbfiles)
    if (n > 0) {
		for (i in 1:n) {
		    cat("Converting zidb file: ", i, " on ", n, "\n", sep = "")
		    zidbToZid(zidbfiles[i])
		}
	}
}


## Link the database to R objects
zidbLink <- function (zidbfile)
	db2env(dbInit(zidbfile))

## Read only Rdata file from a .zidb database
zidbDatRead <- function (zidbfile)
{	
    Zidb <- zidbLink(zidbfile)
    ZI.sample <- Zidb$.Data
	return(ZI.sample)
}

## Functions to plot a collage
zidbPlot <- function (main = "ZooImage collage", ...)
{
	par(mfrow = c(1, 1), mar = c(0.1, 0.1, 2.1, 0.1))
	plot(0:1, 0:1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
		xaxs = "i", yaxs = "i", xlim = 0:1, ylim = 0:1, bty = "o", main = main, ...)
}

## Function to get a vignette from the database, rescale it and draw it in its
## corresponding vignette area
zidbDrawVignette <- function (rawimg, type = "jpg", area, nx = 5, ny = 5, vmar = 0.01)
{
	## Coordinates of centers for each vignette, on a graph area of [0, 1] on x and y
	nv <- nx * ny
	## Coordinates for centers of each vignette area
	xc <- (1:nx) / nx - 1/(nx * 2)
	yc <- (ny:1) / ny - 1/(ny * 2) # Because we want to start at the top and it is the higher coord
	## x and y coordinates for each vignette (fill from left to right and top to bottom)
	vcoord <- expand.grid(list(x = xc, y = yc))
	## Half width and half height of a vignette area
	vhw <- ((xc[2] - xc[1]) - vmar) / 2
	vhh <- ((yc[1] - yc[2]) - vmar) / 2

	## Coordinates of top-left and bottom-right for vignettes areas
	vtl <- vcoord
	vtl$x <- vtl$x - vhw
	vtl$y <- vtl$y + vhh
	vbr <- vcoord
	vbr$x <- vbr$x + vhw
	vbr$y <- vbr$y - vhh

	## rawimg is a raw object containing JPEG or PNG data
	## area is the number of vignette area in the collage where to draw the vignette
	area <- as.integer(area[1])
	if (area < 1 || area > length(vtl$x)) stop("Wrong vignette area number")

	## Conversion from a raw object to a displayable image is done using readPNG() or readJPEG()
	## from the png/jpeg packages... For fast processing, use native format,
	## but 16bit not accepted for PNG and there is a problem in case of transparency channel
	## (if any) in PNG images on windows devices
	if (type == "png") {
		vigimg <- readPNG(rawimg, native = TRUE)
	} else vigimg <- readJPEG(rawimg, native = TRUE)
	vigdim <- dim(vigimg) # Dimensions of the image in pixels
	## Determine top-left and bottom-right points of vignette bounding rectangle
	## for optimum display...
	## top-left point is always from the grid
	xleft <- vtl$x[area]
	ytop <- vtl$y[area]

	## Size of internal collage area (which is [0,1] both in x and y) in pixels
	totpx <- dev.size(units = "px")
	plt <- par("plt")
	totpx[1] <- totpx[1] * (plt[2] - plt[1]) # Width of collage area in pixels
	totpx[2] <- totpx[2] * (plt[4] - plt[3]) # Height of collage are in pixels

	## Size of vignette areas in pixels
	vwpx <- vhw * 2 * totpx[1]
	vhpx <- vhh * 2 * totpx[2]

	## If the vignette is smaller than the area, it fits without rescaling!
	if (vigdim[2] <= vwpx && vigdim[1] <= vhpx) {
		xright <- xleft + 2 * vhw / vwpx * vigdim[2]
		ybottom <- ytop - 2 * vhh / vhpx * vigdim[1]
	} else { # We need to rescale down the vignette to fit it in the area
		## Which dimension will fit the whole area?
		vigratio <- vigdim[2] / vigdim[1]
		arearatio <- vwpx / vhpx
		if (vigratio < arearatio) { # Fit height
			ybottom <- ytop - (2 * vhh)
			xright <- xleft + (2 * vhh * vigratio / arearatio)
		} else { # Fit width
			xright <- xleft + (2 * vhw)
			ybottom <- ytop - (2 * vhw / vigratio * arearatio)
		}
	}

	## Interpolation only works outside of windows!
	interpolate <- (names(dev.cur()) != "windows")

	## Note that if there is a transparency layer, a special treatment
	## is required for windows devices, see ?readPNG

	## Now, display that vignette in the collage
	rasterImage(vigimg, xleft, ybottom, xright, ytop, interpolate = interpolate)
}
