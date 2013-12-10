## ZooImage >= 3 importation routines

#### Importation of FlowCAM data without image reanalysis ######################
## Read a FlowCAM .ctx file
readFlowCAMctx <- function (ctx, stop.it = TRUE)
{
	## Check arguments
	stop.it <- isTRUE(as.logical(stop.it))
	## ctx must be an existing file
	if (!file.exists(ctx))
		if (stop.it)
			stop("'ctx' must be an existing (.ctx) file") else return(NULL)
	
	## Read .ctx data
	dat <- scan(ctx, what = character(), sep = "\t", skip = 0,
		blank.lines.skip = TRUE, flush = TRUE, quiet = TRUE, comment.char = "")
	
	## This is an .ini format
	V <- parseIni(dat, label = "")
	
	## Rework a few fields
	
	## Strings are imported as factors, but we really want characters
	factorsAsStrings <- function (x)
		as.data.frame(lapply(x, function (x)
		if (is.factor(x)) as.character(x) else x), stringsAsFactors = FALSE)
	V <- lapply(V, factorsAsStrings)
	
	## Empty strings are imported as logical with NA value
	logicalNaAsStrings <- function (x)
		as.data.frame(lapply(x, function (x)
		if (is.logical(x) && is.na(x)) "" else x), stringsAsFactors = FALSE)
	V <- lapply(V, logicalNaAsStrings)
	
	## Special conversion into POSIXct for $General$RunStartTime and $RunEndTime
	V$General$RunStartTime <- as.POSIXct(V$General$RunStartTime)
	V$General$RunEndTime <- as.POSIXct(V$General$RunEndTime)
	
	## We need these keys that may not be present in old .ctx files	
	if (is.null(V$Fluid$TotalVolumeML)) {
		## Volume calculation
		Height <- (V$CaptureRegion$AcceptableBottom -
			V$CaptureRegion$AcceptableTop) * V$Fluid$CalibConstant
		Width <- (V$CaptureRegion$AcceptableRight -
			V$CaptureRegion$AcceptableLeft) * V$Fluid$CalibConstant
		Area <- Height * Width
		## Volume of one image
		Volume <- (Area / (1e8)) * (V$Fluid$FlowCellDepth / 10000) # mL
		V$Fluid$TotalVolumeML <- Volume * V$CaptureStats$RawImageTotal
	}

	## This is missing in 1.5.14, but can be calculated
	if (is.null(V$CameraBehavior$AutoImageRate))
		V$CameraBehavior$AutoImageRate <- V$CaptureStats$RawImageTotal /
			V$CaptureStats$ImageCaptureTotal.Seconds
	
	## In 1.5.14, no distinction beween ThresholdDark and ThresholdLight
	## So, copy Threshold to both of them
	if (!is.null(V$CaptureParameters$Threshold) &&
		is.null(V$CaptureParameters$ThresholdDark)) {
		V$CaptureParameters$ThresholdDark <- V$CaptureParameters$Threshold
		V$CaptureParameters$ThresholdLight <- V$CaptureParameters$Threshold
	}
	
	## In 1.5.14, no RecalibrationIntervalMinutes but SaveIntervalMinutes
	V$Files$RecalibrationIntervalMinutes <- V$Files$SaveIntervalMinutes 
	
	## Calculated fields (wrong units or other problems)
	mins <- V$RunTermination$MaxRunTimeMinutes
	if (length(mins) == 0) mins <- 0
	secs <- V$RunTermination$MaxRunTimeSeconds
	if (length(secs) == 0) secs <- 0
	V$RunTermination$MaxRunTime <- mins * 60 + secs
	
	## Return the resulting list
	V	
}

## Examples
#ctxFile <- "/Users/phgrosjean/Desktop/Intercalibration/BE.ArMix.2009-04-29.300A4X_01/BE.ArMix.2009-04-29.300A4X_01.ctx"
#readFlowCAMctx(ctxFile)
## A 1.5.14 file
#ctxFile1 <- "/Users/phgrosjean/Documents/Pgm/ZooPhytoImage_1.2-1-examples/FlowCAM-example-FIT-VIS/143-144526.ctx"
#readFlowCAMctx(ctxFile1)

## Read a flowCAM .lst file
readFlowCAMlst <- function (lst, skip = 2, read.ctx = TRUE)
{
    ## Check arguments
	## lst must be an existing file
	if (!file.exists(lst))
		stop("'lst' must be an existing (.lst) file")
	## skip at least 2 rows, but for realtime, can skip more...
	skip <- as.integer(skip)[1]
	if (skip < 2) {
		warning("'skip' cannot be lower than 2... fixed!")
		skip <- 2
	}
	read.ctx <- isTRUE(as.logical(read.ctx))
	
	## For format 017 we have now column names hardcoded
	header <- scan(lst, what = character(), nlines = 2L, quiet = TRUE)
	if (as.integer(header[1]) >= 17 && substr(header[2], 1, 10) == "num-fields") {
		## Format >= 17. Columns names are hardcoded!
		nfields <- as.integer(strsplit(header[2], "|", fixed = TRUE)[[1]][2])
		if (!length(nfields) || is.na(nfields) || nfields < 44)
			stop("Unrecognized .lst file format: number of fields is ", nfields)
		skip <- nfields + 2
		## Read column header information
		hcol <- scan(lst, what = character(), sep = "|", skip = 2L,
			nlines = nfields, quiet = TRUE)
		if (length(hcol) != nfields * 2)
			stop("Unrecognized .lst file format: incorrect header")
		hcol <- matrix(hcol, ncol = 2, byrow = TRUE)
		cnames <- hcol[, 1]
		## Make sure all names start with FIT_ and are Capitalized
		capital <- function(x) {
			s <- strsplit(x, "_")
			sapply(s, function (x) paste(toupper(substring(x, 1, 1)),
				substring(x, 2), sep = "", collapse = "_"))
		}
		cnames <- paste("FIT", capital(cnames), sep = "_")
		## Special replacements
		cnames <- sub("Abd", "ABD", cnames)
		cnames <- sub("Esd", "ESD", cnames)
		cnames <- sub("FIT_Ch([1-9])_Width", "FIT_Ch\\1_TOF", cnames)
		## We need to replace names by their zooimage equivalent
		cnames[cnames == "FIT_Id"] <- "Id" # The only one not starting woth FIT_
		cnames[cnames ==  "FIT_ABD_Area"] <-  "FIT_Area_ABD"
		cnames[cnames == "FIT_ABD_Diameter"] <- "FIT_Diameter_ABD"
		cnames[cnames == "FIT_ESD_Diameter"] <- "FIT_Diameter_ESD"
		cnames[cnames == "FIT_Raw_Perimeter"] <- "FIT_Raw_Perim"
		cnames[cnames == "FIT_Raw_Convex_Perimeter"] <- "FIT_Raw_Convex_Perim"
		cnames[cnames == "FIT_Collage_File"] <- "FIT_Filename"
		cnames[cnames == "FIT_Timestamp"] <- "FIT_Timestamp1"
		cnames[cnames == "FIT_Image_X"] <- "FIT_SaveX"
		cnames[cnames == "FIT_Image_Y"] <- "FIT_SaveY"
		cnames[cnames == "FIT_Image_W"] <- "FIT_PixelW"
		cnames[cnames == "FIT_Image_H"] <- "FIT_PixelH"
		cnames[cnames == "FIT_Src_X"] <- "FIT_CaptureX"
		cnames[cnames == "FIT_Src_Y"] <- "FIT_CaptureY"
		cnames[cnames == "FIT_Src_Image"] <- "FIT_Source_Image"
		cnames[cnames == "FIT_Cal_Image"] <- "FIT_Calibration_Image"
		## Note: in comparison to old format, we have in addition:
		#"FIT_Camera", "FIT_Fringe_Size", "FIT_Circle_Fit", "FIT_Ch1_Area",            
        #"FIT_Ch2_Area", "FIT_Ch3_Area"    
		
		## Read the data in
		tab <- read.table(lst, header = FALSE, sep = "|", dec = ".", 
            skip = skip, col.names = cnames)
		## Add missing fields from the previous versions
		tab$FIT_Ch4_Peak <- NA
		tab$FIT_Ch4_TOF <- NA
		tab$FIT_High_U32 <- NA
        tab$FIT_Low_U32 <- NA
        tab$FIT_Total <- NA
		tab$FIT_Timestamp2 <- as.character(NA)
	} else { # Older format. We have to guess column names!	
		## Determine version of the FlowCAM's table according to number of cols
		ncol <- length(read.table(lst, header = FALSE, sep = ":", dec = ".",
			skip = skip, nrows = 1))
    
		## Read .lst data
		## TODO: if export file exists, verify column names here (.csv file)
		if (ncol == 44) {  # This should be FlowCAM II
			tab <- read.table(lst, header = FALSE, sep = ":", dec = '.',
				skip = skip, col.names = c("Id", "FIT_Cal_Const", "FIT_Raw_Area",
				"FIT_Raw_Feret_Max", "FIT_Raw_Feret_Min", "FIT_Raw_Feret_Mean",
				"FIT_Raw_Perim", "FIT_Raw_Convex_Perim", "FIT_Area_ABD",
				"FIT_Diameter_ABD", "FIT_Length", "FIT_Width", "FIT_Diameter_ESD",
				"FIT_Perimeter", "FIT_Convex_Perimeter", "FIT_Intensity",
				"FIT_Sigma_Intensity", "FIT_Compactness", "FIT_Elongation",
				"FIT_Sum_Intensity", "FIT_Roughness", "FIT_Feret_Max_Angle",
				"FIT_Avg_Red", "FIT_Avg_Green", "FIT_Avg_Blue", "FIT_PPC",
				"FIT_Ch1_Peak", "FIT_Ch1_TOF", "FIT_Ch2_Peak", "FIT_Ch2_TOF",
				"FIT_Ch3_Peak", "FIT_Ch3_TOF", "FIT_Ch4_Peak", "FIT_Ch4_TOF",
				"FIT_Filename", "FIT_SaveX", "FIT_SaveY", "FIT_PixelW",
				"FIT_PixelH", "FIT_CaptureX", "FIT_CaptureY", "FIT_High_U32",
				"FIT_Low_U32", "FIT_Total"))
        
			## Add columns present in .lst from FlowCAM III (same table for all)
			tab$FIT_Feret_Min_Angle <- NA
			tab$FIT_Edge_Gradient <- NA
			tab$FIT_Timestamp1 <- NA
			tab$FIT_Timestamp2 <- NA
			tab$FIT_Source_Image <- NA
			tab$FIT_Calibration_Image <- NA
    
		} else if (ncol == 47) { # This should be FlowCAM III
			tab <- read.table(lst, header = FALSE, sep = ":", dec = '.',
				skip = skip, col.names = c("Id", "FIT_Cal_Const", "FIT_Raw_Area",
				"FIT_Raw_Feret_Max", "FIT_Raw_Feret_Min", "FIT_Raw_Feret_Mean",
				"FIT_Raw_Perim", "FIT_Raw_Convex_Perim", "FIT_Area_ABD",
				"FIT_Diameter_ABD", "FIT_Length", "FIT_Width", "FIT_Diameter_ESD",
				"FIT_Perimeter", "FIT_Convex_Perimeter", "FIT_Intensity",
				"FIT_Sigma_Intensity", "FIT_Compactness", "FIT_Elongation",
				"FIT_Sum_Intensity", "FIT_Roughness", "FIT_Feret_Max_Angle",
				"FIT_Feret_Min_Angle", "FIT_Avg_Red", "FIT_Avg_Green",
				"FIT_Avg_Blue", "FIT_PPC", "FIT_Ch1_Peak", "FIT_Ch1_TOF",
				"FIT_Ch2_Peak", "FIT_Ch2_TOF", "FIT_Ch3_Peak", "FIT_Ch3_TOF",
				"FIT_Ch4_Peak", "FIT_Ch4_TOF", "FIT_Filename", "FIT_SaveX",
				"FIT_SaveY", "FIT_PixelW", "FIT_PixelH", "FIT_CaptureX",
				"FIT_CaptureY", "FIT_Edge_Gradient", "FIT_Timestamp1",
				"FIT_Timestamp2", "FIT_Source_Image", "FIT_Calibration_Image"))
        
			## Add columns present in list files from FlowCAM II
			tab$FIT_High_U32 <- NA
			tab$FIT_Low_U32 <- NA
			tab$FIT_Total <- NA
		
		} else stop("Unrecognized FlowCAM format") # TODO: adapt for the new soft
	}
	
	## New variables calculation (present in export .csv from the FlowCAM)
	## Code already checked
    tab$FIT_Volume_ABD <- (4/3) * pi * (tab$FIT_Diameter_ABD/2)^3
    tab$FIT_Volume_ESD <- (4/3) * pi * (tab$FIT_Diameter_ESD/2)^3
    tab$FIT_Aspect_Ratio <- tab$FIT_Width / tab$FIT_Length
    tab$FIT_Transparency <- 1 - (tab$FIT_Diameter_ABD/tab$FIT_Diameter_ESD)
    tab$FIT_Red_Green_Ratio <- tab$FIT_Avg_Red / tab$FIT_Avg_Green
    tab$FIT_Blue_Green_Ratio <- tab$FIT_Avg_Blue / tab$FIT_Avg_Green
    tab$FIT_Red_Blue_Ratio <- tab$FIT_Avg_Red / tab$FIT_Avg_Blue
    tab$FIT_Ch2_Ch1_Ratio <- tab$FIT_Ch2_Peak / tab$FIT_Ch1_Peak
    
    ## Try to extract metadata from .ctx file, if it exists
    ctx <- sub("\\.lst$", ".ctx", lst)
	if (read.ctx && file.exists(ctx)) {
		ctxData <- readFlowCAMctx(ctx)
		## TODO: return data in correct ZooImage format directly
		attr(tab, "FlowCAM.metadata") <- ctxData
    }
	
    tab
}

## Example
#lstFile <- "/Users/phgrosjean/Desktop/Intercalibration/BE.ArMix.2009-04-29.300A4X_01/BE.ArMix.2009-04-29.300A4X_01.lst"
#res <- readFlowCAMlst(lstFile)
#lstFile1 <- "/Users/phgrosjean/Documents/Pgm/ZooPhytoImage_1.2-1-examples/FlowCAM-example-FIT-VIS/143-144526.lst"
#res1 <- readFlowCAMlst(lstFile1)

## Temporary name!
importFlowCAM <- function (lst, rgb.vigs = TRUE)
{
	dat <- readFlowCAMlst(lst, skip = 2, read.ctx = TRUE)
	## Check results
	if (!is.data.frame(dat) && NROW(dat) < 1)
		stop("Problem while importing FlowCAM data, or empty series")
	if (is.null(attr(dat, "FlowCAM.metadata")))
		stop("Problem while importing FlowCAM metadata from the .ctx file")
	
	## Create metadata from FlowCAM.metadatata
	## TODO...
	
	## ImportVignettes
	#require(tiff)
	#require(png)
	
	## List all tiff files in the directory (but exclude masks with _bin.tif)
	sampledir <- dirname(lst)
	odir <- setwd(sampledir)
	on.exit(setwd(odir))
	
	## Make sure zidbdir exists and is empty
	## TODO: use a fresh dir, or erase existing one with user's acceptation
	zidbdir <- file.path(dirname(sampledir), "_import", basename(sampledir))
	if (file.exists(zidbdir) && dir(zidbdir) != 0)
		stop("The destination dir already exists and is not empty!")
	dir.create(zidbdir, recursive = TRUE, showWarnings = FALSE)
	
	tif <- dir(sampledir, pattern = "[0-9]\\.tif$", full.names = FALSE)
	## Separate the list into collages and background calibration images
	isCal <- grepl("^.*cal_image_[0-9]+\\.tif$", tif)
	calFiles <- tif[isCal]
	colFiles <- tif[!isCal]
	## Check we have at least one image for each set
	if (length(calFiles) == 0)
		stop("No background calibration image found")
	if (length(colFiles) == 0)
		stop("No collages found")
		
	## Read all background calibration images into a list
	cals <- list()
	for (i in 1:length(calFiles)) {
		cals[[i]] <- readTIFF(source = calFiles[i])
		## If the image is RGB, we got three dimensions to reduce to two
		cdim <- dim(cals[[i]])
		if (length(cdim) == 3 && cdim[3] == 3) {
			## Calculate the CIE 1931 linear luminance Y as grayscale
			## Y = 0.2126 R + 0.7152 G + 0.0722 B
			cals[[i]] <- 0.2126 * cals[[i]][, , 1] + 0.7152 * cals[[i]][, , 2] +
				0.0722 * cals[[i]][, , 3]
		}
		if (length(dim(cals[[i]])) != 2)
			stop("unrecognized calibration image type; ",
				"cannot convert it to 8-bit grayscale")	
	}
	
	## Read collages one by one and extract vignettes from them,
	## using information gathered into dat
	colFile <- "" # File of current collage
	## Since R indexing starts at 1 but FlowCAM pixel indexing starts at 0,
	## add one where it is required
	dat1 <- dat
	dat1$FIT_SaveX <- dat$FIT_SaveX + 1
	dat1$FIT_SaveY <- dat$FIT_SaveY + 1
	dat1$FIT_CaptureX <- dat$FIT_CaptureX + 1
	dat1$FIT_CaptureY <- dat$FIT_CaptureY + 1
	
	## Extract a submatrix, given coordinates X1, Y1, X2, Y2
	crop <- function (mat, coords)
		mat[coords[2]:coords[4], coords[1]:coords[3]]
	
	## Determine best gray level for background after substraction
	gray <- attr(dat, "FlowCAM.metadata")$CaptureParameters$ThresholdLight
	if (!length(gray)) {
		warning("Unknown threshold gray level; using 40")
		gray <- 40 # Target something like 40
	}
	gray <- gray / 255
	## Threshold = 2 * gray, since we add it once while subtracting background
	threshold <- 1 - 2 * gray
	
	## Proceed with each vignette
	for (i in 1:nrow(dat1)) {
		d <- dat1[i, ]
		## Do we need to load the next collage?
		if (as.character(d$FIT_Filename) != colFile) {
			filename <- as.character(d$FIT_Filename)
			collage <- readTIFF(source = filename)
			colFile <- d$FIT_Filename
			colFiles <- colFiles[colFiles != filename]
			## If the image is RGB, we got three dimensions to reduce to two
			cdim <- dim(collage)
			if (length(cdim) == 3 && cdim[3] == 3) {
				## Calculate the CIE 1931 linear luminance Y as grayscale
				## Y = 0.2126 R + 0.7152 G + 0.0722 B
				collage <- 0.2126 * collage[, , 1] + 0.7152 * collage[, , 2] +
					0.0722 * collage[, , 3]
			}
			if (length(dim(collage)) != 2)
				stop("unrecognized collage image type; ",
					"cannot convert it to 8-bit grayscale")
		}
		
		## Get coordinates of the vignette in that collage
		size <- c(d$FIT_PixelW, d$FIT_PixelH) - 1 # Still the problem of 0 vs 1
		colCoords <- c(d$FIT_SaveX, d$FIT_SaveY)
		colCoords <- c(colCoords, colCoords + size)
		calCoords <- c(d$FIT_CaptureX, d$FIT_CaptureY)
		calCoords <- c(calCoords, calCoords + size)
			
		## Extract the vignette and corresponding background from the collage
		vig <- crop(collage, colCoords)
		## If  FIT_Calibration_Image is NA, use first one => TODO: check this!
		if (is.na(d$FIT_Calibration_Image)) d$FIT_Calibration_Image <- 1
		back <- crop(cals[[d$FIT_Calibration_Image]], calCoords)
		
		## Substract background and save vignette
		vig2 <- 1 + vig - back - gray
		vig2[vig2 > 1] <- 1
		vig2[vig2 < 0] <- 0
		
		if (isTRUE(rgb.vigs)) {
			## Calculate mask
			mask <- matrix(1, nrow = nrow(vig2), ncol = ncol(vig2))
			mask[vig2 > threshold] <- 0
			## Do we need to fill holes?
			## TODO...
			
			## Combine grayscales and mask into a RGB image
			vig2 <- structure(c(vig2, vig2, mask), dim = c(dim(vig2), 3))
		}
		
		## Write this vignette
		vigFile <- file.path(zidbdir,
			sub("\\.tif$", paste0("_", i, ".png"), filename))
		writePNG(image = vig2, target =  vigFile)
	}
	
	## Create zidb
	## TODO...
	dat
}

## Example
## Test version 2.2.1
#lstFile <- "/Users/phgrosjean/Desktop/Intercalibration/BE.ArMix.2009-04-29.300A4X_01/BE.ArMix.2009-04-29.300A4X_01.lst"
#res <- importFlowCAM(lstFile)
## Test version 1.5.14
## TODO: This does not work (incorrect number of dimensions => imports images as an array?)
#lstFile1 <- "/Users/phgrosjean/Documents/Pgm/ZooPhytoImage_1.2-1-examples/FlowCAM-example-FIT-VIS/143-144526.lst"
#res1 <- importFlowCAM(lstFile1)
