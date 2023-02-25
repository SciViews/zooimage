# Copyright (c) 2004-2018, Ph. Grosjean <phgrosjean@sciviews.org>
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
# along with ZooImage. If not, see <http://www.gnu.org/licenses/>.

# Get information about a sample, given its name
sampleInfo <- function(filename,  type = c("sample", "fraction", "image",
"scs", "date", "id", "frac", "imgnbr"), ext = "_dat[135][.]zim$") {
  base <- basename(as.character(filename))
  if (ext != "") base <- sub(ext, "", base)

  # Filename without extension is supposed to follow the convention:
  # scs.date.id+f[img] with scs.date.id forming an unique sample identifier
  # Note: not all verifications are conducted. So, it sometimes returns a
  # result even if the name does not conform to this specification!
  # TODO: check that the name follows the convention and determine what is
  #       optional, like date, for instance)
  switch(match.arg(type),
    sample = sub("\\+[a-zA-Z][0-9.]+$", "", base),
    fraction = sub("[0-9.]+$", "", base),
    image = base,
    scs = sub("[+.].+$", "", base),
    date = {
      res <- try(
        as.Date(sub("^.*([0-9]{4}-[0-1][0-9]-[0-3][0-9]).*$", "\\1", base)),
        silent = TRUE)
      if (inherits(res, "try-error")) {
        warning("Wrong sample filename: impossible to extract the sample date")
        as.Date(NA)
      } else res
    },
    id = sub("^.*\\..*\\.(.*)\\+.*$", "\\1", base),
    frac = sub("^.*\\+([a-zA-Z]).*$", "\\1",base),
    imgnbr = as.numeric(sub("^.*\\+[a-zA-Z]([0-9.]*)$", "\\1", base)),
    {
      warning("'type' must be 'sample', 'fraction', 'image', 'scs', 'date', ",
        "'id', 'frac' or 'imgnbr'")
      character(0)
    }
  )
}

# Convert underscores into spaces
underscoreToSpace <- function(string)
  gsub("_", " ", string)

# Trim leading and trailing white spaces and tabs
trimString <- function(string)
  sub("\\s+$", "", sub("^\\s+", "", string))

# All sample with at least one entry in a given object
listSamples <- function(ZIobj) {
  if (!inherits(ZIobj, c("ZIDat", "ZIDesc","ZITrain","ZITest"))) {
    warning("'ZIobj' must be a 'ZIDat', 'ZIDesc', 'ZITrain' or 'ZITest' object")
    return(character(0))
  }

  # List all samples represented in a given object
  if (inherits(ZIobj, "ZIDat")) {
    res <- sort(unique(sampleInfo(as.character(ZIobj$Label),
    type = "sample", ext = "")))
  } else if (inherits(ZIobj, "ZIDesc")) {
    res <- sort(unique(as.character(ZIobj$Label)))
  } else if (inherits(ZIobj, c("ZITrain", "ZITest"))) {
    res <- as.character(ZIobj$Id)
    res <- sub("_[0-9]*$", "", res)
    res <- sort(unique(sampleInfo(res, type = "sample", ext = "")))
  }
  res
}

# Unique identifiers (Ids) are a combination of Label and Item
makeId <- function(ZIDat)
  paste(ZIDat$Label, ZIDat$Item, sep = "_")

# Add classes into a ZIDat object, from ZITrain or ZITest objects
addClass <- function(ZIDat, ZIobj) {
  # Is there a 'Class' variable in ZIobj?
  Cl <- ZIobj$Class
  if (!length(Cl))
    stop("No 'Class' column found in the ZIobj object")
  # Select only those items that are in ZIDat, in the correct order...
  Id <- ZIobj$Id
  if (!length(Id)) Id <- makeId(ZIobj)
  if (!length(Id)) stop("unable to get particle Ids from 'ZIobj'")
  names(Cl) <- Id
  ZIDat$Class <- Cl[makeId(ZIDat)]
  ZIDat
}

# Reanalyze images using scikit-image (python via reticulate)
skimageVars <- function(zidbfile) {
  use_python("/usr/bin/python3")
  #use_virtualenv("skimage")
  skimage <- import("skimage")
  np <- import("numpy", convert = FALSE)

  # Lazy loading data from one ZIDB file in R
  db1 <- zidbLink(zidbfile)

  # Get the list of all vignettes in this dataset
  items1 <- ls(db1) # Contains data in *_dat1 and vignettes in *_nn
  vigs1 <- items1[-grep("_dat", items1)]
  lvigs1 <- length(vigs1)
  if (!lvigs1)
    stop("No vignettes found in the file '", zidbfile,
      "'. Are you sure it is correct?")

  # Read one vignette at a time into R, and calculate skimage attributes
  for (i in 1:lvigs1) {
    # Note: (try 1 for good vignette -only one item- and
    # 18 for a bad vignette with two items)
    vig <- vigs1[i]
    png <- db1[[vig]]
    img <- readPNG(png, native = FALSE)

    # Blue channel: the mask is greylevel 50, and the rest is visual again
    mask <- (img[ , , 3] != 50/255) + 0

    # We convert the [0, 1] scale of png into [0, 255] and get it as Numpy array
    # But we first need integer inside our object
    mask2 <- as.integer(mask * 255L)
    dim(mask2) <- dim(mask)
    mask3 <- skimage$util$img_as_ubyte(np$array(mask2)) < 128
    # Note: connectivity is supposed to be mask3.ndim in Python, with value = 2L here
    label_mask3 <- skimage$measure$label(mask3, connectivity = 2L)
    # Again, in R, automatic conversion into numeric!
    dim_lm3 <- dim(label_mask3)
    label_mask3 <- as.integer(label_mask3)
    dim(label_mask3) <- dim_lm3
    # Get the inverted OD image
    invod <- as.integer(img[ , , 1] * 255L)
    dim(invod) <- dim(img[ , , 1])

    # Measure the particles
    # This does not work for convex_area and solidity because an array
    # passed from R to Python is F-contiguous while a C-contiguous array is needed!
    #props <- skimage$measure$regionprops(label_image = label_mask3, intensity_image = invod)
    py$mask <- label_mask3
    py$invod <- invod
    py_run_string("import skimage; props = skimage.measure.regionprops(label_image = skimage.util.img_as_uint(mask.copy(order = 'C')), intensity_image = invod.copy(order = 'C'))", convert = FALSE)
    props <- py$props
    # In case we got several blobs, check which one is the right one
    item <- 1
    l <- length(props)
    # In case we got several items, check which one is better filling the area
    # (ZooImage increases the bbox by 150%, but sometimes, it fails because the
    # object is too close to the border(s)!)
    if (l > 1) {
      idim <- dim(mask)
      deltas <- numeric(0)
      for (j in 1:l) {
        bbox <- unlist(props[[j]]$bbox)
        deltas[[j]] <- (idim[1] - (bbox[3] - bbox[1]) * 1.5) +
          (idim[2] - (bbox[4] - bbox[2]) * 1.5)
      }
      item <- which.min(deltas)
    }
    prop <- props[[item]]

    # Now, get items (note: same columns as for the ZOoscan dataset)
    inertia_tensor <- as.numeric(prop$inertia_tensor)
    inertia_tensor_eigvals <- as.numeric(prop$inertia_tensor_eigvals)
    moments_hu <- as.numeric(prop$moments_hu)
    moments_normalized <- as.numeric(prop$moments_normalized)
    weighted_moments_hu <- as.numeric(prop$weighted_moments_hu)
    weighted_moments_normalized <- as.numeric(prop$weighted_moments_normalized)
    res1 <- data.frame(
      objid = vig,
      area = prop$area,
      convex_area = prop$convex_area,
      eccentricity = prop$eccentricity,
      equivalent_diameter = prop$equivalent_diameter,
      euler_number = prop$euler_number,
      filled_area = prop$filled_area,
      inertia_tensor0 = inertia_tensor[1],
      inertia_tensor1 = inertia_tensor[2],
      inertia_tensor2 = inertia_tensor[3],
      inertia_tensor3 = inertia_tensor[4],
      inertia_tensor_eigvals0 = inertia_tensor_eigvals[1],
      inertia_tensor_eigvals1 = inertia_tensor_eigvals[2],
      major_axis_length = prop$major_axis_length,
      max_intensity = prop$max_intensity,
      mean_intensity = prop$mean_intensity,
      min_intensity = prop$min_intensity,
      minor_axis_length = prop$minor_axis_length,
      moments_hu0 = moments_hu[1],
      moments_hu1 = moments_hu[2],
      moments_hu2 = moments_hu[3],
      moments_hu3 = moments_hu[4],
      moments_hu4 = moments_hu[5],
      moments_hu5 = moments_hu[6],
      moments_hu6 = moments_hu[7],
      moments_normalized0 = moments_normalized[1],
      moments_normalized1 = moments_normalized[2],
      moments_normalized2 = moments_normalized[3],
      moments_normalized3 = moments_normalized[4],
      moments_normalized4 = moments_normalized[5],
      moments_normalized5 = moments_normalized[6],
      moments_normalized6 = moments_normalized[7],
      moments_normalized7 = moments_normalized[8],
      moments_normalized8 = moments_normalized[9],
      moments_normalized9 = moments_normalized[10],
      moments_normalized10 = moments_normalized[11],
      moments_normalized11 = moments_normalized[12],
      moments_normalized12 = moments_normalized[13],
      moments_normalized13 = moments_normalized[14],
      moments_normalized14 = moments_normalized[15],
      moments_normalized15 = moments_normalized[16],
      perimeter = prop$perimeter,
      solidity = prop$solidity,
      weighted_moments_hu0 = weighted_moments_hu[1],
      weighted_moments_hu1 = moments_hu[2],
      weighted_moments_hu2 = moments_hu[3],
      weighted_moments_hu3 = moments_hu[4],
      weighted_moments_hu4 = moments_hu[5],
      weighted_moments_hu5 = moments_hu[6],
      weighted_moments_hu6 = moments_hu[7],
      weighted_moments_normalized0 = weighted_moments_normalized[1],
      weighted_moments_normalized1 = weighted_moments_normalized[2],
      weighted_moments_normalized2 = weighted_moments_normalized[3],
      weighted_moments_normalized3 = weighted_moments_normalized[4],
      weighted_moments_normalized4 = weighted_moments_normalized[5],
      weighted_moments_normalized5 = weighted_moments_normalized[6],
      weighted_moments_normalized6 = weighted_moments_normalized[7],
      weighted_moments_normalized7 = weighted_moments_normalized[8],
      weighted_moments_normalized8 = weighted_moments_normalized[9],
      weighted_moments_normalized9 = weighted_moments_normalized[10],
      weighted_moments_normalized10 = weighted_moments_normalized[11],
      weighted_moments_normalized11 = weighted_moments_normalized[12],
      weighted_moments_normalized12 = weighted_moments_normalized[13],
      weighted_moments_normalized13 = weighted_moments_normalized[14],
      weighted_moments_normalized14 = weighted_moments_normalized[15],
      weighted_moments_normalized15 = weighted_moments_normalized[16]
    )
    # What about centroid, weighted_centroid, moments, moments_central, orientation,   and weighted_moments?
    if (i == 1) res <- res1 else res <- rbind(res, res1)
  }
  res
}

# Default list of variables to drop
# Version 3.0-1: added a list of useless FIT variables to be dropped
dropVars <- function() {
  res <- try(get("ZI.dropVarsDef"), silent = TRUE)
  if (inherits(res, "try-error"))
    res <- getOption("ZI.dropVarsDef",
      c("Id", "Label", "Item", "X", "Y", "XM", "YM", "BX", "BY", "Width",
      "Height", "Angle", "XStart", "YStart", "Dil", "Predicted",
      "Predicted2", "FIT_Cal_Const", "FIT_Avg_Red", "FIT_Avg_Green",
      "FIT_Avg_Blue", "FIT_PPC", "FIT_Ch1_Peak", "FIT_Ch1_TOF",
      "FIT_Ch2_Peak", "FIT_Ch2_TOF", "FIT_Ch3_Peak", "FIT_Ch3_TOF",
      "FIT_SaveX", "FIT_SaveY", "FIT_PixelW", "FIT_PixelH",
      "FIT_CaptureX", "FIT_CaptureY", # Keep this one?"FIT_Edge_Gradient",
      "FIT_Source_Image", "FIT_Calibration_Image", "FIT_High_U32",
      "FIT_Low_U32", "FIT_Total", "FIT_Red_Green_Ratio",
      "FIT_Blue_Green_Ratio", "FIT_Red_Blue_Ratio",
      "FIT_Ch2_Ch1_Ratio", "FIT_Ch4_Peak", "FIT_Ch4_TOF", "FIT_Timestamp1",
      "FIT_Timestamp2", "FIT_Camera", "FIT_FringSize",
      "FIT_Ch1_Area", "FIT_Ch2_Area", "FIT_Ch3_Area",
      "FIT_TimeStamp1", "FIT_Source_Image.1",
      "X.Item.1", "FeretAngle", "Count",
      "Skew", "Kurt", "Solidity", # Last 3: NAs with multiple ROIs

      "MinFeret", "AR", "Round", # Problems with these variables at IFREMER!?

      # Added in zooimage v.5:
      "FIT_Filename", "FIT_Feret_Min_Angle", "FIT_Feret_Max_Angle",

      # This is somehow redundant with other variables
      "FIT_Raw_Area", "FIT_Raw_Perim", "FIT_Raw_Convex_Perim",
      "FIT_Raw_Feret_Max", "FIT_Raw_Feret_Min", "FIT_Raw_Feret_Mean",
      "FIT_Diameter_ABD", # This one is indeed ECD

      # Changes in variables names
      "FIT_Ppc", "FIT_Fringe_Size", "FIT_Circle_Fit",

      # Found in format 17 of a color FlowCAM (from KAUST) and not used yet
      "FIT_Symmetry", "FIT_Circularity_Hu", "FIT_Intensity_Calimage",
      "FIT_Raw_Convex_Hull_Area", "FIT_Raw_Filled_Area",
      "FIT_CircleFit", "FIT_Edge_Gradient"

      # TODO: should we drop also Id.1, Class, Validated and Suspect???
    ))
  as.character(res)
}

# Calculate derived variables... default function
calcVars <- function(x, drop.vars = NULL, drop.vars.def = dropVars()) {
  # This is the calculation of derived variables
  # Note that you can make your own version of this function for more
  # calculated variables!

  # Calculate derived variables... FlowCAM's Visual Spreadsheet
  calcVarsVIS <- function(x, drop.vars = NULL, drop.vars.def = dropVars()) {
    # Use only FIT_xxx vars, andderived attributes (26 attributes in total):
    # ECD, FIT_Area_ABD, FIT_Length, FIT_Width, FIT_Diameter_ESD,
    # FIT_Perimeter, FIT_Convex_Perimeter, FIT_Intensity, FIT_Sigma_Intensity,
    # FIT_Compactness, FIT_Elongation, FIT_Sum_Intensity, FIT_Roughness,
    # FIT_Volume_ABD, FIT_Volume_ESD, FIT_Aspect_Ratio, FIT_Transparency,
    # CV, MeanFDia, Transp2, FeretRoundness & Perim_Ratio

    # A small hack to correct some 0 (which can be problematic in further calcs)
    noZero <- function(x) {
      x[x == 0] <- 1e-09
      x
    }

    # Euclidean distance between two points
    distance <- function(x, y)
      sqrt(x^2 + y^2)

    # All FIT_Raw_xxx vars have their counterpart resized in um:
    # FIT_Raw_Area -> FIT_Diameter_ABD
    # FIT_Raw_Feret_Max -> FIT_Length
    # FIT_Raw_Feret_Min -> FIT_Width
    # FIT_Raw_Feret_Mean -> FIT_Diameter_ESD
    # FIT_Raw_Perim -> FIt_Perimeter
    # FIT_Raw_Convex_Perim -> FIt_Convex_Perimeter
    # (=> all FIT_Raw_xxx should be eliminated in dropVars()!)

    # (re)calculate ECD from FIT_DIameter_ABD (was once calc from FIT_Raw_Area)
    x$ECD <- noZero(ecd(x$FIT_Area_ABD))
    x$FIT_Area_ABD <- noZero(x$FIT_Area_ABD)
    x$FIT_Length <- noZero(x$FIT_Length)
    x$FIT_Width <- noZero(x$FIT_Width)
    x$FIT_Diameter_ESD <- noZero(x$FIT_Diameter_ESD)
    x$FIT_Perimeter <- noZero(x$FIT_Perimeter)
    x$FIT_Convex_Perimeter <- noZero(x$FIT_Convex_Perimeter)
    x$FIT_Intensity <- noZero(x$FIT_Intensity)
    x$FIT_Sigma_Intensity <- noZero(x$FIT_Sigma_Intensity)
    x$FIT_Sum_Intensity <- noZero(x$FIT_Sum_Intensity)
    x$FIT_Compactness <- noZero(x$FIT_Compactness)
    x$FIT_Elongation <- noZero(x$FIT_Elongation)
    x$FIT_Roughness <- noZero(x$FIT_Roughness)
    x$FIT_Aspect_Ratio <- noZero(x$FIT_Aspect_Ratio)
    x$FIT_Volume_ABD <- noZero(x$FIT_Volume_ABD)
    x$FIT_Volume_ESD <- noZero(x$FIT_Volume_ESD)
    x$FIT_Transparency <- noZero(x$FIT_Transparency)
    x$FIT_Edge_Gradient <- noZero(x$FIT_Edge_Gradient)

    # Additional calculated variables
    # This is FIT_Aspect_Ratio! x$ARFeret <- x$FIT_Width/x$FIT_Length
    # For later on:
    x$EdgeRange <- abs(x$FIT_Intensity - x$FIT_Edge_Gradient)
    x$CV <- x$FIT_Sigma_Intensity/x$FIT_Intensity * 100
    x$MeanFDia <- (x$FIT_Length + x$FIT_Width) / 2
    x$Transp2 <- 1 - (x$FIT_Diameter_ABD/x$MeanFDia)
    x$Transp2[x$Transp2 < 0] <- 0
    x$FeretRoundness <- 4 * x$FIT_Area_ABD/(pi * sqrt(x$FIT_Length))
    # ImageJ calculation
    x$Circ. <- 4 * pi * x$FIT_Area_ABD / sqrt(x$FIT_Perimeter)
    # For later on:
    x$EdgeCV <- x$FIT_Sigma_Intensity/x$FIT_Edge_Gradient * 100
    x$EdgeSDNorm <- x$FIT_Intensity/x$EdgeRange
    x$Perim_Ratio <- x$FIT_Convex_Perimeter / x$FIT_Perimeter

    # Eliminate variables that are not predictors... and use Id as rownames
    Id <- x$Id
    if (length(Id)) rownames(x) <- Id

    # Variables to drop
    # For those samples treated with FIT_VIS in ImageJ, we need to get rid of
    # the ImageJ variables
    x$Area <- NULL
    x$Mean <- NULL
    x$StdDev <- NULL
    x$Mode <- NULL
    x$Min <- NULL
    x$Max <- NULL
    x$Perim. <- NULL
    x$Major <- NULL
    x$Minor <- NULL
    x$Circ. <- NULL
    x$Feret <- NULL
    x$IntDen <- NULL
    x$Median <- NULL

    dropAll <- unique(as.character(c(drop.vars, drop.vars.def)))
    for (dropVar in dropAll) x[[dropVar]] <- NULL

    # Return the recalculated data frame
    x
  }

  # For data from the FlowCAM, we use a specific function
  if (any(names(x) == "FIT_Length"))
    return(calcVarsVIS(x, drop.vars = drop.vars, drop.vars.def = drop.vars.def))

  # A small hack to correct some 0 (which can be problematic in further calcs)
  noZero <- function(x) {
    x[x == 0] <- 0.000000001
    x
  }

  # Euclidean distance between two points
  distance <- function(x, y)
    sqrt(x^2 + y^2)

  x$Minor <- noZero(x$Minor)
  x$Major <- noZero(x$Major)
  x$AspectRatio <- x$Minor / x$Major
  x$CentBoxD <- distance(x$BX + x$Width/2 - x$X, x$BY + x$Height/2 - x$Y)
  x$GrayCentBoxD <- distance(x$BX + x$Width/2 - x$XM, x$BY + x$Height/2 - x$YM)
  x$CentroidsD <- distance(x$X - x$XM, x$Y - x$YM)
  x$Range <- x$Max - x$Min
  x$MeanPos <- (x$Max - x$Mean) / x$Range
  x$SDNorm <- x$StdDev / x$Range
  x$CV <- x$StdDev / x$Mean * 100
  x$Area <- noZero(x$Area)
  #x$logArea <- log(x$Area)
  x$Perim. <- noZero(x$Perim.)
  #x$logPerim. <- log(x$Perim.)
  #x$logMajor <- log(x$Major)
  #x$logMinor <- log(x$Minor)
  #x$logECD <- log(noZero(x$ECD))
  x$Feret <- noZero(x$Feret)
  #x$logFeret <- log(x$Feret)
  x$MeanDia <- (x$Major + x$Minor) / 2
  x$MeanFDia <- (x$Feret + x$Minor) / 2
  #x$logMeanDia <- log(x$MeanDia)
  #x$logMeanFDia <- log(x$MeanFDia)
  x$Transp1 <- 1 - (x$ECD / x$MeanDia)
  x$Transp1[x$Transp1 < 0] <- 0
  x$Transp2 <- 1 - (x$ECD / x$MeanFDia)
  x$Transp2[x$Transp2 < 0] <- 0
  PA <- x$Perim.^2/16 - x$Area
  x$Elongation <- ifelse(PA <= 0, 1, x$Area / (x$Perim./4 - PA^.5)^2)
  x$Compactness <-  x$Perim.^2/4/pi/x$Area  # env. 1/Circ.
  x$Roundness <- 4 * x$Area / (pi * sqrt(x$Major))

  # Eliminate variables that are not predictors... and use Id as rownames
  Id <- x$Id
  if (length(Id)) rownames(x) <- Id

  # Variables to drop
  dropAll <- unique(as.character(c(drop.vars, drop.vars.def)))
  for (dropVar in dropAll) x[[dropVar]] <- NULL

  # Return the recalculated data frame
  x
}

# Calculate equivalent circular diameter (similar to equivalent spherical
# diameter, but for 2D images)
ecd <- function(area, cells = 1)
  2 * sqrt(area / cells / pi)

# Parse an ini file (.zim, .zie, etc. are .ini files!)
# TODO: manage the case where there is no '=' in the data!
parseIni <- function(data, label = "1") {
  # Parse an ini file (tag=value => 'tag', 'value')
  # and make a list with different sections

  # Is str a section?
  is.section <- function(str)
    as.logical(length(grep("^\\[.+\\]$", trimString(str)) > 0))

  # Get the name of a section
  get.section.name <- function(str)
    sub("^\\[", "", sub("\\]$", "", trimString(str)))

  # Transform a vector of characters into a data frame,
  # possibly with type conversion
  vector.convert <- function(vec)
    as.data.frame(lapply(as.list(vec), type.convert, as.is = TRUE))

  if (!length(data) || !inherits(data, "character"))
    return(character(0))

  # Trim leading and trailing white spaces
  data <- trimString(data)

  # Convert underscore to space
  data <- underscoreToSpace(data)

  # Eliminate empty lines
  data <- data[data != ""]
  data <- paste(data, " ", sep = "")
  if (!length(data)) return(character(0))
  # Substitute the first '=' sign by another separator unlikely to appear in
  # the argument
  data <- sub("=", "&&&&&", data)

  # Split the strings according to this separator
  data <- strsplit(data, "&&&&&")

  # Get a matrix
  data <- t(as.data.frame(data))
  rownames(data) <- NULL

  # Make sure we have a section for the first entries (otherwise, use [.])
  if (!is.section(data[1, 1]))
    data <- rbind(c("[.]", "[.]"), data)
  Names <- as.vector(trimString(data[, 1]))
  Dat <- as.vector(trimString(data[, 2]))

  # Determine which is a section header
  Sec <- grep("\\[.+\\]$", Names)
  SecNames <- get.section.name(Names[Sec])

  # Make a vector of sections
  if (length(Sec) == 1) {
    SecNames <- rep(SecNames, length(Names))
  } else {
    SecNames <- rep(SecNames, c(Sec[2:length(Sec)], length(Names) + 1) - Sec)
  }

  # Replace section headers from all vectors
  Names[Sec] <- "Label"
  Dat[Sec] <- label
  names(Dat) <- Names

  # Transform SecNames in a factor
  SecNames <- as.factor(SecNames)

  # Split Dat on sections
  DatSec <- split(Dat, SecNames)

  # For each section, transform the vector in a data frame and possibly
  # convert its content
  DatSec <- lapply(DatSec, vector.convert)

  # Eliminate "Label" if it is ""
  if (label == "")
    DatSec <- lapply(DatSec, function(x) x[-1])

  DatSec
}

# Grayscale calibration in O.D. scale
# TODO: rework all this using ImageJ in zooimagej (should be much faster)
calibrate <- function(ODfile) {
  # TODO: include also a spatial calibration procedure
  # (with a black circle around the center of the image)
  # and check also other characteristics, especially the sharpness

  cal <- c(NA, NA)
  names(cal) <- c("WhitePoint", "BlackPoint")
  msg <- character(0)

  if (!file.exists(ODfile)) {
    msg <- paste("O.D. file '", ODfile, "' not found!", sep = "")
    attr(cal, "msg") <- msg
    return(cal)
  }

  # Is it a test file?
  #if (.isTestFile(ODfile)) {
  #  # We behave like if the file was correct and return fake calibration data!
  #  cal <- c(1000, 50000)
  #  names(cal) <- c("WhitePoint", "BlackPoint")
  #  attr(cal, "msg") <- character(0)
  #  return(cal)
  #}

  #filedir <- dirname(ODfile)
  #if (filedir != ".") {
  #  # Temporary change directory to the one where the file is located
  #  inidir <- setwd(filedir)
  #  on.exit(setwd(inidir))
  #  ODfile <- basename(ODfile)
  #}

  # The command to use depends on the format of the image (determined on the
  # extension)
  #ext <- tolower(rev(strsplit(ODfile, "\\.")[[1]])[1])
  #pgmfile <- ODfile
  #if (ext == "tif") {
  #  ## First, convert into a .pgm file
  #  pgmfile <- paste(ODfile, "pgm", sep = ".")
####    netpbm_tifftopnm( ODfile, pgmfile )
  #  delfile <- TRUE
  #  ext <- "pgm"
  #} else delfile <- FALSE
  #if (ext != "pgm")
  #  return(paste("Unrecognized image format for '", ODfile, "'", sep = ""))
####  OD <- netpbm_pgmhist(pgmfile, delete = delfile)

  ## Make sure we work with 16bit images
  #if (max(OD$Gray) < 256) {
  #  msg <- c(msg, "O.D. seems to be a 8bit image (16bit required)")
  #} else {
  #  ## Eliminate values with low number of points
  #  OD <- OD[OD$Count > 100, ]

  # PhG: new code... fully implemented in R
  grays <- readTIFF(ODfile, as.is = TRUE)
  grays <- sort.int(as.integer(grays), method = "quick")
  grays <- as.data.frame(unclass(rle(grays)))
  OD <- grays[grays$lengths > 200, ]
  names(OD) <- c("Count", "Gray")

  # Look at range: should be widespread enough, but without saturation
  rngOD <- range(OD$Gray)
  if (rngOD[2] > 65500) msg <-
    c(msg, "Images are overexposed, or whitepoint is already calibrated")
  if (rngOD[2] < 55000)
    msg <- c(msg, "Images are underexposed")

  # Saturation on the left-side of the histogram is not much a problem!
  if (rngOD[2] - rngOD[1] < 40000)
    msg <- c(msg, "Images lack contrast")
  # We should end up with four segments
  graylev <- OD$Gray
  gap <- (diff(graylev) > 500)

  # There are not *exactly* four gaps => problem with the image!
  if (sum(gap) != 4) {
    msg <- c(msg, "Impossible to calibrate O.D.: wrong image")
  } else {
    # Get the five peaks, analyze them and get modes for blank, NDx2,
    # NDx4 and NDx8
    peaks <- as.factor(cumsum(c(0, gap)) + 1)
    peaksgray <- split(graylev, peaks)
    names(peaksgray) <- c("Black", "NDx8", "NDx4", "NDx2", "White")

    # These are supposed to be all narrow peaks... check this
    peakspan <- sapply(peaksgray, range)
    peaksrange <- peakspan[2, ] - peakspan[1, ]

    # 1.2-2: width of black peak is much larger for Epson 4990
    # => be more tolerant for that peak
    if (any(peaksrange > c(20000, rep(5000, 4)))) {
      wrongpeaks <- paste(names(peaksrange)[peaksrange > 5000], collapse = ", ")
      msg <- c(msg, paste("Wrong O.D. image: lack of homogeneity for",
        wrongpeaks))
    }

    # Look for the gray levels at the top of the peaks
    peaksheight <- split(OD$Count, peaks)
    names(peaksheight) <- c("Black", "NDx8", "NDx4", "NDx2", "White")
    findmax <- function(x)
      which.max(lowess(x, f = 0.05, iter = 1)$y)
    peaksval <- sapply(peaksheight, findmax)

    # Get the number of pixels in the white peak
    nbrwhite <- peaksheight$White[peaksval["White"]]

    # Replace the location by the actual gray level
    for (i in 1:5)
      peaksval[i] <- peaksgray[[i]][peaksval[i]]
    # If the number of pixels for pure white is larger than the white
    # peak found, replace it by pure white (65535)
    nbrpurewhite <- OD[nrow(OD), 2]
    if (nbrpurewhite > nbrwhite)
      peaksval["White"] <- 65535

    # Now, we need to calibrate the black and white points
    WhitePoint <- 65535 - peaksval["White"]

    # Perform a correction for the white point
    peaksval <- peaksval + WhitePoint

    # Transform those gray levels into O.D.
    peaksOD <- log(peaksval) * 65535 / log(65535)

    # Create a data frame with gray levels and corresponding OD for
    # White, NDx2, NDx4 and NDx8
    calib <- data.frame(Gray = peaksOD[5:2], OD = c(0, 0.3, 0.6, 0.9))

    # Fit a line on these data
    calib.lm <- lm(OD ~ Gray, data = calib)

    # Check that calibration line is fine (i.e., the ANOVA should
    # reject H0 at alpha = 5%)
    if (anova(calib.lm)[["Pr(>F)"]][1] > 0.01)
      msg <- c(msg, "Wrong OD calibration: not a straight line relation at alpha level = 0.01")

    # Check also that R squared is at least 0.98
    rsq <- summary(calib.lm)$r.squared
    if (rsq < 0.98)
      msg <- c(msg, paste("Bad OD calibration (R squared = ",
        formatC(rsq, digits = 3), ")", sep = ""))

    # Check linearity of the relationship by fitting a second order
    # polynome and by looking at the t-test for the x square parameter
    calib2.lm <- lm(OD ~ I(Gray^2) + Gray, data = calib)
    if (summary(calib2.lm)$coefficients["I(Gray^2)", "Pr(>|t|)"] < 0.01)
      msg <- c(msg, "Nonlinear OD calibration at alpha level = 0.01")

    # Calculate the value of the black point to get 0.004 OD per gray
    # level after conversion (see the manual)
    ccoef <- coef(calib.lm)
    BlackPoint <- (1.024 - ccoef[1]) / ccoef[2]

    # Get the calibration data
    cal[1] <- round(WhitePoint)
    cal[2] <- round(BlackPoint)
  }
  attr(cal, "msg") <- msg
  cal
}
# example:
# setwd("g:/zooplankton/madagascar2macro")
# calibrate("test.tif")

# Decimal separator to use in import/export ZooImage files
getDec <- function() {
  Dec <- getOption("OutDec", ".")
  # It must be either "." or ","!
  if (!Dec %in% c(".", ","))
    Dec <- "."
  Dec
}

# Add a comment (from a zimfile) into a zip archive
zipNoteAdd <- function(zipfile, zimfile) {
  zipfile <- as.character(zipfile)
  if (length(zipfile) != 1) {
    warning("exactly one 'zipfile' must be provided")
    return(FALSE)
  }
  if (!file.exists(zipfile)) {
    warning("'zipfile' not found: '", basename(zipfile), "'")
    return(FALSE)
  }

  zimfile <- as.character(zimfile)
  if (length(zimfile) != 1) {
    warning("exactly one 'zimfile' must be provided")
    return(FALSE)
  }
  if (!file.exists(zimfile)) {
    warning("'zimfile' not found: '", basename(zimfile), "'")
    return(FALSE)
  }

  if (isWin()) {
    cmd <- sprintf('%s /c type "%s" | "%s" -zq "%s" ', Sys.getenv("COMSPEC"),
      zimfile, Sys.getenv("R_ZIPCMD", "zip"), zipfile)
    res <- try(system(cmd, show.output.on.console = FALSE, invisible = TRUE,
      intern = FALSE), silent = TRUE)
  } else {
    cmd <- sprintf('zip -zq "%s" < "%s" ', zipfile, zimfile)
    res <- try(system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE,
      intern = FALSE), silent = TRUE)
  }
  if (inherits(res, "try-error")) {
    warning(as.character(res)) # Turn error into warning
    return(FALSE)
  }

  if (res != 0) {
    warning("error while adding .zim data to '", basename(zipfile), "'")
    FALSE
  } else TRUE
}

# Extract the comment from the zipfile
zipNoteGet <- function(zipfile, zimfile = NULL) {
  zipfile <- as.character(zipfile)
  if (length(zipfile) != 1) {
    warning("exactly one 'zipfile' must be provided")
    return(NULL)
  }
  if (!file.exists(zipfile)) {
    warning("'zipfile' not found: '", basename(zipfile), "'")
    return(NULL)
  }

  if (length(zimfile)) {
    zimfile <- as.character(zimfile)
    if (length(zimfile) != 1) {
      warning("exactly one 'zimfile' must be provided")
      return(NULL)
    }
  }
  # Make sure old data do not remain in zimfile
  unlink(zimfile)

  # We use unzip... and assume it is located at the same place as zip!
  if (isWin()) {
    zippgm <- Sys.getenv("R_ZIPCMD", "zip")
    unzippgm <- sub("zip$", "unzip", zippgm)
    if (unzippgm == zippgm || inherits(try(system("unzip", intern = TRUE),
      silent = TRUE), "try-error")) {
      warning("'unzip' program is required, but not found")
      return(NULL)
    }
    cmd <- sprintf('"%s" -zq "%s"', unzippgm, zipfile)
    res <- try(system(cmd, invisible = TRUE, intern = TRUE), silent = TRUE)
  } else {# Linux or MacOS
    cmd <- sprintf('unzip -zq "%s"', zipfile)
    res <- try(system(cmd, intern = TRUE), silent = TRUE)
  }
  if (inherits(res, "try-error")) {
    warning(as.character(res))
    return(NULL)
  }

  if (length(res) < 2) {
    warning("no comment data found in '", basename(zipfile), "'")
    return(character(0))
  }

  # Write the output to the file if needed and return the result
  if (length(zimfile)) {
    cat(res, file = zimfile, sep = "\n")
    invisible(res)
  } else res
}

.make_scales <- function(pixels.per.unit, units = "mm", base.dir = tempdir()) {
  # Depending on the range of pixels.per.unit, we cook different bar scales
  # This range should be wide enough... or you should use different units!
  if (pixels.per.unit <= 12) {# For instance, 300dpi
    coef <- 8
    vals <- c("2.4", "4", "8")
  } else if (pixels.per.unit <= 25) {# For instance, 600dpi
    coef <- 4
    vals <- c("1.2", "2", "4")
  } else if (pixels.per.unit <= 50) {# For instance, 1200dpi
    coef <- 2
    vals <- c("0.6", "1", "2")
  } else if (pixels.per.unit <= 100) {# For instance, 2400dpi
    coef <- 1
    vals <- c("0.3", "0.5", "1")
  } else if (pixels.per.unit <= 200) {# For instance, 4800dpi
    coef <- 0.5
    vals <- c(".15", ".25", "0.5")
  } else {# >= 9600dpi
    coef <- 0.25
    vals <- c(".07", ".12", ".25")
  }
  labels <- paste0(vals, units)
  images <- file.path(base.dir, c("scale30.png", "scale50.png", "scale100.png"))

  left <- floor((100 - pixels.per.unit * coef) / 2) / 100
  right <- 1 - (ceiling((100 - pixels.per.unit * coef) / 2) / 100)

  # 100 pixels-wide scale
  png(images[3], width = 100, height = 16, antialias = "none")
  opar <- par(no.readonly = TRUE)
  par(mai = c(0, 0.025, 0, 0.025), oma = c(0, 0, 0, 0), lend = 2)
  plot(c(left, right), c(0.8, 0.8), type = "l",
    xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", lwd = 4, col = "black",
    xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", bty = "n")
  text(0.5, 0.35, labels = labels[3], adj = c(0.5, 0.5))
  dev.off()

  # 50 pixels-wide scale
  png(images[2], width = 50, height = 16, antialias = "none")
  opar <- par(no.readonly = TRUE)
  par(mai = c(0, 0.025, 0, 0.025), oma = c(0, 0, 0, 0), lend = 2)
  plot(c(left, right), c(0.8, 0.8), type = "l",
    xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", lwd = 4, col = "black",
    xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", bty = "n")
  text(0.5, 0.35, labels = labels[2], adj = c(0.5, 0.5))
  dev.off()

  # 30 pixels-wide scale
  png(images[1], width = 30, height = 16, antialias = "none")
  opar <- par(no.readonly = TRUE)
  par(mai = c(0, 0.025, 0, 0.025), oma = c(0, 0, 0, 0), lend = 2)
  plot(c(left, right), c(0.8, 0.8), type = "l",
    xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", lwd = 4, col = "black",
    xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", bty = "n")
  text(0.5, 0.35, labels = labels[1], adj = c(0.5, 0.5), cex = 0.75)
  dev.off()

  list(coef = coef, labels = labels, images = images)
}
# Test...
#.make_scales(96) #2400dpi
#.make_scales(72)
#.make_scales(48) #1200dpi
#.make_scales(24) #600dpi
#.make_scales(12) #300dpi
#.make_scales(192)#4800dpi
#.make_scales(384)#9600dpi

makeZIVignettes <- function(orig.dir = getwd(), target.dir = dirname(orig.dir),
clean.work = FALSE) {
  # The orig.dir is supposed to be "_work" subdir of where we did image analysis
  odir <- setwd(orig.dir)
  on.exit(setwd(odir))

  # all_ok indicates if all images were correctly processed (only set to FALSE
  # in case of error)
  all_ok <- TRUE

  # List of _dat1|3|5.zim files
  zims <- dir(pattern = "_dat[135]\\.zim$")
  if (!length(zims))
    stop("No '_dat[135].zim' files in 'orig.dir'")
  # List of _col1|3|5.zim files
  imgs <- dir(pattern = "_col[135]\\.tif$")
  # Check that both lists agree, and there are such files
  if (!length(imgs))
    stop("No '_col[135].tif' files in 'orig.dir'")
  if (length(zims) != length(imgs) ||
      any(sub("_dat[135]\\.zim$", "", zims) !=
          sub("_col[135]\\.tif$", "", imgs)))
    stop("You must have pairs of '_dat[135].zim' and ",
      "'_col[135].tif' files in 'orig.dir'")

  # For each _dat[135].zim file, create the directory with vignettes
  # and _dat[135].zim files
  # (renamed _dat1.zim after transforming them into ZI1-compatibles files)
  # as one got it directly from ZI1-5 processes
  l <- length(zims)
  # Scale bars are recalculated according to the size of one pixel.
  # Start with a silly value to make sure it is calculated at first image
  lastpixsize <- -1
  for (i in 1:l) {
    zim <- zims[i]
    img <- imgs[i]
    # Compute the directory name
    smp <- sub("\\+[A-Z][0-9]*_dat5\\.zim$", "", zim)
    smpdir <- file.path(target.dir, smp)
    message("Processing image ", i, "/", l, ", for sample ", smp, "... ",
      sep = "")
    flush.console()

    # If the directory exists, check it is really a dir, not a file!
    if (file.exists(smpdir)) {
      if (!file.info(smpdir)$isdir)
        stop("Sample directory exists for ", smp,
          " but does not appear to be a directory!")
      #cat("skipping (file already exists)\n")
    }
    dir.create(smpdir, showWarnings = FALSE)

    # Read the zim file and do some corrections in it
    zimdat <- readLines(zim)
    if (length(zimdat) < 10 || substring(zimdat[1], 1, 2) != "ZI")
      stop("The following .zim file seems corrupted: ", zim)
    # Correct ZI1, ZI3 or ZI5 into ZI1 (we'll make it compatible with v.1!)
    zimdat[1] <- "ZI1"
    # Determine where the table of data is starting in the file
    dpos <- (1:length(zimdat))[zimdat == "[Data]"]
    if (length(dpos) != 1)
      stop("Data section not found or multiple Data sections in ", zim)
    # Code, Min, Max, SubPart, SubMethod contain all values for all images
    getKeyValue <- function(dat, key, multiple = FALSE) {
      l <- length(dat)
      regexp <- paste0("^", key, "=")
      position <- (1:l)[grepl(regexp, dat)]
      if (!length(position))
        return(list(pos = integer(0), value = character(0)))
      value <- trimws(sub(regexp, "", dat[position]))
      if (isTRUE(multiple)) {
        # Split items according to comas
        value <- trimws(strsplit(value, ",")[[1]])
      }
      list(pos = position, value = value)
    }

    # Just keep the one that suits this particular image
    code <- getKeyValue(zimdat, "Code", multiple = TRUE)
    if (length(code$pos) != 1)
      stop("Error in zim file '", zim, "': no or several 'Code=' entries")
    # Add number to code
    code$label <- code$value
    lcodes <- length(code$value)
    ucodes <- unique(code$value)
    for (ucode in ucodes) {
      upos <- (1:lcodes)[code$value == ucode]
      code$label[upos] <- paste0(ucode, 1:length(upos))
    }
    # Get the code for the current image
    icode <- sub("^.+\\+([A-Z][0-9]*)\\_dat[135]\\.zim$", "\\1", zim)
    # If icode has no numbers, add 1 at the end
    if (grepl("[A-Z]$", icode)) icode <- paste0(icode, "1")
    if (!icode %in% code$label) {
      # Try also without the number
      icode <- substring(icode, 1, 1)
    }
    if (!icode %in% code$label) {
      # Finally try with "1" at the end
      icode <- paste0(icode, "1")
    }
    if (!icode %in% code$label)
      stop("Code ", icode, " not found in the .zim file for ", zim)
    # Determine the position of image in the codes
    ipos <- (1:lcodes)[code$label == icode]
    # Keep only corresponding code
    zimdat[code$pos] <- paste0("Code=", code$value[ipos])
    # Do the same for Min, Max, SubPart and SubMethod
    # Min
    Min <- getKeyValue(zimdat, "Min", multiple = TRUE)
    if (length(Min$pos) != 1)
      stop("Error in zim file '", zim, "': no or several 'Min=' entries")
    if (length(Min$value) != lcodes)
      stop("Non matching number of items for Code= and Min= entries in ", zim)
    zimdat[Min$pos] <- paste0("Min=", Min$value[ipos])
    # Max
    Max <- getKeyValue(zimdat, "Max", multiple = TRUE)
    if (length(Max$pos) != 1)
      stop("Error in zim file '", zim, "': no or several 'Max=' entries")
    if (length(Max$value) != lcodes)
      stop("Non matching number of items for Code= and Max= entries in ", zim)
    zimdat[Max$pos] <- paste0("Max=", Max$value[ipos])
    # SubPart
    SubPart <- getKeyValue(zimdat, "SubPart", multiple = TRUE)
    if (length(SubPart$pos) != 1)
      stop("Error in zim file '", zim, "': no or several 'SubPart=' entries")
    if (length(SubPart$value) != lcodes)
      stop("Non matching number of items for Code= and SubPart= entries in ",
        zim)
    zimdat[SubPart$pos] <- paste0("SubPart=", SubPart$value[ipos])
    # SubMethod
    SubMethod <- getKeyValue(zimdat, "SubMethod", multiple = TRUE)
    if (length(SubMethod$pos) != 1)
      stop("Error in zim file '", zim, "': no or several 'SubMethod=' entries")
    if (length(SubMethod$value) != lcodes)
      stop("Non matching number of items for Code= and SubMethod= entries in ",
        zim)
    zimdat[SubMethod$pos] <- paste0("SubMethod=", SubMethod$value[ipos])
    # Special treatment for 'Time' (get it and take it out of there!)
    smptime <- getKeyValue(zimdat, "Time")
    zimdat <- zimdat[-smptime$pos]
    smptime <- smptime$value
    # In case smptime is just hh:mm, add :00 to get hh:mm:ss
    if (grepl("^[0-9]{1,2}:[0-9]{2}$", smptime))
      smptime <- paste0(smptime, ":00")
    if (grepl("^[0-9]:", smptime))
      smptime <- paste0("0", smptime)
    # Just in case CellPart is missing, add it before Replicates
    # with default value 0.73
    if (!any(grepl("^CellPart=", zimdat))) {
      reppos <- (1:length(zimdat))[grepl("^Replicates=", zimdat)]
      if (length(reppos))
        zimdat[reppos] <- paste0("CellPart=0.73\n", zimdat[reppos])
    }
    # Write the modified zim file in the destination directory
    # TODO: we shouldstart to accept versions 3 and 5 all over ZI now!
    writeLines(zimdat, file.path(smpdir, sub("_dat[135]\\.zim$", "_dat1.zim",
      basename(zim))))

    # Read the color (.tif) image
    pic <- readTIFF(img)
    idat <- read.delim(zim, skip = dpos)
    idat$name <- paste(idat$Label, idat$X.Item, sep = "_")
    # Size of one pixel
    pixunit <- getKeyValue(zimdat, "PixelUnit")$value[1]
    if (is.na(pixunit) || !length(pixunit) || pixunit == "")
      pixunit <- "mm" # Default unit, if not specified
    pixsize <- as.numeric(getKeyValue(zimdat, "PixelSize")$value[1])
    if (is.na(pixsize))
      stop("Impossible to find the size of a pixel in the image ",
        img, " from ", zim)

    if (pixsize != lastpixsize) {# Recalculate coef and scale bars
      scales <- .make_scales(1 / pixsize, pixunit)
      lastpixsize <- pixsize
      # Read the three scale bar files (0.3, 0.5 and 1mm at 2400dpi)
      #scale0.3 <- readPNG(file.path(getTemp('ZIetc'),"Scale2400_0.3mm.png"))
      scale0.3 <- readPNG(scales$images[1])
      if (!is.matrix(scale0.3)) scale0.3 <- scale0.3[, , 1]
      #scale0.5 <- readPNG(file.path(getTemp('ZIetc'),"Scale2400_0.5mm.png"))
      scale0.5 <- readPNG(scales$images[2])
      if (!is.matrix(scale0.5)) scale0.5 <- scale0.5[, , 1]
      #scale1 <- readPNG(file.path(getTemp('ZIetc'),"Scale2400_1mm.png"))
      scale1 <- readPNG(scales$images[3])
      if (!is.matrix(scale1)) scale1 <- scale1[, , 1]
    }

    # Transform coordinates into pixel sizes
    idat$BX <- round(idat$BX/pixsize)
    idat$BY <- round(idat$BY/pixsize)
    idat$Width <- round(idat$Width/pixsize)
    idat$Height <- round(idat$Height/pixsize)
    # Create vignettes
    pl <- dim(pic)[2]
    ph <- dim(pic)[1]
    for (j in 1:nrow(idat)) {
      Width <- idat$Width[j]
      Height <- idat$Height[j]
      BX <- round(idat$BX[j] - Width / 4)
      BY <- round(idat$BY[j] - Height / 4)
      BX2 <- round(BX + (Width * 1.5))
      BY2 <- round(BY + (Height * 1.5))
      # Constrain bounding box inside the picture
      if (BX < 1) BX <- 0
      if (BY < 1) BY <- 1
      if (BX2 > pl) BX2 <- pl
      if (BY2 > ph) BY2 <- ph
      # Crop the picture into a new image
      if (length(dim(pic)) == 2) {# Grayscale picture
        vig <- pic[BY:BY2, BX:BX2]
      } else {# Color picture
        vig <- pic[BY:BY2, BX:BX2, ]
      }
      # Add the scale at the top-left
      if (Width * 1.5 < 50) { # Use the 0.3mm scale
        xmax <- min(30, dim(vig)[2])
        ymax <- min(16, dim(vig)[1])
        scale <- scale0.3[1:ymax, 1:xmax]
      } else if (Width * 1.5 < 100) { # Use the 0.5mm scale
        xmax <- min(50, dim(vig)[2])
        ymax <- min(16, dim(vig)[1])
        scale <- scale0.5[1:ymax, 1:xmax]
      } else {# Use the 1mm scale
        xmax <- min(100, dim(vig)[2])
        ymax <- min(16, dim(vig)[1])
        scale <- scale1[1:ymax, 1:xmax]
      }
      if (length(dim(vig)) == 2) {# Grayscale picture
        vig[1:ymax, 1:xmax][scale < 1] <- scale[scale < 1]
      } else {# Color picture
        vig[1:ymax, 1:xmax, 2][scale < 1] <- scale[scale < 1]
        sel <- scale < 1 & vig[1:ymax, 1:xmax, 3] > 0.2
        vig[1:ymax, 1:xmax, 3][sel] <- scale[sel]/2
      }
      # Write this into a png file
      writePNG(vig, file.path(smpdir, paste(idat$name[j], "png", sep = ".")))
    }
    # Delete _work files if required
    if (isTRUE(clean.work)) {
      unlink(zim)
      unlink(img)
    }
    # Done
    #cat("OK\n")
    #flush.console()
    # Before switching to another picture, or at the end, create the .zidb file
    if (i == l) {
      if (!zidbMake(smpdir, smptime = smptime, replace = TRUE,
        delete.source = TRUE))
        all_ok <- FALSE
    } else {
      nextsmp <- sub("\\+[A-Z][0-9]+_dat[135]\\.zim$", "", zims[i + 1])
      if (nextsmp != smp) {
        if (!zidbMake(smpdir, smptime = smptime, replace = TRUE,
          delete.source = TRUE))
          all_ok <- FALSE
      }
    }
  }
  invisible(all_ok)
}
