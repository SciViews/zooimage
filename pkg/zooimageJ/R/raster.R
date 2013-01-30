### Based on a contribution by Paul Murell

#setGeneric("as.raster" )
#
#setMethod( "as.raster", "jobjRef", function (x) {
as.raster.jobjRef <- function (x, ...)
{	
	## First check that this is an instance of ij.ImagePlus
    if (!x %instanceof% "ij.ImagePlus")
    	stop("'x' must be a reference to an \"ImagePlus\" object")
    	
    ## Image dimensions
    w <- x$getWidth()
    h <- x$getHeight()
    ## Force to RGB image
    ijp <- x$getProcessor()
    ijrgb <- ijp$convertToRGB()
    ## Grab the pixels (which should now be integers)
    pixels <- ijrgb$getPixels()
    ## Convert the integers to R colours
    colourChars <- as.character(as.hexmode(pixels))
    red <- substr(colourChars, 3, 4)
    green <- substr(colourChars, 5, 6)
    blue <- substr(colourChars, 7, 8)
    colours <- paste("#", red, green, blue, sep = "")
    r <- colours
    dim(r) <- c(h, w)
    class(r) <- "raster"
    return(r) 
}
