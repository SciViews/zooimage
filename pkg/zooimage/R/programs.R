
program <- function( prog, args, ..., dir ){
	
	prog <- ZIpgm( prog, dir )
	cmd <- paste( prog, sprintf( args, ... ), sep = " ") 
	system( cmd, intern = TRUE, invisible = TRUE )
	
}

# {{{ xite scripts 
xite <- function( prog, args, ... ){
	program( prog, args, ..., dir = "xite" )
}

xite_pnm2biff <- function( input, output ){
	if( !file.exists( output ) ){
		xite( "pnm2biff", '"%s" "%s"', input, output)
		checkFileExists( output, message = 'error converting file "%s" to BIFF format' )
	}
}

xite_statistics <- function( file ){
	
	out <- as.numeric(xite( "statistics", '-m "%s" ', file ) ) 
	if( is.na( out ) ){
		stop( "Unable to get mean gray value from the blank-field image!" )
	}
	out
}

xite_divide <- function( meangray, image, bf, cor ){
	
	out <- xite( "divide", ' -s "%s" "%s" "%s"', image, bf, cor )
	checkFileExists( cor, message = "Error while correcting blank-field for %s" )
	
}

xite_biff2tiff <- function( cor, tif ){
	out <- xite( "biff2tiff", ' "%s" "%s"', cor, tif )
	checkFileExists( tif, message = "Error while converting corrected image to TIFF format!" )
}
# }}}

# {{{ imagemagick scripts
imagemagick <- function( prog, args, ... ){
	program( prog, args, ..., dir = "imagemagick" )
}

imagemagick_identify <- function( file ){
	size <- imagemagick( "identify", '  -format "%s" %s', '%w %h', file )
	size <- as.numeric( strsplit( size, " " ) [[1]] )
	if (is.na(Size) || is.null(Size) || length(Size) != 2 || Size[1] < 100 || Size[2] < 100) {
		stop("Error while getting image size with 'identify'", FileConv ) 
	}
	size
}

imagemagick_convert <- function( file, size1, size2 ){
	imagemagick( "convert", ' "%s" -resize %dx%d -median 2.0 -resize %dx%d! "%s"', 
		file, size2[1], size2[2], size1[1], size1[2], file )
}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:
