
#' get a template file from the "ZITemplate" option
template <- function( file = "default.zim", dir = getOption("ZITemplates") ){
	f <- file.path( dir, file )
	checkFileExists( f, "template file '%s' does not exist" )
	f
}

