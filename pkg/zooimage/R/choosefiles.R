

if( !isWin() ){
	# choose.files is only available on windows, so we fall 
	# back on tcl-tk equivalent function
	choose.files <- function( default = "", caption = "Select files",
	     multi = TRUE, filters = Filters,
		 index = nrow(Filters) ){
		
		call <- match.call( )
		call[[1]] <- as.name( "tk_choose.files")
		eval( call, envir = parent.frame() )	
	}
}

