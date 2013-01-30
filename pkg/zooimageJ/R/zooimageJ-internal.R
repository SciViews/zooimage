.onAttach <- function (libname, pkgname) {
	#if (substring(R.Version()$os, 1, 6) == "darwin") {
	#	# There is a problem starting Java with AWT on Mac OS X (Snow) Leopard
	#	# This tries to correct it!
	#	options(java.parameters = paste(getOption("java.parameters"),
	#		"-Djava.awt.headless=true"))
	#}
	# Note: this does not solve *all* the problem because methods in IJ that
	#       rely on genericDialog still do not work. The only solution I [PhG]
	#       have found until now is to start RImageJ from within JGR.
	.jpackage(pkgname)
	if (!is.null(ImageJ)) ImageJ$show()
	packageStartupMessage("ImageJ version: ", IJ$getVersion(), "\n", sep = "")
}

.onUnload <- function (libpath) {
	## Close ImageJ
	try(ImageJ$quit(), silent = TRUE)	
}
