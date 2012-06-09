exitPgm <- function ()
{
	## This is useful to allow updating the packages!
	detach("package:phytoimage", unload = TRUE)
	detach("package:zooimage", unload = TRUE)
	cat("phytoimage package unloaded; To restart it, issue:\n> library(phytoimage)\n")
}

closePhytoImage <- function ()
{
	#zooimage::closeAssistant()
	exitPgm()
}

