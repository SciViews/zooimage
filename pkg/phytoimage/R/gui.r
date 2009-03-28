"ExitPgm" <-
	function(){
	detach("package:phytoimage")	# This is useful to allow updating the package!
	detach("package:zooimage")
	cat("phytoimage package unloaded; To restart it, issue:\n> library(phytoimage)\n")
}

"closePhytoImage" <-
	function() {
	require(zooimage)
	closeAssistant()	# This is a ZooImage function!
	ExitPgm()
}

