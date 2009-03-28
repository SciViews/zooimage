# Functions for manipulating a log file for ZooImage.
#
# Copyright (c) 2004, Ph. Grosjean <phgrosjean@sciviews.org>

"logProcess" <-
	function(message, topic = NULL, file = file.path(tempdir(), "ZooImage.log"),
	logit = TRUE, stop = FALSE, show.log = stop) {
	if (!logit) {
		if (stop) stop(message)
		warning(message)
		return()
	}
	if (!file.exists(file))
		cat("===",  getTemp("ZIname"), "log started", format(Sys.time()), "===\n\n", file = file)
	if (!is.null(topic) && topic != "") message <- paste(topic, message, sep = " - ")
	if (stop) message <- paste("*CRITICAL*:", message)
	message <- paste(sub("\n$", "", message), "\n", sep = "")
	cat(message, file = file, append = TRUE)
	if (show.log) logView(file)
}

"logClear" <-
	function(file = file.path(tempdir(), "ZooImage.log")) {
	unlink(file)
}

"logView" <-
	function(file = file.path(tempdir(), "ZooImage.log"), title = paste(getTemp("ZIname"), "log"), clear = TRUE, warn = FALSE) {
	if (file.exists(file)) {
    	file.show(file, title = title, delete.file = clear)
	} else if (warn) warning("Log file '", file, "' is not found!")
}
