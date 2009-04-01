# Copyright (c) 2004, Ph. Grosjean <phgrosjean@sciviews.org>
#
# This file is part of ZooImage .
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
# along with ZooImage.  If not, see <http://www.gnu.org/licenses/>.

# Functions for manipulating a log file for ZooImage.

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

# TODO: improve these
logError <- function( e, msg= NULL, ... ){
	logProcess( if( is.null(msg)) e$msg else msg, e$context, stop = TRUE, ... )
}
logWarning <- function( w, msg= NULL,... ){
	logProcess( if( is.null(msg)) e$msg else msg, w$context, stop = FALSE, ... )
}

