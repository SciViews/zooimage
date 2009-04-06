# Copyright (c) 2009, Ph. Grosjean <phgrosjean@sciviews.org>
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


# {{{ finish.loopfunction
#' Called at the looping function (*.all)
#' 
#' @param ok logical; TRUE if there was a problem
#' @param ok.console.msg the message to write to the console if ok is TRUE
#' @param ok.log.msg the message to write to the log file if ok is TRUE
#' @param nok.console.msg the message to write to the console is ok is FALSE
#' @param nok.log.msg the message to write to the log when ok is FALSE
#' @param show.log logical; if TRUE the log file is shown at the end
#' @param show.console logical; if TRUE messages are written to the console
#' @return ok, invisibly
finish.loopfunction <- function(
	ok = TRUE, 
	ok.console.msg     	 = "-- Done! --\n" ,
	ok.log.msg 	 = "\n-- OK, no error found. --", 
	nok.console.msg    	 = " -- Done! [ERROR(S) FOUND] --\n", 
	nok.log.msg  = "-- Error(s)! --", 
	bell = TRUE,
	show.log = FALSE, 
	show.console = TRUE){

	# {{{ \a rings the bell on most platforms!
	Bell <- if (bell) "\a" 
	# }}}
	
	# {{{ dispatch
	if (ok) {
		logProcess( ok.log.msg )
		if( show.console ) cat(Bell, ok.console.msg , sep = "" )
	} else {
		logProcess( nok.log.msg)
		if( show.console ) cat(Bell, nok.console.msg, sep = "" )
	}
	# }}}
	
	# {{{ show the log if needed
	if (show.log) logView()
	# }}}
	
	invisible( ok )
}
# }}}

#' import grepl from the future 2.9.0 version
grepl <- if( as.numeric( version$major ) >= 2 && as.numeric( version$minor >= 9) )
	base:::grepl else function (pattern, x, ignore.case = FALSE, extended = TRUE, perl = FALSE,
	    fixed = FALSE, useBytes = FALSE) {
	    index <- grep( pattern, x, ignore.case = ignore.case, 
			extended = extended, perl = perl, fixed = fixed, useBytes = useBytes )
		if( length( index ) == 0 ) return( rep( FALSE, length( x ) ) )
		replace( rep( FALSE, length(x) ), index, TRUE )
	} 

