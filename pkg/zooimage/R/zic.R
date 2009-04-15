# {{{ Copyright (c) 2004-2007, Ph. Grosjean <phgrosjean@sciviews.org>
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
# }}}


#' check that the file is a zic file
check.zic <- function( file ){
	
	# Now this should be a .zic file directly
	checkFileExists( file )
	
	# First line of the file must be "ZI1"
	checkFirstLine( file ) 
	
	# Second line must be [path]
	Line2 <- scan( file , character(), skip = 1, nmax = 1, quiet = TRUE)
	if (tolower(Line2) != "[path]") {
		stop("not a ZooImage1 .zic file, or corrupted!")
	}
	
}
