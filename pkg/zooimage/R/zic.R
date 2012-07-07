## Copyright (c) 2004-2012, Ph. Grosjean <phgrosjean@sciviews.org>
##
## This file is part of ZooImage
## 
## ZooImage is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
## 
## ZooImage is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with ZooImage.  If not, see <http://www.gnu.org/licenses/>.

## Check that the file is a zic file
zicCheck <- function (zicfile)
{	
	## This should be a .zic file directly
	if (!checkFileExists(zicfile)) return(NULL)
	
	## First line of the file must be "ZI1", "ZI2", or "ZI3"
	if (!checkFirstLine(zicfile)) return(NULL) 
	
	## Second line must be [path]
	Line2 <- scan(zicfile, character(), skip = 1, nmax = 1, quiet = TRUE)
	if (tolower(Line2) != "[path]")
		stop("not a ZooImage .zic file, or corrupted!")
}
