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

#### OK #### (used in many places...)
## Transforms a file extension to a pattern for ignore.case matching of the  
## extension: extension (with or without the dot at the beginning)
## returns a regular expression pattern that can be used
##          to match files with this extension
## example: extensionPattern("tif")

## TODO: eliminate this function!
## Called at the looping function (*.all) 
## ok: logical; TRUE if there was a problem
## ok.console.msg: the message to write to the console if ok is TRUE
## ok.log.msg: the message to write to the log file if ok is TRUE
## nok.console.msg: the message to write to the console is ok is FALSE
## nok.log.msg: the message to write to the log when ok is FALSE
## show.log: logical; if TRUE the log file is shown at the end
## show.console: logical; if TRUE messages are written to the console
## return ok, invisibly
finishLoop <- function (ok = TRUE, ok.console.msg = "-- Done! --\n",
ok.log.msg = "\n-- OK, no error found. --",
nok.console.msg = " -- Done! [ERROR(S) FOUND] --\n",
nok.log.msg  = "-- Error(s)! --", 
bell = TRUE, show.log = FALSE, show.console = TRUE)
{
	## \a rings the bell on most platforms!
	Bell <- if (bell) "\a" 
	
	## Dispatch
	if (ok) {
		logProcess(ok.log.msg)
		if (show.console) cat(Bell, ok.console.msg , sep = "")
	} else {
		logProcess(nok.log.msg)
		if (show.console) cat(Bell, nok.console.msg, sep = "")
	}
	
	## Show the log if needed
	if (show.log) logView()
	
	return(invisible(ok))
}

## TODO: use the original zip() function in R + eliminate all the rest!
## Zip the content of the directory into the zipfile
## and delete the directory if needed
# Modif Kev zip is now available in R
### Problem: zip shows all compressed files on R consol --> need invisible method
zip <- function (zipfile, directory, delete.source = FALSE) {
	### Zip the zid file
	utils:::zip(zipfile = zipfile, files = directory)
	if (delete.source)
		unlink(x = directory, recursive = TRUE)
	checkFileExists(zipfile, message = "Error creating zip file '%s'")
}

#zip <- function (zipfile , directory, delete.source = FALSE,
#comment.file = NULL, delete.zipfile.first = TRUE)
#{
#	## We need the system to be capable of zipping
## bug to check if there is a zip program under windows
##	checkZipAvailable()
#
#	## Delete old zip file, if it exists
#	if (delete.zipfile.first && file.exists(zipfile))
#		unlink(zipfile)
#
#	## Test if we need and can add the comment file
#	comment <- !is.null(comment.file) && file.exists(comment.file)
#
#	## Build the list of parameters for zip
#	zippar <- sprintf("-rq9%s%s", if (delete.source) "m" else "",
#		if (comment) "z" else "")
#
#	## Create the basic command
#	cmd <- sprintf('"%s" %s "%s" "%s"', ZIpgm("zip", "misc"), zippar,
#		zipfile, directory)
#
#	## Call the command
#	result <- if (isWin()) {
#		## modify the windows command so that the message is piped into the zip command
#		if (comment) {
#			cmd <- sprintf('%s /c type "%s" | %s', Sys.getenv("COMSPEC"),
#				comment.file, cmd)
#		}
#		system(cmd, show.output.on.console = TRUE, invisible = TRUE)
#	} else {
#		## Modify the command if we need and can add the comment file
#		if (comment)
#			cmd <- sprintf('%s < "%s"', cmd, comment.file)
#		## Send the error stream to the null device
#		cmd <- paste(cmd, ' 2> /dev/null')
#		## Call the command
#		system(cmd)
#	}
#
#	checkFileExists(zipfile, message = "Error creating zip file '%s'")
#	return(invisible( result == 0))
#}
#

zipNoteAdd <- function (zip, comment.file,
on.failure = stop(sprintf(on.failure.msg , comment.file, zip)),
on.failure.msg = "problem adding comment from '%s' to file '%s' ", on.success)
{
	##checkZipAvailable()

	cmd <- if (isWin()) {
		sprintf('%s /c type "%s" | "%s" -z "%s" ',
			Sys.getenv("COMSPEC"), comment.file, zip)
	} else {
		sprintf('zip -z "%s" < "%s" ', zip, comment.file)
	}
	res <- system(cmd, show.output.on.console = FALSE, invisible = TRUE,
		intern = TRUE)
	if (res != 0) {
		on.failure
	} else if (!missing(on.success)) {
		on.success
	}
	return(invisible(res))
}

## Unzip a zip file in a directory
## zipfile: the zip file to extract
## path: the path where to extract
## delete.source: logical; if TRUE the zipfile is deleted after unzipped
unzip <- function (zipfile, path, delete.source = FALSE) {
	utils:::unzip(zipfile, exdir = path, overwrite = TRUE)
	if (delete.source) unlink(zipfile)
}

## Extract the comment from the zipfile
## Comments that are written in the zipfile can be retrieved using
## the zipnote command, the first lines all start with @ signs, and are not
## the comment
## zipfile: the zip file from which to extract the comment
## outfile: if not NULL, indicates the file the comment should be sent to
## return the character vector corresponding to the comment. The character
##         vector is still returned when the outfile is used, but it is returned
##         invisibly in that case
zipNote <- function (zipfile, outfile = NULL)
{
	## Check that the system is zipnote capable
#	checkZipnoteAvailable()

	## Build the command
	cmd <- sprintf('"%s" "%s" ' , ZIpgm("zipnote", "misc"), zipfile)

	## Call the command and grab the result
	out <- if (isWin()) {
		system(cmd, intern = TRUE, show.output.on.console = FALSE,
			invisible = TRUE)
	} else {
		system(cmd, intern = TRUE)
	}

	## Filter out things that are not comments
	out <- out[!grepl("^@", out)]

	## Write the output to the file if needed and return the result
	if (!is.null(outfile)) {
		cat(out, file = outfile, sep = "\n")
		return(invisible(out))
	} else {
		return(out)
	}
}

#checkZipnoteAvailable <- function ()
#{
#	checkCapabilityAvailable("zipnote", 
#		sprintf('"%s" -h %s', ZIpgm("zipnote", "misc"),
#		if(!isWin()) " > /dev/null" else ""), 
#		"zipnote - program from Info-Zip not found!")
#}

#checkJavaAvailable <- function ()
#{
#	checkCapabilityAvailable("java", 
#		'java -version ', 
#		"java: program not found! Please, install it!")
#}
