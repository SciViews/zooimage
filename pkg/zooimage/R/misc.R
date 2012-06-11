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

getSample <- function (x, unique = FALSE, must.have, msg)
{
	res <- sub("[+].*", "", as.character(x))
	if (isTRUE(unique)) res <- unique(res)
	if (!missing(must.have)) {
		if (!all(must.have %in% res)) {
			if (missing(msg))
				msg <- sprintf("sample '%s' not in ZIDat",
					paste(must.have, sep = ","))
			stop(msg)
		}
	}
	return(res)
}

backspaces <- function (n = getOption("width"))
	paste(rep("\b", n), collapse = "")

## Get the current call stack
callStack <- function ()
{
	calls <- sys.calls()
	out <- lapply(calls, function(.) {
		out <- try( as.character(.[[1]] ), silent = TRUE)
		if (inherits(out, "try-error")) NULL else out
	})
	out <- unlist(out[!sapply(out, is.null)])
	return(out)
}

## Checks if the file has the extension
hasExtension <- function (file, extension = "zip",
pattern = extensionPattern(extension))
	grepl(pattern, file)

## List files with given extension
## dir: directory to list files
## extension: file extension to accept. This will be 
## modified by extensionPattern so that the test is case independent
listFilesExt <- function (dir, extension = "zip",
pattern = extensionPattern(extension), ... )
{
	checkDirExists(dir)
	list.files(dir, pattern = pattern , ...)
}

zimList <- function (zidir, ...)
	listFilesExt(zidir, extension = "zim", ...)

zimDatList <- function (zidir, ...)
	listFilesExt(zidir, extension = "_dat1.zim", ...)

zipList <- function (zidir, ...)
	listFilesExt(zidir, extension = "zip", ...)

zidList <- function (zidir, ...)
	listFilesExt(zidir, extension = "zid", ...)
	
zidbList <- function (zidir, ...)
	listFilesExt(zidir, extension = "zidb", ...)

jpgList <- function (dir, ...)
	listFilesExt(dir, extension = "jpg", ...)
	
pngList <- function (dir, ...)
	listFilesExt(dir, extension = "png", ...)

## Transforms a file extension to a pattern for ignore.case matching of the  
## extension: extension (with or without the dot at the beginning)
## returns a regular expression pattern that can be used
##          to match files with this extension
## example: extensionPattern("tif")
extensionPattern <- function (extension = "tif",
add.dot = !grepl("[.]", extension))
{
	extensionLetters <- substring(extension, 1:nchar(extension),
		1:nchar(extension))
	parts <- ifelse(extensionLetters %in% c(letters, LETTERS), 
		paste("[", extensionLetters, casefold(extensionLetters, upper = TRUE),
		"]", sep = ""), extensionLetters)
	pattern <- paste(parts, collapse = "") 
	if (add.dot) pattern <- paste(".", pattern, sep = "")
	pattern <- gsub( "[.]", "[.]", pattern)
	return(paste(pattern, "$", sep = ""))
}

## Check if a file exists
## file: file to check
## extension: if given the file should have this extension
## message: message to give when the file is not found
checkFileExists <- function (file, extension, message = "file not found: %s",
force.file = FALSE)
{
	message <- sprintf(message, file)
	if (!file.exists(file)) stop(message) 
	if (force.file && file.info(file)$isdir)
		stop(sprintf('file "%s" is a directory', file))
	if (!missing(extension) && !grepl(extensionPattern(extension), file)) {
		message <- sprintf("'%s' is not a '%s' file", file, extension)
		stop(message)
	}
	return(invisible(NULL))
}

checkFileExistAll <- function (files, extension)
{
	if (!all( file.exists(files)))
		stop("one or more file does not exist")
	if (!missing(extension) && ! all(hasExtension(files, extension)))
		stop("one or more files have wrong extension")
}

## Checks if a directory exists
## dir: the directory to check
## message: the message to throw into stop if the directory does
##  not exists or is not a directory
checkDirExists <- function (dir,
message = 'Path "%s" does not exist or is not a directory')
{
	message <- sprintf(message, dir)
	if (!file.exists(dir) || !file.info(dir)$isdir)
		stop(message)
}

checkEmptyDir <- function (dir, message = "not empty")
{	
	if (file.exists(dir)) {
		if (length(list.files(dir, all.files = TRUE) > 0))
			stop(message)
	} else {
		forceDirCreate(dir)
	}	
}

## Force creation of a directory
## First, if the path exists but is not a directory, this stops.
## Then, if it did not exist, it calls dir.create to attempt to create it
## If the creation was not sucessful, it stops 
## path: the path of the directory to create
forceDirCreate <- function (path, ...)
{	
	if (file.exists(path) && !file.info(path)$isdir)
		stop ("not a directory")
	out <- dir.create(path, ...)
	if (!out) stop("could not create directory")
	return(out)
}

## Checks the first line of a file against some expected content
checkFirstLine <- function (file, expected = c("ZI1", "ZI2", "ZI3"), 
message = 'file "%s" is not a valid ZooImage version <= 3 file', stop = FALSE)
{
	Line1 <- scan(file, character(), nmax = 1, quiet = TRUE)
	res <- Line1 %in% expected
	if (!res && stop) {
		message <- sprintf(message, file)
		stop(message)
	}
	return(invisible(res)) 
}

listDirs <- function (dir, ...)
{
	out <- list.files(dir)
	out[file.info(file.path(dir, basename(out)))$isdir]
}

## Get a template file from the "ZITemplate" option
template <- function (file = "default.zim", dir = getOption("ZITemplates"))
{
	f <- file.path(dir, file)
	checkFileExists(f, message = "template file '%s' does not exist")
	return(f)
}

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

## Zip the content of the directory into the zipfile
## and delete the directory if needed
zip <- function (zipfile , directory, delete.source = FALSE,
comment.file = NULL, delete.zipfile.first = TRUE)
{
	## We need the system to be capable of zipping
	checkZipAvailable()

	## Delete old zip file, if it exists
	if (delete.zipfile.first && file.exists(zipfile))
		unlink(zipfile)

	## Test if we need and can add the comment file
	comment <- !is.null(comment.file) && file.exists(comment.file)

	## Build the list of parameters for zip
	zippar <- sprintf("-rq9%s%s", if (delete.source) "m" else "",
		if (comment) "z" else "")

	## Create the basic command
	cmd <- sprintf('"%s" %s "%s" "%s"', ZIpgm("zip", "misc"), zippar,
		zipfile, directory)

	## Call the command
	result <- if (isWin()) {
		## modify the windows command so that the message is piped into the zip command
		if (comment) {
			cmd <- sprintf('%s /c type "%s" | %s', Sys.getenv("COMSPEC"),
				comment.file, cmd)
		}
		system(cmd, show.output.on.console = TRUE, invisible = TRUE)
	} else {
		## Modify the command if we need and can add the comment file
		if (comment)
			cmd <- sprintf('%s < "%s"', cmd, comment.file)
		## Send the error stream to the null device
		cmd <- paste(cmd, ' 2> /dev/null')
		## Call the command
		system(cmd)
	}

	checkFileExists(zipfile, message = "Error creating zip file '%s'")
	return(invisible( result == 0))
}

zipNoteAdd <- function (zip, comment.file,
on.failure = stop(sprintf(on.failure.msg , comment.file, zip)),
on.failure.msg = "problem adding comment from '%s' to file '%s' ", on.success)
{
	checkZipAvailable()

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
	checkZipnoteAvailable()

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
