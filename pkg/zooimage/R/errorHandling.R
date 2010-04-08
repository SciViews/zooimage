# Copyright (c) 2009, Ph. Grosjean <phgrosjean@sciviews.org>
#
# This file is part of ZooImage
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

# Masking stop in the NAMESPACE of ZooImage
#
# The base function "stop" is masked in the namespace
# of zooimage so that instead of throwing an error, the stop
# function throws a condition of class ZooImageError that wraps
# information about the environment in which the error is created
#
# When a function in zooimage calls stop, this function is used to dispatch
# the error either to the standard stop function or to the generation of a
# zooImageError condition when a batch function is in the call stack
"stop" <- function (..., call. = TRUE, domain = NULL)
{
	calls <- callStack()
	calls <- head(calls, -2)
	if (!tail(calls, 1) %in% names(zooImageErrorDrivers)) {
		# The calling function does not have a driver, we use the regular stop
		# TODO: maybe this should be a default ZooImageError instead
		base:::stop(..., call. = call., domain = domain)
	} else {
		# The calling function has a driver, we throw the condition
		# using the appropriate driver
		message <- do.call(paste, list(...))
		condfun <- getZooImageErrorFunction(calls)
		err <- condfun(message, env = parent.frame())
		base:::stop(err)
	}
}

# Masking warning in the NAMESPACE of ZooImage
#
# The base function "warning" is masked in the namespace
# of zooimage so that instead of throwing a warning, the warning
# function throws a condition of class ZooImageWarning that wraps
# information about the environment in which the warning is created
#
# When a function in zooimage calls warning, this function is used to dispatch
# the error either to the standard warning function or to the generation of a
# zooImageWarning condition when a batch function is in the call stack
#
warning <- function (..., call. = TRUE, immediate. = FALSE, domain = NULL)
{
	calls <- callStack()
	if (all( regexpr("\\.all$", calls) == -1)) {
		base:::warning(..., call. = call., domain = domain)
	} else {
		message <- do.call(paste, list(...))
		signalCondition(getZooImageWarningFunction(calls)(message,
			env = parent.frame()))
	}
}

# Error condition used in ZooImage batch treatments
#
# This function creates a condition of class "zooImageError".
# These conditions are used in conjunction with the calling handler
# mechanism in zooImage batch calls to grab additional information
# about the context in which the stop function was called
#
# This function is called when a function that is directly or indirectly called
# by a batch treatment function calls the stop function
# msg: the error message
# env: the environment in which the problem occured
zooImageError <- function (msg = "error", env = parent.frame(),
errorClass = NULL, context = NULL, verbose = getOption("verbose"))
{
 	err <- simpleError(message = msg)
 	err$env <- env
	if (!is.null(context)) {
	  if (context %in% ls(env)) err$context <- env[[context]]
	  err$message <- if (verbose)
		sprintf("<%s> [%s] %s", errorClass, err$context, msg) else
		sprintf("[%s] %s", err$context, msg)
	}
	class(err) <- c(errorClass, "zooImageError", "error", "condition")
 	return(err)
}

# If a ZooImage function has a driver in this list
# the stop function will signal a condition built with the driver
# instead of doing the normal thing
# TODO: check that all function requiring a driver has one
zooImageErrorDrivers <- list(
	# --------------------------------------- zid.R
	"verify.zid" = "zidir",
	"verify.zid.all" = "path",
	"clean.after.zid" = "path",
	"uncompress.zid.all" = "zidfiles",
	"read.zid" = "zidfile",

	# --------------------------------------- utilities.R
	"get.sampleinfo" = "filename",

	# --------------------------------------- zim.R
	"make.zim" = "images",
	"verify.zim" = "zimfile",
	"extract.zims" = "zipfiles",

	# -------------- zic.R
	"check.zic" = "file",

	# --------------------------------------- zie.R
	"make.zie" = "Filemap",
	"BuildZim" = "Smp",
	"checkFileExists" = "file",
	"checkFirstLine" = "file",
	"checkDirExists" = "dir",
	"get.ZITrain" = "dir",
	"force.dir.create" = "path",
	"checkEmptyDir" = "dir",
	"make.RData" = "zidir",
	"process.sample" = "Sample",
	"process.samples" = "Samples"
)

# List of warning drivers
zooImageWarningDrivers <- list(
	"verify.zid" = "zidir"
)

# ZooImage errors associated with a context
zooImageErrorContext <- function (fun, context)
{
	force(context)
	function(msg, env = parent.frame()) {
		zooImageError(msg, env, paste("zooImageError", fun, sep = "_"),
			context = context)
	}
}

# ZooImage warnings associated with a context
zooImageWarningContext <- function (fun, context)
{
	force(context)
	function(msg, env = parent.frame()) {
		zooImageWarning(msg, env, paste("zooImageWarning", fun, sep = "_"),
			context = context)
	}
}

# Get the appropriate condition generating function
getZooImageConditionFunction <- function (calls, drivers, default, context.fun)
{
	fun <- tail(calls, 1)
	driver <- drivers[[fun]]
	if (is.character(driver)) {
		driver <- context.fun(fun, driver)
	} else if (is.null(fun)) {
		driver <- default
	}
	# TODO: maybe further checking on the arguments of the driver
	if (!inherits(driver, "function")) stop( "wrong driver" )
	return(driver)
}

# Get the appropriate error function
getZooImageErrorFunction <- function (calls)
{
	getZooImageConditionFunction(calls, zooImageErrorDrivers, zooImageError,
		zooImageErrorContext)
}

# Get the appropriate warning function
getZooImageWarningFunction <- function (calls)
{
	getZooImageConditionFunction(calls, zooImageWarningDrivers, zooImageWarning,
		zooImageWarningContext)
}

# Extract a object from the environment in which the error was generated
#
# When a ZooImageError is created, it contains the environment in which the
# error was created (the frame above the environment of the stop function)
# This utility function can be used to extract an object from
# this environment
#
# x: the zooImageError
# dots: what to extract from the environment
`[[.zooImageError` <- function (x, ...)
	x$env[[...]]

# Warning condition used in ZooImage batch treatments
#
# This function creates a condition of class "zooImageWarning".
# These conditions are used in conjunction with the calling handler
# mechanism in zooImage batch calls to grab additional information
# about the context in which the warning function was called
#
# This function is called when a function that is directly or indirectly called
# by a batch treatment function calls the warning function
#
# msg: the error message
# env: the environment in which the problem occured
zooImageWarning <- function (msg = "warning", env = parent.frame())
{
   w <- simpleWarning(message = msg)
   w$env <- env
   class(w) <- c("zooImageWarning", "warning", "condition")
   return(w)
}

# Extract an object from the environment in which the warning was generated
#
# When a ZooImageWarning is created, it contains the environment in which the
# warning was created (the frame above the environment of the warning function)
# This utility function can be used to extract an object from this environment
#
# x: the zooImageWarning
# dots: what to extract from the environment
`[[.zooImageWarning` <- function (x, ...)
   x$env[[...]]

# Extract only the message of the error
#
# err: error (generated by stop)
# Returns the message without the "Error in ... :" part
extractMessage <- function (err)
{
   err[1] <- sub("^.*?:", "", err[1])
   return(err)
}
