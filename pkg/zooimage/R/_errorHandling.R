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

#' Masking stop in the NAMESPACE of ZooImage 
#' 
#' The base function "stop" is masked in the namespace
#' of zooimage so that instead of throwing an error, the stop
#' function throws a condition of class ZooImageError that wraps 
#' information about the environment in which the error is created
#' 
#' @details When a function in zooimage calls stop, this function 
#' is used to dispatch the error either to the standard stop function
#' or to the generation of a zooImageError condition when a batch function
#' is in the call stack
#' 
#' @param dots see ?Stop
#' @param call. see ?stop
#' @param domain see ?stop
stop <- function( ..., call.= TRUE, domain = NULL ){
   calls <- sapply( sys.calls(), function(.) as.character( .[[1]] ) )
   if( all( regexpr( "\\.all$", calls ) == -1 ) ){
     base:::stop( ..., call.=call., domain = domain )
   } else{
     message <- do.call( paste, list(...) )
	 signalCondition( getZooImageErrorFunction( calls )(message, env = parent.frame() ) )
   }
} 

#' Masking warning in the NAMESPACE of ZooImage 
#' 
#' The base function "warning" is masked in the namespace
#' of zooimage so that instead of throwing a warning, the warning
#' function throws a condition of class ZooImageWarning that wraps 
#' information about the environment in which the warning is created
#' 
#' @details When a function in zooimage calls warning, this function 
#' is used to dispatch the error either to the standard warning function
#' or to the generation of a zooImageWarning condition when a batch function
#' is in the call stack
#' 
#' @param dots see ?Stop
#' @param call. see ?stop
#' @param immediate. See ?stop
#' @param domain see ?stop
warning <- function( ..., call.= TRUE, immediate.= FALSE, domain = NULL ){
   calls <- sapply( sys.calls(), function(.) as.character( .[[1]] ) )
   if( all( regexpr( "\\.all$", calls ) == -1 ) ){
     base:::warning( ..., call.=call., domain = domain )
   } else{
     message <- do.call( paste, list(...) )
     signalCondition( getZooImageWarningFunction( calls )(message, env = parent.frame() ) )
   }
} 


#' Error condition used in ZooImage batch treatments
#' 
#' This function creates a condition of class "zooImageError". 
#' These conditions are used in conjunction with the calling handler
#' mechanism in zooImage batch calls to grab additional information
#' about the context in which the stop function was called
#' 
#' @details this function is called when a function that is
#' directly or indirectly called by a batch treatment function
#' calls the stop function
#'
#' @param msg the error message
#' @param env the environment in which the problem occured
zooImageError <- function( msg = "error", env = parent.frame(), errorClass = NULL, context = NULL ){
 err <- simpleError( message = msg )
 err$env <- env             
 if( !is.null( context ) ){
   if( context %in% names( env ) ){
	 err$context <- env[[ context ]]
   }
 }
 class( err ) <- c(errorClass, "zooImageError", "error", "condition" )
 err
}


# if a zoo image function has a driver in this list
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
	"get.sampleinfo" =  "filename", 
	
	# --------------------------------------- zim.R
	"make.zim" = "images", 
	"verify.zim" = "zimfile",
	"extract.zims" = "zipfiles"
)

zooImageWarningDrivers <- list(
	"verify.zid" = "zidir"
)


zooImageErrorContext <- function( fun, context ) {
	function( msg, env = parent.frame() ){
		zooImageError( msg, env, paste( "zooImageError", fun, sep = "_") , context = context )
	}
}

zooImageWarningContext <- function( fun, context ) {
	function( msg, env = parent.frame() ){
		zooImageWarning( msg, env, paste( "zooImageWarning", fun, sep = "_") , context = context )
	}
}


getZooImageConditionFunction <- function( calls, drivers, default, context.fun ){
	fun <- tail(calls)[1]
	driver <- drivers[[ fun ]]
	if( is.character( driver ) ){
		driver <- context.fun( fun, driver )
	} else if( is.null( fun ) ){
		 driver <- default
	 } 
	 # TODO: maybe further checking on the arguments of the driver
	 if( !inherits( driver, "function" ) ){
		 stop( "wrong driver" )
	 }
	 driver
}

getZooImageErrorFunction <- function( calls ){
	 getZooImageConditionFunction( calls, zooImageErrorDrivers, zooImageError, zooImageErrorContext )
}
    
getZooImageWarningFunction <- function( calls ){
	getZooImageConditionFunction( calls, zooImageWarningDrivers, zooImageWarning, zooImageWarningContext )
}
  


#' Extracts a object from the environment in which the error was generated
#' 
#' When a ZooImageError is created, it contains the environment in which the 
#' error was created (the frame above the environment of the stop function)
#' This utility function can be used to extract an object from
#' this environment
#' 
#' @param x the zooImageError
#' @param dots what to extract from the environment
`[[.zooImageError` <- function( x, ...){
  x$env[[ ... ]]
}


#' Warning condition used in ZooImage batch treatments
#' 
#' This function creates a condition of class "zooImageWarning". 
#' These conditions are used in conjunction with the calling handler
#' mechanism in zooImage batch calls to grab additional information
#' about the context in which the warning function was called
#' 
#' @details this function is called when a function that is
#' directly or indirectly called by a batch treatment function
#' calls the warning function
#'
#' @param msg the error message
#' @param env the environment in which the problem occured
zooImageWarning <- function( msg = "warning", env = parent.frame() ){
 w <- simpleWarning( message = msg )
 w$env <- env
 class( w ) <- c("zooImageWarning", "warning", "condition" )
 w
}

#' Extracts a object from the environment in which the warning was generated
#' 
#' When a ZooImageWarning is created, it contains the environment in which the 
#' warning was created (the frame above the environment of the warning function)
#' This utility function can be used to extract an object from
#' this environment
#' 
#' @param x the zooImageWarning
#' @param dots what to extract from the environment
`[[.zooImageWarning` <- function( x, ...){
  x$env[[ ... ]]
}

#' extracts only the message of the error
#' 
#' @param err error (generated by stop)
#' @return the message without the "Error in ... :" part
extractMessage <- function( err ){
	err[1] <- sub( "^.*?:", "", err[1] )
	err
}

