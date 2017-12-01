## Copyright (c) 2004-2015, Ph. Grosjean <phgrosjean@sciviews.org>
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
## along with ZooImage. If not, see <http://www.gnu.org/licenses/>.

## Get the directory containing the plankton sorter files
planktonSorterDir <- function ()
	system.file("planktonSorter", package = "zooimage")

## Generate a planktonSorter.html page
planktonSorterPage <- function (groups, vigns, title = "Plankton sorter/Step1",
id = title, step = 1, port = NULL, file = NULL) {
    if (is.null(port)) {
		## Make sure the R Httpd server is started
		tools <- getNamespace("tools")
		if (R.Version()$`svn rev` >= 67550) {
			port <- tools::startDynamicHelp(NA)
		} else {
			port <- tools$httpdPort
		}
		if (port == 0) port <- startDynamicHelp(TRUE)
		if (port == 0) stop("Impossible to start the R httpd server")
			
		e <- tools$.httpd.handlers.env
		e[["planktonSorterValidate"]] <- planktonSorterValidate
	}
	paths <- groups
	## Add the "[other]" group...
	groups <- names(paths)
	if (!any(groups == "[other]")) {
		## Add the [other] group
		paths <- c(paths, `[other]` = "_/[other]")
		groups <- c(groups, "[other]")
	}
	title <- as.character(title)[1]
    
    ## Create header of the document
	html <- '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>'
    html <- c(html, sprintf("<title>%s</title>", title))
    html <- c(html, sprintf('<meta http-equiv="Content-Type" content="text/html; charset=%s">',
		.mimeEncoding("UTF-8")))  # .mimeEncoding() = zooimage:::.mimeEncoding()
	html <- c(html, sprintf(
'	<script src="../planktonSorter/jquery-1.11.0.min.js"></script>
	<link rel="stylesheet" type="text/css" href="../planktonSorter/planktonSorter.css">	
	<script src="../planktonSorter/planktonSorter.js"></script>
	<script language="javascript" type="text/javascript">
		function back () {
			var ww = window.open("../step%s/planktonSorter.html", "_self");
		}
	</script>
</head>
', as.character(step - 1))
	)
	
	## Create the toolbar with the hidden submission form in it
	if (step <= 1) disabled <- "disabled" else disabled <- "disabled" #disabled <- ""
	html <- c(html, sprintf('<body>
<div id="toolbar" style="width:20000px;">
	<form id="submitForm" action="http://127.0.0.1:%s/custom/planktonSorterValidate" method="post">
		<input id="results" type="hidden" name="%s" value="">
		<input type="button" onclick="back()" value="<< Back" %s>
		<input type="button" onclick="saveDragDropNodes()" value="Validate">
	</form>
</div>', as.character(round(port)[1]), id, disabled))

    ## Create headers for all categories
    html <- c(html, '<div id="header" style="width:20000px;">',
        sprintf('	<span id="header_box" title="%s">%s</span>', paths, groups),
        '</div>'
    )
    
    ## Create the "Unclassified" area
    html <- c(html, '
<div id="dragDropContainer">
	<div id="topBar">
		<p></p>
	</div>
	
	<div id="listOfItems">
		<div>
			<p>Unclassified</p>
			<ul id="_">
			</ul>
		</div>
	</div>
'       
    )
    ## Add one div per group and populate it with the corresponding vignettes
    addGroup <- function (grp, vigns) {		
		vignNames <- sub("\\.[a-zA-Z]+$", "", vigns)
		c(sprintf('		
		<div>
			<p>%s</p>
			<ul id="%s">', grp, grp),
			sprintf('				<li id="%s"><img src="%s" alt="%s" class="preview"></li>',
				vignNames, vigns, vignNames),
'			</ul>
		</div>'
        )
    }
	
    ## Size of main container is 210 + 91*nGroups
	size <- 210 + 91 * length(groups) # PHG: with 90, the [other] group sometimes is wrapped!
    html <- c(html,
		sprintf('	<div id="mainContainer" style="width:%spx;">	
		<!-- ONE <UL> for each "group" -->', as.character(size)))
    ## Add the groups, plus an [other] group too!
	for (group in groups)
		html <- c(html, addGroup(group, vigns[names(vigns) == group]))
	
	## Finalize the page
	html <- c(html,
'   </div>
</div>

<ul id="dragContent"></ul>

<div id="dragDropIndicator"><img src="../planktonSorter/insert.gif"></div>

</body>
</html>'
    )
    
	if (is.null(file)) {
		html
	} else {
		cat(html, sep = "\n", file = file, append = FALSE)
		invisible(html)
	}
}

planktonSorterReport <- function (title = "Plankton sorter/Step1 - Results",
id = title, step = 1, port = NULL, file = NULL) {
    if (is.null(port)) {
		## Make sure the R Httpd server is started	
		tools <- getNamespace("tools")
		if (R.Version()$`svn rev` >= 67550) {
			port <- tools::startDynamicHelp(NA)
		} else {
			port <- tools$httpdPort
		}
		if (port == 0) port <- startDynamicHelp(TRUE)
		if (port == 0) stop("Impossible to start the R httpd server")
	}
	title <- as.character(title)[1]
    
    ## Create header of the document
	html <- '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>'
    html <- c(html, sprintf("<title>%s</title>", title))
    html <- c(html, sprintf('<meta http-equiv="Content-Type" content="text/html; charset=%s">',
		.mimeEncoding("UTF-8")))   # .mimeEncoding() = zooimage:::.mimeEncoding()
	html <- c(html, sprintf('	<meta http-equiv="cache-control" content="no-cache">
	<link rel="stylesheet" type="text/css" href="../planktonSorter/planktonSorter.css">
	<script language="javascript" type="text/javascript">
		function back () {
			var ww = window.open("../step%s/planktonSorter.html", "_self");
		}
		
		function done () {
			var results = document.getElementById(\'results\');
			var saveString = "done>>> " + results.name;

			// Post these results to the R process
			results.value = saveString;
			document.getElementById(\'submitForm\').submit();
		}
		
		function next () {
			var results = document.getElementById(\'results\');
			var saveString = "iterate>>> " + results.name;

			// Post these results to the R process
			results.value = saveString;
			document.getElementById(\'submitForm\').submit();
		}
		
	</script>
</head>
', as.character(step))
	)
	
	## Create the toolbar with the hidden submission form in it
	html <- c(html, sprintf('<body>
<div id="toolbar" style="width:20000px;">
	<form id="submitForm" action="http://127.0.0.1:%s/custom/planktonSorterValidate" method="post">
		<input id="results" type="hidden" name="%s" value="">
		<input type="button" onclick="back()" value="<< Back" disabled>
		<input type="button" onclick="next()" value="Next >>">
		<span>&nbsp&nbsp&nbsp</span>
		<input type="button" onclick="done()" value="Done">
	</form>
</div>', as.character(round(port)[1]), id))

    ## Create headers for all categories
    html <- c(html, sprintf('	<br><h3>%s</h3>
<p><img src="ReportError.png?v=%s" alt="Suspects and error"></p>', title,
		as.character(round(runif(1, max = 100000))))
    )
    
	## Finalize the page
	html <- c(html,
'
</body>
</html>'
    )
    
	if (is.null(file)) {
		html
	} else {
		cat(html, sep = "\n", file = file, append = FALSE)
		invisible(html)
	}
}

planktonSorterValidate <- function (path, query, body, ...) {
    if (!length(body)) return()

	## Special cases "iterate>>> " or "done>>> "
	if (substring(body, 1, 11) == "iterate>>> ") {
		res <- substring(body, 12)
		res <- unlist(strsplit(res, "/", fixed = TRUE))
		reportfile <- get(res[1], envir = TempEnv())$iterate()
		
	} else if (substring(body, 1, 8) == "done>>> ") {
		res <- substring(body, 9)
		res <- unlist(strsplit(res, "/", fixed = TRUE))
		res <- get(res[1], envir = TempEnv())$done()
		
#		html <- '<html>
#<head>
#<title>Final results</title>
#</head>
#<body>
#<h3>Final results</h3>
#'
#		html <- c(html, res,
#'
#</body>
#</html>'
#		)
#		return(list(payload = paste(html, collapse = "\n"), `content-type` = "text/html"))
		
	} else {
		## General case: got validation results...
		res <- unlist(strsplit(body, ";", fixed = TRUE))
		res <- unlist(strsplit(res, "/", fixed = TRUE))
		res <- matrix(as.character(res), ncol = 2, byrow = TRUE)

		## Get data first row
		object <- res[1, 1]
		#step <- as.numeric(res[1, 2])
		#Class <- as.vector(res[-1, 1])
		#Vigns <- as.vector(res[-1, 2])
		reportfile <- get(object, envir = TempEnv())$validate(res)
	}
	
	## Update the errorCorrection object accordingly, and save this also in the Zidb file
	#url <- paste0("file://", reportfile)
	#html <- get(object, envir = TempEnv())$validate(res)
	
    html <- '<html>
<head>
<title>Waiting for R process...</title>
<meta name="keywords" content="automatic redirection">
<script language="javascript" type="text/javascript">
	 var ww = window.open(window.location, "_self");
	 window.close();
</script>
</head>
<body>
Waiting for R process...
</body>
</html>'
	
    list(payload = html, `content-type` = "text/html")
}

#html <- sprintf(
#'<html>
#<head>
#<title>Generating report...</title>
#<meta http-equiv="refresh" content="0; URL=%s">
#<meta name="keywords" content="automatic redirection">
#</head>
#<body>
#Generating report...
#<br>
#If it does not load, try to
#<a href="%s">get it</a> 
#manually.
#</body>
#</html>', url, url)

activeLearning <- function (train, add.mode = "SV+NSV", threshold = NA)
{
  ## Active learning (adaptation of the training set with contextual items)
  if (!inherits(train, "ZITrain"))
    stop("'train' does not appear to be a valid training set, or problem when reading the training set")
  
  ## Call contextSelection (for selection of contextual samples)
  CtxSmp <- contextSelection()
  if (length(CtxSmp) < 1) {
    warning("No contextual samples selected! Initial training set will be used...")
  } else {
    ## Call addItemsToTrain (for augmentation of the training set)
    train <- addItemsToTrain(train = train, CtxSmp = CtxSmp, add.mode = add.mode, 
                             threshold = threshold, dropItemsToTrain = dropItemsToTrain)
  }
  #.assignGlobal(ZIT, train)
  classifier <- ZIClass(Class ~ ., data = train[!(names(train) %in% "AddedItems")], 
                        method = "mlRforest", calc.vars = calcVars, ntree = 200, cv.k = 10)
  attr(classifier, "path") <- attr(train, "path")
  
  ## Return the augmented training set
  train
}

correctError <- function(zidb, classifier, data = zidbDatRead(zidb), mode = "validation",
fraction = 0.05, sample.min = 100, sample.max = 200, grp.min = 2,
random.sample = 0.1, algorithm = "rf", diff.max = 0.2, prop.bio = NULL,
reset = TRUE, result = NULL) {
	## Default value for result
	if (is.null(result))
		result <- paste(sub("\\.[zZ][iI][dD][bB]$", "",
			basename(zidb)), "valid", sep = "_")
		
	## Look if the zidb file exists
	zidb <- as.character(zidb)[1]
	if (!file.exists(zidb))
		stop("zidb file '", zidb, "' not found")
	## Make sure data is correct
	if (missing(data)) {
		## Check that the dat file is read from zidb
		if (!inherits(data, "ZIDat"))
			stop("Corrupted zidb or data file")
	} else {
		## Make sure data refers to the same particules as in zidb
		data2 <- zidbDatRead(zidb)
		if (!inherits(data, "ZIDat"))
			stop("Corrupted zidb or data file")
		if (any(sort(makeId(data)) != sort(makeId(data2))))
			stop("'data' and 'zidb' does not refer to the same vignettes")
	}
	## If data contains a class, make sure its levels match those of the chosen classifier
	groups <- sort(basename(attr(classifier, "path")))
	if (!is.null(data$Class)) {
		if (any(sort(levels(data$Class)) != groups))
			stop("There is a 'Class' variable in 'data', but its levels do not match those of the 'classifier'")
	}
	## For the rest, the arguments will be checked in the errorCorrection() function
	
	## We need to give a unique name for this object. It is the zidb basename
	## plus a digest of the groups
	sample <- sub("\\.[zZ][iI][dD][bB]$", "", basename(zidb))
	dgroups <- digest(groups)
	Name <- paste(sample, dgroups, sep = "__")
	
	## Check the directory and reset it, if asked for
	testdir <- file.path(tempdir(), sample)
	if (file.exists(testdir)) {
		if (isTRUE(reset)) {
			unlink(testdir, recursive = TRUE)
		} else {
			stop("Sample seems currently under validation process; use reset = TRUE instead")
		}
	}
	
	## Create this object in TempEnv()
	ec <- errorCorrection (data, classifier, zidb = zidb, mode = mode,
		fraction = fraction, sample.min = sample.min, sample.max = sample.max,
		grp.min = grp.min, random.sample = random.sample, algorithm = algorithm,
		diff.max = diff.max, prop.bio = prop.bio, testdir = testdir, id = Name,
		result = result, envir = parent.frame())
	if (mode != "stat") assignTemp(Name, ec)
	
	## Start its first iteration...
	ec$iterate()
	
	## Return the object
	ec
}
