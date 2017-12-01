## Zoo/PhytoImage simplified analysis UI (run the application)
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

## TODO: allow for placing samples in subdirs + use tree view 

## Get the working directory
if (!exists(".ZI"))
    stop("You must run this app from within a method script!")
inidir <- dirname(.ZI$wdir)
cat("Directory:", inidir, "\n")

## Used to print a report after exiting the shiny app
print.reportObj <- function (x, ...) {
    line <- paste0(c("\n", rep('-', getOption("width")), "\n"))
    cat(line, paste0(x, collapse = "\n"), line, sep = "")
    invisible(x)
}

## Additional functions required by the UI
## Same a headerPanel, but taking less space, using h5 instead of h1
smallHeaderPanel <- function (title, windowTitle = title) {
    tagList(tags$head(tags$title(windowTitle)), div(class = "span12", 
        style = "padding: 2px 0px;", strong(title)))
}

#smallTitlePanel <- function (title, windowTitle = title) {
#    tagList(tags$head(tags$title(windowTitle)), h5(style = "padding: 2px 0px;", 
#        title))
#}

## Define UI for default process using a config .R script in zooimage
## TODO: change the title according to actual name and version of the software
## TODO: translate UI strings (English and French interfaces)
uiTitle <- paste0("Zoo/PhytoImage version 5.4-0 (UMONS/IFREMER rephy release) - ",
    .ZI$method, " - ", .ZI$user)


### List all available methods
#Methods <- dir(file.path(inidir, "_analyses"), pattern = "\\.R$")
#if (!length(Methods)) stop("No methods defined in that directory")
### Eliminate .R
#Methods <- sub("\\.R$", "", Methods)
#Methods <- .ZI$method

### Prepare for first method
#source(paste(file.path(inidir, "_analyses", .ZI$method), "R", sep = "."), chdir = TRUE)

## List all samples currently available
listSamples <- function (path, method, unanalyzed.only = FALSE) {
    res <- dir(path)
    if (!length(res)) return(character(0))
    ## Eliminate hidden dirs and files (starting with "_")
    res <- res[substr(res, 1, 1) != "_"]
    if (!length(res)) return(character(0))
    ## Keep only dirs or .zidb files
    res <- res[grepl("\\.zidb$", res) | file.info(file.path(inidir, res))$isdir]
    if (!length(res)) return(character(0))
    ## Copy res to files, and eliminate .zidb extensions from res
    files <- rev(res)
    res <- rev(sub("\\.zidb$", "", res))
    ## Where there is a dir and a .zidb file for the same sample, eliminate dir
    keep <- !duplicated(res)
    ## Select files and dir, rereverting rev and files
    res <- rev(res[keep])
    if (!length(res)) return(character(0))
    files <- rev(files[keep])
    ## Determine which sample is imported (has a .zidb file)
    imp <- grepl("\\.zidb$", files)
    ## Determine if some of these files are already processed 
    proc <- dir(file.path(path, "_analyses", method),
        pattern = "\\_valid.RData$")
    if (length(proc)) {
        ## Keep only those items that are in res
        procsmp <- sub("_valid\\.RData$", "", proc)
        proc <- (res %in% procsmp)
    } else proc <- rep(FALSE, length(res))
    ## Create names with smp [ ]/[I]/[A]
    status <- rep("[ ]", length(res))
    status[imp] <- "[I]"
    status[proc] <- "[A]"
    nms <- paste(status, res)
  
    ## If keep unanalyzed only, select corresponding items
    #    if (isTRUE(as.logical(unanalyzed.only))) {
    #        res <- res[!proc]
    #        nms <- nms[!proc]
    #        files <- files[!proc]
    #        imp <- imp[!proc]
    #    }
  
    ## Create a list with samples, files and processed
    list(samples = res, names = nms,  files = files, imported = imp,
        analyzed = proc)
}
AllSamples <- listSamples(inidir, method = .ZI$method)



calcSample <- function (Sample, input, output, session)
{
    ## Is this sample already imported?
    ## Try to import it anyway with replace = FALSE
    if (file.exists(file.path(inidir, Sample))) {
        ## Get .lst file first
        Lst <- dir(file.path(inidir, Sample), pattern = "\\.lst$",
            full.names = TRUE)[1]
        if (length(Lst)) {
            res <- try(importFlowCAM(Lst, rgb.vigs = FALSE, replace = FALSE),
                silent = TRUE)
            if (inherits(res, "try-error")) {
                stop("Error importing sample", Sample)
            } else { # Update list
                Method <- .ZI$method #input$method
                AllSamples <- listSamples(inidir, method = Method)
                #, input$newonlyCheck)
                ## Is this sample validated?
                ## TODO: if reimported => backup validation data and clear it now!
                if (file.exists(file.path(inidir, "_analyses", Method,
                    paste(Sample, "valid.RData", sep = "_")))) {
                    tag <- "[A]"
                } else tag <- "[I]"
        
                updateSelectInput(session, "sample", choices = AllSamples$names,
                    selected = paste(tag, Sample))
            }
        }
    }
}

