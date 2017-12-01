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

# Drop items in training set
dropItemsToTrain <- function (train, cl, drop.nb)
{
    ## Parameters:
    ## train: the training set
    ## cl: the class to consider
    ## drop.nb: the number of items to drop
    
    train[-sample(which(train$Class == cl), drop.nb), ]
}

# Add validated items (from contextual samples) in training set
addItemsToTrain <- function (train, CtxSmp, add.mode = "SV+NSV", threshold = NA,
dropItemsToTrain = dropItemsToTrain) 
{
    ## Parameters:
    ## train: the training set to complete
    ## CtxSmp: the contextual samples containing validated items
    ## add.mode: the mode for adding items (SV: Validated Suspects,
    ##           NSV: Validated Non-Suspects, SV+NSV: both)
    ## threshold: the maximal number of items in each class of training set 
    ## dropItemsToTrain: the function to drop items in training set (depending on threshold)
  
    ## Is 'add.mode' a valid mode?
    if (length(add.mode) != 1 && !is.character(add.mode))
        stop("'add.mode' must be a character string of length one")
    if (!add.mode %in% c("SV","NSV","SV+NSV"))
        stop("'add.mode' must be one of 'SV', 'NSV' or 'SV+NSV'")
    threshold <- as.integer(threshold)[1]
    if (threshold < 1 & !is.na(threshold))
        stop("threshold must be a positive integer or not assigned")
  
    ## Variables: 
    CtxData <- NULL   ## the contextual data containing validated items
    for (i in 1:length(CtxSmp)) {
        dat <- get(load(CtxSmp[i]))
        if (i == 1) {
            CtxData <- dat
        } else {
            CtxData <- merge(CtxData, dat, all = TRUE)
        }
    }
    CtxData$Id.1 <- NULL
    CtxData$X.Item.1 <- NULL
    ## Delete the items already present in the training set
    CtxData <- CtxData[!(CtxData$Id %in% train$Id), ]
    CtxValidData <- CtxData[which(CtxData$Validated == TRUE), ]
    CtxSVData <- CtxValidData[which(CtxValidData$Suspect == TRUE), ]
    CtxNSVData <- CtxValidData[which(CtxValidData$Suspect == FALSE), ]
  
    ## The classes to complete (only the classes in training set)
    ## Classes <- intersect(unique(train$Class), unique(CtxData$Class))
    ## the classes to complete (classes in both training set and contextual samples)
    Classes <- c(intersect(unique(train$Class), unique(CtxValidData$Class)), 
        setdiff(unique(CtxValidData$Class[!is.na(CtxValidData$Class)]), 
        unique(train$Class)))
    ## Paths in train + new classes
    paths <- c(attr(train, "path"), 
        setdiff(unique(CtxValidData$Class[!is.na(CtxValidData$Class)]), 
        basename(attr(train, "path"))))
    ## items in initial training set (FALSE) or items added from contextual samples (TRUE)
    train$AddedItems <- FALSE
  
    ## For each class in 'Classes'
    for (cl in Classes) {
        nbPresent <- length(which(train$Class == cl))
        nbAddSV <- NULL        # The number of items in SV to add
        nbAddNSV <- NULL       # The number of items in NSV to add
        if (nbPresent <= 1) {
            cat(paste("\n'", cl, "': ", nbPresent, " item in training set", sep = ""))
        } else {
            cat(paste("\n'", cl, "': ", nbPresent, " items in training set", sep = ""))
        }
    
        ## If threshold not reached
        if (nbPresent < threshold | is.na(threshold)) { 
            if (add.mode == "SV") {  
                # SV (Validated Suspects)
                DataSV <- CtxSVData[which(CtxSVData$Class == cl), ]
                if (NROW(DataSV) < 1) {
                    cat("\n   Adding 0 suspect item")
                } else {
                    nbAddSV <- min((threshold-nbPresent), NROW(DataSV), na.rm = TRUE)
                    if (nbAddSV == 1) {
                        cat(paste("\n   Adding", nbAddSV, "suspect item", sep = " "))
                    } else {
                        cat(paste("\n   Adding", nbAddSV, "suspect items", sep = " "))
                    }
                    AddedDataSV <- DataSV[sample(NROW(DataSV), nbAddSV),
                        !(names(CtxValidData) %in% c("Validated","Suspect"))]
                    AddedDataSV$AddedItems <- TRUE
                    train <- merge(train, AddedDataSV, 
                        by = intersect(names(train), names(AddedDataSV)), all = TRUE)
                }
            }
      
            if (add.mode == "NSV") {  
                # NSV (Validated Non-Suspects)
                DataNSV <- CtxNSVData[which(CtxNSVData$Class == cl), ]
                if (NROW(DataNSV) < 1) {
                    cat("\n   Adding 0 non-suspect item")
                } else {
                    nbAddNSV <- min((threshold-nbPresent), NROW(DataNSV), na.rm = TRUE)
                    if (nbAddNSV == 1) {
                        cat(paste("\n   Adding", nbAddNSV, "non-suspect item", sep = " "))
                    } else {
                        cat(paste("\n   Adding", nbAddNSV, "non-suspect items", sep = " "))
                    } 
                    AddedDataNSV <- DataNSV[sample(NROW(DataNSV), nbAddNSV),
                        !(names(CtxValidData) %in% c("Validated","Suspect"))]
                    AddedDataNSV$AddedItems <- TRUE
                    train <- merge(train, AddedDataNSV, 
                         by = intersect(names(train), names(AddedDataNSV)), all = TRUE)
                }
            }
      
            if (add.mode == "SV+NSV") {
                # SV (Validated Suspects)
                DataSV <- CtxSVData[which(CtxSVData$Class == cl), ]
                # NSV (Validated Non-Suspects)
                DataNSV <- CtxNSVData[which(CtxNSVData$Class == cl), ]
                nbAddSV <- min(c(ceiling(((threshold-nbPresent) /
                    (NROW(DataSV)+(NROW(DataNSV)))) * NROW(DataSV)),
                    NROW(DataSV)), na.rm = TRUE)   # ratio
                nbAddNSV <- min((threshold-nbAddSV-nbPresent), NROW(DataNSV), na.rm = TRUE)
        
                if (nbAddSV == 0) {
                    cat("\n   Adding 0 suspect item")
                } else {
                    if (nbAddSV == 1) {
                        cat(paste("\n   Adding", nbAddSV, "suspect item", sep = " "))
                    } else {
                        cat(paste("\n   Adding", nbAddSV, "suspect items", sep = " "))
                    }  
                    AddedDataSV <- DataSV[sample(NROW(DataSV), nbAddSV), 
                        !(names(CtxValidData) %in% c("Validated","Suspect"))]
                    AddedDataSV$AddedItems <- TRUE
                    train <- merge(train, AddedDataSV, 
                        by = intersect(names(train), names(AddedDataSV)), all = TRUE)
                }
        
                if (nbAddNSV == 0) {
                    cat("\n   Adding 0 non-suspect item")
                } else {
                    if (nbAddNSV == 1) {
                        cat(paste("\n   Adding", nbAddNSV, "non-suspect item", sep = " "))
                    } else {
                        cat(paste("\n   Adding", nbAddNSV, "non-suspect items", sep = " "))
                    }  
                    AddedDataNSV <- DataNSV[sample(NROW(DataNSV), nbAddNSV), 
                        !(names(CtxValidData) %in% c("Validated","Suspect"))]
                    AddedDataNSV$AddedItems <- TRUE
                    train <- merge(train, AddedDataNSV, 
                        by = intersect(names(train), names(AddedDataNSV)), all = TRUE)
                }
            }
            cat("\n")
        } else if (nbPresent >= threshold & !is.na(threshold)) { 
            ## If threshold reached or exceeded      
            if (add.mode == "SV") {  
                # SV (Validated Suspects)
                DataSV <- CtxSVData[which(CtxSVData$Class == cl), ]
                drop.nb <- min(nbPresent-threshold+ceiling(5*threshold/100), 
                    nbPresent-threshold+NROW(DataSV))
        
                if (drop.nb == 0) {
                    cat(paste("\n   Dropping", drop.nb, "item", sep = " "))
                } else {
                    if (drop.nb == 1) {
                        cat(paste("\n   Dropping", drop.nb, "item", sep = " "))
                    } else {
                        cat(paste("\n   Dropping", drop.nb, "items", sep = " "))
                    }
                    train <- dropItemsToTrain(train = train, cl = cl, drop.nb = drop.nb)
          
                    if (NROW(DataSV) < 1) {
                        cat("\n   Adding 0 suspect item")
                    } else {
                        nbAddSV <- min(threshold-(nbPresent-drop.nb), NROW(DataSV))
                        if (nbAddSV == 1) {
                            cat(paste("\n   Adding", nbAddSV, "suspect item", sep = " "))
                        } else {
                            cat(paste("\n   Adding", nbAddSV, "suspect items", sep = " "))
                        }
                        AddedDataSV <- DataSV[sample(NROW(DataSV), nbAddSV), 
                            !(names(CtxValidData) %in% c("Validated","Suspect"))]
                        AddedDataSV$AddedItems <- TRUE
                        train <- merge(train, AddedDataSV, 
                            by = intersect(names(train), names(AddedDataSV)), all = TRUE)
                    }
                }
            }
      
            if (add.mode == "NSV") { 
                # NSV (Validated Non-Suspects)
                DataNSV <- CtxNSVData[which(CtxNSVData$Class == cl), ]
                drop.nb <- min(nbPresent-threshold+ceiling(5*threshold/100), 
                       nbPresent-threshold+NROW(DataNSV))
        
                if (drop.nb == 0) {
                    cat(paste("\n   Dropping", drop.nb, "item", sep = " "))
                } else {
                    if (drop.nb == 1) {
                        cat(paste("\n   Dropping", drop.nb, "item", sep = " "))
                    } else {
                        cat(paste("\n   Dropping", drop.nb, "items", sep = " "))
                    }
                    train <- dropItemsToTrain(train = train, cl = cl, drop.nb = drop.nb)
          
                    if (NROW(DataNSV) < 1) {
                        cat("\n   Adding 0 non-suspect item")
                    } else {
                        nbAddNSV <- min(threshold-(nbPresent-drop.nb), NROW(DataNSV))
                        if (nbAddNSV == 1) {
                            cat(paste("\n   Adding", nbAddNSV, "non-suspect item", sep = " "))
                        } else {
                            cat(paste("\n   Adding", nbAddNSV, "non-suspect items", sep = " "))
                        }
                        AddedDataNSV <- DataNSV[sample(NROW(DataNSV), nbAddNSV), 
                            !(names(CtxValidData) %in% c("Validated","Suspect"))]
                        AddedDataNSV$AddedItems <- TRUE
                        train <- merge(train, AddedDataNSV, 
                            by = intersect(names(train), names(AddedDataNSV)), all = TRUE)
                    }
                }
            }
      
            if (add.mode == "SV+NSV") {  
                # SV (Validated Suspects)
                DataSV <- CtxSVData[which(CtxSVData$Class == cl), ]
                # NSV (Validated Non-Suspects)
                DataNSV <- CtxNSVData[which(CtxNSVData$Class == cl), ]
        
                drop.nb <- min(nbPresent-threshold+ceiling(5*threshold/100), 
                    nbPresent-threshold+NROW(DataSV)+NROW(DataNSV))
                nbAddSV <- min(ceiling(5*threshold/100), NROW(DataSV))
                nbAddNSV <- min(threshold-nbAddSV+drop.nb-nbPresent, NROW(DataNSV))
        
                if (drop.nb == 0) {
                    cat(paste("\n   Dropping", drop.nb, "item", sep = " "))
                } else {   
                    if (drop.nb == 1) {
                        cat(paste("\n   Dropping", drop.nb, "item", sep = " "))
                    } else {
                        cat(paste("\n   Dropping", drop.nb, "items", sep = " "))
                    }
                    train <- dropItemsToTrain(train = train, cl = cl, drop.nb = drop.nb)
          
                    if (nbAddSV == 0) {
                        cat("\n   Adding 0 suspect item")
                    } else {
                        if (nbAddSV == 1) {
                            cat(paste("\n   Adding", nbAddSV, "suspect item", sep = " "))
                        } else {
                            cat(paste("\n   Adding", nbAddSV, "suspect items", sep = " "))
                        }
                        AddedDataSV <- DataSV[sample(NROW(DataSV), nbAddSV), 
                            !(names(CtxValidData) %in% c("Validated","Suspect"))]
                        AddedDataSV$AddedItems <- TRUE
                        train <- merge(train, AddedDataSV, 
                            by = intersect(names(train), names(AddedDataSV)), all = TRUE)
                    }
          
                    if (nbAddNSV == 0) {
                        cat("\n   Adding 0 non-suspect item")
                    } else {
                        if (nbAddNSV == 1) {
                            cat(paste("\n   Adding", nbAddNSV, "non-suspect item", sep = " "))
                        } else {
                            cat(paste("\n   Adding", nbAddNSV, "non-suspect items", sep = " "))
                        }
                        AddedDataNSV <- DataNSV[sample(NROW(DataNSV), nbAddNSV), 
                            !(names(CtxValidData) %in% c("Validated","Suspect"))]
                        AddedDataNSV$AddedItems <- TRUE
                        train <- merge(train, AddedDataNSV, 
                            by = intersect(names(train), names(AddedDataNSV)), all = TRUE)
                    }
                }
            }
            cat("\n") 
        }
    }
    train <- train[order(train$Id), ]
    attr(train, "path") <- paths
    train
}

## Function to select contextual samples (WITH GRAPH)
# contextSelection <- function()
# {
#   CtxSmp <- NULL
#   #res <- dlgMessage("Do you want to select contextual samples?", type = "yesno")$res
#   res <- "yes"
#   while (res != "no") {
#     ## Graph for selection of contextual samples ('_valid.RData' files)
#     ctxSmpdir <- dlgDir(title = paste("Select dir containing contextual samples ('*_valid.RData' files)"))$res
#     ctxSmp <- list.files(ctxSmpdir, pattern = "_valid.RData")
#     
#     if(length(ctxSmp) < 1) {
#       warning("No validated files selected in this directory!")
#       return(invisible(NULL))
#     } else {
#       if (length(ctxSmp) > 20) {
#         msg <- paste(length(ctxSmp), "validated samples in this directory!\nPlease pre-select the most relevant samples...")
#         res <- dlgMessage(msg, type = "okcancel")$res
#         if (res == "cancel") return(invisible(NULL))
#         ctxSmp <- dlgList(ctxSmp, multiple = TRUE, title = "Select contextual samples (20 max):")$res
#         if (!length(ctxSmp)) return(invisible(NULL))
#       }
#       nbValid <- NULL
#       nbNotValid <- NULL
#       nbItems <- NULL
#       
#       ## Function to identify the bars selected by user (drawn rect)
#       identifyBar <- function(x, y = NULL, n = length(x), ...) {
#         sel <- rep(FALSE, length(x))
#         res <- NULL
#         while(sum(sel) < n) {
#           ans <- identify(x[!sel], y[!sel], n = 1, plot = FALSE, tolerance = .1)
#           if(!length(ans)) break
#           ans <- which(!sel)[ans]
#           rect(0, graph.y[ans]-.5, nbItems[which(bp == graph.y[ans])], graph.y[ans]+.5, lwd = 3)
#           sel[ans] <- TRUE
#           res <- c(res, ans)
#         }
#         dev.off()
#         res
#       }
#       
#       ## How many validated items in each selected contextual samples?
#       for (i in 1:length(ctxSmp)) {
#         dat <- get(load(paste(ctxSmpdir, ctxSmp[i], sep = "/")))
#         nbItems <- c(nbItems, nrow(dat))
#         nbNotValid <- c(nbNotValid, nrow(dat) - length(which(dat$Validated == TRUE)))
#         nbValid <- c(nbValid, length(which(dat$Validated == TRUE)))
#       }
#       #ctxSmp <- ctxSmp[which(nbValid != 0)]
#       #nbItems <- nbItems[which(nbValid != 0)]
#       #nbNotValid <- nbNotValid[which(nbValid != 0)]
#       #nbValid <- nbValid[which(nbValid != 0)]
#       nb <- rbind(nbValid, nbNotValid)
#       
#       ## Display the numbers of validated items in a graph
#       par(mar=c(4,10,4,4))
#       bp <- barplot(nb, horiz = TRUE, axes = FALSE, col = c("green4", "red"), border = NA, space = .6)
#       axis(2, at = bp, label = sub("_valid.*", "", ctxSmp), las = 2, cex.axis = .6)
#       axis(1, cex.axis = .6)
#       title("Contextual samples selection", adj = 1)
#       title(paste("\n\n\nClick bars and right-click when done."), adj = 1, font.main = 2, cex.main = .6)
#       graph.y <- do.call("c", lapply(1:length(bp), function(x) rep(bp[x], nbItems[x])))
#       graph.x <- do.call("c", lapply(unique(bp), function(x) seq(1, nbItems[which(bp == x)])))
#       legend("topright", legend = c("Validated", "Not validated"), col = c("green4", "red"), cex = .6, lty = 1, lwd = 3)
#       
#       selectedCtxSmp <- identifyBar(x = graph.x, y = graph.y)
#       ctxSmp <- ctxSmp[which(bp %in% unique(graph.y[selectedCtxSmp]))]
#       ctxSmp <- paste(ctxSmpdir, ctxSmp, sep = "/")
#       CtxSmp <- c(CtxSmp, ctxSmp)
#     }    
#     res <- dlgMessage("Do you want to select more contextual samples?", type = "yesno")$res
#   }
#   CtxSmp <- unique(CtxSmp)
#   CtxSmp
# }

## Function to select contextual samples (WITH LIST)
contextSelection <- function()
{
    CtxSmp <- NULL
    res <- "yes"
    while (res != "no") {   
        smpdir <- dlgDir(title = paste("Select dir containing contextual samples ('*_valid.RData' files)"))$res
        smp <- list.files(smpdir, pattern = "_valid.RData")
#       while (length(smp) < 1) {
#           dlgMessage("No validated files in this directory!", type = "ok")
#           smpdir <- dlgDir(title = paste("Select dir containing contextual samples ('*_valid.RData' files)"))$res
#           smp <- list.files(smpdir, pattern = "_valid.RData")
#       }
    
        nbValid <- NULL
        if (length(smp) < 1) {
            dlgMessage("No validated files in this directory!", type = "ok")
        } else {          
            ## How many validated items in each selected contextual samples?
            for (i in 1:length(smp)) {
                dat <- get(load(paste(smpdir, smp[i], sep = "/")))
                nbValid <- c(nbValid, length(which(dat$Validated == TRUE)))
            }
            smp <- smp[which(nbValid != 0)]
            nbValid <- nbValid[which(nbValid != 0)]
    
            if (length(smp) < 1) {
                dlgMessage("No validated files in this directory!", type = "ok")
            } else {
                ctxSmp <- dlgList(paste(sub("_valid.*", "", smp),
                    " (", nbValid, " validated items)", sep = ""),
                    multiple = TRUE, title = "Select contextual samples:")$res
                if (!length(ctxSmp)) return(invisible(NULL))
                ctxSmp <- paste(smpdir, smp[sub("_valid.*", "", smp) %in%
                    sub(" \\(.*", "", ctxSmp)], sep = "/")
                CtxSmp <- c(CtxSmp, ctxSmp)    
            }
        }
        res <- dlgMessage("Do you want to continue the selection?", type = "yesno")$res
    }
    unique(CtxSmp)
}
