## Copyright (c) 2004-2015, Ph. Grosjean <phgrosjean@sciviews.org>
## & Guillaume Wacquet
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

## TODO: rework countCells into ZICell and return a ZICell object
##      with predict() method that should replace computeCell() function
## Create a ZICell object that can be used to predict the number of cells per
## colonies in the particles that are analyzed
## A ZICell object is a list of predictive models, one per plankton class
## For the classes, where there is no model defined, the conversion is
## assumed to be one-for-one (one cell in each particle)
#ZICell <- function (formula, data, method = getOption("ZI.mlearning",
#"mlMda"), calc.vars = getOption("ZI.calcVars", calcVars), drop.vars = NULL,
#drop.vars.def = dropVars(), cv.k = 10, cv.strat = TRUE, ...,
#subset, na.action = na.omit)
#{
#TODO: code of ZICell().    
#}

## TODO: adapt from this:
# Logarithmic transformation of FlowCAM parameters
logFeatures <- function (data) {
    ## Parameter:
    ## data: the measurements table.
  
    ## Features based on grey levels are dropped
    ## TODO: also allow this to work with other data than FlowCAM!
    vars <- c("ECD", "FIT_Area_ABD", "FIT_Length", "FIT_Width",
        "FIT_Diameter_ESD", "FIT_Perimeter", "FIT_Convex_Perimeter",
        "FIT_Compactness", "FIT_Elongation", "FIT_Roughness", "FIT_Volume_ABD",
        "FIT_Volume_ESD", "CV", "MeanFDia", "FeretRoundness", "Perim_Ratio",
        "FIT_Aspect_Ratio", "Transp2", "Nb_cells")
    for (i in 1:length(vars)) 
        data[[paste("log", vars[i], sep = ".")]] <- log(data[[vars[i]]])
    data
}

## Compute and save the predictive model (cells counting) from training set
## Avoid using train and traindir and take the risk to get out-of-synch between the
## two... on the other hand, rebuilding a training set takes time => how to speed
## this up?
cellModel <- function (train, traindir, class, method = "mda")
{
    ## Parameters:
    ## train: a ZITrain file
    ## traindir: the directory containing a training set and a _count file
    ## class: the group in training set to process (for the moment, only a terminal folder)
    ## method: the predictive method to use: lm, lda, mda (default).
  
    if (!inherits(train, "ZITrain"))
        stop("'train' does not appear to be a valid training set, or problem when reading the training set")
    
    ## Does 'class' exist in this training set?
    if (length(class) < 1 && !is.character(class))
        stop("'class' must be a single (or multiple) character string(s)")
  
    ## Does 'method' is a valid method?
    if (length(method) != 1 && !is.character(method))
        stop("'method' must be a single character string")
    if (!method %in% c("lm","lda","mda"))
        stop("'method' must be one of 'lm', 'lda' or 'mda'")
  
    ## Is there already a "_cellModels.RData" file?
    models <- NULL
    odir <- setwd(traindir)
    on.exit(setwd(odir))
    modelsFile <- "_cellModels.RData"
    modelsPath <- file.path(traindir, modelsFile)
    if (file.exists(modelsFile))
        models <- readRDS(modelsFile)
  
    cat("Building predictive models...")
    for (cl in class) {
        if (!cl %in% levels(train$Class)) {
            warning("'", cl, "' is not a class of the terminal classes in the training set")
        } else {
            nbCounted <- sum(!is.na(train$Nb_cells[train$Class == cl]))
            if (nbCounted < 1) {
                warning("Nothing counted for '", cl, "'!", sep = "") 
            } else {
                if (cl %in% names(models))
                    warning("A model exists for '", cl, "'. It will be replaced!", sep="")
        
                train2 <- calcVars(train[train$Class == cl & !is.na(train$Nb_cells), ])
                train2 <- logFeatures(train2)
                form <- as.formula(log.Nb_cells ~ log.FIT_Perimeter +
                    log.FIT_Convex_Perimeter + log.FIT_Diameter_ESD +
                    log.FIT_Volume_ESD + log.FIT_Area_ABD + log.FIT_Volume_ABD +
                    log.ECD + log.FeretRoundness + log.FIT_Length +
                    log.FIT_Compactness + log.FIT_Elongation + log.MeanFDia +
                    log.FIT_Roughness + log.Perim_Ratio + log.Transp2 +
                    log.FIT_Width + log.FIT_Aspect_Ratio + log.CV)
      
                if (method == "lm")
                    model <- lm(form, data = train2)
                if (method == "lda")
                    model <- lda(form, data = train2)
                if (method == "mda")
                    model <- mda(form, data = train2, start.method = "kmeans",
                        keep.fitted = TRUE, method = gen.ridge, iter = 10)
       
                models[[cl]] <- model
                saveRDS(models, file = modelsPath)
            }
        }
    }
    cat("Done!")
}

## TODO: calc.vars.cells
## TODO: rework cellCompute() into a predict.ZICell() method
## This should really be the predict() method of a ZICell object!
## Compute and save the number of cells for each particle in a sample
#predict.ZICell <- function (object, newdata, ...) 
#{
## Compute and save the number of cells for each particle in a sample
cellCompute <- function (data, cellModels) 
{
    ## Parameters:
    ## data: sample containing the particles to count.
    ## cellModels: the file containing the models for cells countings.

    ## Preparation of the data
    newdata <- data # To match arguments of the future method
    newdata$Id.1 <- NULL
    newdata$X.Item.1 <- NULL
    newdata$Nb_cells <- 1
      
    object <- readRDS(cellModels) # To be eliminated from the predict() method!
    ## List classes in data for which a predictive model exists
    objClasses <- names(object)
    Classes <- intersect(unique(newdata$Class), objClasses)
    
    if (!length(Classes)) {
        warning("No predictive models for classes in this sample")
    } else {
        ## For each class, compute the number of cells in each particle
        for (cl in Classes) {
            data2 <- calcVars(newdata[newdata$Class == cl &
                !is.na(newdata$Nb_cells), ])
            #print(data2[1:5,])
            data2 <- logFeatures(data2)
            #print(data2[1:5,])
            
            if (class(object[[cl]])[1] == "lm")
                pred <- predict(object[[cl]], newdata = data2)
            if (class(object[[cl]])[1] == "lda")
                pred <- predict(object[[cl]], newdata = data2)$class
            if (class(object[[cl]])[1] == "mda")
                pred <- predict(object[[cl]], newdata = data2)
        
            ## Logarithmic counts --> integer counts
            newdata[which(newdata$Class == cl &
                !is.na(newdata$Nb_cells)), ]$Nb_cells <-
                round(exp(as.numeric(pred)))
        }
    }
    ## Return the modified data
    newdata
}

cellCount <- function (traindir, class, reset = FALSE)
{
    ## Parameters:
    ## traindir: the directory containing a training set
    ## class: the group in training set to process (for the moment, only a terminal folder)
    ## reset: delete all existing counting for the class before proceeding
  
    ## traindir: is it a correct training set directory?
    ## TODO: do we just need to check, or do we need to create the ZITrain object completely?
    cat("Reading training set...")    
    train <- getTrain(traindir)
    if (!inherits(train, "ZITrain"))
        stop("'traindir' does not appear to ba a valid training set, or problem when reading the training set")
    cat(" done!\n")

    ## Does 'class' exist in this training set?
    if (length(class) != 1 && !is.character(class))
        stop("'class' must be a single character string")
    if (!class %in% levels(train$Class))
        stop("'class' must be one of the terminal classes in the training set")
    
    ## Get the path corresponding to this class
    paths <- attr(train, "path")
    path <- paths[basename(paths) == class]
    
    ## TODO: possibly simplify the ZITrain object?
    ## Create the "Nb_cells" column
    train$Nb_cells <- NA
    
    ## Is there already a "_count.RData" file?
    odir <- setwd(traindir)
    on.exit(setwd(odir))
    countFile <- "_count.RData"
    countPath <- file.path(traindir, countFile)
    if (file.exists(countFile)) {
        train2 <- readRDS(countFile)
        ## Add new items from train
        train <- rbind(train2, train[!train$Id %in% train2$Id, ])
        train <- train[order(train$Id), ]
    }
    
    ## Stats about countings
    cat("Number of already counted particles in each class:\n")
    print(table(train$Class[!is.na(train$Nb_cells)]))
    
    ## Possibly reset countings for class
    if (isTRUE(reset)) {
        cat("Warning: resetting countings for class '", class, "'\n", sep = "")
        train$Nb_cells[train$Class == class] <- NA
    }
    
    ## Indicate the number of items to count
    cat("Counting cells in '", class, "'...\n", sep = "")
    nbToCount <- sum(is.na(train$Nb_cells[train$Class == class]))
    if (nbToCount < 1)
        stop("No more items to count for '", class, "'!\n", sep = "")
    if (nbToCount == 1) {
        cat("One vignette left to count...\n", sep = "")
    } else cat(nbToCount, " remaining vignettes to process...\n", sep = "")

    ## Location of items to count
    itemsToCount <- (1:nrow(train))[train$Class == class & is.na(train$Nb_cells)]
    
    for (i in itemsToCount) {
        imgFile <- file.path(path, paste(train$Id[i], "jpg", sep = "."))
        readImg <- readJPEG
        if (!file.exists(imgFile)) {
            imgFile <- file.path(path, paste(train$Id[i], "png", sep = "."))
            readImg <- readPNG
        }
        if (!file.exists(imgFile))
            stop("Vignette not found! (", imgFile, " or .jpg)")
        img <- readImg(imgFile, native = TRUE)
        dev.new()
        dimg <- dim(img)
        h <- dimg[1]
        w <- dimg[2] 
        plot(c(0, w), c(0, h), type = 'n', xlab = "", ylab = "", axes = FALSE)
        rasterImage(img, 0, 0, w, h) 
        title(paste("Class:", class), adj = 0)
        title(paste("\n\n\nVignette:", train$Id[i]), adj = 0, font.main = 1,
            cex.main = .8)
        title(sub = paste0(
            "Particle Length: ",
            toString(signif(train$FIT_Length[i]), digits = 4), " \u00B5m",
            "\nParticle Width: ",
            toString(signif(train$FIT_Width[i]), digits = 4), " \u00B5m",
            "\nParticle ECD: ",
            toString(signif(train$ECD[i]), digits = 4), " \u00B5m\n\n\n"),
            adj = 0, font.sub = 1, cex.sub = .8)
        title(sub = "Click items and right-click when done.\nClose windows to end.",
            adj = 1, font.sub = 2, cex.sub = .8)
          
        nbCells <- locator(10000, type = "p", col = "red", pch = 16, cex = 2)
        dev.off()
        n <- length(nbCells$x)
        if (n < 1) n <- NA
        train$Nb_cells[i] <- n
        
        cat(paste0("Number of cells in ", train$Id[i], ": ", train$Nb_cells[i],
            "\n"))
        saveRDS(train, file = countPath)
    }
    
    ## Indication about what was done
    nbCounted <- sum(!is.na(train$Nb_cells[train$Class == class]))
    if (nbCounted < 1)
        stop("Nothing counted for '", class, "'!\n", sep = "")
    if (nbCounted == 1) {
        cat("One vignette counted for '", class, "'...\n", sep = "")
    } else cat(nbCounted, " vignettes counted for class '", class, "'...\n", sep = "")
    
    ## Statistics for countings for this class
    Counts <- as.character(train$Nb_cells[train$Class == class])
    Counts <- paste0(Counts, "cells")
    print(table(Counts))
    
    ## Call 'cellModel' function to build predictive model
    # To avoid small dimension, within-class singularity problems and build relevant model
    res <- try(cellModel(train = train, traindir = traindir, class = class, method = "mda"),
               silent = TRUE)
    if (inherits(res, "try-error"))
      warning("More counted vignettes are required for class '", class, "' to build model!")
    
    invisible(train)
}
