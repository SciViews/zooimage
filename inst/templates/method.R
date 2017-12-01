## Zoo/PhytoImage process REPHY version 1.0
## Copyright (c) 2014, Philippe Grosjean (phgrosjean@sciviews.org)
## Note: we need to start with the directory containing this script as default one!
# e.g., setwd(~/Desktop/ZooPhytoImage/_analyses)
## Should use source("~/dir/file.R", chdir = TRUE) to get there temporarily

################################################################################
#### Parameters for this method
## This is the name of this method
.ZI <- list(user = "", date = Sys.time(), method = "Rephy 4X lugol v.1.0",
    wdir = getwd(), system = "")
.ZI$scriptfile <- paste(.ZI$method, "R", sep = ".")
## This is the training set to use
.ZI$train <- "trainRephy_4Xlugol.01"
.ZI$traindir <- file.path("..", "_train", .ZI$train)
.ZI$trainfile <- paste(.ZI$traindir, "RData", sep = ".")
.ZI$classif <- "classrfRephy_4Xlugol.01"
.ZI$classifile <- file.path("..", "_train",
    paste(.ZI$classif, "RData", sep = "."))
.ZI$classifcmd <- paste0('ZIClass(Class ~ ., data = ', .ZI$train,
    ', method = "mlRforest", calc.vars = calcVars, cv.k = 10)')
.ZI$cellsModelsfile <- file.path(.ZI$traindir, "_cellModels.RData", sep = "")
## Conversion factors for biovolume
## Biovolume calculation is P1 * ECD^P3 + P2
## TODO: fill this table, or use read.delim() on a text file
## TODO: also use number of cells per colony here...
.ZI$biovolume <- data.frame(
    Class = c("Chaetoceros_spp", "[other]"),
    P1 = c(1, 1),
    P2 = c(0, 0),
    P3 = c(1, 1)
)
.ZI$breaks <- seq(0, 200, by = 10) # In um
################################################################################

if (!require(zooimage)) stop("Please, install the 'zooimage' package")
if (!require(svDialogs)) stop("Please, install the 'svDialogs' package")
if (!require(shiny)) stop("Please, install the 'shiny' package")

## First of all, get system info and ask for the user name
.ZI$system <- paste(ZIverstring, R.version$version.string, R.version$platform,
    sep = "; ")
.ZI$user <- dlgInput("Who are you?", Sys.info()["user"])$res
if (!length(.ZI$user) || .ZI$user == "") { # The user clicked the 'cancel' button
    stop("You must identify yourself!")
}

## Change the way warnings are displayed
.owarn <- getOption("warn")
options(warn = 1) # Immediate issue of warnings
## Start... check that I am in the right directory
## The directory should be '_analyses', there must be a file named <method>.R
## in it, and a file named "../_train/<train>[.RData] must be available too!
if (basename(.ZI$wdir) != "_analyses")
    stop("I am not in the right directory (should be '_analyses')")
if (!file.exists(.ZI$scriptfile))
    stop("A .R script file named '", .ZI$scriptfile,
        "' is not found in the current directory")
if (!file.exists(.ZI$traindir) & !file.exists(.ZI$trainfile))
    stop("Training set '", .ZI$train, "' not found in '",
        dirname(.ZI$traindir), "'") 
    
## Make sure the subdirectory for this method is created
if (!file.exists(.ZI$method)) {
    dir.create(.ZI$method)
} else if (!file.info(.ZI$method)$isdir) {
    stop("A file exists for the method '", .ZI$method,
        "', but it is not a directory!")
}

## Start reporting results
cat("\n=== Session with method", .ZI$method, "===\n\n")

## Do we need to import the training set?
if (!file.exists(.ZI$trainfile)) {
    cat("Please wait: we import the training set data now...")
    assign(.ZI$train, getTrain(.ZI$traindir))
    cat(" done!\n")
    cat("\nThe training set is saved as native R data for faster access.\n")
    save(list = .ZI$train, file = .ZI$trainfile)
} else { # Load the training set now
    cat("Loading the training set '", .ZI$train, "'...", sep = "")
    load(.ZI$trainfile)
    cat(" done!\n")
}
.ZITrain <- get(.ZI$train) # Copied into .ZITrain for easier use
## Give some stats about the training set
cat("The initial training set contains:\n")
print(table(.ZITrain$Class))

## Do we need to recreate the classifier?
if (!file.exists(.ZI$classifile)) {
    ## TODO: recreate it!
    cat("\nPlease wait: we build the classifier now...")
    assign(.ZI$classif, eval(parse(text = .ZI$classifcmd)))
    cat(" done!\n")
    cat("\nThe classifier is saved as native R data for faster access.\n")
    save(list = .ZI$classif, file = .ZI$classifile)
} else { # Load the classifier now
    cat("\nLoading the classifier '", .ZI$classif, "'...", sep = "")
    load(.ZI$classifile)
    cat(" done!\n")
}
.ZIClass <- get(.ZI$classif) # Copied into .ZIClass for easier use
attr(.ZIClass, "ActiveLearning") <- FALSE # No active learning yet!
## Give some stats about the classifier
cat("The classifier is:\n\n")
print(.ZIClass)

## Launch the errorcorrection Shiny app
cat("\nStarting error correction session...\n")
runApp(system.file("gui", "errorcorrection", package = "zooimage"))

## Reset the system
options(warn = .owarn)

## Done
cat("\n================================= done! =====\n")

## TODO: if we have .zid files + description.zis => convert first into .zidb!
