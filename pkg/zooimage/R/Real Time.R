# read.lst for both FlowCAM II and III by Kevin Denis
read.lst <- function (x, skip = 2) {
  # Determine the version of the FlowCAM
  ncol <- length(read.table(x, header = FALSE, sep = ":", dec = ".", skip = 2, nrow = 1))
  if(ncol <= 44){
    # FlowCAM II with 44 columns
    # read the table
    tab <- read.table(x, header = FALSE, sep = ":", dec = '.',
    col.names = c("Id", "FIT_Cal_Const", "FIT_Raw_Area", "FIT_Raw_Feret_Max",
    "FIT_Raw_Feret_Min", "FIT_Raw_Feret_Mean",
    "FIT_Raw_Perim", "FIT_Raw_Convex_Perim", "FIT_Area_ABD", "FIT_Diameter_ABD",
    "FIT_Length", "FIT_Width", "FIT_Diameter_ESD", "FIT_Perimeter", "FIT_Convex_Perimeter",
    "FIT_Intensity", "FIT_Sigma_Intensity", "FIT_Compactness", "FIT_Elongation",
    "FIT_Sum_Intensity", "FIT_Roughness", "FIT_Feret_Max_Angle", "FIT_Avg_Red",
    "FIT_Avg_Green", "FIT_Avg_Blue", "FIT_PPC", "FIT_Ch1_Peak",
    "FIT_Ch1_TOF", "FIT_Ch2_Peak", "FIT_Ch2_TOF", "FIT_Ch3_Peak", "FIT_Ch3_TOF",
    "FIT_Ch4_Peak", "FIT_Ch4_TOF", "FIT_Filename", "FIT_SaveX",
    "FIT_SaveY", "FIT_PixelW", "FIT_PixelH", "FIT_CaptureX",
    "FIT_CaptureY", "FIT_High_U32", "FIT_Low_U32", "FIT_Total"), skip = skip)
    # Add columns present in list files from FlowCAM III
    tab$FIT_Feret_Min_Angle <- NA
    tab$FIT_Edge_Gradient <- NA
    tab$FIT_Timestamp1 <- NA
    tab$FIT_Timestamp2 <- NA
    tab$FIT_Source_Image <- NA
    tab$FIT_Calibration_Image <- NA
    tab$FIT_Ch2_Ch1_Ratio <- tab$FIT_Ch2_Peak / tab$FIT_Ch1_Peak
    # new variables calculation (present in dataexport.csv from the FlowCAM)
    tab$FIT_Volume_ABD <- (4/3) * pi * (tab$FIT_Diameter_ABD/2)^3
    tab$FIT_Volume_ESD <- (4/3) * pi * (tab$FIT_Diameter_ESD/2)^3
    tab$FIT_Aspect_Ratio <- tab$FIT_Width / tab$FIT_Length
    tab$FIT_Transparency <- 1 - (tab$FIT_Diameter_ABD/tab$FIT_Diameter_ESD)
    tab$FIT_Red_Green_Ratio <- tab$FIT_Avg_Red / tab$FIT_Avg_Green
    tab$FIT_Blue_Green_Ratio <- tab$FIT_Avg_Blue / tab$FIT_Avg_Green
    tab$FIT_Red_Blue_Ratio <- tab$FIT_Avg_Red / tab$FIT_Avg_Blue
  } else {
    # FlowCAM III with 47 columns
    # read the table
    tab <- read.table(x, header = FALSE, sep = ":", dec = '.',
    col.names = c("Id", "FIT_Cal_Const", "FIT_Raw_Area", "FIT_Raw_Feret_Max", "FIT_Raw_Feret_Min",
    "FIT_Raw_Feret_Mean", "FIT_Raw_Perim", "FIT_Raw_Convex_Perim", "FIT_Area_ABD",
    "FIT_Diameter_ABD", "FIT_Length", "FIT_Width", "FIT_Diameter_ESD", "FIT_Perimeter",
    "FIT_Convex_Perimeter", "FIT_Intensity", "FIT_Sigma_Intensity", "FIT_Compactness",
    "FIT_Elongation", "FIT_Sum_Intensity", "FIT_Roughness", "FIT_Feret_Max_Angle",
    "FIT_Feret_Min_Angle", "FIT_Avg_Red", "FIT_Avg_Green", "FIT_Avg_Blue", "FIT_PPC",
    "FIT_Ch1_Peak", "FIT_Ch1_TOF", "FIT_Ch2_Peak", "FIT_Ch2_TOF", "FIT_Ch3_Peak",
    "FIT_Ch3_TOF", "FIT_Ch4_Peak", "FIT_Ch4_TOF", "FIT_Filename", "FIT_SaveX",
    "FIT_SaveY", "FIT_PixelW", "FIT_PixelH", "FIT_CaptureX", "FIT_CaptureY", "FIT_Edge_Gradient",
    "FIT_Timestamp1", "FIT_Timestamp2", "FIT_Source_Image", "FIT_Calibration_Image"), skip = skip)
    # Add columns present in list files from FlowCAM II
    tab$FIT_High_U32 <- NA
    tab$FIT_Low_U32 <- NA
    tab$FIT_Total <- NA
    # new variables calculation (present in dataexport.csv from the FlowCAM)
    tab$FIT_Volume_ABD <- (4/3) * pi * (tab$FIT_Diameter_ABD/2)^3
    tab$FIT_Volume_ESD <- (4/3) * pi * (tab$FIT_Diameter_ESD/2)^3
    tab$FIT_Aspect_Ratio <- tab$FIT_Width / tab$FIT_Length
    tab$FIT_Transparency <- 1 - (tab$FIT_Diameter_ABD/tab$FIT_Diameter_ESD)
    tab$FIT_Red_Green_Ratio <- tab$FIT_Avg_Red / tab$FIT_Avg_Green
    tab$FIT_Blue_Green_Ratio <- tab$FIT_Avg_Blue / tab$FIT_Avg_Green
    tab$FIT_Red_Blue_Ratio <- tab$FIT_Avg_Red / tab$FIT_Avg_Blue
    tab$FIT_Ch2_Ch1_Ratio <- tab$FIT_Ch2_Peak / tab$FIT_Ch1_Peak
  }
  return(tab)
}

# Prediction of classes in real-time
"predict.ZIClass.Real.Time" <-
	function(object, ZIDat, calc.vars = TRUE, class.only = FALSE, type = "class", na.rm = NULL, ...) {
	# Make sure we have correct objects
	if (!inherits(object, "ZIClass"))
		stop("'object' must be a ZIClass object!")
	if (!inherits(ZIDat, "ZIDat") && !inherits(ZIDat, "data.frame"))
		stop("'ZIDat' must be a ZIDat object, or a data.frame!")
	# Possibly load a specific package for prediction
	package <- attr(object, "package")
	if (is.null(package)) {
        # This is for old version, we make sure to load
        # MASS, RandomForest, class, rpart, e1071, ipred
        # Rem: nnet has a special treatment in nnet2
        (require(MASS) || stop("package 'MASS' is required!"))
        (require(RandomForest) || stop("package 'RandomForest' is required!"))
        (require(class) || stop("package 'class' is required!"))
        (require(rpart) || stop("package 'rpart' is required!"))
        (require(e1071) || stop("package 'e1071' is required!"))
        (require(ipred) || stop("package 'ipred' is required!"))
    } else {
        # Make sure that the specific required package is loaded
        eval(parse(text = paste("require(", package, ")", sep = "")))
    }

  class(object) <- class(object)[-1]
	data <- as.data.frame(ZIDat)
	if (calc.vars) data <- attr(object, "calc.vars")(data)
	if (!is.null(na.rm)) na.omit(data)
  if(type != "prob"){
    Ident <- predict(object, newdata = data, type = type)
  } else {
  if(inherits(object, "randomForest")) {
    Ident <- predict(object, newdata = data, type = type)
  }
  if(inherits(object, "lda")) {
    Ident <- predict(object, newdata = data)$posterior
  }
  }
	# Special case for prediction from an LDA (list with $class item)
	if (inherits(Ident, "list") && "class" %in% names(Ident))
		Ident <- Ident$class
	if (!class.only) {
		res <- cbind(ZIDat, Ident)
		class(res) <- class(ZIDat)
	} else res <- Ident
	return(res)
}

# Calculation of biomass
Biomass <- function(tab){
		res <- NULL
    grps <- levels(tab$Ident)
    for(i in 1:length(grps)){
      res[i] <- sum(tab$Biomass[tab$Ident %in% grps[i]])
		}
		names(res) <- grps
		return(res)
}

# Plot of histograms and line for exponential decrease for size spectra
hist.spectrum <- function(spect, breaks = seq(0.05, 0.6, by = 0.05),
	width = 0.1, xlab = "classes (mm)", ylab = "log(abundance + 1)", main = "",
	ylim = c(0, 10)) {
	spect.lm <- lm(spect ~ breaks[-length(breaks)])
    print(summary(spect.lm))
    slope <- format(coef(spect.lm)[2], digits = 3)
    main <- paste(main, " (slope = ", slope, ")", sep = "")
	barplot(spect, width = 0.1, space = 0, xlab = xlab, ylab = ylab, main = main, ylim = ylim)
	#abline(a = coef(spect.lm)[1], b = coef(spect.lm)[2], col = 2, lwd = 2)
  return(invisible(spect.lm))
}

# Function which controls arguments
loop.opts <- function(lst = ".", # path of the list file of the current FlowCAM experiment
  classif = ZIC, # Classifier
  type = NULL, # Null: barplot, "l" : line alpha code
  SizeThreshold = NULL, # NULL or Size threshold in µm alpha code
  Export_Collages = NULL, # NULL or Number of collages by artificial sample alpha code
  ZIprevSmp = NULL, # Comparison with one previous sample
  ZIlist = NULL,  # Comparison several previous samples
  Abd.all = TRUE, # NULL or TRUE
  Abd.gp = NULL, # NULL or groups to plot
  Spec.all = NULL,  # NULL or TRUE
  Spec.gp = NULL, # NULL or groups to plot
  Bio.all = NULL, # NULL or TRUE
  Bio.gp = NULL, # NULL or groups to plot
  breaks = seq(0.05, 3, by = 0.1),  # in mm
  conv = ".", #c(1, 0, 1), # or conversion table
  ZICompAbd = NULL,
  ZICompSpectra = NULL,
  ZICompBiomass = NULL,
  ZICompSlope = NULL,
  ZICompAbd.gp = NULL,
  ZICompBio.gp = NULL
  ){
  # Print in global environment default values for tab, rec, and TabGroups
  tab <<- NULL
  rec <<- NULL
  # Check argument
  if(!is.character(lst)){
    stop("lst must be a character string with the path of the list file")
  } else {
    options(Path = lst) # Path used in the process function and lst an arguement of the loop.opts function
  }
  if(!inherits(classif, "ZIClass")){
    stop("classif must be a classifier of class ZIClass")
  } else {
    options(Classifier = classif)
  }
  # Lines graphical representation
  if(!is.null(type)){
    options(type = "l")
    TabGroups <<- NULL
  } else {
    options(type = FALSE)
  }
  # Size threshold
  if(!is.null(SizeThreshold)){
    options(SizeThreshold = SizeThreshold)
    TabGroupsSize <<- NULL
  } else {
    options(SizeThreshold = FALSE)
  }
  # Collage and results exportation
  if(!is.null(Export_Collages)){
    options(MaxCollages = Export_Collages)
    Collages <<- NULL
  } else {
    options(MaxCollages = FALSE)
  }
  # Abundances
  if(!is.null(Abd.all)){
    options(Abd.all = TRUE)
  } else {
    options(Abd.all = FALSE)
  }
  if(!is.null(Abd.gp)){
    options(Abd.gp = Abd.gp)
  } else {
    options(Abd.gp = FALSE)
  }
  # Size Spectrum
  # total size spectrum
  if (!is.null(Spec.all)){
    if (!is.null(Spec.gp)){
      stop("total spectrum only")
    } else {
      options(Spec.all = TRUE)
    #options(breaks = breaks)
    }
  } else {
    options(Spec.all = FALSE)
  }
  # Size spectrum by groups
  if (!is.null(Spec.gp)){
    if (!inherits(Spec.gp, "character")) stop("groups must be a vector with names of groups")
    gp.Spec <- as.list(Spec.gp)
    names(gp.Spec) <- Spec.gp # list with levels = names of groups
    options(gp.Spec = gp.Spec)
    #options(breaks = breaks)
  } else {
    options(gp.Spec = FALSE)
  }
  # Biomass
  # Biomass for all groups
  if(!is.null(Bio.all)){
    options(Bio.all = TRUE)
  #options(conv = conv)
  } else {
    options (Bio.all = FALSE)
  }
  # Biomass by group
  if(!is.null(Bio.gp)){
    if (!inherits(Bio.gp, "character")) stop("groups must be a vector with names of groups")
    gp.Bio <- as.list(Bio.gp)
    names(gp.Bio) <- Bio.gp
    options(gp.Bio = gp.Bio)
    #options(conv = conv)
  } else {
    options(gp.Bio = FALSE)
  }
  if(!is.numeric(breaks)){
    stop("breaks must be the size intervall")
  } else {
    options(breaks = breaks)
  }
  if(!is.character(conv)){
   stop("conv must be the path of a conversion table")
  } else {
    Conv <- read.table(conv, header = TRUE, sep = "\t")
    options(conv = Conv)
  }
  #### Parameters for the comparison in near real time ####
  # the sample to compare with
  if(!is.null(ZIprevSmp)){
    if(!is.character(ZIprevSmp)) {
      stop("'ZIprevSmp' must be the path of the list file to compare")
    } else {
      options(ZIprevSmp = ZIprevSmp)
    }
  } else {
    options(ZIprevSmp = FALSE)
  }
  # comparison of abundances
  if(!is.null(ZICompAbd)){
    options(ZICompAbd = TRUE)
  } else {
    options(ZICompAbd = FALSE)
  }
  # Compa of abundances of some groups
  if(!is.null(ZICompAbd.gp)){
    options(ZICompAbd.gp = ZICompAbd.gp)
  } else {
    options(ZICompAbd.gp = FALSE)
  }
  # comparison of size spectra
  if(!is.null(ZICompSpectra)){
    options(ZICompSpectra = TRUE)
  } else {
    options(ZICompSpectra = FALSE)
  }
  # Comparison of Biomass
  if(!is.null(ZICompBiomass)){
    options(ZICompBiomass = TRUE)
  } else {
    options(ZICompBiomass = FALSE)
  }
  # Comparison of biomass by groups
  if(!is.null(ZICompBio.gp)){
    options(ZICompBio.gp = ZICompBio.gp)
  } else {
    options(ZICompBio.gp = FALSE)
  }
  # Comparison of size spectra slope
  if(!is.null(ZICompSlope)){
    options(ZICompSlope = TRUE)
  } else {
    options(ZICompSlope = FALSE)
  }
  # Comparison with more than one sample
  #if(!is.null(ZICompMultiple)) options(CompMultiple = TRUE)
  if(!is.null(ZIlist)){
    if(!is.character(ZIlist)){
      stop("'ZIlist' must be a character string of the files to analyze")
    } else {
      options(ZIlist = ZIlist)
    }
  } else {
    options(ZIlist = FALSE)
  }
}

# Function to plot information about current sample
SampleCurrent <- function(){
  if(!is.character(getOption("type")) && !is.numeric(getOption("SizeThreshold"))){  # if we want to have the line representation
    # Check if rec in R
    if (!exists("rec", env = .GlobalEnv)) stop("There is no recognition table in memory")
    # Plot the different graphes
    if (getOption("Abd.all")){
      barplot(table(rec$Ident)/nrow(rec)*100, xlab = "Groups", ylab = "Abundance (%)", main = "Relative abundance")# to improve
    }
    if (is.character(getOption("Abd.gp"))){
      barplot((table(rec$Ident)[names(table(rec$Ident)) %in% getOption("Abd.gp")])/nrow(rec)*100, xlab = "Groups", ylab = "Abundance (%)", main = "relative abundance by groups")
    }
    if (getOption("Spec.all")){
      Spec <- Spectrum(ZIDat = rec, use.Dil = FALSE, breaks = getOption("breaks"), RealT = TRUE)
      #barplot(Spec$total/nrow(rec)*100, xlab = "size interval", ylab = "Abundance", main = "Total size spectrum") # in relative abundance
      barplot(Spec$total, xlab = "size interval", ylab = "Abundance", main = "Total size spectrum")
    }
    if (is.list(getOption("gp.Spec"))){
      Spec <- Spectrum(ZIDat = rec, use.Dil = FALSE, breaks = getOption("breaks"), groups = getOption("gp.Spec"), RealT = TRUE)
      par(mfrow = c(length(getOption("gp.Spec")),1))
      for(i in 1:length(getOption("gp.Spec"))) {
        #barplot(Spec[[i]]/nrow(rec)*100, xlab = "size interval", ylab = names(getOption("gp.Spec")[i]), main = "Size spectra by groups") # in relative abundance
        barplot(Spec[[i]], xlab = "size interval", ylab = "Abundance", main = paste("Size spectrum for", names(getOption("gp.Spec")[i]), sep = " "))
      }
    }
    if (getOption("Bio.all")){
      Bio <- Bio.sample(ZIDat = rec, conv = getOption("conv"), exportdir = NULL, RealT = TRUE)
      barplot(Bio/sum(Bio)*100, xlab = "Groups", ylab = "Biomass (%)", main = "Relative biomass")
    }
    if (is.list(getOption("gp.Bio"))){
      Bio <- Bio.sample(ZIDat = rec, conv = getOption("conv"), exportdir = NULL, RealT = TRUE)
      barplot(Bio[names(Bio) %in% getOption("gp.Bio")]/sum(Bio)*100, xlab = "Groups", ylab = "Biomass (%)", main = "Relative biomass by groups")
    }
  }
}

# Function of comparison between current sample and a previous FlowCAM digitization
CompaSamplePrev <- function(){
  #if(!is.character(getOption("ZIlist"))){ # If we do not compare sample to a list
  if(unique(getOption("ZIprevSmp") != FALSE)){
    if(!is.null(getOption("ZIprevSmp"))){ # If we want to compare with a previous sample
      if(is.character(getOption("ZIlist"))) stop("You must select only one prevous sample and not a list of samples")
      if(!is.character(getOption("ZIprevSmp"))) stop ("You must provide a character string") # check if prevSmp is empty
      # Calculate general table for sample to compare
      PrevTable <- read.lst(getOption("ZIprevSmp"))
      PrevRec <- predict.ZIClass.Real.Time(getOption("Classifier"), PrevTable, calc.vars = TRUE, class.only = FALSE)
      PrevSmp <- Bio.sample(ZIDat = PrevRec, conv = getOption("conv"), exportdir = NULL, RealT = TRUE)
      PrevSmp <- Bio.tab
      rm(Bio.tab, envir = .GlobalEnv)
      # Calculate table for the sample currently analysed
      if (!exists("rec", env = .GlobalEnv)){
        stop("You must have a recognition file in memory")
      } else {
        CurrentSmp <- Bio.sample(ZIDat = rec, conv = getOption("conv"), exportdir = NULL, RealT = TRUE)
        CurrentSmp <- Bio.tab
      }
      # Comparision of the two samples
      if (getOption("ZICompAbd")){
        # Statistics
        print(paste("Difference in abundance between the previous and the current sample is", nrow(PrevSmp)- nrow(CurrentSmp), "particles", sep = " "))
        # Dominant Species
        PrevSpecies <- sort(table(PrevRec$Ident), decreasing = TRUE)
        #print(paste("Dominant species of the previous sample :", max(PrevSpecies), "particles of", names(PrevSpecies)[PrevSpecies == max(PrevSpecies)], sep = " "))
        print(paste("The 3 most abundant taxa of the previous sample :",
          PrevSpecies[1], " particles of ", names(PrevSpecies)[PrevSpecies == PrevSpecies[1]], ", ",
          PrevSpecies[2], " particles of ", names(PrevSpecies)[PrevSpecies == PrevSpecies[2]], ", ",
          PrevSpecies[3], " particles of ", names(PrevSpecies)[PrevSpecies == PrevSpecies[3]], ", ",
          sep = ""))
        CurrentSpecies <- sort(table(rec$Ident), decreasing = TRUE)
        print(paste("The 3 most abundant taxa of the current sample :",
          CurrentSpecies[1], " particles of ", names(CurrentSpecies)[CurrentSpecies == CurrentSpecies[1]], ", ",
          CurrentSpecies[2], " particles of ", names(CurrentSpecies)[CurrentSpecies == CurrentSpecies[2]], ", ",
          CurrentSpecies[3], " particles of ", names(CurrentSpecies)[CurrentSpecies == CurrentSpecies[3]], ", ",
          sep = ""))
        # Graphic representation
        par(mfrow = c(2,1))
        barplot(table(PrevSmp$Ident)/nrow(PrevSmp)*100, xlab = "Groups", ylab = "Abundance (%)", main = "Relative abundance in the previous sample") # to improve
        barplot(table(CurrentSmp$Ident)/nrow(CurrentSmp)*100, xlab = "Groups", ylab = "Abundance (%)", main = "Relative abundance in the current sample") # to improve
      }
      # Comparison of abundances for some groups
      if(is.character(getOption("ZICompAbd.gp"))){
        par(mfrow = c(2,1))
        barplot((table(PrevSmp$Ident)[names(table(PrevSmp$Ident)) %in% getOption("ZICompAbd.gp")])/nrow(PrevSmp)*100,
          xlab = "Groups", ylab = "Abundance (%)", main = "Relative abundance by groups in the previous sample")
        barplot((table(CurrentSmp$Ident)[names(table(CurrentSmp$Ident)) %in% getOption("ZICompAbd.gp")])/nrow(CurrentSmp)*100,
          xlab = "Groups", ylab = "Abundance (%)", main = "Relative abundance by groups in the current sample")
      }
      if(getOption("ZICompSpectra")){
        # Graphs
        PrevDat <- PrevSmp$FIT_Diameter_ABD/1000
        Prevspc <- table(cut(PrevDat, breaks = getOption("breaks")))/length(PrevDat)*100
        CurrentDat <- CurrentSmp$FIT_Diameter_ABD/1000
        Currentspc <- table(cut(CurrentDat, breaks = getOption("breaks")))/length(CurrentDat)*100
        # Compa of size spectra
        par(mfrow = c(2,1))
        barplot(Prevspc, xlab = "size interval", ylab = "Abundance", main = "Previous sample")
        barplot(Currentspc, xlab = "size interval", ylab = "Abundance", main = "Current sample")
      }
      if(getOption("ZICompBiomass")){
        # Graphs
        par(mfrow = c(2,1))
        BioPrev <- Biomass(PrevSmp)
        BioCurrent <- Biomass(CurrentSmp)
        barplot(BioPrev/sum(BioPrev)*100, xlab = "Groups", ylab = "Biomass (%)", main = "Relative biomass in the previous sample")
        barplot(BioCurrent/sum(BioCurrent)*100, xlab = "Groups", ylab = "Biomass (%)", main = "Relative biomass in the current sample")
      }
      if(is.character(getOption("ZICompBio.gp"))){
        BioPrev <- Biomass(PrevSmp)
        BioCurrent <- Biomass(CurrentSmp)
        par(mfrow = c(2,1))
        barplot(BioPrev[names(BioPrev) %in% getOption("ZICompBio.gp")]/sum(BioPrev)*100,
          xlab = "Groups", ylab = "Biomass (%)", main = "Relative biomass by groups in the previous sample")
        barplot(BioCurrent[names(BioCurrent) %in% getOption("ZICompBio.gp")]/sum(BioCurrent)*100,
          xlab = "Groups", ylab = "Biomass (%)", main = "Relative biomass by groups in the current sample")
      }
      if(getOption("ZICompSlope")){
        par(mfrow = c(2,1))
        hist.spectrum(spect = log(as.vector(table(cut(PrevSmp$FIT_Diameter_ABD/1000, breaks = getOption("breaks"))))+1), breaks = getOption("breaks"))
        hist.spectrum(spect = log(as.vector(table(cut(CurrentSmp$FIT_Diameter_ABD/1000, breaks = getOption("breaks"))))+1), breaks = getOption("breaks"))
      }
    }
  }
}

# Function of comparison between current sample and a list of previous FlowCAM digitizations
CompaSampleList <- function() {
  if(unique(getOption("ZIlist") != FALSE)){
  #if(!is.character(getOption("ZIprevSmp"))){ # If we do not compare sample a previous sample
    if(!is.null(getOption("ZIlist"))){ # If we want to compare with a list of samples
      # comparison between more than one sample :
      if(is.character(getOption("ZIprevSmp"))) stop ("You must select some samples in a list and not only one sample") # check if prevSmp is empty
      if(!is.character(getOption("ZIlist"))) stop("the list of list files must be a character string")
      SelectSamples <- lapply(getOption("ZIlist"), FUN = read.lst) # read all list files
      names(SelectSamples) <- gsub(".lst$", "", basename(getOption("ZIlist")))
      # Predictions of selected samples
      for(i in 1:length(names(SelectSamples))){
        SelectSamples[[i]] <- predict.ZIClass.Real.Time(getOption("Classifier"), SelectSamples[[i]], calc.vars = TRUE, class.only = FALSE)
        SelectSamples[[i]] <- Bio.sample(ZIDat = SelectSamples[[i]], conv = getOption("conv"), exportdir = NULL, RealT = TRUE)
        SelectSamples[[i]] <- Bio.tab
      }
      # Calculate table for the sample currently analysed
      if (!exists("rec", env = .GlobalEnv)){
        stop("must have a recognition file in memory")
      } else {
        CurrentSmp <- Bio.sample(ZIDat = rec, conv = getOption("conv"), exportdir = NULL, RealT = TRUE)
        CurrentSmp <- Bio.tab
      }
      # Comparison of abundances
      if (getOption("ZICompAbd")){
        par(mfrow=c(length(SelectSamples) + 1, 1))
        barplot(table(CurrentSmp$Ident) / nrow(CurrentSmp)*100, xlab = "Groups", ylab = "Abundance (%)", main = "Current sample")
        for (i in 1 : length(SelectSamples)) barplot(table(SelectSamples[[i]]$Ident)/nrow(SelectSamples[[i]])*100, xlab = "Groups", ylab = "Abundance (%)", main = names(SelectSamples[i]))
      }
      # Comparison of abundances by groups
      if(is.character(getOption("ZICompAbd.gp"))){
        par(mfrow=c(length(SelectSamples) + 1, 1))
        barplot(table(CurrentSmp$Ident)[names(table(CurrentSmp$Ident)) %in% getOption("ZICompAbd.gp")]/nrow(CurrentSmp)*100,
          xlab = "Groups", ylab = "Abundance (%)", main = "Relative abundance by groups in the current sample")
        for (i in 1 : length(SelectSamples)){
          barplot(table(SelectSamples[[i]]$Ident)[names(table(SelectSamples[[i]]$Ident)) %in% getOption("ZICompAbd.gp")]/nrow(SelectSamples[[i]])*100,
          xlab = "Groups", ylab = "Abundances (%)", main = paste("Relative abundance by groups in", names(SelectSamples[i]), sep = " "))
        }
      }
      # comparison of Spectra
      if(getOption("ZICompSpectra")){
        par(mfrow=c(length(SelectSamples) + 1, 1))
        barplot(table(cut(CurrentSmp$FIT_Diameter_ABD/1000, breaks = getOption("breaks"))),
        xlab = "size interval", ylab = "Abundance", main = "Total size spectrum for the current sample")
        for (i in 1 : length(SelectSamples)){
          barplot(table(cut(SelectSamples[[i]]$FIT_Diameter_ABD/1000, breaks = getOption("breaks"))),
          xlab = "size interval", ylab = "Abundance", main = paste("Total size spectrum of", names(SelectSamples[i]), sep = " "))
        }
      }
      # comparison of biomass
      if(getOption("ZICompBiomass")){
        par(mfrow=c(length(SelectSamples)+1, 1))
        barplot(Biomass(CurrentSmp)/sum(Biomass(CurrentSmp))*100, xlab = "Groups", ylab = "Biomass (%)", main = "Relative biomass in the current sample")
        for (i in 1 : length(SelectSamples)) barplot(Biomass(SelectSamples[[i]])/sum(Biomass(SelectSamples[[i]]))*100, xlab = "Groups", ylab = "Biomass (%)", main = paste("Relative biomass in ",names(SelectSamples[i]), sep = " "))
      }
      # Comparison of biomass by groups
      if(is.character(getOption("ZICompBio.gp"))){
        par(mfrow=c(length(SelectSamples)+1, 1))
        barplot(Biomass(CurrentSmp)[names(Biomass(CurrentSmp)) %in% getOption("ZICompBio.gp")]/sum(Biomass(CurrentSmp)),
          xlab = "Groups", ylab = "Biomass (%)", main = "Relative biomass by groups in the current sample")
        for (i in 1 : length(SelectSamples)){
          barplot(Biomass(SelectSamples[[i]])[names(Biomass(SelectSamples[[i]])) %in% getOption("ZICompBio.gp")]/sum(Biomass(SelectSamples[[i]]))*100,
          xlab = "Groups", ylab = "Biomass (%)", main = paste("Relative biomass by groups in", names(SelectSamples[i]), sep = " "))
          }
      }
      # comparison of slopes
      if(getOption("ZICompSlope")){
        par(mfrow=c(length(SelectSamples)+1, 1))
        hist.spectrum(spect = log(as.vector(table(cut(CurrentSmp$FIT_Diameter_ABD/1000, breaks = getOption("breaks"))))+1), breaks = getOption("breaks"))
        for (i in 1 : length(SelectSamples)) hist.spectrum(spect = log(as.vector(table(cut(SelectSamples[[i]]$FIT_Diameter_ABD/1000, breaks = getOption("breaks"))))+1), breaks = getOption("breaks"))
      }
    }
  }
} # end of multi sample part

# Function which recognizes unknown particles
process <- function() {
  #if (!exists("tab", env = .GlobalEnv)) {
  if (is.null(tab)) {
    # First iteration
    # At the beginning, pos <- 0, TabGroups <- NULL, tab <- NULL, rec <- NULL
    # Code to execute at regular basis
    tab <- read.lst(getOption("Path"), skip = 2)
    # if no measurements in the list file
    if(nrow(tab) == 0){
      warning("The list file is empty")
      tab <<- NULL
      #rm(tab, envir = .GlobalEnv) # add , envir = .GlobalEnv
    } else {
      #return the object in R
      tab <<- tab # extract tab from tcltk to R consol
      # recognition of first tab
      if(is.null(rec)){
        # First iteration
        rec <<- predict.ZIClass.Real.Time(getOption("Classifier"), tab, calc.vars = TRUE, class.only = FALSE)
      } else {
        rec <<- rbind(rec1, rec)
      }
      # Table of groups
      if(is.character(getOption("type"))){
        if(is.null(TabGroups)){
          # First iteration
          TabGroups <<- table(rec$Ident)
        } else {
          TabGroups <<- cbind(TabGroups, table(rec$Ident))
        }
      }
      if(is.numeric(getOption("SizeThreshold"))){
        if(is.null(TabGroupsSize)){
          TabGroupsSize <<- table(rec[rec$FIT_Diameter_ABD < getOption("SizeThreshold"), "Ident"])
        } else {
          TabGroupsSize <<- cbind(TabGroupsSize, table(rec[rec$FIT_Diameter_ABD < getOption("SizeThreshold"), "Ident"]))
        }
      }
    }
  } else {
    # There is one lst (non empty tab) list in memory
    pos <- tab[nrow(tab), 1] # number of rows to skip to get new measurements
    rec1 <- rec # recogntion table from the previous iteration
    # read the complete table to know if new results have been added
    n <- read.lst(getOption("Path"), skip = 2) # read new tab
    # chech if new measurements added in n
    if (pos != n[nrow(n),1]){
      # there is new measurements in the list file
      tab <- read.lst(getOption("Path"), skip = pos + 2)
      #return the object
      tab <<- tab # extract tab from tcltk to R consol
      # recognition of tab
      rec <<- predict.ZIClass.Real.Time(getOption("Classifier"), tab, calc.vars = TRUE, class.only = FALSE)
      if(is.character(getOption("type"))){
        TabGroups <<- cbind(TabGroups, table(rec$Ident))
      }
      if(is.numeric(getOption("SizeThreshold"))){
        TabGroupsSize <<- cbind(TabGroupsSize, table(rec[rec$FIT_Diameter_ABD < getOption("SizeThreshold"), "Ident"]))
      }
      rec <<- rbind(rec1, rec)
    } else {
      # There is no new measurements in list file
      print("There are no new measurements in list file or experiment finished")
      #rm(tab, envir = .GlobalEnv) # add , envir = .GlobalEnv
    }
  }
  # Graphs using 'SampleCurrent' function
}

# Function to plot particles in function of a size threshold
plotLines <-function(){
  if(is.character(getOption("type"))){  # if we want to have the line representation
    if(!is.na(ncol(TabGroups))){ # do not plot at the first iteration
      # Select all groups
      if (getOption("Abd.all")){
        Table <- TabGroups
      }
      # Select only wanted groups
      if (is.character(getOption("Abd.gp"))){
        Table <- TabGroups[rownames(TabGroups) %in% getOption("Abd.gp"), ]
      }
      if(is.numeric(getOption("SizeThreshold"))){
        # plot both graphs
        par(mfrow = c(2,1))
      }
      # Graphical representation
      plot(c(1, ncol(Table), NA, NA), c(NA, NA, min(Table), max(Table)), xlab = "iterations", ylab = "abundance", main = "Total abundance")
      legend(x = 1, y = max(Table), legend = rownames(Table), fill = as.numeric(as.factor(rownames(Table))))
      for(i in 1 : nrow(Table)){
        lines(Table[i,], col = i)
      }
    }
  }
  if(is.numeric(getOption("SizeThreshold"))){
    if(!is.na(ncol(TabGroupsSize))){
      # Select all groups
      if (getOption("Abd.all")){
        Table.Size <- TabGroupsSize
      }
      # Select only wanted groups
      if (is.character(getOption("Abd.gp"))){
        Table.Size <- TabGroupsSize[rownames(TabGroupsSize) %in% getOption("Abd.gp"), ]
      }
      # Graphical representation
      plot(c(1, ncol(Table.Size), NA, NA), c(NA, NA, min(Table.Size), max(Table.Size)),
        xlab = "iterations", ylab = "abundance", main = paste("Total abundance for groups smaller than", getOption("SizeThreshold"), "µm",sep = " "))
      legend(x = 1, y = max(Table.Size), legend = rownames(Table.Size), fill = as.numeric(as.factor(rownames(Table.Size))))
      for(i in 1 : nrow(Table.Size)){
        lines(Table.Size[i,], col = i)
      }
    }
  }
}

# function to create artificial sub samples
Export <- function(){
  if(is.numeric(getOption("MaxCollages"))){
    # List collage in the current directory
    LIST <- list.files(dirname(getOption("Path")), recursive = FALSE, pattern = ".tif$", full.names = TRUE)
    # List calibration image in the current directory
    Calib <- LIST[grep("cal", LIST)]
    if(is.null(Collages)){
      # first exportation
      if(length(LIST[-grep("cal", LIST)]) >= getOption("MaxCollages") + 1){ # only collages
        # Check and create new subdirectory
        New <- paste(dirname(getOption("Path")), paste(basename(dirname(getOption("Path"))), "000001", sep = "_"), sep = "/")
        if(!file.exists(New)){
          # create the directory
          dir.create(New)
        }
        file.copy(c(Calib, LIST[-grep("cal", LIST)][1:getOption("MaxCollages")]), to = paste(New, basename(c(Calib, LIST[-grep("cal", LIST)][1:getOption("MaxCollages")])), sep = "/"), overwrite = FALSE)
        file.copy(getOption("Path"), paste(New, basename(getOption("Path")), sep = "/"))
        file.remove(LIST[-grep("cal", LIST)][1:getOption("MaxCollages")])
        # export rec table
        write.table(rec, file = paste(New, paste(basename(New), "results.txt", sep ="_"), sep = "/"),
          sep = "\t", dec = ".", col.names = TRUE, na = "NA", row.names = FALSE)
        Collages <<- 2 # use it to create new subdirectories
      }
    } else {
      if(length(LIST[-grep("cal", LIST)]) >= getOption("MaxCollages") + 1){ # only collages
        # Search a new calibration image
        # Check and create new subdirectory
        if(Collages < 10){ # 1 to 9
          dirNumber <- paste("00000", Collages, sep ="")
        }
        if(Collages < 100 && Collages > 9){ # 10 to 99
          dirNumber <- paste("0000", Collages, sep ="")
        }
        if(Collages < 1000 && Collages > 99){ # 100 to 999
          dirNumber <- paste("000", Collages, sep ="")
        }
        if(Collages < 10000 && Collages > 999){ # 1000 to 9999
          dirNumber <- paste("00", Collages, sep ="")
        }
        if(Collages < 100000 && Collages > 9999){ # 10000 to 99999
          dirNumber <- paste("0", Collages, sep ="")
        } else {
          dirNumber <- Collages # 100000 to 999999
        }
        New <- paste(dirname(getOption("Path")), paste(basename(dirname(getOption("Path"))), dirNumber, sep = "_"), sep = "/")
        if(!file.exists(New)){
          # create the directory
          dir.create(New)
        }
        if(length(Calib) >= 2){
          # There is a new calibration image in the directory
          file.copy(c(Calib, LIST[-grep("cal", LIST)][1:getOption("MaxCollages")]), to = paste(New, basename(c(Calib, LIST[-grep("cal", LIST)][1:getOption("MaxCollages")])), sep = "/"), overwrite = FALSE)
          file.copy(getOption("Path"), paste(New, basename(getOption("Path")), sep = "/"))
          file.remove(c(Calib[1],LIST[-grep("cal", LIST)][1:getOption("MaxCollages")]))
          Collages <<- Collages + 1
        } else {
          file.copy(c(Calib, LIST[-grep("cal", LIST)][1:getOption("MaxCollages")]), to = paste(New, basename(c(Calib, LIST[-grep("cal", LIST)][1:getOption("MaxCollages")])), sep = "/"), overwrite = FALSE)
          file.copy(getOption("Path"), paste(New, basename(getOption("Path")), sep = "/"))
          file.remove(LIST[-grep("cal", LIST)][1:getOption("MaxCollages")])
          Collages <<- Collages + 1
        }
        # export rec table
        write.table(rec, file = paste(New, paste(basename(New), "results.txt", sep ="_"), sep = "/"),
          sep = "\t", dec = ".", col.names = TRUE, na = "NA", row.names = FALSE)
      }
    }
  }
}

# Function to select groups to keep for the comparison
SelectGroups <- function(ZIC){
  select.list(levels(attr(ZIC, "classes")), multiple = TRUE, title = "Select taxa you want to plot")
}

# Function to select samples to compare with the current one
SmpToComp <- function(Prev = NULL, Samples = NULL, Table = NULL){
  # Only one previous list file
  if(!is.null(Prev)){
    if(!is.character(Prev)) stop("You must provide the path of the list file to compare")
    Smp <- Prev
  }
  # More than one previous list file
  if(!is.null(Samples)){
    if(!is.character(Samples)) stop("You must provide the path of a directory containing list files to compare")
    Names <- gsub(".lst$", "", basename(Samples)) # name of the list
    samples <- select.list(Names, multiple = TRUE, title = "Select samples to compare") # selection of samples to compare
    Smp <- Samples[Names %in% samples]
  }
  # With a table of recognition
  if(!is.null(Table)){
    print("not implemented yet")
    Smp <- NULL
  }
  return(Smp)
}

# save results in the directory you choose and delete files created with loop.opts function
save.loop.res <- function(lst, Classif, breaks = seq(0.25, 2, by = 0.1), conv = c(1, 0, 1), save.dir = NULL){
  if (!exists("rec", env = .GlobalEnv)){
    rec <- predict.ZIClass.Real.Time(Classif, read.lst(lst), calc.vars = TRUE, class.only = FALSE)
  }
  if(!is.null(save.dir)){
    if(!is.character(save.dir)) stop("The exportation path must be a character string")
  } else {
    save.dir <- choose.dir()
  }
  Bio.sample(ZIDat = rec, conv = conv, exportdir = NULL, RealT = TRUE)
  write.table(Bio.tab, file = paste(save.dir, paste(basename(dirname(lst)), "AbdBio.txt", sep = "_"), sep = "\\"), sep = "\t", dec = ".", col.names = TRUE, na = "NA", row.names = FALSE)
  # delete objects from R environment
  if(exists("tab", env = .GlobalEnv)) rm(tab, envir = .GlobalEnv)
  if(exists("rec", env = .GlobalEnv)) rm(rec, envir = .GlobalEnv)
  if(exists("Bio.tab", env = .GlobalEnv)) rm(Bio.tab, envir = .GlobalEnv)
}

# tcl function to control the loop
tclFun_<- function(f, name = deparse(substitute(f))) {
  # Register a simple R function (without arguments) as a callback in Tcl,
  # and give it the same name)
  # Indeed, .Tcl.callback(f) in tcltk package does the job... but it gives
  # cryptic names like R_call 0x13c7168
  require(tcltk) || stop("Package 'tcltk' is needed!")
  # Check that 'f' is a function with no arguments (cannot handle them, currently)
  is.function(f) || stop("'f' must be a function!")
  is.null(formals(f)) || stop("The function used cannot (yet) have arguments!")
  # Make sure the name of the function is valid
  if (!is.character(name)) stop("'name' must be a character string!") else
  name <- make.names(name[1])
  res <- .Tcl.callback(f)
  # Make sure this is correct (R_call XXXXXXXX)
  if (length(grep("R_call ", res) > 0)) # Create a proc with the same name in Tcl
  .Tcl(paste("proc ", name, " {} {", res, "}", sep = ""))
  # Return the R_call XXXXXXXX string, as .Tcl.callback() does
  return(res)
  # Rem: if you delete the R 'f' function, the Tcl 'f' function still works (?!)
}

# loop to run process and compa
loopAsynch <- function() {
  # Code to execute
  continue <- TRUE
  # External function to execute at regular interval
  process()
  SampleCurrent()
  plotLines()
  Export()
  CompaSamplePrev()
  CompaSampleList()
  # Reexecute the function at regular interval
  if (exists("...stop", env = .GlobalEnv)) {
    # Stop loop
    rm(list = "...stop", envir = .GlobalEnv)
    continue <- FALSE
  }
  if (!continue) {
    timer <- NULL
  } else {
    # (Re)install the timer
    timer <- .Tcl("after 15000 loopAsynch") # run function after 'number' ms
  }
  return(invisible(timer))
}
