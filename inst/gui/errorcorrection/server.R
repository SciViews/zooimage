## Zoo/PhytoImage simplified analysis UI (server code)
## Copyright (c) 2004-2015, Ph. Grosjean <phgrosjean@sciviews.org>
## & Guillaume Wacquet <guillaume.wacquet@umons.ac.be>
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
## TODO: add "Stat" button for fully validated samples 
## TODO: translate server messages (English and French interfaces)
## TODO: allow downloading the data with something like:
## In server.R:
#output$downloadData <- downloadHandler(
#  filename = function() {
#    paste('data-', Sys.Date(), '.csv', sep='')
#  },
#  content = function(file) {
#    write.csv(data, file)
#  }
#)
#
## In ui.R:
#downloadLink('downloadData', 'Download')
## 
## - Use includeMarkdown()
##
## - Use renderDataTable(), e.g.,
## Pass a callback function to DataTables using I()
#renderDataTable(iris, options = list(
#  iDisplayLength = 5,
#  fnInitComplete = I("function(oSettings, json) {alert('Done.');}")
#))

shinyServer(function (input, output, session) {
    
    doAnalysis <- reactive({
        generalMessage <- function(message) {
            paste0("______________________________________________________________________",
                #"\nÉchantillons totaux:    ", length(AllSamples$names),
                "\nÉchantillons à traiter: ", sum(!AllSamples$analyzed),
                "\nÉchantillons analysés:  ", sum(AllSamples$analyzed),
                "\n\n", message, "\n",
                "______________________________________________________________________\n")
        }
        
        if (input$goButton == 0)
            return(generalMessage("(Aucun échantillon n'a encore été analysé au cours de cette session)."))
        isolate({
            Sample <- substring(input$sample, 5)
            ZIDB <- file.path(inidir, paste(Sample, "zidb", sep = "."))
            ## Determine if we already got some data...
            ## First look at "demo" data _valid0.RData
            DemoFile <- file.path(inidir, "_analyses", .ZI$method, #input$method,
                paste(Sample, "valid0.RData", sep = "_"))
            SampleFile <- file.path(inidir, "_analyses", .ZI$method, #input$method,
                paste(Sample, "valid.RData", sep = "_"))
            MetaFile <- file.path(inidir, "_analyses", .ZI$method, #input$method,
                paste(Sample, "valid.txt", sep = "_"))
            ResFile <- file.path(inidir, "_analyses", .ZI$method, #input$method,
                paste(Sample, "res.RData", sep = "_"))
            
            ValidData <- paste(Sample, "valid", sep = "_")
            ResData <- paste(Sample, "res", sep = "_")
            if (exists(ValidData, inherits = FALSE)) rm(list = ValidData)
#             CtxSmp <- contextSelection()
#             if (length(CtxSmp) < 1) {
#                 warning("No contextual samples selected! Initial training set will be used.")
#             } else {
#                 ## TODO: merge with activeLearningGUI
#                 .ZITrain <- addItemsToTrain(.ZITrain, CtxSmp,
#                     dropItemsToTrain = dropItemsToTrain)
#             }
            
            # PhG: This is problematic with scanner data, so, inactivate it for now
            #.ZITrain <- activeLearning(.ZITrain)
            assign(.ZI$classif, eval(parse(text = .ZI$classifcmd)))
            .ZIClass <- get(.ZI$classif)
            if (file.exists(DemoFile)) { # Run in demo mode
                res <- load(DemoFile)
                DemoData <- get(res)
                rm(list = res)
                ce <- correctError(zidb = ZIDB, classifier = .ZIClass,
                    data = DemoData, mode = "demo")
                ## Note: we save just nothing, because we are in demo mode?
                ## or do we save data?
            } else {
                ## Are there some data already available?
                if (file.exists(SampleFile)) { # Reanalyze the sample
                    res <- load(SampleFile)
                    SampleData <- get(res)
                    rm(list = res)
                    ce <- correctError(zidb = ZIDB, classifier = .ZIClass,
                        data = SampleData)
                } else { # Nothing available: start from scratch
                    ce <- correctError(zidb = ZIDB, classifier = .ZIClass)
                }
                
            } #x <- "Demo found" else x <- "Demo not found"
            
            
            ## Backup sample and metadata files if they exist
            if (file.exists(SampleFile))
                file.copy(SampleFile, paste(SampleFile, "bak", sep = "."))
            unlink(SampleFile)
            if (file.exists(MetaFile))
                file.copy(MetaFile, paste(MetaFile, "bak", sep = "."))
            unlink(MetaFile)
            if (file.exists(ResFile))
                file.copy(ResFile, paste(ResFile, "bak", sep = "."))
            unlink(ResFile)
            
            ## The following code fails while we are still validating items...
            ## TODO: associate name of validator + date
            res <- try(save(list = ValidData, file = SampleFile), silent = TRUE)
            while (inherits(res, "try-error")) {
                Sys.sleep(0.5) # Wait 1/2 sec
                res <- try(save(list = ValidData, file = SampleFile),
                    silent = TRUE)
            }
            ## Save associated metadata
            cat("zooimage version: 5.4-3\n", file = MetaFile)
            cat("method: ", .ZI$method, "\n", sep = "",
                file = MetaFile, append = TRUE)
            cat("user: ", .ZI$user, "\n", sep = "",
                file = MetaFile, append = TRUE)
            cat("date: ", as.character(Sys.time()), "\n", sep = "",
                file = MetaFile, append = TRUE)
            cat("training set: ", .ZI$train, "\n", sep = "",
                file = MetaFile, append = TRUE)
            if("AddedItems" %in% names(.ZITrain)) {
                cat("contextual samples: ",
                    as.character(unique(.ZITrain$Label[.ZITrain$AddedItems == TRUE])),
                    sep = "\n", file = MetaFile, append = TRUE)
            }
            ## should be../ more
            #cat("training file: ", .ZI$trainfile, "\n", sep = "",
            #    file = MetaFile, append = TRUE)
            cat("classifier: ", .ZI$classif, "\n", sep = "",
                file = MetaFile, append = TRUE)
            ## should be../ more
            #cat("classifier file: ", .ZI$classifile, "\n", sep = "",
            #    file = MetaFile, append = TRUE)
            cat("classifier cmd: ", .ZI$classifcmd, "\n", sep = "",
                file = MetaFile, append = TRUE)
            cat("size breaks: ", paste(.ZI$breaks, collapse = "-"), "\n", sep = "",
                file = MetaFile, append = TRUE)
            cat("biovolume conversion: \n", sep = "",
                file = MetaFile, append = TRUE)
            write.table(.ZI$biovolume, sep = "\t", dec = ".", row.names = FALSE,
                col.names = TRUE, file = MetaFile, append = TRUE)
                        
            ## Calculate results for this sample
            dat2 <- get(ValidData)
            cl <- levels(dat2$Class) # All classes
            ## We used first uppercase for classes of interest, thus:
            cl <- cl[grepl("^[A-Z]", cl)]
            ## Now, we also want to calculate separate abundances for most abundant classes
            ## i.e., those with at least 50 individuals measured
            detail <- cl[cl %in% levels(dat2$Class)[table(dat2$Class) >= 50]]
            ## Calculate results for this sample
            ## TODO: correct the bug with keep = cl => replacement has different number of rows
            #assign(ResData, processSample(dat2, keep = cl, detail = detail,
            #    biomass = .ZI$biovolume, breaks = .ZI$breaks, classes = "Class"))
            #assign(ResData, processSample(dat2, keep = NULL, detail = detail,
            #    biomass = .ZI$biovolume, breaks = .ZI$breaks, classes = "Class"))
            ## With cellModels...
            assign(ResData, processSample(dat2, keep = NULL, detail = detail, cells = .ZI$cellModelsfile,
                biomass = .ZI$biovolume, breaks = .ZI$breaks, classes = "Class"))
            ## Save it
            save(list = ResData, file = ResFile)

            ## Report success            
            x <- paste("(L'échantillon", Sample, "vient d'être analysé).")
            
            Method <- .ZI$method #input$method
            AllSamples <- listSamples(inidir, method = Method)
            
            if (file.exists(file.path(inidir, "_analyses", Method,
                paste(Sample, "valid.RData", sep = "_")))) {
                tag <- "[A]"
            } else tag <- "[I]"
                    
            updateSelectInput(session, "sample", choices = AllSamples$names,
                selected = paste(tag, Sample))
            
            return(generalMessage(x))
        })
    })

    #output$generalSummary <- renderText({
    #  if (input$stopButton) { # Manage clean closing of the page
    #    ## Réactiver R
    #    ## TODO: change this code to get the name of R application under Mac OS X
    #    GUI <- .Platform$GUI
    #    if (GUI == "Rgui") { # Code for RGui under Windows
    #        try(bringToTop(-1), silent = TRUE)
    #    } else if (GUI == "AQUA") { # Code for R/R64/SciViews R64.app
    #        ## This works from Mac OS X 10.5 Leopard:
    #        try(system("osascript -e 'tell application id \"Rgui\" to activate'",
    #            ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
    #        #try(system("osascript -e 'tell application \"R\" to activate'",
    #        #    ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
    #        #try(system("osascript -e 'tell application \"R64\" to activate'",
    #        #    ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
    #        #try(system("osascript -e 'tell application \"SciViews R64\" to activate'",
    #        #    ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
    #    } else if (grepl("^mac", .Platform$pkgType)) { # Try code for Terminal.app
    #        try(system("osascript -e 'tell application \"Terminal\" to activate'",
    #            ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
    #    }
    #    
    #    ## Stop the application, returning a short report of what was done
    #    report <- structure("Content of my report here...", class = "reportObj")
    #    stopApp(report)
    #    
    #    ## Indicate the app is disconnected
    #    paste(strong(em("Application déconnectée!")))
    #  
    #  } else { # Indicate number of samples to process and number analyzed
    #    ## TODO: make this reactive to the change to the list of samples
    #    paste(em("A traiter:"), strong(em(sum(!AllSamples$analyzed))),
    #      em(" -  analysés:"), strong(em(sum(AllSamples$analyzed))))
    #  }
    #})
    
    output$sampleSummary <- renderPrint(width = 80, {
      if (input$stopButton) {
        #updateTabsetPanel(session, "mainTabset", selected = "Résumé")
      } else {
            ## Also update the list of samples, depending on both method and newonlyCheck
         #   AllSamples <- listSamples(inidir, method = .ZI$method, input$newonlyCheck)
         #   updateSelectInput(session, "sample", choices = AllSamples$names)
            Sample <- substring(input$sample, 5)
            calcSample(Sample, input, output, session)
            ## Link to the .zidb file and provide a summary of this sample
            cat("===", Sample, "===\n")
            ZIDB <- file.path(inidir, paste(Sample, "zidb", sep = "."))
            Dat <- zidbDatRead(ZIDB)
            cat("Échantillon contenant", nrow(Dat), "particules numérisées.\n")
            if (substr(input$sample, 1, 3) == "[A]") {
                ## Get analysis statistics about this sample
                #if (!exists("SampleData")) {
                    ## Download the data!
                    SampleFile <- file.path(inidir, "_analyses", .ZI$method, #input$method,
                        paste(Sample, "valid.RData", sep = "_"))
                    if (file.exists(SampleFile)) {
                        res <- load(SampleFile)
                        SampleData <- get(res)
                        rm(list = res)
                    }
                #}
                res <- try(print(table(SampleData$Class)), silent = TRUE)
                if (inherits(res, "try-error"))
                    cat("\nStatistiques d'analyse pour l'échantillon non disponibles\n")
            } else cat("\nCet échantillon n'est pas encore analysé avec la méthode '", .ZI$method, "'.", sep = "")
            #head(Dat)
            #print(summary(Dat[, c("ECD")]))
            #print(attr(Dat, "metadata"))
            #plot(Dat$Area, Dat$Perim.)
            #cat("Ici, le résumé de", Sample)
            cat("\n", doAnalysis())
        }
    })
    
    output$sampleTable <- renderDataTable(options = list(pageLength = 50), {  #renderTable({
        if (input$stopButton) {
            updateTabsetPanel(session, "mainTabset", selected = "Résumé")
        } else {
            doAnalysis()
            Sample <- substring(input$sample, 5)
            calcSample(Sample, input, output, session)
            ## Link to the .zidb file and provide a summary of this sample
            #cat("===", Sample, "===\n")
            ZIDB <- file.path(inidir, paste(Sample, "zidb", sep = "."))
            
            ## Depending if the file is analyzed or not, we look at the
            ## ZITest or ZIDat object
            if (substr(input$sample, 1, 3) == "[A]") {
                ## Get analysis statistics about this sample
                #if (!exists("SampleData")) {
                    ## Download the data!
                    SampleFile <- file.path(inidir, "_analyses", .ZI$method, #input$method,
                        paste(Sample, "valid.RData", sep = "_"))
                    if (file.exists(SampleFile)) {
                        res <- load(SampleFile)
                        SampleData <- get(res)
                        rm(list = res)
                    }
                #}
                res <- try(Dat50 <- head(SampleData, n = 50), silent = TRUE)
                if (inherits(res, "try-error")) {
                    Dat <- zidbDatRead(ZIDB)
                    Dat50 <- head(Dat, n = 50)
                    Dat50b <- Dat50
                    Dat50b$Label <- NULL
                    Dat50b$Item <- NULL
                    Dat50b$ECD <- NULL
                    data.frame(Label = Dat50$Label, Item = Dat50$Item,
                        ECD = Dat50$ECD, Dat50b)
                } else {
                    Dat50b <- Dat50
                    Dat50b$Label <- NULL
                    Dat50b$Item <- NULL
                    Dat50b$ECD <- NULL
                    Dat50b$Class <- NULL
                    Dat50b$Predicted <- NULL
                    Dat50b$Id <- NULL
                    Dat50b$Id.1 <- NULL
                    data.frame(Label = Dat50$Label, Item = Dat50$Item,
                        ECD = Dat50$ECD, Class = Dat50$Class, Dat50b)
                        #Dat50$Predicted,Dat50$ECD, Dat50b)
                }
            } else {
                Dat <- zidbDatRead(ZIDB)
                Dat50 <- head(Dat, n = 50)
                Dat50b <- Dat50
                Dat50b$Label <- NULL
                Dat50b$Item <- NULL
                Dat50b$ECD <- NULL
                data.frame(Label = Dat50$Label, Item = Dat50$Item,
                    ECD = Dat50$ECD, Dat50b)
            }
        }
    })
    
    output$samplePlot <- renderPlot({
        if (input$stopButton) {
            updateTabsetPanel(session, "mainTabset", selected = "Résumé")
        } else {
            
            ## This is only in shiny 0.10.2!!
            #withProgress(message = 'Calculation in progress',
            #    detail = '...', value = 0, {
            #    for (i in 1:15) {
            #        incProgress(1/15, detail = paste0("...", i, "/15"))
            #        Sys.sleep(0.25)
            #    }
            #})
            
            Sample <- substring(input$sample, 5)
            calcSample(Sample, input, output, session)
            ## Link to the .zidb file and provide a summary of this sample
            #cat("===", Sample, "===\n")
            ZIDB <- file.path(inidir, paste(Sample, "zidb", sep = "."))
            Dat <- zidbDatRead(ZIDB)
            hist(Dat$ECD, col = "cornsilk", breaks = "FD",
                main = "Distribution de la taille des particules",
                xlab = "ECD", ylab = "Fréquences")
        }
    })
    
    output$vignettesPlot <- renderPlot({
        if (input$stopButton) {
            updateTabsetPanel(session, "mainTabset", selected = "Résumé")
        } else {
            Sample <- substring(input$sample, 5)
            calcSample(Sample, input, output, session)
            ## Link to the .zidb file and provide a summary of this sample
            #cat("===", Sample, "===\n")
            ZIDB <- file.path(inidir, paste(Sample, "zidb", sep = "."))
            DB <- zidbLink(ZIDB)
            Items <- ls(DB) # Contains data in *_dat1 and vignettes in *_nn
            ## Eliminate items that are not vignettes
            noVig <- grep("_dat1", Items)
            if (length(noVig)) Vigs <- Items[-noVig] else Vigs <- Items
            ## Display a 5*5 thumbnail of the first 25 vignettes
            zidbPlotNew(Sample)
            ImgType <- DB$.ImageType
            for (i in 1:30)
                zidbDrawVignette(DB[[Vigs[i]]], type = ImgType, item = i,
                    nx = 6, ny = 5)
        }
    })
    
    output$sampleResults <- renderPrint({
        if (input$stopButton) {
            updateTabsetPanel(session, "mainTabset", selected = "Résumé")
        } else {
            ## Also update the list of samples, depending on both method and newonlyCheck
         #   AllSamples <- listSamples(inidir, method = .ZI$method, input$newonlyCheck)
         #   updateSelectInput(session, "sample", choices = AllSamples$names)
            Sample <- substring(input$sample, 5)
            calcSample(Sample, input, output, session)
            ## Link to the .zidb file and provide a summary of this sample
            cat("===", Sample, "===\n")
            ZIDB <- file.path(inidir, paste(Sample, "zidb", sep = "."))
            Dat <- zidbDatRead(ZIDB)
            cat("Échantillon contenant", nrow(Dat), "particules numérisées.\n")
            if (substr(input$sample, 1, 3) == "[A]") {
                ## Get analysis statistics about this sample
                #if (!exists("SampleData")) {
                    ## Download the data!
                    SampleFile <- file.path(inidir, "_analyses", .ZI$method, #input$method,
                        paste(Sample, "valid.RData", sep = "_"))
                    if (file.exists(SampleFile)) {
                        res <- load(SampleFile)
                        SampleData <- get(res)
                        rm(list = res)
                    }
                #}
                 
                # Show results for this sample...
                ResFile <- file.path(inidir, "_analyses", .ZI$method, #input$method,
                    paste(Sample, "res.RData", sep = "_"))
                if (file.exists(ResFile)) {
                    res <- load(ResFile)
                    ResData <- get(res)
                    rm(list = res)
                    ## Print results
                    print(ResData)
                } else {
                    cat("Aucuns résultats trouvés pour cet échantillon!\n")
                }                
            } else cat("\nCet échantillon n'est pas encore analysé avec la méthode '", .ZI$method, "'.", sep = "")
            #head(Dat)
            #print(summary(Dat[, c("ECD")]))
            #print(attr(Dat, "metadata"))
            #plot(Dat$Area, Dat$Perim.)
            #cat("Ici, le résumé de", Sample)
            cat("\n", doAnalysis())
        }
    })
})
