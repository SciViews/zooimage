## Zoo/PhytoImage simplified analysis UI (UI definition)
## Copyright (c) 2014, Philippe Grosjean (Philippe.Grosjean@umons.ac.be)
## TODO: add "Stat" button for fully validated samples

#library(shinysky)

shinyUI(fluidPage(title = uiTitle,  
    
    smallHeaderPanel(uiTitle),
  
    fluidRow(
        sidebarPanel(
            ##TODO: put this in header! helpText("Session name + link to inidir"),
            selectInput("sample", "Prélèvement/échantillon:", AllSamples$names,
                width = "100%"),
            #selectInput("method", "Méthode:", Methods, width = "100%"),
            #checkboxInput("newonlyCheck", "Seulement les échantillon non analysés"),
            #actionButton("importButton", "Reimporter"),
            actionButton("goButton", "(Re)analyser"),
            actionButton("stopButton", "Retourner à R")#,
#            hr(),
#            htmlOutput("generalSummary")#,
            #busyIndicator(text = "Analyse de XXX en cours..",
            #    wait = 1000)
        ),
        
        mainPanel(
            tabsetPanel(id = "mainTabset",
                tabPanel("Résumé", icon = icon("list-alt"),
                    verbatimTextOutput("sampleSummary")),
                tabPanel("Tableau", icon = icon("table"),
                    dataTableOutput("sampleTable")),  #tableOutput("sampleTable")),
                tabPanel("Vignettes", icon = icon("calendar"),
                    plotOutput("vignettesPlot")),
                tabPanel("Graphique", icon = icon("bar-chart-o"), 
                    plotOutput("samplePlot")),
                tabPanel("Résultats", icon = icon("refresh"),
                    verbatimTextOutput("sampleResults"))
            )
        )
    )
))
