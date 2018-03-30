## Zoo/PhytoImage simplified analysis UI (UI definition)
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

## TODO: add "Stat" button for fully validated samples

#library(shinysky)

shinyUI(fluidPage(title = uiTitle,

    smallHeaderPanel(uiTitle),

    fluidRow(
        sidebarPanel(
            ##TODO: put this in header! helpText("Session name + link to inidir"),
            selectInput("sample", "Sampling/samples:", AllSamples$names,
                width = "100%"),
            #selectInput("method", "Méthode:", Methods, width = "100%"),
            #checkboxInput("newonlyCheck", "Seulement les échantillon non analysés"),
            #actionButton("importButton", "Reimporter"),
            actionButton("goButton", "(Re)analyser"),
            actionButton("stopButton", "Return to R")#,
#            hr(),
#            htmlOutput("generalSummary")#,
            #busyIndicator(text = "Analyse de XXX en cours..",
            #    wait = 1000)
        ),

        mainPanel(
            tabsetPanel(id = "mainTabset",
                tabPanel("Summary", icon = icon("list-alt"),
                    verbatimTextOutput("sampleSummary")),
                tabPanel("Table", icon = icon("table"),
                    dataTableOutput("sampleTable")),
                tabPanel("Vignettes", icon = icon("calendar"),
                    plotOutput("vignettesPlot")),
                tabPanel("Plot", icon = icon("bar-chart-o"),
                    plotOutput("samplePlot")),
                tabPanel("Results", icon = icon("refresh"),
                    verbatimTextOutput("sampleResults"))
            )
        )
    )
))
