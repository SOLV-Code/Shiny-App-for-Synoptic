# Server funtion for the State of the Salmon Synoptic Status Evaluation Tool
# Developed by B. Dorner and B. MacDonald
# Mach, 2020

#------------------------ Setup -----------------------

list.of.packages <- c("shiny",
                      "shinydashboard",
                      "tibble",
                      "devtools",
                      "ggplot2",
                      "dplyr",
                      "GGally",
                      "DT",
                      "htmltools",
                      "shinyWidgets",
                      "plotly",
                      "forcats",
                      "rsconnect",
                      "brew",
                      "ini",
                      "xfun",
                      "readxl",
                      "markdown",
                      "parcoordsSoS",
                      "crosstalk",
                      "sp",
                      "leaflet",
                      "leaflet.extras",
                      "shinyBS",
                      "shinycssloaders",
                      "sparkline")
# 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
# NOTE: to make this work with shinyapps.io, you MUST install parcoordsSoS from the github repo, i.e.
# by using install_github("brigitte-dorner/parcoordsSoS") before uploading.
# Downloading the code and building parcoordsSoS locally will let you run the app
# locally, but the install on shinyapps.io will fail unless all required packages
# are installed from CRAN or github.

# helper functions
source('helpers.R')

# ==========Define server components ================

# Define server logic 
function(input, output, session){
  source('DataFilters.R', local=TRUE)
  source('SharedComponents.R', local=TRUE)
  source('Map.R', local=TRUE)
  source('ParCoords.R', local=TRUE)
  source('DataTable.R', local=TRUE)
  source('HistogramSummary.R', local=TRUE)
} # end server function
#   


