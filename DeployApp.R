# Feb 13, 2019
# Created by B. MacDonald, DFO
# Objective: This code launches Version 1 of Synoptic Status Evaluation Tool being developed by DFO's State of the Salmon Program. The shiny
#            app will be used to evaluate Wild Salmon Policy metrics for Pacific salmon populations in the BC/Yukon region.  
# This code is under development




rm(list=ls())


#devtools::install_github("jerryzhujian9/ezR")
#source("jerry zhu radar code_filled.R")


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
                      "markdown")                      

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

if(!"parcoords" %in% installed.packages()[,"Package"]) devtools::install_github("timelyportfolio/parcoords")
#if(!"ezR" %in% installed.packages()[,"Package"]) devtools::install_github("jerryzhujian9/ezR")
library("parcoords")
library("shinydashboard")
#library("ezR")

data.start <- readxl::read_excel("data/FR SK metrics.xls")



# -==== end Spinner code


# ====================== Helper Functions ==============================





# Run the application 
#app <- shinyApp(ui = ui, server = server)

#runApp()
rsconnect::deployApp(appTitle="SOS SST V2.2 git")

rsconnect::showLogs()

#library(rsconnect)
#rsconnect::deployApp('C:/Users/macdonaldbro/OneDrive/State of Salmon Shared/Data/Shiny Code/Deployed')