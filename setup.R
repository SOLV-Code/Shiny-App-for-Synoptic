


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
