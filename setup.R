
# explanatory information for the different metrics
metricInfo <<- list(
  Base.Unit.CU.ShortName = "Conservation Unit",
  Base.Unit.Species = "Species Code (Sk = Sockeye, Ck = Chinook, ...)",
  FAZ = "Freshwater Adaptive Zone",
  BaseUnit.Watershed = "Watershed",
  Recent.Total = "This is an explanation of the Recent.Total metric",
  Lower.Ratio = "This is an explanation of the Lower.Ratio metric",
  Upper.Ratio = "This is an explanation of the Upper.Ratio metric",
  LongTerm.Ratio = "This is an explanation of the LongTerm.Ratio metric",
  LongTerm.Trend = "This is an explanation of the LongTerm.Trend metric",
  WSP.status = "This is an explanation of the WSP.status metric",
  Recent.ER = "This is an explanation of the Recent.ER metric",
  Management.Timing = "Management timing")

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

#if(!"parcoords" %in% installed.packages()[,"Package"]) devtools::install_github("timelyportfolio/parcoords")
devtools::install_github("brigitte-dorner/parcoords")
#if(!"ezR" %in% installed.packages()[,"Package"]) devtools::install_github("jerryzhujian9/ezR")
library(parcoords)
library(shinydashboard)
library(forcats)
#library("ezR")

#data.start <- readxl::read_excel("data/FR SK metrics.xls")
data.start <<- read.csv("data/FR SK metrics.csv")
