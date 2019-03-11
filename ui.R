# Trial for Parallel Plot brushing to produce radar plots and table
# Written by B. MacDonald
# Sept 21 2018
# parallel coorordinates code is from the parcoords package accessed from timelyportfolio github
# uses ezR package for rescale
# coord_radar function from ezR package
# requires dataframes: data.new, data.row

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


# ====================== Define UI Components =========================

library(shinydashboard)
library(shinyWidgets)
library(markdown)


sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    #   id = "tabs",
    menuItem("DISCLAIMER", tabName="DISCLAIM"),
    menuItem("CU Status Summary", tabName="CUSelection"),
    menuItem("View Full Data", tabName="AllData"),
    br(),br(), br(),br(), br(),br(),
    br(),br(), br(),br(), br(),br(),
    br(),br(), br(),br(), br(),br(),
    hr(),# Two line breaks for visual separation
    h5("Built with",
       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
       "by",
       img(src="https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height="30px"),
       ".")
  )
)

body <- shinydashboard::dashboardBody(
  tabItems(
    tabItem(
      tabName = "DISCLAIM",
      h2("DISCLAIMER"),
      fluidRow(
        column(width=8,
               includeMarkdown("Markdown/about.md")
        )
      )
    ),
    tabItem(
      tabName = "AllData",
      h2("Data"),
      tags$div('style' = "text-align:right;", downloadButton("allData_Download", "Download")),
      tags$div(style = 'overflow-x: scroll',  DT::dataTableOutput("allData_Table", width="70%"))
    ),
      
    tabItem(
      tabName = "CUSelection",
      box(title = "Start here", width=12, status="info", solidHeader=TRUE, collapsible=TRUE, collapsed=FALSE,
          uiOutput("box_DataFilters")),
      
      box(title = "View/select CUs on a map", width=12, status="info", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE,
          uiOutput("box_LeafletMap")),
      
      box(title = "View/select CUs by performance metric (parallel coordinates plot)", width=12, status="info", solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE,
          uiOutput("box_Parcoords")),

      box(title = "View/select CUs on a data table", width=12, status="info", solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE,
          div(style = 'overflow-x: scroll', uiOutput("box_Data"))),
      
      box(title = "Summary report", width=12, status="info", solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE,
          uiOutput("box_HistoSummary")),
      
      box(title = "Radar plots", width=12, status="info", solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE,
          uiOutput("box_RadarPlots"))
    )
  )
)

# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(title="Working"),
  #  dashboardSidebar(disable=F),
  sidebar,
  body
)



