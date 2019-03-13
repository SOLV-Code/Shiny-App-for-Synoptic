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
library(shinyBS)


sidebar <- shinydashboard::dashboardSidebar(
  
  shinydashboard::sidebarMenu(
    id = "tabs",
    menuItem("DISCLAIMER", tabName="DISCLAIM"),
    menuItem("CU Status Summary", tabName="CUSelection"),
    menuItem("View Full Data", tabName="AllData"),
    
    conditionalPanel("input.tabs == 'CUSelection'",
                     tags$hr(),
                     actionButton("sidebarMenu_clearSelection", label = "Clear Selection", style=ButtonStyle)),
    
    tags$div(
      `style` = "position: absolute; bottom: 0;",
      hr(),
      h5("Built with",
        img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
        "by",
        img(src="https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height="30px"),
        ".")
    )
  )
)

body <- shinydashboard::dashboardBody(`style` = "min-height: 400px",
  shinyjs::useShinyjs(),
  tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),

tags$head(tags$style(
  HTML('.skin-blue {min-height: 400px !important;}')
)),
tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
                            background-color: #3c8dbc;
                            }
                            .skin-blue .main-header .logo:hover {
                            background-color: #3c8dbc;
                            }
                            '))),
tags$head(tags$style(
  HTML('.content-wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
)),
tags$head(tags$style(
  HTML('.content-wrapper {color: #000000 !important;}')
)),
tags$head(tags$style(
  HTML('.tooltip-inner {width: 400px !important;}')
)),

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
      box(title = "Start here", width=12, solidHeader=TRUE, collapsible=TRUE, collapsed=FALSE, status=BoxHeaderStatus,
          uiOutput("box_DataFilters")),
      
      box(title = "Select CUs by attributes and/or metric values", width=12, solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status=BoxHeaderStatus,
          uiOutput("box_DataSelectors")),
      
      box(title = "View/select CUs on a map", width=12, solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status=BoxHeaderStatus,
          uiOutput("box_LeafletMap")),
      
      box(title = "View/select CUs by performance metric (parallel coordinates plot)", width=12, solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE, status=BoxHeaderStatus,
          uiOutput("box_Parcoords")),

      box(title = "View/select CUs on a data table", width=12, solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE, status=BoxHeaderStatus,
          div(style = 'overflow-x: scroll', uiOutput("box_SelectedDataTable"))),
      
      box(title = "Summary report", width=12, solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE, status=BoxHeaderStatus,
          uiOutput("box_HistoSummary")),
      
      box(title = "Radar plots", width=12, solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE, status=BoxHeaderStatus,
          uiOutput("box_RadarPlots")),
      
       
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100),
      uiOutput("rendered"),
      uiOutput("tooltips"),
      plotOutput("distPlot"),
      uiOutput("FilterMFTooltips")
      
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



