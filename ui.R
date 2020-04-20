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
library(shinydashboardPlus)
library(shinyWidgets)
library(markdown)
library(shinyBS)


sidebar <- shinydashboard::dashboardSidebar(
  
  shinydashboard::sidebarMenu(
    id = "tabs",
    menuItem("DISCLAIMER", tabName="DISCLAIM"),
    menuItem("CU Status Summary", tabName="CUSelection"),
    #menuItem("View Full Data", tabName="AllData"),
    
    conditionalPanel("input.tabs == 'CUSelection'",
                     tags$hr(),
                     bsButton(inputId = "sidebarMenu_clearHighlighting", 
                              label = "Clear highlighting", 
                              style='primary', 
                              type='action'),
                     bsButton(inputId = "sidebarMenu_freezeDataToHighlighted", 
                              label = "Work with highlighted CUs only", 
                              style='primary', 
                              type='action', 
                              disabled=TRUE),
                     bsButton(inputId = "sidebarMenu_resetDataToFilter", 
                              label = "Revert to full dataset", 
                              style='primary', 
                              type='action', 
                              disabled=TRUE),
                     conditionalPanel("input.UIPanels == 'Map' || input.UIPanels == 'TSPlots' || input.UIPanels == 'Table'", 
                                      checkboxInput(inputId = 'sidebarMenu_showPops', label = 'Show sites', value = FALSE)),
                     tags$hr(),
                     tags$div(id = 'insertMarkInfoPane')
                    ),
    tags$div(
      style = "position: absolute; bottom: 0;",
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
  includeScript('www/customJSCode.js'),
  tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),
  tags$head(tags$style(
    HTML('.skin-blue {min-height: 400px !important;}')
  )),
  
  # styling for default button look
#  tags$head(tags$style(
#    HTML('.primary {color: #fff; background-color: #337ab7; border-color: #2e6da4, height:70px; font-size: 100%}'))),

  tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
                            background-color: #3c8dbc;
                            }
                            .skin-blue .main-header .logo:hover {
                            background-color: #3c8dbc;
                            }
                            '))),
  
  tags$head(tags$style(
    HTML('.content-wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden; line-hight: 1}')
  )),
  
  tags$head(tags$style(
    HTML('.content-wrapper {color: #000000 !important;}')
  )),
  
  tags$head(tags$style(
    HTML('.tooltip-inner {width: 400px !important;}')
  )),
  
  tags$head(tags$style(
    HTML('.sidebar-sparkline-box { border: none; 
                           margin: 10px; 
                           white-space: normal; 
                           display: grid; 
                           font-weight: normal; 
                           font-size: 12px; }')
  )),
  
  tags$head(tags$style(
    HTML('.sidebar-sparkline-box-header {padding-left: 10px; padding: 2px; font-weight: bold; font-size: 14px}')
  )),
  
  tags$head(tags$style(
    HTML('.sidebar-sparkline-box-subheader {padding: 2px; font-weight: bold; font-size: 12px}')
  )),
  
  tags$head(tags$style(
    HTML('.sidebar-sparkline-box-text {padding: 2px; font-weight: normal; font-size: 12px}')
  )),
  
  tags$head(tags$style(
#    HTML('.sidebar-sparkline-canvas {padding: 2px; width: 95% !important; height: auto !important; display: grid !important}')
    HTML('.sidebar-sparkline-canvas {padding: 2px; width: 95% !important; height: auto !important; display: grid !important}')
  )),
  
  tags$head(tags$style(
    HTML('.full-sparkline-box { border: none; 
                             margin: 10px; 
                             white-space: normal; 
                             display: grid; 
                             font-weight: normal; 
                             font-size: 14px; }')
  )),

  tags$head(tags$style(
    HTML('.full-sparkline-box-header {padding-left: 10px; padding: 2px; font-weight: bold; font-size: 16px}')
  )),

  tags$head(tags$style(
    HTML('.full-sparkline-box-subheader {padding: 2px; font-weight: bold; font-size: 14px}')
  )),

  tags$head(tags$style(
    HTML('.full-sparkline-box-text {padding: 2px; font-weight: normal; font-size: 14px}')
  )),

  tags$head(tags$style(
    HTML('.full-canvas .sparkline canvas {padding: 5px; width: 200px !important; height: 50px !important}')
  )),

  tags$head(tags$style(
    HTML('.sidebar-canvas .sparkline canvas {padding: 2px; width: 95% !important; height: auto !important; display: grid !important}')
  )),

  tags$head(tags$style(
    HTML('.dataTableHead {padding: 5px;}')
  )),

  tags$head(tags$style(
    HTML('.scroll-container {
            width: 1200px;   
            overflow: auto;    
            scrollbar-base-color:#ffeaff
         }'))),

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
      tags$div(style = 'text-align:right;', downloadButton("allData_Download", "Download")),
      tags$div(style = 'overflow-x: scroll',  DT::dataTableOutput("allData_Table", width="70%"))
    ),
      
    tabItem(
      tabName = "CUSelection",
        bsCollapse(id = 'UIPanels', open = 'Filter',
          bsCollapsePanel(title = "Start here: Choose the data you want to work with", uiOutput("box_DataFilters"), value='Filter', style='primary'),
          #hid the data selector box
          #bsCollapsePanel(title = "Select by attributes and/or metric values", uiOutput("box_DataSelectors"), value='Select', style='primary'),
          bsCollapsePanel(title = "View and highlight on map", uiOutput("box_LeafletMap"), value='Map', style='primary'),
          bsCollapsePanel(title = "Compare CUs", uiOutput("box_Parcoords"), value='Parcoords', style='primary'),
          bsCollapsePanel(title = "Time series and status overview", shinycssloaders::withSpinner(uiOutput("box_TSPlots")), value='TSPlots', style='primary'),
          bsCollapsePanel(title = "Table view and download", div(style = 'overflow-x: scroll', uiOutput("box_Table")), value='Table', style='primary'),
          bsCollapsePanel(title = "Summary report", uiOutput("box_HistoSummary"), value='Histogram', style='primary')
          #hid the radar plots
          #bsCollapsePanel(title = "Radar plots", uiOutput("box_RadarPlots"), value='Radar', style='primary')
      )
    )
  )
)


# Define UI for application 
ui <- dashboardPage( 
  dashboardHeader(
# sample code for adding buttons and other elements to header bar    
#    tags$li(class = "dropdown", actionButton("btn1", label = "Button 1", style=ButtonStyle)),
#    tags$li(class = "dropdown", conditionalPanel("input.tabs == 'CUSelection'",
#                                                  actionButton("btn3", label = "Clear Selection", style=ButtonStyle))),
     # tags$li(class = "dropdown",
     #         tags$style(".main-header {max-height: 100px}"),
     #         tags$style(".main-header .logo {height: 100px}")
     # ),
    # Use image in title
    #title = tags$a(href='http://company.fr/',     # Note we can add a web link to the logo in the future using this structure if we want!
     #              tags$img(src='logo.jpg'))
    title = tags$img(src='Final - State of the Salmon Program - LT. Design-03.png', height="60px")
  ),     

  #  dashboardSidebar(disable=F),
  sidebar,
  body
)



