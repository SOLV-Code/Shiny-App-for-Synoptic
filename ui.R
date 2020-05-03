# web interface for SoS database and SSET toolbox
# Written by B. Dorner & B. MacDonald
# April 2020
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
  tags$link(rel = "stylesheet", type = "text/css", href = "customStyles.css"),

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



