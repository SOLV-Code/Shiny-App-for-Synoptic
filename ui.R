# web interface for SoS database and SSET toolbox
# Written by B. Dorner & B. MacDonald
# May 2020
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
                     tags$div(style = 'padding-left: 7px; padding-right: 10px;',
                       conditionalPanel("input.UIPanels == 'Map' || input.UIPanels == 'Parcoords'", 
                                       selectInput(inputId = 'sidebarMenu_colorScheme', 
                                                   label = 'Color CUs by', 
                                                   choices = default.colorTheme, 
                                                   selected = default.colorTheme, 
                                                   multiple = FALSE,
                                                   width = '100%')),      
                       conditionalPanel("input.dataFilters_change == 'Annual'",
                                        selectInput( inputId="sidebarMenu_year",					 
                                                     label="Assessment Year",
                                                     choices = as.character(data.CU.Metrics.Years),
                                                     selected = as.character(data.CU.Metrics.Years[length(data.CU.Metrics.Years)]),
                                                     width = '100%'
                                                     )),
                       conditionalPanel("input.UIPanels == 'Map' || input.UIPanels == 'TSPlots'", 
                                        selectInput(inputId = 'sidebarMenu_additionalSpawnerTS', 
                                                     label = 'Additional Spawner Time Series', 
                                                     choices = sparkAdditionalDataTypeOpts,
                                                     selected = 'none',
                                                     multiple = FALSE)),
                       conditionalPanel("input.UIPanels == 'Map' || input.UIPanels == 'TSPlots' || input.UIPanels == 'Table'",
                                        tags$div(class = 'sitesMenu', 
                                          fluidRow(
                                            column(width=6, style = 'padding-left: 15px; padding-right: 5px;',
                                                  tags$div(title = 'Show sample sites associated with each CU', 
                                                           checkboxInput(inputId = 'sidebarMenu_showPops', 
                                                                         label = 'Show sites', 
                                                                         value = FALSE))),
                                            column(width=6, style = 'padding-left: 2px; padding-right: 5px;',
                                                  conditionalPanel("input.sidebarMenu_showPops",
                                                                   style = 'padding-top: 10px;',
                                                                   tags$span('All', 
                                                                    prettySwitch(inputId = 'sidebarMenu_WSPSites',
                                                                                label = 'WSP',
                                                                                value = TRUE,
                                                                                status = 'primary',
                                                                                fill = TRUE,
                                                                                inline = TRUE),
                                                                    style = 'padding-right: -5px;',
                                                                    title = 'Show only Wild Salmon Policy sites?'
                                                                    ))))))),
                     tags$hr(),
                     actionButton(inputId = "sidebarMenu_clearHighlighting", 
                                  label = "Clear highlighting", 
                                  style=ButtonStyle, 
                                  width = '90%',
                                  disabled=TRUE),
                     actionButton(inputId = "sidebarMenu_freezeDataToHighlighted", 
                              label = "Work with highlighted CUs only", 
                              style=ButtonStyle, 
                              width = '90%',
                              disabled=TRUE),
                     actionButton(inputId = "sidebarMenu_resetDataToFilter", 
                              label = "Revert to full dataset", 
                              style=ButtonStyle, 
                              width = '90%',
                              disabled=TRUE),
                     tags$hr(),
                     tags$div(id = 'insertMarkInfoPane')
                    ),
    tags$div(
      style = "position: absolute; bottom: -100px;",
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
  tags$head(tags$script('
                        var window_size = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                            window_size[0] = window.innerWidth;
                            window_size[1] = window.innerHeight;
                            Shiny.onInputChange("window_size", window_size);
                        });
                        $(window).resize(function(e) {
                            window_size[0] = window.innerWidth;
                            window_size[1] = window.innerHeight;
                            Shiny.onInputChange("window_size", window_size);
                        });
                    ')),
  tabItems(
    tabItem(
      tabName = "DISCLAIM",
      h2("DISCLAIMER"),
      fluidRow(
        column(width=8,
               includeMarkdown("Markdown/about.md"),
               actionBttn("contact_Btn2", label = "Contact", size="sm", style="minimal", color='primary')
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
          bsCollapsePanel(title = "Compare CUs", div(style = 'overflow-x: scroll', uiOutput("box_Parcoords")), value='Parcoords', style='primary'),
          bsCollapsePanel(title = "Time series and status overview", div(style = 'overflow-x: scroll', shinycssloaders::withSpinner(uiOutput("box_TSPlots"))), value='TSPlots', style='primary'),
#          bsCollapsePanel(title = "Time series and status overview", shinycssloaders::withSpinner(uiOutput("box_TSPlots")), value='TSPlots', style='primary'),
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
    title = tags$img(src='SSET - State of the Salmon Program - LT. Design-03.png', height="60px"),
#    title = tags$img(src='Final - State of the Salmon Program - LT. Design-03.png', height="60px"),
    tags$li(class = "dropdown", tags$div(style='padding-right: 15px', 
                                         actionBttn("contact_Btn", label = "Contact", size="sm", style="minimal")))
  ),     

  #  dashboardSidebar(disable=F),
  sidebar,
  body
)



