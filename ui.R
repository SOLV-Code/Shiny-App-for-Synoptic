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

#data.start <- readxl::read_excel("data/FR SK metrics.xls")

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    #   id = "tabs",
    menuItem("DISCLAIMER", tabName="DISCLAIM"),
    menuItem("CU Status Summary", tabName="Flow"),
    menuItem("Full Data", tabName="AllData"),
#    menuItem("Radar Plots", tabName="Radar"),
#    menuItem("Areas", tabName="Areas"),
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
      tags$div('style' = "text-align:right;", 
               downloadButton("downloadAllData", "Download")
      ),
      DT::dataTableOutput("AllData", width="50%")
    ),
    tabItem(
      tabName = "Radar",
      h2("Radar plots of selected data"),
      h5("CU metrics are plotted in proportion to each other. Metric scores have been inverted so that larger triangles depict lower scores. Only CUs with all 3 metrics are shown."),
      br(),
      h4("Select metrics for radar plots:"),
      fluidRow(
        column(width=3,
               selectInput(inputId = "selected_metric_1",
                           label = "",
                           choices = c("ShortTerm.Trend",  "Recent.Percentile", 
                                       "Recent.Total", "Lower.Ratio", "Upper.Ratio"),
                           selected = c("Recent.Total"),
                           multiple=FALSE)
        ),
        column(width=3,
               selectInput(inputId = "selected_metric_2",
                           label = "",
                           choices = c("ShortTerm.Trend",  "Recent.Percentile", 
                                       "Recent.Total", "Lower.Ratio", "Upper.Ratio"),
                           selected = c("Lower.Ratio"),
                           multiple=FALSE)
        ),
        column(width=3,
               selectInput(inputId = "selected_metric_3",
                           label = "",
                           choices = c("ShortTerm.Trend",  "Recent.Percentile", 
                                       "Recent.Total", "Lower.Ratio", "Upper.Ratio"),
                           selected = c("Upper.Ratio"),
                           multiple=FALSE)
        )
      ),
      checkboxInput(inputId = "faceted",
                    label = "Select faceting:",
                    value = TRUE),
      #tags$head(tags$style(HTML(mycss))),
      #div(id = "plot-container",
      #    tags$img(src = "spinner.gif",
      #             id = "loading-spinner"),
          plotOutput("radarPlot", height="550px", width="700px")
        #  textOutput("incomplete_plots")
      #)
      
    ),
    tabItem(
      tabName = "Areas",
      h2("Proportional radar plot areas for selected metrics"),
      DT::dataTableOutput("Areas", width="50%")
    ),
    tabItem(
      tabName="Summary",
      h2("Summary of selected data"),
      selectInput(inputId = "selected_type",
                  label = "Select units for summarizing brushed CUs",
                  choices = c("Proportion",  "Number"),
                  selected = c("Proportion"),
                  multiple=FALSE),
      h3("Selected CUs by Management Timing Group"),
      plotlyOutput("summaryPlot_MT", width="70%"),
      br(), br(),
      h3("Selected CUs by Freshwater Adaptive Zone"),
      plotlyOutput("summaryPlot_FAZ",width="70%"),
      br(), br(),
      conditionalPanel("input.select_change == 'Annual'",
                       h3("Selected CUs by WSP Integrated Status")
      ),
      conditionalPanel("input.select_change == 'Change'",
                       h3("Selected CUs by Change in WSP Integrated Status")
      ),
      plotlyOutput("summaryPlot_WSP",width="70%"),
      br(), br(),
      conditionalPanel("input.select_change == 'Annual'",
                       h3("Selected CUs by Exploitation Rate")
      ),
      conditionalPanel("input.select_change == 'Change'",
                       h3("Selected CUs by Change in Exploitation Rate")
      ),
      plotlyOutput("summaryPlot_ER",width="70%")
    ),
    tabItem(
      tabName = "Flow",
      box(title = "Start here", width=12, status="info", solidHeader=TRUE, collapsible=TRUE, collapsed=FALSE,
          uiOutput("selectors")),
      
      box(title = "View/select CUs on a map", width=12, status="info", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE,
          uiOutput("leafletMap")),
      
      box(title = "View/select CUs by performance metric (parallel coordinates plot)", width=12, status="info", solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE,
          uiOutput("parcoordsPlot")),

      box(title = "View/select CUs on a data table", width=12, status="info", solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE,
          div(style = 'overflow-x: scroll', uiOutput("data"))),
      
      box(title = "Summary report", width=12, status="info", solidHeader=TRUE, collapsible=TRUE,  collapsed=TRUE,
          uiOutput("summary"))
      
      
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



