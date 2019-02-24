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


#library(ezR)
library(shinydashboard)
library(shinyWidgets)
library(parcoords)
library(plotly)
library(markdown)

#data.start <- readxl::read_excel("data/FR SK metrics.xls")

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    #   id = "tabs",
    menuItem("DISCLAIMER", tabName="DISCLAIM"),
    menuItem("Select filters", tabName="Filters"),
    menuItem("All Data", tabName="AllData"),
    menuItem("Parallel Coordinates Plot", tabName="Parallel"),
    menuItem("Extracted Data", tabName="Data"),
    menuItem("Radar Plots", tabName="Radar"),
    menuItem("Areas", tabName="Areas"),
    menuItem("Summary of selected data", tabName="Summary"),
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
      tabName = "Filters",
      h2("Select filters"),
      fluidRow(
        column(width=3,
               selectInput( inputId="selected_species",					 
                            label="Selected Species:",
                            choices = levels(factor(data.start$Base.Unit.Species)),
                            selected="SK",
                            multiple=FALSE)
        ),
        column(width=3,
               selectInput( inputId="selected_watershed",					 
                            label="Selected Watershed:",
                            choices =levels(factor(data.start$BaseUnit.Watershed)),
                            selected="Fraser",
                            multiple=FALSE)
       )
        # column(width=3,
        #        selectInput(inputId="selected_metrics", 
        #                    label="Selected Metrics:", 
        #                    choices=names(data.start)[4:ncol(data.start)], 
        #                    multiple=TRUE, 
        #                    selected=names(data.start[4:ncol(data.start)]), 
        #                    selectize=TRUE)),
        # column(width=3,
        #        selectInput(inputId="selected_cus", 
        #                    label="Selected CUs:", 
        #                    choices=CUs,
        #                    multiple=TRUE, 
        #                    selected=CUs, 
        #                    selectize = TRUE))
      )
    ),
    tabItem(
      tabName = "Parallel",
      h2("Parallel coordinate plots"),
       fluidRow(
         column(width = 4, h3("Step 1:")),
         column(width = 4, h3("Step 2:")),
         column(width=4, h3("Step 3:"))
       ),
       fluidRow(
        #column(width=1),
        column(width = 3,  h4("Select 'Annual' to view WSP metrics for a specific year, or 'Change' to view changes 
                              in metrics between two years")),
        column(width = 1),
        conditionalPanel( "input.select_change == 'Annual'",
                          column(width = 3,  h4("Select year to view WSP metric values"))),
        conditionalPanel( "input.select_change == 'Change'",
                          column(width = 3,  h4("Select years to calculate change"))),
        column(width = 1),
        column(width=3, h4("Click and drag mouse over vertical axes to select CUs")),
        column(width=1)
       ),
      
       fluidRow(
       # column(width=1), 
        column(width=3,
                radioButtons( "select_change",
                              label="",
                              choiceNames = list( HTML("<p style='font-size:125%;'>Annual</p>"), HTML("<p style='font-size:125%;'>Change</p>")),
                              choiceValues = list("Annual","Change"),
                              inline=TRUE,
                              selected="Annual")
        ),
        column(width=1),
        conditionalPanel("input.select_change == 'Annual'",
                         column( width=4,
                                 selectInput( inputId="selected_year",					 
                                                  label="",
                                                  choices = levels(as.factor(data.start$Year)),
                                                  selected= levels(as.factor(data.start$Year))[1])
                         )
        ),
        conditionalPanel("input.select_change == 'Change'",
                         column( width=2,
                                 selectInput( inputId="selected_changeyear_1",					 
                                              label="Initial Year:",
                                              choices = levels(as.factor(data.start$Year))[-(length(levels(as.factor(data.start$Year))))],  # choices do not include the last year
                                              selected= dplyr::first(levels(as.factor(data.start$Year))) )
                         ),
                         column( width=2,
                                 selectInput( inputId="selected_changeyear_2",					 
                                              label="Last Year:",
                                              choices = levels(as.factor(data.start$Year))[-1],      # choices do not include the first year
                                              selected= dplyr::last(levels(as.factor(data.start$Year))) )
                         )
        ),
        column(width=1)
       ),
      tags$div('style' = "text-align:right;", 
        actionButton(inputId = "reset_brush",
                     label="Reset Brushing",icon("paper-plane"), 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4, height:70px; width:180px; font-size: 130%"),
        actionButton(inputId = "scale_to_selected",
                     label="Scale to Selected",icon("search-plus"), 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4, height:70px; width:180px; font-size: 130%")
      ),
      
      parcoordsOutput("parcoords", height="600px"),           # 400px is defaultheight
      uiOutput("parcoordsControls")
      
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
      tabName = "Data",
      h2("Data selected in parallel coordinate plot"),
      tags$div('style' = "text-align:right;", 
        downloadButton("downloadSelectedData", "Download")
      ),
      DT::dataTableOutput("SelectedData", width="50%")
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



