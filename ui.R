# Trial for Parallel Plot brushing to produce radar plots and table
# Written by B. MacDonald
# Sept 21 2018
# parallel coorordinates code is from the parcoords package accessed from timelyportfolio github
# uses ezR package for rescale
# coord_radar function from ezR package
# requires dataframes: data.new, data.row

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


#source("Synoptic data prep_timevarying.R")





# ====================== Define UI Components =========================


#library(ezR)
library(shinydashboard)
library(shinyWidgets)
library(parcoords)
library(plotly)
library(markdown)
data.start <- readxl::read_excel("data/FR SK metrics.xls")

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
      )
    ),
    tabItem(
      tabName = "Parallel",
      h2("Parallel coordinate plots"),
      h4("Select a year to view WSP metrics, or select 'change' to view changes in metrics between years."),
      fluidRow(
        column( width=4,
                radioButtons( "select_change",
                              label="Select change or annual values:",
                              choices=c("Annual", "Change"),
                              selected="Annual")
        ),
        conditionalPanel("input.select_change == 'Annual'",
                         column( width=3,
                                 sliderTextInput( inputId="selected_year",					 
                                                  label="Select Year:",
                                                  choices = levels(as.factor(data.start$Year)),
                                                  selected= levels(as.factor(data.start$Year))[1])
                         )
        ),
        conditionalPanel("input.select_change == 'Change'",
                         column( width=3,
                                 selectInput( inputId="selected_changeyear_1",					 
                                              label="Select Initial Year:",
                                              choices = levels(as.factor(data.start$Year))[-(length(levels(as.factor(data.start$Year))))],  # choices do not include the last year
                                              selected= dplyr::first(levels(as.factor(data.start$Year))) )
                         ),
                         column( width=3,
                                 selectInput( inputId="selected_changeyear_2",					 
                                              label="Select Last Year:",
                                              choices = levels(as.factor(data.start$Year))[-1],      # choices do not include the first year
                                              selected= dplyr::last(levels(as.factor(data.start$Year))) )
                         )
        )
      ),
      parcoordsOutput("parcoords", height="600px"),           # 400px is defaultheight
      actionButton(inputId = "reset_brush",
                   label="Reset Brushing"),
      #tags$button(id="btn-resetbrush", onclick="reset_brush()", "reset brush")
      fluidRow(
        column(width=2),
        column( width=1, 
                numericInput("axis_1_max", label= "Set max value", value= 16, min=0, max=16, step=1)), 
        column( width=1,
                numericInput("axis_2_max", label= "Set max value", value= 30, min=0, max=30, step=1)),
        column( width=1,
                numericInput("axis_3_max", label= "Set max value", value= 10, min=0, max=10, step=1)),
        column( width=1,
                numericInput("axis_4_max", label= "Set max value", value= 10, min=0, max=10, step=1)),
        column( width=1,
                numericInput("axis_5_max", label= "Set max value", value= 10, min=0, max=10, step=1)),
        conditionalPanel("input.select_change == 'Annual'",  column(width=5)),
        conditionalPanel("input.select_change == 'Change'",  
                         column(width=1,
                                numericInput("axis_6_max", label= "Set max value", value= 1, min=-1, max=1, step=0.01)),
                         column(width=1,
                                numericInput("axis_7_max", label= "Set max value", value= 5, min=-5, max=5, step=1)),
                         column(width=3)
        )
      ),
      conditionalPanel("input.select_change == 'Change'",
                       fluidRow(
                         column(width=2),
                         column( width=1, 
                                 numericInput("axis_1_min", label= "Set min value", value= -10, min=-10, max=16, step=1)), 
                         column( width=1,
                                 numericInput("axis_2_min", label= "Set min value", value= -10, min=-10, max=30, step=1)),
                         column( width=1,
                                 numericInput("axis_3_min", label= "Set min value", value= -10, min=-10, max=10, step=1)),
                         column( width=1,
                                 numericInput("axis_4_min", label= "Set min value", value= -10, min=-10, max=10, step=1)),
                         column( width=1,
                                 numericInput("axis_5_min", label= "Set min value", value= -1, min=-1, max=10, step=1)),
                         column( width=1,
                                 numericInput("axis_6_min", label= "Set min value", value= -1, min=-1, max=10, step=0.01)),
                         column( width=1,
                                 numericInput("axis_7_min", label= "Set min value", value= -1, min=-1, max=10, step=1)),
                         column(width=3)
                       )
      )
    ),
    tabItem(
      tabName = "AllData",
      h2("Data"),
      DT::dataTableOutput("AllData", width="50%")
    ),
    tabItem(
      tabName = "Data",
      h2("Data selected in parallel coordinate plot"),
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



