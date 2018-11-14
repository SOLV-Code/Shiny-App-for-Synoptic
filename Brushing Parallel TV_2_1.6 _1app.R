# Trial for Parallel Plot brushing to produce radar plots and table
# Written by B. MacDonald
# Sept 21 2018
# parallel coorordinates code is from the parcoords package accessed from timelyportfolio github
# uses ezR package for rescale
# coord_radar function from ezR package
# requires dataframes: data.new, data.row

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


rm(list=ls())


#
#devtools::install_github("jerryzhujian9/ezR")

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
                      "plotly")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

if(!"parcoords" %in% installed.packages()[,"Package"]) devtools::install_github("timelyportfolio/parcoords")
if(!"ezR" %in% installed.packages()[,"Package"]) devtools::install_github("jerryzhujian9/ezR")
library("parcoords")
library("ezR")


source("Synoptic data prep_timevarying.R")

# Spinner code 

mycss <- "
    #plot-container {
    position: relative;
    }
    #loading-spinner {
    position: absolute;
    left: 50%;
    top: 50%;
    z-index: -1;
    margin-top: -33px;  /* half of the spinner's height */
    margin-left: -33px; /* half of the spinner's width */
    }
    #plot.recalculating {
    z-index: -2;
    }
"

# -==== end Spinner code


# ====================== Helper Functions ==============================

# Re-scale function 
ez.rescale02 = function (x) {
  if (is.numeric(x)) {
    # get rid of negative values
    if (min(x, na.rm=TRUE)<0) {x = x - min(x, na.rm=TRUE)}
    # scale all postive to max of 1
    if (max(x, na.rm=TRUE)!=0) {x = x/max(x, na.rm=TRUE)}
    result = abs(x-1)
  } else {
    result = x
  }
  return(result)
}

# ====================== use this guys package =========================
#source("jerry zhu radar code_filled.R")
#devtools::install_github("jerryzhujian9/ezR")

#library(ezR)

sidebar <- dashboardSidebar(
                sidebarMenu(
                   #   id = "tabs",
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

body <- dashboardBody(
  tabItems(
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
                                                  selected= first(levels(as.factor(data.start$Year))) )
                         ),
                         column( width=3,
                                 selectInput( inputId="selected_changeyear_2",					 
                                              label="Select Last Year:",
                                              choices = levels(as.factor(data.start$Year))[-1],      # choices do not include the first year
                                              selected= last(levels(as.factor(data.start$Year))) )
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
                    value = FALSE),
      tags$head(tags$style(HTML(mycss))),
      div(id = "plot-container",
          tags$img(src = "spinner.gif",
                   id = "loading-spinner"),
          plotOutput("radarPlot", height="550px", width="700px"),
          textOutput("incomplete_plots")
      )
      
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
      plotlyOutput("summaryPlot_MT", width="60%"),
      br(), br(),
      h3("Selected CUs by Freshwater Adaptive Zone"),
      plotlyOutput("summaryPlot_FAZ",width="60%"),
      br(), br(),
      h3("Selected CUs by WSP Integrated Status"),
      plotlyOutput("summaryPlot_WSP",width="60%")
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

  


# Define server logic 
server <- function(input, output,session){
     
       observeEvent(                            # will need to update selections once we have more than 2 years so both cannot be same year
            {input$select_changeyear_1},{ 
                 updateSelectInput(session, "selected_changeyear_2", choices=levels(as.factor( unique(data.start$Year[data.start$Year > input$selected_changeyear_1]))), 
                 selected= levels(as.factor( unique(data.start$Year[data.start$Year > input$selected_changeyear_1])))[1] )  
       })    
  
       observeEvent(                            # will need to update selections once we have more than 2 years so both cannot be same year
            {input$select_changeyear_2},{ 
                 updateSelectInput(session, "selected_changeyear_1", choices=levels(as.factor( unique(data.start$Year[data.start$Year < input$selected_changeyear_2]))), 
                 selected= levels(as.factor( unique(data.start$Year[data.start$Year < input$selected_changeyear_2])))[1] )  
       }) 
  
      data.new <- reactive({
                req(input$selected_species, input$selected_watershed, input$selected_year)
                
                df <- data.start  %>% filter(Base.Unit.Species %in% input$selected_species) %>%
                                      select_if(colSums(!is.na(.)) > 0)
                
                              
                if(input$selected_watershed != "All"){
                    df <- df %>% filter(BaseUnit.Watershed %in% input$selected_watershed) %>%
                                 select_if(colSums(!is.na(.)) > 0)
                }
                
                if(input$select_change == "Change" ){                     ########## WILL NEED TO SET THIS UP SO IT UPDATES METRICS AUTOMATICALLY WITHOUT CHANGING THIS - LOOK TO METRICS FILE FOR LIST OF NAMES
                      func <- function(x){x-lag(x, default=first(x))}
                      
                      df <- df %>% group_by(Base.Unit.CU.ShortName) %>%
                                     filter(Year %in% c(input$selected_changeyear_1, input$selected_changeyear_2)) %>%
                                     arrange(Year, .by_group=TRUE) %>%
                                     mutate(WSP.numeric = as.numeric(WSP.status)) %>%
                                     mutate_at(.vars = vars(Recent.Total, Lower.Ratio, Upper.Ratio, LongTerm.Ratio, ShortTerm.Trend, Recent.ER, WSP.numeric), .funs= func) %>%
                                     filter(Year== max(Year)) %>%
                                     select(-WSP.status)
                } 
                if(input$select_change=="Annual"){
                      df <- df %>% filter(Year %in% input$selected_year)
                }
                
                df <- df %>% select(-Year)
                df <- as.data.frame(df)
      })
      
      
      # Create data.row for the parallel plot to have names
      data.row <- eventReactive(data.new(),{
            df <- as.data.frame(data.new())        
              rownames(df) <- df[,1]  
              df <- df %>% select(-one_of("Base.Unit.CU.ShortName", "Base.Unit.Species", "BaseUnit.Watershed")) %>%
                           select(-Management.Timing, Management.Timing) %>%
                           select(-FAZ, FAZ)
              # Must re-order rows so most full rows are first and also have a full row as the first (no NAs)
              na.order <- order(rowSums(is.na(df)))
              df <- df[na.order,]
              #[,-c(1:2)]
      })
      
      # Update max values to equal actual mx or will break at "which" below when none are ">=" the max
       observe({ 
            df <- data.row()
            
            for(i in 1:5){
              lab <- c(paste("Set maximum of", colnames(df)[i]))
              updateNumericInput(session, paste0("axis_",i,"_max"), label=lab, value=max(data.row()[,i],na.rm=T),max=max(data.row()[,i],na.rm=T))
            }
            
       })  
       
       observeEvent({input$select_change},{
                              if(input$select_change == "Change"){
                                    for(i in 6:7){
                                      lab <- c(paste("Set maximum of", colnames(data.row())[i]))
                                      updateNumericInput(session, paste0("axis_",i,"_max"), label=lab, value=max(data.row()[,i],na.rm=T),max=max(data.row()[,i],na.rm=T))
                                    }    
                               
                                    for(i in 1:7){
                                      lab <- c(paste("Set minimum of", colnames(data.row())[i]))
                                      updateNumericInput(session, paste0("axis_",i,"_min"), label=lab, value=min(data.row()[,i],na.rm=T),min=min(data.row()[,i],na.rm=T))
                                    }
                              }
       })
      
      # Reset upper limits of metrics if added
      data.par.plot <- reactive({
                             req(input$axis_1_max, input$axis_2_max, data.row())                
                             df <- data.row()
                             max.vals <- list(max1=input$axis_1_max, max2=input$axis_2_max, max3=input$axis_3_max, max4=input$axis_4_max, max5=input$axis_5_max)
                             min.vals <- list(min1=input$axis_1_min, min2=input$axis_2_min, min3=input$axis_3_min, min4=input$axis_4_min, min5=input$axis_5_min,
                                              min6=input$axis_6_min, min7=input$axis_7_min  )
                             
                             for(i in 1:5){
                               df[which(df[,colnames(df)[i]] >=max.vals[[i]]) ,colnames(df)[i]] <- max.vals[[i]]
                             }
                             
                             if(input$select_change == "Change"){
                               df[which(df[,colnames(df)[6]] >= input$axis_6_max),colnames(df)[6]] <- input$axis_6_max
                               df[which(df[,colnames(df)[7]] >= input$axis_7_max),colnames(df)[7]] <- input$axis_7_max
                               for(i in 1:7){
                                  df[which(df[,colnames(df)[i]] <=min.vals[[i]]) ,colnames(df)[i]] <- min.vals[[i]]
                               }
                             }                   
                             print(df)
                             df
                         })
      
      observeEvent({
          input$reset_brush
          #data.row()
          data.par.plot()
           },{
                #data.par <- data.row()
                
                #if(input$axis_1_max < max(data.row()[,1], na.rm=T)) data.par <- data.par.plot()
                data.par <- data.par.plot()
                if(input$select_change=="Annual"){
                      output$parcoords<- renderParcoords({
                                                    parcoords( data.par,
                                                               autoresize=TRUE,
                                                               color= list(colorScale=htmlwidgets::JS("d3.scale.category10()"), colorBy="Management.Timing"),
                                                               rownames=T,
                                                               alpha=0.6, 
                                                               alphaOnBrushed = 0,
                                                               brushMode="1D-axes-multi",
                                                               brushPredicate="and",
                                                               reorderable = TRUE,
                                                               #queue = TRUE,
                                                               #height=NULL
                                                               tasks = list(
                                                                 htmlwidgets::JS(sprintf(
                                                                   "
                                                                   function(){
                                                                   debugger
                                                                   this.parcoords.dimensions()['WSP.status']
                                                                   .yscale = d3.scale.ordinal()
                                                                   .domain([%s])
                                                                   .rangePoints([
                                                                   1,
                                                                   this.parcoords.height()-this.parcoords.margin().top - this.parcoords.margin().bottom
                                                                   ])
                                                                   
                                                                   this.parcoords.removeAxes();
                                                                   this.parcoords.render();
                                                                   // duplicated from the widget js code
                                                                   //  to make sure reorderable and brushes work
                                                                   // if( this.x.options.reorderable ) {
                                                                   this.parcoords.reorderable();
                                                                   
                                                                   //
                                                                   
                                                                   
                                                                   if( this.x.options.brushMode ) {
                                                                   // reset the brush with None
                                                                   this.parcoords.brushMode('None')
                                                                   this.parcoords.brushMode(this.x.options.brushMode);
                                                                   this.parcoords.brushPredicate(this.x.options.brushPredicate);
                                                                   }
                                                                   
                                                                   
                                                                   
                                                                   
                                                                   // delete title from the rownames axis
                                                                   d3.select('#' + this.el.id + ' .dimension .axis > text').remove();
                                                                   this.parcoords.render()
                                                                   
                                                                   }
                                                                   "     ,
                                                                   paste0(shQuote(rev(levels(data.par$WSP.status))),collapse=",")
                                                                 ))
                                                               )
                                                              
                                                    )
                      })
                } # end if input$change=="Annual"
                
                if(input$select_change=="Change"){
                      output$parcoords<- renderParcoords({
                            parcoords( data.par,
                                       autoresize=TRUE,
                                       color= list(colorScale=htmlwidgets::JS("d3.scale.category10()"), colorBy="Management.Timing"),
                                       rownames=T,
                                       alpha=0.6, 
                                       alphaOnBrushed = 0,
                                       brushMode="1d-axes",
                                       brushPredicate="and",
                                       reorderable = TRUE
                                       #queue = TRUE,
                                       #height=NULL
                            )
                      })
                } # end  if input$select_change="Change"
        }
    )
  
   # 
   #  selected <- reactive({
   #        rownames(data.row) %in% input$parcoords_brushed_row_names
   # #       datatable(data.row[ids,])
   #  })
    
      output$AllData <- DT::renderDataTable({
        datatable(data.row())
      })
      
      brushed.data <- reactive({
            if(length(input$parcoords_brushed_row_names)>0){
                df <- data.new() %>% filter(Base.Unit.CU.ShortName %in% input$parcoords_brushed_row_names)
            }
            else{df <- data.new()}
        write.csv(df, "brushed.data.csv")
          df
      })
      
      output$SelectedData <-  DT::renderDataTable({
                                       #ids <- rownames(data.row) %in% input$parcoords_brushed_row_names
                                  df <- brushed.data() 
                                  rownames(df) <- df[,1] 
                                  df <- df[-c(1:3)]
                                  datatable(df)
      })

      
      
      
  observe({ 
      updateSelectInput(session, "selected_metric_1", choices=colnames(data.row()), selected=colnames(data.row())[1])
  })
  
  observeEvent(
    {input$selected_metric_1
    data.row()},
    {
     choices_2 <- data.row() %>% select(-one_of(input$selected_metric_1)) %>%
                                 colnames()
     updateSelectInput(session, "selected_metric_2", choices=choices_2, selected = choices_2[1])
    })
  
  observeEvent(
    {input$selected_metric_1
    input$selected_metric_2
    data.row()},
    {
      choices_3 <- data.row() %>% select(-one_of(input$selected_metric_1,input$selected_metric_2)) %>%
        colnames()
      updateSelectInput(session, "selected_metric_3", choices=choices_3, selected=choices_3[1])
    })
  
   # Pull selected metrics data
   metrics_subset <- reactive({
                           req(input$selected_metric_1, input$selected_metric_2, input$selected_metric_3)
                           df <- brushed.data() %>% select(one_of("Base.Unit.CU.ShortName", input$selected_metric_1,
                                                           input$selected_metric_2, input$selected_metric_3))
                          
                           # Filter our CUs with fewer than 3 metrics for Radar plot
                           df <-  df[rowSums(!is.na(df)) >= 4 ,]             
                        
                      #2) rescale
                           print(df)
                      data.frame(lapply(df, ez.rescale02))
   })
   # 
   
  #  # Radar Plots Code of Brushed CUs
   observeEvent(
     {input$faceted
      metrics_subset()},{

      # Long format
       data.radar <- metrics_subset()
                       
                      #  select(-Management.Timing)

       df.long <- tidyr::gather(data.radar, variable,value,-"Base.Unit.CU.ShortName",factor_key = T)

       if(input$faceted == FALSE){
              p <- ggplot(df.long,  aes(x = variable, y = value,
                                        group = Base.Unit.CU.ShortName,
                                        color=Base.Unit.CU.ShortName,
                                        fill=Base.Unit.CU.ShortName,
                                        linetype=Base.Unit.CU.ShortName)) +
                                        geom_polygon(aes(), alpha=0.4, size = 1, show.legend = FALSE) +
                                        xlab("") + ylab("") +
                                        coord_radar()+
                                        scale_linetype_manual(values=rep("solid",nlevels(factor(df.long$Base.Unit.CU.ShortName))))+
                                        theme(axis.text.x = element_text(size = rel(0.8), angle = 0),
                                                         axis.ticks.y = element_blank(),
                                                         axis.text.y = element_blank()) +
                                                   guides(color = guide_legend(ncol=2)) +
                                                   geom_line(aes(), size = 1)
        }
        if(input$faceted == TRUE){
               p <- ggplot(df.long,  aes(x = variable, y = value,
                                         group = Base.Unit.CU.ShortName,
                                         color=Base.Unit.CU.ShortName,
                                         fill=Base.Unit.CU.ShortName,
                                         linetype=Base.Unit.CU.ShortName)) +
                                         geom_polygon(aes(), alpha=0.4, size = 1, show.legend = FALSE) +
                                         xlab("") + ylab("") +
                                         coord_radar()+
                                         scale_linetype_manual(values=rep("solid",nlevels(factor(df.long$Base.Unit.CU.ShortName))))+
                                         theme(strip.text.x = element_text(size = rel(1)),
                                                          axis.ticks.x = element_blank(),
                                                          axis.text.x = element_text(size = rel(0.8), angle = 0),
                                                          axis.ticks.y = element_blank(),
                                                          axis.text.y = element_blank()) +
                                           guides(color = "none") +
                                           facet_wrap(~Base.Unit.CU.ShortName)
        }

        output$radarPlot <- renderPlot({
                  print(p)
        })

  })

   output$Areas <- DT::renderDataTable({
      df <- metrics_subset()
     
      # Each will be the sum of 3 triangles
      a <- df[,2]
      b <- df[,3]
      c <- df[,4]
      
      side1 <- a*b*sin(pi/5)/2
      side2 <- b*c*sin(pi/5)/2
      side3 <- c*a*sin(pi/5)/2
      area <- side1 + side2 + side3
      df <- cbind(df, Area=area)
      datatable(df)
   })
   
   
 # Summary Plots tab
   summary.prep <- function(variable=variable, type=input$selected_type){ # type = "Proportion" or "Number"
       subset_data <-subset(data.start, !duplicated(Base.Unit.CU.ShortName) )
       brushed_data <- brushed.data()
       subset_data[,variable] <- factor(subset_data[,variable])
       y <- factor(levels(subset_data[,variable]), ordered=TRUE, levels=levels(subset_data[,variable]))
       total <- as.vector(table(subset_data[,variable]))
       
       brush_full_selection <- factor(brushed_data[,variable], levels = levels(subset_data[,variable]))
       selected <- as.vector(table(brush_full_selection))
       not.selected <- total-selected
       perc <- selected/total
       if(type == "Proportion")  data.sum <- data.frame(y, selected=perc, unselected =(1-perc), text=c(paste("Total # of CUs:",as.character(total))))
       if(type =="Number")       data.sum <- data.frame(y, selected, unselected=not.selected, text=c(paste("Total # of CUs:",as.character(total))))
    
       p <- plot_ly(data.sum, x = ~selected, y = ~y, text=~text, type = 'bar', orientation = 'h', name = 'Selected', 
                    marker = list(color = "darkred")) %>% 
                    add_trace(x = ~unselected, name = 'Unselected', 
                    marker = list(color = "rgba(128,128,128,0.6)")) %>%        # last number is transparenct;  colours for plotly @ https://reeddesign.co.uk/test/namedcolors.html
                    layout(barmode = 'stack', xaxis = list(title = c(paste(input$selected_type, "of CUs"))), yaxis = list(title = variable))
    
   }
   
   # Percentage.Plots <- function(variable="Management.Timing"){
  observeEvent({
    brushed.data()
    input$selected_type},{
                p.1 <- summary.prep(variable="Management.Timing", type=input$selected_type)
                output$summaryPlot_MT <- renderPlotly({
                    print(p.1)
                })
                p.2 <- summary.prep(variable="FAZ", type=input$selected_type)
                output$summaryPlot_FAZ <- renderPlotly({
                  print(p.2)
                })
                p.3 <- summary.prep(variable="WSP.status", type=input$selected_type)
                output$summaryPlot_WSP <- renderPlotly({
                  print(p.3)
                })
           
    }
  )
   
   
  #   
   
   # 
    # # Add observer to write to screen if metric = NA
    #  incomplete <- reactive({
    #        metrics_subset() %>%  filter(!complete.cases(.)) %>%
    #                         select(Base.Unit.CU.ShortName)
    #  })    
    #  
    # observeEvent(incomplete(),{
    #     if(length(incomplete()$Base.Unit.CU.ShortName) == 0 ){
    #                     output$incomplete_plots <- renderText({paste("")})
    #     }
    #     if(length(incomplete()$Base.Unit.CU.ShortName) > 0){
    #                     output$incomplete_plots <- renderText({
    #                                               paste("The following CUs are missing metric values for",
    #                                                     input$selected_metric,
    #                                                     ":",
    #                                                     toString(incomplete()$Base.Unit.CU.ShortName) )
    #                      })
    #     }
    # })
    # # 
 
 # Automatically end session when browser is closed 
 #session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)




