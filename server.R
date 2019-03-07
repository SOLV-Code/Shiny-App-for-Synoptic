# Server funtion for the State of the Salmon Synoptic Status Evaluation Tool
# Developed by B. MacDonald
# Feb 13, 2019

#------------------------ Setup -----------------------

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
                      "markdown",
                      "parcoords",
                      "crosstalk",
                      "sp")
# 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


#--------- ---------- Helper functions ------------------



# assemble the id of a shiny input widget or a variable name from a prefix and a postfix, e.g. widget.1 
sId <- function(pre, post) {paste(pre, post, sep=".")}

# sum columns using select_if function of dplyr to remove empty columns
sumfun <- function(x){sum(!is.na(x)) > 0}

# the names of the numeric metrics in a data frame
numericMetrics <- function(ds) {names(ds)[unlist(lapply(ds, is.numeric))]}

# Arrange data frame according to specified column order, 
# filtering out any columns that are specified in 'hide'
# If no order is given, just removes the columns specified as hidden
arrangeColumns <- function(ds, colOrder=NULL, hide=NULL) {
  if (is.null(colOrder)) {
    colOrder <- names(ds)
  } else {
    colOrder <- colOrder[colOrder %in% names(ds)] # get rid of any columns that aren't present in the original data
  }
  ds[, colOrder[!(colOrder %in% hide)]]
}

# ==========Define server components ================


# Define server logic 
function(input, output, session){

# ------------- Data filtering --------------------------  
  values <- reactiveValues(dataFilters_select_year = max(data.start$Year), 
                           dataFilters_select_change = "Annual",
                           dataFilters_select_species = unique(as.character(data.start$Base.Unit.Species)),
                           dataFilters_select_watershed = unique(as.character(data.start$BaseUnit.Watershed)),
                           dataFilters_select_FAZ = unique(as.character(data.start$FAZ)),
                           dataFilters_select_management_timing = unique(as.character(data.start$Management.Timing)),
                           dataFilters_select_CUs = unique(as.character(data.start$Base.Unit.CU.ShortName)),
                           dataFilters_select_metrics = c(as.character(CUMetrics), as.character(CUAttributes)),
                           dataFilters_select_attribs = as.character(CUAttributes))
  
  observeEvent(input$dataFilters_select_year, {values$dataFilters_select_year <- input$dataFilters_select_year})
  observeEvent(input$dataFilters_select_change, {values$dataFilters_select_change <- input$dataFilters_select_change})
  observeEvent(input$dataFilters_select_species, {values$dataFilters_select_species <- input$dataFilters_select_species})
  observeEvent(input$dataFilters_select_watershed, {values$dataFilters_select_watershed <- input$dataFilters_select_watershed})  
  observeEvent(input$dataFilters_select_FAZ, {values$dataFilters_select_FAZ <- input$dataFilters_select_FAZ})
  observeEvent(input$dataFilters_select_management_timing, {values$dataFilters_select_management_timing <- input$dataFilters_select_management_timing})
  observeEvent(input$dataFilters_select_CUs, {values$dataFilters_select_CUs <- input$dataFilters_select_CUs})
  observeEvent(input$dataFilters_select_metrics, {values$dataFilters_select_metrics <- input$dataFilters_select_metrics})
  
  # -- some helper functions and structures for nested filtering 
  # a list of data attributes and corresponding selector input widgets
  inputIds <- list('Base.Unit.Species' = 'dataFilters_select_species',
                   'BaseUnit.Watershed' = 'dataFilters_select_watershed',
                   'FAZ' = 'dataFilters_select_FAZ',
                   'Management.Timing' = 'dataFilters_select_management_timing',
                   'Base.Unit.CU.ShortName' = 'dataFilters_select_CUs')
  
  # given a list of data attributes and a data frame, 
  # update the associated selector widgets to show the choices corresponding
  # to the values available in the data frame for the given attributes
  updatePickerInputs <- function(attribs, df) {
    for (a in attribs) {
        pickerOpts <- unique(as.character(df[ , a]))
        updatePickerInput(session, inputIds[[a]], choices=pickerOpts, selected=pickerOpts)
    }
  }
  
  # given a list of data attributes and a data frame, 
  # filter data frame based on the set of selector values
  # associated with the given data attributes  
  getFilteredDF <- function(attribs, df) {
    selection <- rep(T, nrow(df))
    for (a in attribs) {
      selection <- selection & (df[, a] %in% values[[inputIds[[a]]]])
    }
    df[selection, ]
  }
  
  # -- logic for nested filtering of data:
  # all other filters are limited to what's available for the selected species
  observeEvent(values$dataFilters_select_species, {
    df <- getFilteredDF(c('Base.Unit.Species'), data.start)
    updatePickerInputs(c('BaseUnit.Watershed', 'FAZ', 'Management.Timing', 'Base.Unit.CU.ShortName'), df) 
  })
  
  # watershed limits what FAZs and CUs are available
  observeEvent(values$dataFilters_select_watershed, {
    df <- getFilteredDF(c('Base.Unit.Species', 'BaseUnit.Watershed', 'Management.Timing'), data.start)
    updatePickerInputs(c('FAZ', 'Base.Unit.CU.ShortName'), df) 
  })
  
  # FAZ an Management Timing limit what CUs are available
  observeEvent({
    values$dataFilters_select_FAZ
    values$dataFilters_select_management_timing
  }, {
    df <- getFilteredDF(c('Base.Unit.Species', 'BaseUnit.Watershed','Management.Timing', 'FAZ'), data.start)
    updatePickerInputs(c('Base.Unit.CU.ShortName'), df) 
  })
  

  observeEvent(                            # will need to update selections once we have more than 2 years so both cannot be same year
    {input$dataFilters_select_changeyear_1},{ 
      updateSelectInput(session, "dataFilters_select_changeyear_2", 
                        choices=unique(as.character( unique(data.start$Year[data.start$Year > input$dataFilters_select_changeyear_1]))), 
                        selected = unique(as.character(data.start$Year[data.start$Year > input$dataFilters_select_changeyear_1]))[1] )  
    })    
  
  observeEvent(                            # will need to update selections once we have more than 2 years so both cannot be same year
    {input$dataFilters_select_changeyear_2},{ 
      updateSelectInput(session, "dataFilters_select_changeyear_1", 
                        choices=unique(as.character( unique(data.start$Year[data.start$Year < input$dataFilters_select_changeyear_2]))), 
                        selected = unique(as.character(data.start$Year[data.start$Year < input$dataFilters_select_changeyear_2]))[1] )  
    }) 
  
  output$box_DataFilters <- renderUI({
    pickerOptsSingleSelect <- list(`show-tick`=TRUE)
    pickerOptsMultiSelect <- list(`show-tick`=TRUE, `actions-box`=TRUE, `selected-text-format`='count')
    wellPanelStyle <- "background: white"
    yrs <- unique(sort(as.numeric(data.start$Year)))
    tagList(
      fluidRow(
        column(width=5,
               wellPanel(style = wellPanelStyle, tags$b("Step1:",  "Filter your data"), tags$hr(),
                         fluidRow(column(width=5, tags$div('By Species:')),
                                  column(width=7, pickerInput(inputId="dataFilters_select_species",					 
                                                              label=NULL,
                                                              choices=unique(as.character(data.start$Base.Unit.Species)),
                                                              selected=unique(as.character(data.start$Base.Unit.Species))[1],
                                                              multiple=FALSE,
                                                              options=pickerOptsSingleSelect))),
                         fluidRow(column(width=5, tags$div('By Watershed:')),
                                  column(width=7, pickerInput(inputId="dataFilters_select_watershed",					 
                                                              label=NULL,
                                                              choices=unique(as.character(data.start$BaseUnit.Watershed)),
                                                              selected=unique(as.character(data.start$BaseUnit.Watershed)),
                                                              multiple=TRUE,
                                                              options=pickerOptsMultiSelect))),
                         fluidRow(column(width=5, tags$div('By FAZ:')),
                                  column(width=7, pickerInput(inputId="dataFilters_select_FAZ",					 
                                                              label=NULL,
                                                              choices=unique(as.character(data.start$FAZ)),
                                                              selected=unique(as.character(data.start$FAZ)),
                                                              multiple=TRUE,
                                                              options=pickerOptsMultiSelect))),
                         fluidRow(column(width=5, tags$div('By Management Timing:')),
                                  column(width=7, pickerInput(inputId="dataFilters_select_management_timing",					 
                                                              label=NULL,
                                                              choices=unique(as.character(data.start$Management.Timing)),
                                                              selected=unique(as.character(data.start$Management.Timing)),
                                                              multiple=TRUE,
                                                              options=pickerOptsMultiSelect))),
                         fluidRow(column(width=5, tags$div('By CU:')),
                                  column(width=7, pickerInput(inputId="dataFilters_select_CUs",					 
                                                              label=NULL,
                                                              choices=unique(as.character(data.start$Base.Unit.CU.ShortName)),
                                                              selected=unique(as.character(data.start$Base.Unit.CU.ShortName)),
                                                              multiple=TRUE,
                                                              options=pickerOptsMultiSelect)))
               )),
        column(width=3, 
               wellPanel(style = wellPanelStyle, tags$b("Step2:", "Select metrics of interest"), tags$hr(),
                         fluidRow(
                           column(width=12, pickerInput(inputId="dataFilters_select_metrics", 
                                                        label="", 
                                                        choices=list('Metrics' = CUMetrics,
                                                                     'Attributes' = CUAttributes),
                                                        selected=c(CUMetrics, CUAttributes),
                                                        multiple=TRUE,
                                                        options=pickerOptsMultiSelect))
                         ))),
        column(width=4, 
               wellPanel(style = wellPanelStyle, tags$b("Step3:", "Show analysis for a single year, or change between years?"), tags$hr(),
                         fluidRow(
                           column(width=6, radioButtons( "dataFilters_select_change",
                                                         label="",
                                                         choices = list("Single year" = "Annual", "Change over time" ="Change"),
                                                         selected="Annual") ),
                           column(width=6, conditionalPanel("input.dataFilters_select_change == 'Annual'",
                                                            pickerInput( inputId="dataFilters_select_year",					 
                                                                         label="",
                                                                         choices = yrs,
                                                                         selected = yrs[1])),
                                  conditionalPanel("input.dataFilters_select_change == 'Change'",
                                                   pickerInput( inputId="dataFilters_select_changeyear_1",					 
                                                                label="Initial Year:",
                                                                choices = yrs[1:(length(yrs)-1)],  # choices do not include the last year
                                                                selected= yrs[1]),
                                                   pickerInput( inputId="dataFilters_select_changeyear_2",					 
                                                                label="Last Year:",
                                                                choices = yrs[2:length(yrs)],      # choices do not include the first year
                                                                selected = yrs[length(yrs)])) )
                         )))
        
      ))
  })
  
  # ----------------- Shared data structures used in several widgets ---------------
  
  # This dataset has all filter applied.
  # One row per CU, either original metric values or change in metric values (if change selected)
  # row.names set to CU names (i.e., Base.Unit.CU.ShortName)
  data.filtered <- reactive({
    # Filter by species
    df <- data.start  %>% filter(Base.Unit.Species %in% values$dataFilters_select_species) %>% dplyr::select_if(sumfun)
    # Filter by watershed
    df <- df %>% filter(BaseUnit.Watershed %in% values$dataFilters_select_watershed) %>% dplyr::select_if(sumfun)
    # Filter by FAZ
    df <- df %>% filter(FAZ %in% values$dataFilters_select_FAZ) %>% dplyr::select_if(sumfun)
    # Filter by Management Timing
    df <- df %>% filter(Management.Timing %in% values$dataFilters_select_management_timing) %>% dplyr::select_if(sumfun)
    # Filter by CU
    df <- df %>% filter(Base.Unit.CU.ShortName %in% values$dataFilters_select_CUs) %>% dplyr::select_if(sumfun)

    # calculate change in metric values if "change" selected
    if(values$dataFilters_select_change == "Change" ){                     ########## WILL NEED TO SET THIS UP SO IT UPDATES METRICS AUTOMATICALLY WITHOUT CHANGING THIS - LOOK TO METRICS FILE FOR LIST OF NAMES
      func <- function(x){x-dplyr::lag(x, default=dplyr::first(x))}
      
      df <- df %>% group_by(Base.Unit.CU.ShortName) %>%
        filter(Year %in% c(input$dataFilters_select_changeyear_1, input$dataFilters_select_changeyear_2)) %>%
        arrange(Year, .by_group=TRUE) %>%
        mutate(WSP.numeric = as.numeric(WSP.status)) %>%
        dplyr::mutate_at(.vars = vars(Recent.Total, Lower.Ratio, Upper.Ratio, LongTerm.Ratio, ShortTerm.Trend, Recent.ER, WSP.numeric), .funs= func) %>%
        filter(Year== max(Year)) %>%
        select(-WSP.status)
    } else { # use annual values
      df <- df %>% filter(Year %in% values$dataFilters_select_year)
    }
    if (!is.null(df)) {df <- df %>% select(-Year)}
    
    # remove any metrics and categories the user doesn't want to see
    # make this general by keeping everything that's not explicitly excluded
    drops <- c(as.character(CUMetrics[!(CUMetrics %in% values$dataFilters_select_metrics)]), 
               as.character(CUAttributes[!(CUAttributes %in% values$dataFilters_select_metrics)]))
    df <- df %>% select(-drops)
    df <- as.data.frame(df)
    row.names(df) <- df$Base.Unit.CU.ShortName
    df
  })
 
  # a shared datastructure for use with crosstalk communication
  sharedDS <- SharedData$new(data.filtered, group="CUmetrics")
  
  # Same as data.filtered(), but all rows removed that aren't currently selected
  data.selected <- reactive({
    if (any(sharedDS$selection())) {
      df <- data.filtered() %>% filter(Base.Unit.CU.ShortName %in% row.names(sharedDS$data()[sharedDS$selection(),]))
    }
    else{df <- data.filtered()}
  })
  
  # use this to get selection of rows 'shiny-style', i.e., as indices of selected rows
  data.selectedRows <- reactive({
    if(!is.null(sharedDS$selection())) {
      return(which(sharedDS$selection()))
    } else {
      return(NULL)
    }
  })
  
  # helper function for converting representation of a selection of rows in a Shiny input to
  # the corresponding selection in crosstalk:
  # creates a vector of TRUE/FALSE values, given the indices of the true values 
  # and the length of the output vector
  # return NULL if trueIndices = NULL
  makeBoolVect <- function(trueIndices, len) {
    if (!is.null(trueIndices) && len > 0) {
      v <- rep(FALSE, len)
      v[trueIndices] <- TRUE
      return(v)
    }
    else {
      return(NULL)
    }
  }
  
  # set the data selection from outside crosstalk
  # note: current selection is stored in crosstalk object sharedDS
  data.setSelection <- function(inds) {
    sharedDS$selection(value = makeBoolVect(inds, nrow(isolate(sharedDS$data()))))
  }
  
  #------------------- Parallel Coordinate Plot ------------------
  
  # Create data for the parallel plot (reorder columns and rows)
  data.parcoords <- reactive({
    df <- arrangeColumns(data.filtered(), colOrder=metricOrderParcoords)
    # Must re-order rows so most full rows are first and also have a full row as the first (no NAs)
    df[order(rowSums(is.na(df))),]
  })
  
  # create a shared dataset for use with crosstalk, link to sharedDS by giving it same group
  sharedDS.parcoords <- SharedData$new(data.parcoords, group="CUmetrics")
  
  # create dimensions list with auxiliary information on numeric metrics to pass on to parcoords
  # each element in dims is a list with a set of parameters specific to dims[[metric]], where 'metric'
  # is one of the metrics included in the parcoords dataset
  dims <- reactive({
    dataset <- data.parcoords()
    metrics <- names(dataset)
    names(metrics) <- metrics
    lapply(metrics, 
           function(m) {
             d <- list() # add any information on metric m here that we want to pass on to javascript
             # if there is a checkbox for this dim; allow it to set visibility, otherwise make it always visible
             d[['hide']] <- ifelse (any(names(input) == sId("parcoords_visible", m)), !input[[sId("parcoords_visible", m)]], FALSE) 
             d[['title']] <- getLabel(m)
             if (m %in% numericMetrics(dataset)) {
               d[['nullValue']] <- median(dataset[, m], na.rm = T) # change this to "top" or "bottom" to show nulls above or below chart
               d[['min']] <- min(dataset[, m], na.rm = T)
               d[['max']] <- max(dataset[, m], na.rm = T)
               d[['info']] <- metricInfo[[m]]
                if (sId("parcoords_yrange", m) %in% names(input)) { # if there is an input widget for this dim, allow for it to set the ylims
                 d[['ymin']] <- input[[sId("parcoords_yrange", m)]][1]
                 d[['ymax']] <- input[[sId("parcoords_yrange", m)]][2]
               } else { # ylims not under user control
                 d[['ymin']] <- d[['min']]
                 d[['ymax']] <- d[['max']]
               }
             } else {
               if (is.factor(dataset[ ,m])) {
                 # maintain the order of values in the parcoords plot
                 d[['ordering']] <- levels(dataset[ ,m])
               }
             }
             
             d
           })
  })
 
  observeEvent({input$parcoords_reset_brush}, {sharedDS.parcoords$selection(NULL)})
  
  output$parcoords_Plot <- renderParcoords({ parcoords(data=sharedDS.parcoords,
                                                  autoresize=TRUE,
                                                  color= list(colorScale=htmlwidgets::JS("d3.scale.category10()"), colorBy="Management.Timing"),
                                                  rownames=T,
                                                  alpha=0.6, 
                                                  alphaOnBrushed = 0,
                                                  brushMode="1D-axes-multi",
                                                  brushPredicate="and",
                                                  reorderable = TRUE, 
                                                  dimensions=dims(),
                                                  nullValueSeparator="nullValue")})

  # Create a block with miscellaneous controls for the parcoords plot
  output$parcoords_Controls <- renderUI({
    df <- data.parcoords()
    metrics <- names(data.parcoords())
    names(metrics) <- metrics 
    d <- dims()
    # control widgets for categorical metrics
    catWidgets <- lapply(metrics[!(metrics %in% numericMetrics(df))], 
                         function(m) { 
                           column(2,tags$div(title=metricInfo[[m]],
                                             checkboxInput(inputId = sId("parcoords_visible", m), 
                                                           label=getLabel(m),
                                                           value=!d[[m]][['hide']], width='20px'))) })
    # control widgets for numerical metrics
    numWidgets <- lapply(metrics[metrics %in% numericMetrics(df)], 
                         function(m) { 
                           column(2,tags$div(title=metricInfo[[m]],
                                             checkboxInput(inputId = sId("parcoords_visible", m), 
                                                           label=getLabel(m),
                                                           value=!d[[m]][['hide']], width='20px'),
                                             sliderInput(inputId = sId("parcoords_yrange", m),
                                                         label = NULL,
                                                         min = d[[m]][['min']],
                                                         max = d[[m]][['max']],
                                                         value = c(d[[m]][['ymin']],d[[m]][['ymax']]), 
                                                         width='80px')))})
    fluidRow(do.call(tagList, c(numWidgets, catWidgets)))})

  # reset parcoords graph, 
  # by resetting sliders and thereby triggering corresponding changes in dims()
  observeEvent({
    input$parcoords_reset_brush
    values$dataFilters_select_change
    values$dataFilters_select_metrics}, {
    df <- data.parcoords()
    for (m in names(df)) {
      if (sId("parcoords_yrange", m) %in% names(input)) {
        updateSliderInput(session, sId("parcoords_yrange", m), 
                          value = c(min(df[, m], na.rm=T),max(df[, m], na.rm=T)))
      }
    }
  })
  
  # scale parcoords graph axes to current selection, 
  # by setting sliders and thereby triggering corresponding changes in dims()
  observeEvent({input$parcoords_scale_to_selected}, {
      df <- data.selected()
      if (nrow(df) > 0) {
        for (m in names(df)) {
          if (sId("parcoords_yrange", m) %in% names(input)) { # adjust the sliders so ylims correspond to range of selected data
            updateSliderInput(session, sId("parcoords_yrange", m), 
                              value = c(min(df[, m], na.rm=T),
                                        max(df[, m], na.rm=T)))
          }
        }
      }
    })
  
  output$box_Parcoords <- renderUI({ 
    tagList( tags$div('style' = "text-align:right;", 
                      actionButton(inputId = "parcoords_reset_brush",
                                   label="Reset Brushing",icon("paper-plane"), 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4, height:70px; width:180px; font-size: 130%"),
                      actionButton(inputId = "parcoords_scale_to_selected",
                                   label="Scale to Selected",icon("search-plus"), 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4, height:70px; width:180px; font-size: 130%")),
             
             parcoordsOutput("parcoords_Plot", height="600px"),           # 400px is defaultheight
             uiOutput("parcoords_Controls"))
  })
  
  #------------------- Radar Plots ------------------
  
  # update available selections for the radar plot metrics
  currentRadarMetricOpts <- reactive({ names(data.filtered())[names(data.filtered()) %in% radarMetricOpts] })
  
  # Re-scale function adapted from ezR package (devtools::install_github("jerryzhujian9/ezR")
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
  
  # convert polar to cartesian coordinates
  cartesian <- function(r, phi = NULL) {
    if (is.null(phi)) {phi <- cumsum(rep(360/length(r), length(r))) - 360/length(r)}
    data.frame(x = r*cos(pi/180 * phi), y=r*sin(pi/180 * phi))
  }
  
  # calculate the area of the polygon defined by the spider plot of m,
  # where m is a vector of metrics
  # Note: I don't think this is right? should be 1/2 * sin(120 * pi/180) * ... = 1/2 * sin(pi * 2/3) * ...
  # calcArea3 <- function(m) {sin(pi/5)/2 * (m[1]*m[2] + m[2]*m[3] + m[3]*m[1])}

  # use sp package to calculate area of polygon defined by the spider plot of m,
  # where m is a vector of metrics
  calcArea <- function(m) {
    if (!is.null(m) && length(m) >= 3) {
      p <- Polygon(cartesian(c(m, m[1]))) # Polygon expects the first point to be repeated at the end
      p@area # Polygon is an R4 class
    } else {
      NA
    }
  } 
  
  # Radar coordinates Helper Funciton so does not need to access ezR package
  coord_radar <- function (theta = "x", start = 0, direction = 1)
  {
    theta <- match.arg(theta, c("x", "y"))
    r <- ifelse (theta == "x", "y", "x")
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
                   direction = sign(direction),
                   is_linear = function(coord) TRUE)
  }
  
  observeEvent(currentRadarMetricOpts(), { 
    if (is.null(currentRadarMetricOpts()) || length(currentRadarMetricOpts()) == 0) {
      selected <- NULL
    } else if (length(currentRadarMetricOpts()) < 3) {
      selected <- currentRadarMetricOpts()[1:length(currentRadarMetricOpts())]
    } else {
      selected <- currentRadarMetricOpts()[1:3]
    }
    updatePickerInput(session, "radar_select_metrics", choices=currentRadarMetricOpts(), selected=selected)
  })
  
  observeEvent(input$radar_select_metrics, {
    updatePickerInput(session, "radar_ranking", 
                      choices=c("Area", input$radar_select_metrics), 
                      selected = "Area")
  })
  
  # Pull selected metrics data
  radar_metrics_subset <- reactive({
    req(input$radar_select_metrics)
    df <- data.selected()[,input$radar_select_metrics]
    # Filter our CUs with nas in any of the metrics for Radar plot
    df <-  na.omit(df)   
    if (is.null(df) || (ncol(df) == 0) || (nrow(df) == 0)) {return(NULL)}
    # rescale
    df <- as.data.frame(apply(df, 2, ez.rescale02))
     # calc areas
    df$Area <- as.numeric(apply(df, 1, function(x){calcArea(x)}))
     # sort
    df <- df[order(df[,input$radar_ranking], decreasing=T), ]
    df$CU <- ordered(row.names(df), levels = row.names(df))
    df
   })
  
  generateRadarPlot <- function(df, faceted = T) {
    # Long format
    df.long <- tidyr::gather(df, metricName, metricValue,-"CU")
    df.long$metricName <- sapply(df.long$metricName, getLabel)
    p <- ggplot(df.long,  aes(x = metricName, y = metricValue,
                              group=CU, color=CU,fill=CU, linetype=CU)) + 
                geom_polygon(aes(), alpha=0.4, size = 1, show.legend = FALSE) +  xlab("") + ylab("") + coord_radar() +
                scale_linetype_manual(values=rep("solid",nlevels(df.long$CU)))
  
    if(faceted){
        p <- p + theme(strip.text.x = element_text(size = rel(1)),
                       axis.ticks.x = element_blank(),
                       axis.text.x = element_text(size = rel(0.5)),
                       axis.ticks.y = element_blank(),
                       axis.text.y = element_blank()) +
                 guides(color = "none") +
                 facet_wrap(~CU)
     } else {
       p <- p + theme(axis.text.x = element_text(size = rel(0.8), angle = 0),
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank()) +
                guides(color = guide_legend(ncol=2)) +
                geom_line(aes(), size = 1)
     }
    return(p)
  }
  
  #  create contents of radar plot box 
  observeEvent({
    input$radar_faceted
    radar_metrics_subset()
  }, {
    df <- radar_metrics_subset()
    if (is.null(df) || ncol(df) < 5) {
      # showModal(modalDialog("At least 3 metrics are required for Radar Plot!", easyClose = TRUE))
      output$radar_Plot <- NULL
      output$radar_AreaTable <- NULL
    } else {
      metrics <- names(df)[!(names(df) %in% c("CU", "Area"))]
      # fetch the original metrics for the table - don't want to confuse users by showing scaled
      dfForTable <- cbind(Area=df$Area, data.selected()[row.names(df), metrics])
      names(dfForTable) <- sapply(names(dfForTable), getLabel)
      output$radar_AreaTable <- DT::renderDataTable({DT::datatable(dfForTable)})
      df$Area <- NULL
      output$radar_Plot <- renderPlot({generateRadarPlot(df, input$radar_faceted)})
    }
  })
  
  # UI for radar plots box
  output$box_RadarPlots <- renderUI({
    if (is.null(currentRadarMetricOpts())) {
      selected <- NULL
    } else if (length(currentRadarMetricOpts()) <3 ) {
      selected <- currentRadarMetricOpts()[1:length(currentRadarMetricOpts())]
    } else {
      selected <- currentRadarMetricOpts()[1:3] 
    }
    tagList(
      h2("Radar plots of selected data"),
      h5("CU metrics are plotted in proportion to each other. Metric scores have been inverted so that larger triangles depict lower scores. Only CUs with all 3 metrics are shown."),
      br(),
      h4("Select metrics for radar plots:"),
      fluidRow(
        column(width=4,
               pickerInput(inputId = "radar_select_metrics",
                           label = "Select at least 3 of:",
                           choices = currentRadarMetricOpts(),
                           selected = selected,
                           multiple=TRUE,
                           options= list(`show-tick`=TRUE, `actions-box`=TRUE, `selected-text-format`='count', `max-options`='3'))),
        column(width=3, 
               checkboxInput(inputId = "radar_faceted",
                             label = "One plot per CU:",
                             value = TRUE)),
        column(width=4,
               pickerInput(inputId = "radar_ranking",
                           label = "Order by",
                           choices = c("Area", selected),
                           selected = c("Area"),
                           multiple=FALSE))),
      plotOutput("radar_Plot", height="550px", width="550px"),
      tags$div(style = 'overflow-x: scroll', DT::dataTableOutput("radar_AreaTable", width="70%"))
    )
  })
  
  #------------------- Extracted Data Box ------------------
  
  # Show the filtered data in a table.
  # Ideally, we'd just use crosstalk here, i.e.,
  # output$selectedData_Table <-  DT::renderDataTable({DT::datatable(sharedDS)}, server=FALSE)
  # but DT shows selected values (interprets selection as filter?), instead of 
  # showing full dataset with selected values highlighted. Work around this by using plain
  # shiny for data selection in table for now.
  output$selectedData_Table <- DT::renderDataTable({
    sel <- isolate(data.selectedRows())
    colnames <- as.character(sapply(names(data.filtered()), getLabel))
    if (!is.null(sel)) {
      datatable(data.filtered(), selection=list(selected=sel), colnames=colnames)
    } else {
      datatable(data.filtered(), colnames=colnames)
    }
   }, server=FALSE)
  
  # use this handle to update datatable in response to events
  proxyTableSelectedData <- dataTableProxy('selectedData_Table')
  
  observeEvent(sharedDS$selection(), {
    proxyTableSelectedData %>% selectRows(data.selectedRows())},
               ignoreNULL = FALSE) # make sure this handler fires when selection is reset to NULL
  
  # set crosstalk selection in response to Shiny selection through datatable widget
  observeEvent({input$selectedData_Table_rows_selected}, {
    data.setSelection(input$selectedData_Table_rows_selected)
  })
  
  # Downloadable csv of selected dataset
  output$selectedData_Download <- downloadHandler(
    filename = "selection.csv", 
    content = function(file) {
      selected <- sharedDS$selection()
      if(is.null(sharedDS$selection())) {
        write.csv(sharedDS$data(), file, row.names = TRUE)
      } else {
        write.csv(sharedDS$data()[sharedDS$selection(), ], file, row.names = TRUE)
      }
    })
  
  output$box_Data <- renderUI({ 
    tagList(tags$div('style' = "text-align:right;", downloadButton("selectedData_Download", "Download")),
            DT::dataTableOutput("selectedData_Table", width="50%"))
  })
  
  
  #------------------- Map  ------------------
  
  # attach labels and lat-long info 
  data.spatial <- reactive({withLatLong(withLabels(data.filtered()))})
  
  # create a shared dataset for use with crosstalk 
  # link sharedDSspatial to sharedDS by giving it the same group as sharedDS
  sharedDSspatial <- SharedData$new(data.spatial, group="CUmetrics")
  
  #colorPal <- colorFactor(c("black", "red", "green", "blue"), 
  #                        domain=c("Early_Summer", "Summer", "Late", "Estu"))
  
  output$CUmap <- renderLeaflet({ 
    leaflet(sharedDSspatial) %>%
    addTiles() %>%
    addCircleMarkers(lat = ~lat, lng = ~ long,
                     color = "black",
                     layerId = ~labels,
                     label = ~htmlEscape(labels) 
    #                color = ~colorPal(Management.Timing),
    #                stroke = FALSE,
    #                fillOpacity = 0.4
    )
    #addLegend(pal=colorPal,
    #          values=~Management.Timing,
    #          position="bottomleft")
  })
  

  # toggle CU selection when corresponding marker is clicked
  observeEvent(input$CUmap_marker_click, 
               {
                 # get current selection from crosstalk shared data
                 CUs <- sharedDSspatial$key()
                 sel <- sharedDSspatial$selection()
                 if (is.null(sel)) {sel <- rep(TRUE, length(CUs))} # a NULL selection means everything is selected
                 names(sel) <- CUs
                 # toggle selection
                 sel[input$CUmap_marker_click$id] <- !sel[input$CUmap_marker_click$id]
                 if(all(sel)) {sel <- NULL}
                 # set the crosstalk selection
                 sharedDSspatial$selection(sel)
               }
  )

  output$box_LeafletMap <- renderUI({leafletOutput("CUmap", height = 500)})
  
  #-------------------  Histogram Summary  ------------------
  
  dotHistogram <- function(ds, cat, selected=NULL, customIntervals=NULL) {
    if (is.SharedData(ds)) {
      selected <- ds$selection()
      ds <- as.data.frame(ds$data())
    } 
    dcol <- ds[ ,cat]
    n <- length(dcol)
    if (is.numeric(dcol)) { # convert to factor by binning
      if (!is.null(customIntervals)) {
        dcol <- cut(dcol, breaks=customIntervals[['breaks']], labels=customIntervals[['names']])
      } else { # use hist defaults 
        dcol <- cut(dcol, breaks=hist(ds[ ,cat])$breaks)
      }
    }
    cats <- levels(factor(dcol, exclude=NULL))
    cats[is.na(cats)] <- 'NA'
    ds$x <- ds$y <- rep(NA, n)
    if (is.null(selected)) {selected <- rep(FALSE, n)} 
    for (i in 1:length(cats)) {
      if (cats[i] == 'NA') {
        cinds <- is.na(dcol)
      } else {
        cinds <- !is.na(dcol) & (dcol == cats[i])
      }
      ds$y[cinds] <- i 
      sels <- cinds & selected # the selected rows in this category
      not.sels <- cinds & !selected # the rows in this category not selected
      n.sels <- sum(sels)
      n.notsels <- sum(not.sels)
      if (any(sels)) { ds$x[sels] <- 1:n.sels } # first show the selected points
      if (any(not.sels)) { ds$x[not.sels] <- (n.sels+1):(n.sels+n.notsels) } # now the unselected
    }
    ds$color <- ifelse(selected, "red", "white")
    ds$label <- row.names(ds)
    # create a shared dataset for use with crosstalk
    sharedHisto <- SharedData$new(ds, group="CUmetrics")
    p <- plot_ly(sharedHisto, x=~x, y=~y, height = 200, type="scatter", mode="markers", text=~label, hoverinfo="text",
                 marker = list(size = 10,
                               color = ~color,
                               line = list(color = 'rgba(0, 0, 0, .8)', width = 2))) %>%
      layout(yaxis = list(title="", 
                          zeroline = FALSE, 
                          tickvals = 1:length(cats), 
                          ticktext=cats, 
                          showgrid=FALSE),
             xaxis = list(visible = FALSE))
    p
  }
  
  histoSummaries <- reactive({
    req(input$dataFilters_select_change)
    intervalInfo <- customHistogramInfo[[input$dataFilters_select_change]]
    plots <- list()
    for (a in outputSummaryAttribs) {
      if (!is.null(intervalInfo[[a]])) {
        iInfo <- intervalInfo[[a]]
      } else {
        iInfo <- NULL
      }
      if (a %in% names(sharedDS$data())) {
          plots[[a]] <- dotHistogram(sharedDS, a, customIntervals=iInfo)
      }
    }
    plots
  })
  
  # recreate summaries if the selection changes
  observeEvent({
    sharedDS$data()
    sharedDS$selection()
    }, {
    for(a in names(histoSummaries())) {
        local({ 
          lc_a <- a
          output[[sId(lc_a, 'summary')]] <- renderPlotly({histoSummaries()[[lc_a]]}) 
        })
    }
  })
  
  # build the UI widgets
  output$box_HistoSummary <- renderUI({
    summaryCols <- lapply(histoSummaryAttribs, 
                          function(a) {column(width=4, tags$div(h4(getLabel(a)),
                                                                plotlyOutput(sId(a, 'summary'), height=200)))})
    fluidRow(do.call(tagList, summaryCols))
  })
  
  #------------------- All Data Tab ------------------
  
  
  output$allData_Table <- DT::renderDataTable({
    colnames <- as.character(sapply(names(data.start), getLabel))
    DT::datatable(data.start, colnames=colnames)
  })
  
  # Downloadable csv of selected dataset
  output$allData_Download <- downloadHandler(  
    filename = "data.csv",
    content = function(file) {write.csv(data.start, file, row.names = FALSE)}
  )
  
  
  
} # end server function
#   


