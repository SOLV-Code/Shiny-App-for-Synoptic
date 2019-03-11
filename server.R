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
not.empty <- function(x){sum(!is.na(x)) > 0}

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
  ds[, colOrder[!(colOrder %in% hide)], drop=F]
}

# ==========Define server components ================


# Define server logic 
function(input, output, session){

# ------------- Data filtering --------------------------  
  yrs <- sort(unique(as.numeric(data.start$Year)))
  values <- reactiveValues(dataFilters_select_year = max(data.start$Year), 
                           dataFilters_select_change = "Annual",
                           dataFilters_select_species = unique(as.character(data.start$Base.Unit.Species)),
                           dataFilters_select_watershed = unique(as.character(data.start$BaseUnit.Watershed)),
                           dataFilters_select_FAZ = unique(as.character(data.start$FAZ)),
                           dataFilters_select_management_timing = unique(as.character(data.start$Management.Timing)),
                           dataFilters_select_CUs = unique(as.character(data.start$Base.Unit.CU.ShortName)),
                           dataFilters_select_metrics = c(as.character(CUMetrics), as.character(CUAttributes)),
                           dataFilters_select_attribs = as.character(CUAttributes),
                           dataFilters_select_changeyear_1 = yrs[length(yrs) - 1],
                           dataFilters_select_changeyear_2 = yrs[length(yrs)])
  
  observeEvent(input$dataFilters_select_year, {values$dataFilters_select_year <- input$dataFilters_select_year}, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$dataFilters_select_change, {values$dataFilters_select_change <- input$dataFilters_select_change}, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$dataFilters_select_species, {values$dataFilters_select_species <- input$dataFilters_select_species}, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$dataFilters_select_watershed, {values$dataFilters_select_watershed <- input$dataFilters_select_watershed}, ignoreNULL = FALSE, ignoreInit = TRUE)  
  observeEvent(input$dataFilters_select_FAZ, {values$dataFilters_select_FAZ <- input$dataFilters_select_FAZ}, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$dataFilters_select_management_timing, {values$dataFilters_select_management_timing <- input$dataFilters_select_management_timing}, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$dataFilters_select_CUs, {values$dataFilters_select_CUs <- input$dataFilters_select_CUs}, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$dataFilters_select_metrics, {values$dataFilters_select_metrics <- input$dataFilters_select_metrics}, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$dataFilters_select_changeyear_1, {values$dataFilters_select_changeyear_1 <- input$dataFilters_select_changeyear_1}, ignoreInit = TRUE)
  observeEvent(input$dataFilters_select_changeyear_2, {values$dataFilters_select_changeyear_2 <- input$dataFilters_select_changeyear_2}, ignoreInit = TRUE)
  
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
  }, ignoreNULL = FALSE)
  
  # watershed limits what FAZs and CUs are available
  observeEvent(values$dataFilters_select_watershed, {
    df <- getFilteredDF(c('Base.Unit.Species', 'BaseUnit.Watershed', 'Management.Timing'), data.start)
    updatePickerInputs(c('FAZ', 'Base.Unit.CU.ShortName'), df) 
  }, ignoreNULL = FALSE)
  
  # FAZ an Management Timing limit what CUs are available
  observeEvent({
    values$dataFilters_select_FAZ
    values$dataFilters_select_management_timing
  }, {
    df <- getFilteredDF(c('Base.Unit.Species', 'BaseUnit.Watershed','Management.Timing', 'FAZ'), data.start)
    updatePickerInputs(c('Base.Unit.CU.ShortName'), df) 
  }, ignoreNULL = FALSE)
  

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
                                                                         selected = yrs[length(yrs)])),
                                  conditionalPanel("input.dataFilters_select_change == 'Change'",
                                                   pickerInput( inputId="dataFilters_select_changeyear_1",					 
                                                                label="Initial Year:",
                                                                choices = yrs[1:(length(yrs)-1)],  # choices do not include the last year
                                                                selected= yrs[length(yrs)-1]),
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
    df <- data.start
    # calculate change in metric values if "change" selected
    if(values$dataFilters_select_change == "Change" ){   
        calc.change <- function(x){x-dplyr::lag(x, default=dplyr::first(x))}
        df <- df %>% group_by(Base.Unit.CU.ShortName) %>%
              filter(Year %in% c(input$dataFilters_select_changeyear_1, input$dataFilters_select_changeyear_2)) %>%
              arrange(Year, .by_group=TRUE) %>%
              mutate(WSP.numeric = as.numeric(WSP.status)) 
        df <- df %>% dplyr::mutate_at(.vars=numericMetrics(df), .funs=calc.change) %>%
              filter(Year== max(Year)) %>%
              select(-WSP.status)
        if (!("WSP.status" %in% values$dataFilters_select_metrics)) { # don't keep WSP.numeric column unless WSP.status is of interest
          df <- df %>% select(-WSP.numeric)
        }
    } else { # use annual values
      df <- df %>% filter(Year %in% values$dataFilters_select_year)
    }
    df <- as.data.frame(df)
    row.names(df) <- df$Base.Unit.CU.ShortName
    
    # filter data by row, based on filter criteria 
    selection <- df$Base.Unit.Species %in% values$dataFilters_select_species & 
                 df$BaseUnit.Watershed %in% values$dataFilters_select_watershed &
                 df$FAZ %in% values$dataFilters_select_FAZ & 
                 df$Management.Timing %in% values$dataFilters_select_management_timing & 
                 df$Base.Unit.CU.ShortName %in% values$dataFilters_select_CUs
    df <- df[selection, ]
    df[ ,"Base.Unit.CU.ShortName"] <- NULL
    # remove any metrics and categories the user doesn't want to see
    # make this general by keeping everything that's not explicitly excluded
    drops <- c(as.character(CUMetrics[!(CUMetrics %in% values$dataFilters_select_metrics)]), 
               as.character(CUAttributes[!(CUAttributes %in% values$dataFilters_select_metrics)]))
    # additional columns to drop
    drops <- c(drops, "Year", "Base.Unit.CU.ShortName")
    df[ ,drops] <- NULL
    if (ncol(df) == 1) {
      df <- as.data.frame(df)
    }
    #if (ncol(df) < 2) {df <- NULL}
    df
  })
 
  # a shared datastructure for use with crosstalk communication
  #sharedDS <- SharedData$new(data.filtered, group="CUmetrics")
  
  # keep track of the current selection
  data.currentSelection <- reactiveVal(NULL)
  
  # Same as data.filtered(), but all rows removed that aren't currently selected
  data.selected <- reactive({
    #df <- sharedDS$data(withSelection = T)
    #df <- df[df$selected_, names(df) != 'selected_']
    if (is.null(data.currentSelection())) {
      data.filtered()
    } else {
      data.filtered()[data.currentSelection(), ]
    }
  })
  
  # use this to get selection of rows 'shiny-style', i.e., as indices of selected rows
  data.selectedRows <- reactive({
#    if(!is.null(sharedDS$selection())) {
#      return(which(sharedDS$selection()))
#    } else {
#      return(NULL)
#    }
    data.currentSelection()
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
  data.setSelection <- function(CUs) {
    data.currentSelection(CUs)
  }
  
  # add CUs to the current selection
  data.addToSelection <- function(CUs) {
    sel <- c(data.currentSelection(), CUs)
    data.currentSelection(unique(sel))
  }

  # remove CUs from the current selection
  data.removeFromSelection <- function(CUs) {
    if (any(!(data.currentSelection() %in% CUs))) {
      sel <- data.currentSelection()[!(data.currentSelection() %in% CUs)]
    } else {
      sel <- NULL 
    }
    data.currentSelection(sel)
    
  }
  
  #------------------- Parallel Coordinate Plot ------------------
  
  # Create data for the parallel plot (reorder columns and rows)
  data.parcoords <- reactive({
     df <- arrangeColumns(data.filtered(), colOrder=metricOrderParcoords)
    # Must re-order rows so most full rows are first and also have a full row as the first (no NAs)
    if (!is.data.frame(df)) {df <- NULL} 
    if (!is.null(df)) {
      df <- df[order(rowSums(is.na(df))), ,drop=FALSE]
    }
    df
  })
  
  # create a shared dataset for use with crosstalk, to get around issue with 
  # brushing when parcoords is called with a reactive pre-selection
  sharedDS.parcoords <- SharedData$new(data.parcoords, group="parcoords")
  
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
               if (nrow(dataset) > 0 && any(!is.na(dataset[, m]))) {
                  d[['nullValue']] <- median(dataset[, m], na.rm = T) # change this to "top" or "bottom" to show nulls above or below chart
                  d[['min']] <- min(dataset[, m], na.rm = T)
                  d[['max']] <- max(dataset[, m], na.rm = T)
                  d[['info']] <- metricInfo[[m]]
               } else {
                 d[['nullValue']] <- d[['min']] <- d[['max']] <- 0
                 d[['info']] <- 'no data values available'
               }
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
 
 
  output$parcoords_Plot <- renderParcoords({ p <- try(
                                                parcoords(data=sharedDS.parcoords,
                                                  autoresize=TRUE,
                                                  color= list(colorScale=htmlwidgets::JS("d3.scale.category10()"), colorBy="Management.Timing"),
                                                  rownames=T,
                                                  alpha=0.6, 
                                                  alphaOnBrushed = 0,
                                                  brushMode="1D-axes-multi",
                                                  brushPredicate="and",
                                                  reorderable = TRUE, 
                                                  dimensions=dims(),
                                                  #selectedRows = data.currentSelection(), #this works, but makes it impossible to brush more than one CU at a time
                                                  nullValueSeparator="nullValue"))
                                              if (inherits(p, "try-error")) {
                                                NULL
                                              } else {
                                                p
                                              }})

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
  
  # set selection on brushing
  observeEvent(sharedDS.parcoords$selection(), {
    if (is.null(sharedDS.parcoords$selection())) {
      selectedCUs <- NULL
    } else {
      selectedCUs <- row.names(sharedDS.parcoords$origData())[sharedDS.parcoords$selection()]
    }
    data.setSelection(selectedCUs)
  })
  
  observeEvent(input$parcoords_reset_brush, {
    data.setSelection(NULL)
  })
  
  # set the crosstalk selection in response to changes in the current selection
  observeEvent({
    data.currentSelection()
    input$parcoords_reset_brush
  } , {
    selected <- row.names(sharedDS.parcoords$origData()) %in% data.currentSelection()
    sharedDS.parcoords$selection(selected)
  }, ignoreNULL = F)
  
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
    if (length(data.currentSelection()) == 0L) {
      df <- data.filtered()
    } else {
      df <- isolate(data.selected())
    }
    metrics <- input$radar_select_metrics[input$radar_select_metrics %in% names(df)]
    df <- df[ ,metrics, drop=F]
    # Filter our CUs with nas in any of the metrics for Radar plot
    df <-  na.omit(df)   
    if (is.null(df) || (ncol(df) == 0) || (nrow(df) == 0)) {return(NULL)}
    # rescale
    if (nrow(df) == 1) {
      CU <- row.names(df)[1]
      df <- data.frame(ez.rescale02(df[1 ,]))
      row.names(df) <- c(CU)
    } else { 
      df <- as.data.frame(apply(df, 2, ez.rescale02))
    }
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
  
  # Since the radar plots render slowly, quick selection of individual CUs in one of the
  # other widgets can get the app to derail as the rendering tries to catch up to the changes
  # Using debounce doesn't fix it completely, but it helps.
  # May have to adjust the timer to make this work on the server  
  radar_metrics_subset_slow <- radar_metrics_subset %>% throttle(1000)
  
  #  create contents of radar plot box 
  observeEvent({
    input$radar_faceted
    data.currentSelection()
    radar_metrics_subset_slow()
  }, {
    df <- radar_metrics_subset_slow()
    if (is.null(df) || !is.data.frame(df)) {
      output$radar_Plot <- NULL
      output$radar_AreaTable <- NULL
    } else {
      metrics <- names(df)[!(names(df) %in% c("CU", "Area"))]
      # fetch the original metrics for the table - don't want to confuse users by showing scaled
      dfForTable <- cbind(Area=df$Area, data.filtered()[row.names(df), metrics])
      names(dfForTable) <- sapply(names(dfForTable), getLabel)
      output$radar_AreaTable <- DT::renderDataTable({DT::datatable(dfForTable)})
      df$Area <- NULL
      output$radar_Plot <- renderPlot({generateRadarPlot(df, input$radar_faceted)})
    }
  }, ignoreNULL = FALSE)
  
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
  # Note that using crosstalk here doesn't produce the desired result, since 
  # DT shows only selected values (interprets selection as filter?), instead of 
  # showing full dataset with selected values highlighted. 
  output$selectedData_Table <- DT::renderDataTable({
    selectedCUs <- isolate(data.currentSelection()) # only want to re-render this if data.filtered changes; selection handled by using proxy
    sel <- which(row.names(data.filtered()) %in% selectedCUs)
    colnames <- as.character(sapply(names(data.filtered()), getLabel))
    if (!is.null(sel)) {
      datatable(data.filtered(), selection=list(selected=sel), colnames=colnames)
    } else {
      datatable(data.filtered(), colnames=colnames)
    }
   }, server=FALSE)
  
  observeEvent(data.currentSelection(), {
    selectedCUs <- data.currentSelection()
    sel <- which(row.names(data.filtered()) %in% selectedCUs)
    dataTableProxy('selectedData_Table') %>% selectRows(sel)
  }, ignoreNULL = FALSE) # make sure this handler fires when selection is reset to NULL
  
  # set global selection 
  observeEvent({input$selectedData_Table_rows_selected}, {
    CUs <- row.names(data.filtered())[input$selectedData_Table_rows_selected]
    data.setSelection(CUs)
  }, ignoreNULL = FALSE)
  
  # Downloadable csv of selected dataset
  output$selectedData_Download <- downloadHandler(
    filename = "selection.csv", 
    content = function(file) {
      if(is.null(data.currentSelection())) {
        write.csv(data.filtered(), file, row.names = TRUE)
      } else {
        write.csv(data.selected(), file, row.names = TRUE)
      }
    })
  
  output$box_Data <- renderUI({ 
    tagList(tags$div('style' = "text-align:right;", downloadButton("selectedData_Download", "Download")),
            DT::dataTableOutput("selectedData_Table", width="50%"))
  })
  
  
  #------------------- Map  ------------------
  

  # some helper functions for putting together the html for the CU labels
  # css snipped for showing an arrow rotated by degrees from the horizonal
  makeArrow <- function(degrees) {
    cssStyle <- "-ms-transform:rotate(xxdeg); -webkit-transform:rotate(xxdeg); -moz-transform:rotate(xxdeg); -o-transform:rotate(xxdeg);"
    cssStyle <- gsub('xx', as.character(degrees), cssStyle)
    paste("<div style='", cssStyle, "'>&rarr;</div>", sep="")
  }
  
  # get the polar angle for representing the given change in y over an x distance of 1
  polarAngle <- function(dy) {atan2(dy, 1) * 180 / pi}
  
  # create one row with information on the given metric
  makePopupTableRow <- function(metric, end, start) {
    if (is.null(start)) {
      tr <- tags$tr(tags$td(metric), tags$td(as.character(end)), tags$td(''))
    } else {
      if (is.na(start) || is.na(end)) {
        tr <- tags$tr(tags$td(metric), tags$td('NA'), tags$td(''))
      } else {
        change <- (end - start)/start
        tr <- tags$tr(tags$td(metric), tags$td(end), tags$td(HTML(makeArrow(polarAngle(change)))))
      }
    }
    tr
  }
  
  # create an info pane for the given CU
  makePopup <- function(CU, endDf, startDf=NULL, metrics=mapLabelMetrics) {
    p <- tags$div(
      tags$style("table, th, td {padding: 5px;} table {border-spacing: 10px;}"),
      tags$b(CU),
      tags$table(lapply(metrics[metrics %in% names(endDf)], function(m) {
        if (is.null(startDf)) {
          makePopupTableRow(m, endDf[CU, m], NULL)
        } else {
          makePopupTableRow(m, endDf[CU, m], startDf[CU, m])
        }
      }))
    )
    p
  }
  
  makePopupCol <- function(dfEnd, dfStart=NULL, metrics=mapLabelMetrics) {
    sapply(row.names(dfEnd), function(CU) {
      gsub("[\n]", "", as.character(makePopup(CU, dfEnd, dfStart, metrics)))
    })
  }
  
  fishIcons <- iconList(
    'red-fish' = makeIcon("fish-red.png", "fish-red.png", iconHeight=24, iconWidth=24),
    'black-fish' = makeIcon("fish-black.png", "fish-black.png", iconHeight=24, iconWidth=24)
  )
  
  # marks up the spatial data frame to highlight currently selected items
  withSpatialSelection <- function(df) {
    # current selection, to be used as a group in leaflet
    df$selected <- ifelse(df$Base.Unit.CU.ShortName %in% isolate(data.currentSelection()), 'selected', 'not selected')
    # icons
    df$icon <- ifelse(df$selected == 'selected', 'red-fish', 'black-fish')
    df
  }
  
  data.spatial <- reactive({
    df <- data.filtered()
    if (is.data.frame(df) && nrow(df) > 0) {
      # attach the information in the lookup table
      df$Base.Unit.CU.ShortName <- row.names(df)
      df <- merge(df, data.spatialLookup, all.x=T, all.y=F, by=c("Base.Unit.CU.ShortName"))
      row.names(df) <- df$Base.Unit.CU.ShortName
      # put together a information pane for each CU, to be shown on mouse-over on the map
      if (values$dataFilters_select_year == data.years[1]) {
        df$popup <- makePopupCol(df, NULL)
      } else {
        df$popup <- makePopupCol(df, data.by.year[[data.years[1]]][row.names(df), ])
      }
      # add selection info
      df <- withSpatialSelection(df) 
      # CU polyons
      CUpolys <- sp::merge(data.CUpolygons, df, by=c("CU_INDEX"), all.x=FALSE, all.y=FALSE) 
      # groups for map; use species, and/or management timing if Fraser sox selected
      CUpolys$grp <- as.character(levelLabels[['Base.Unit.Species']][as.character(CUpolys$Base.Unit.Species)])
      if (("Management.Timing" %in% names(CUpolys)) && any(!is.na(CUpolys$Management.Timing)) ) {
        fraserSox <- CUpolys$Base.Unit.Species == "SK" & CUpolys$BaseUnit.Watershed == "Fraser"
        CUpolys$grp[which(fraserSox)] <-as.character(levelLabels[['Management.Timing']][as.character(CUpolys$Management.Timing[fraserSox])])
      }
      CUpolys$grp <- factor(CUpolys$grp, levels <- c(as.character(levelLabels[['Management.Timing']]),
                                                     as.character(levelLabels[['Base.Unit.Species']])))
      # jitter the marker locations so all will be visible
      CUpolys$latitude <- jitter(CUpolys$latitude, factor=10)
      CUpolys$longitude <- jitter(CUpolys$longitude, factor=10)
      CUpolys
    } else {
      NULL
    }
  })


  output$CUmap <- renderLeaflet({
    leaflet.data <- data.spatial()
    if (!is.null(leaflet.data)) {
      leaflet.data <- withSpatialSelection(leaflet.data)
      groups <- levels(leaflet.data$grp) 
      groups <- groups[groups %in% unique(leaflet.data$grp)]
      pal <- colorFactor("Spectral", levels=levels(leaflet.data$grp), ordered=T)
      leafletOutput <- try({leaflet(leaflet.data) %>%
                            addMarkers(lng=~longitude, lat=~latitude, 
                                       layerId = ~Base.Unit.CU.ShortName, 
                                       icon = ~fishIcons[icon],
                                       group = ~selected,
                                      label = ~lapply(popup, HTML)) %>%
                            addPolygons(fillColor=~pal(grp), 
                                        fillOpacity = 1,
                                        stroke=T,
                                        color="black",
                                        weight=2,
                                        opacity= 0.5, 
                                        layerId = ~Base.Unit.CU.ShortName,
                                        group = ~grp,
                                        label = ~lapply(popup, HTML),
                                        highlight = highlightOptions(weight = 10, color="red", bringToFront = TRUE))  %>% 
                            addTiles(group="base")  %>%
                            addLegend(position="bottomright", pal=pal, title="",
                                      values=~grp, 
                                      opacity=1) %>%
                            addLayersControl(overlayGroups = c("selected", "not selected", groups),
                                             options = layersControlOptions(collapsed = FALSE))
                          })
      if (!inherits(leafletOutput, "try-error")) {
         leafletOutput
      } else {
         NULL
      }
    } else {
      NULL
    }
  })
  
  # use this to make changes to the leaflet map without re-rendering the whole thing
  CUmapProxy <- leafletProxy('CUmap')
  
  # toggle CU selection when corresponding marker is clicked
  observeEvent(input$CUmap_marker_click, 
               {
                 CU <- input$CUmap_marker_click$id
                 if (CU %in% data.currentSelection()) {
                   data.removeFromSelection(CU)
                   icon <- fishIcons['black-fish']
                  } else {
                   data.addToSelection(CU)
                   icon <- fishIcons['red-fish']
                  }
                  marker <- isolate(data.spatial()[data.spatial()$Base.Unit.CU.ShortName == CU, ])
                  CUmapProxy %>% removeMarker(layerId = CU) %>%
                                 addMarkers(lng=marker$longitude, lat=marker$latitude, 
                                             layerId = CU, 
                                             icon = icon,
                                             group = 'selected',
                                             label = HTML(marker$popup))
               }
  )
  
  # we don't have a record of which markers need to change color, so redraw them all here
  observeEvent(data.currentSelection(), {
    df <- withSpatialSelection(data.spatial())
    CUmapProxy %>% addMarkers(data=df, lng=~longitude, lat=~latitude, 
                              layerId = ~Base.Unit.CU.ShortName, 
                              icon = ~fishIcons[icon],
                              group = ~selected,
                              label = ~lapply(popup, HTML))
  })
   
  output$box_LeafletMap <- renderUI({leafletOutput("CUmap", height = 500)})
  
  #-------------------  Histogram Summary  ------------------
  
  dotHistogram <- function(values, labels, selected=NULL, customIntervals=NULL) {
    n <- length(values)
    if (is.null(labels)) labels <- as.character(values)
    if (is.null(selected)) {selected <- rep(FALSE, n)}
    df <- data.frame(x=rep(NA, n), y=rep(NA, n), color=ifelse(selected, 'red', 'white'), label=labels)
    if (is.numeric(values)) { # convert to factor by binning
      if (!is.null(customIntervals)) {
        values <- cut(values, breaks=customIntervals[['breaks']], labels=customIntervals[['names']])
      } else { # use hist defaults 
        values <- cut(values, breaks=hist(df[ ,m])$breaks)
      }
    }
    bins <- levels(factor(values, exclude=NULL))
    bins[is.na(bins)] <- 'NA'
    for (i in 1:length(bins)) {
      if (bins[i] == 'NA') {
        cinds <- is.na(values)
      } else {
        cinds <- !is.na(values) & (values == bins[i])
      }
      df$y[cinds] <- i # bins are along the y-axis
      sels <- cinds & selected # the rows in this bin that are currently selected
      not.sels <- cinds & !selected # the rows in this bin that currently aren't selected
      n.sels <- sum(sels)
      n.notsels <- sum(not.sels)
      if (any(sels)) { df$x[sels] <- 1:n.sels } # first show the selected CUs
      if (any(not.sels)) { df$x[not.sels] <- (n.sels+1):(n.sels+n.notsels) } # now the unselected
    }
    # create a shared dataset for use with crosstalk
    #sharedHisto <- SharedData$new(df, group="CUmetrics")
    p <- plot_ly(df, x=~x, y=~y, height = 200, type="scatter", mode="markers", text=~label, hoverinfo="text",
                  marker = list(size = 10,
                                color = ~color,
                                line = list(color = 'rgba(0, 0, 0, .8)', width = 2))) %>%
          layout(yaxis = list(title="", 
                          zeroline = FALSE, 
                          tickvals = 1:length(bins), 
                          ticktext=bins, 
                          showgrid=FALSE),
           xaxis = list(visible = FALSE))
    p
  }
  
  histoSummaries <- reactive({
    intervalInfo <- customHistogramInfo[[values$dataFilters_select_change]]
    plots <- list()
    summaryAttribs <- histoSummaryAttribs[histoSummaryAttribs %in% values$dataFilters_select_metrics]
    for (a in summaryAttribs) {
      if (!is.null(intervalInfo[[a]])) {
        iInfo <- intervalInfo[[a]]
      } else {
        iInfo <- NULL
      }
      
      if (nrow(data.filtered()) > 0 && a %in% names(data.filtered())) {
          vals <- data.filtered()[ ,a]
          CUs <- row.names(data.filtered())
          selectedCUs <- CUs %in% data.currentSelection()
          if (is.numeric(vals)) {
            labels <- paste(CUs, '(', vals, ')', sep=" ")
          } else {
            labels <- CUs
          }
          plots[[a]] <- dotHistogram(vals, labels=labels, selected=selectedCUs, customIntervals=iInfo)
      } else {
        plots[[a]] <- NULL
      }
    }
    plots
  })
  
  # recreate summaries if the selection changes
  observeEvent({
    data.filtered()
    data.currentSelection()
    }, {
    local({ 
      summaries <- histoSummaries()
      for(a in names(summaries)) {
           output[[sId(a, 'summary')]] <- renderPlotly({summaries[[a]]}) 
      }
    })
  })
  
  # build the UI widgets
  output$box_HistoSummary <- renderUI({
    summaryAttribs <- histoSummaryAttribs[histoSummaryAttribs %in% values$dataFilters_select_metrics]
    summaryCols <- lapply(summaryAttribs, 
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


