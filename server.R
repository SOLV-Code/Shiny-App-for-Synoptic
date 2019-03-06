# Server funtion for the State of the Salmon Synoptic Status Evaluation Tool
# Developed by B. MacDonald
# Feb 13, 2019

#devtools::install_github("brigitte-dorner/parcoords")

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


dotHistogram <- function(ds, cat, selected=NULL) {
  if (is.SharedData(ds)) {
    selected <- ds$selection()
    ds <- ds$data()
  } 
  n <- nrow(ds)
  cats <- levels(as.factor(ds[, cat]))
  ds$x <- ds$y <- rep(NA, n)
  if (is.null(selected)) {selected <- rep(TRUE, n)}
  for (i in 1:length(cats)) {
    cinds <- ds[ ,cat] == cats[i] # all rows in this category
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
# ==========Define server components ================


# Define server logic 
function(input, output,session){

  #--------- ---------- Helper functions ------------------
  # assemble the id of a shiny input widget or a variable name from a prefix and a postfix, e.g. widget.1 
  sId <- function(pre, post) {paste(pre, post, sep=".")}
  # sum columns using select_if function of dplyr to remove empty columns
  sumfun <- function(x){sum(!is.na(x)) > 0}

  values <- reactiveValues(select_year = max(data.start$Year), 
                           select_change = "Annual",
                           select_species = unique(as.character(data.start$Base.Unit.Species)),
                           select_watershed = unique(as.character(data.start$BaseUnit.Watershed)),
                           select_FAZ = unique(as.character(data.start$FAZ)),
                           select_management_timing = unique(as.character(data.start$Management.Timing)),
                           select_CUs = unique(as.character(data.start$Base.Unit.CU.ShortName)),
                           select_metrics = as.character(CUMetrics),
                           select_attribs = as.character(CUAttributes))
  
  observeEvent(input$select_year, {values$select_year <- input$select_year})
  observeEvent(input$select_change, {values$select_change <- input$select_change})
  observeEvent(input$select_species, {values$select_species <- input$select_species})
  observeEvent(input$select_watershed, {values$select_watershed <- input$select_watershed})  
  observeEvent(input$select_FAZ, {values$select_FAZ <- input$select_FAZ})
  observeEvent(input$select_management_timing, {values$select_management_timing <- input$select_management_timing})
  observeEvent(input$select_CUs, {values$select_CUs <- input$select_CUs})
  observeEvent(input$select_metrics, {values$select_metrics <- input$select_metrics})

  data.filtered <- reactive({
    # Filter by species
    df <- data.start  %>% filter(Base.Unit.Species %in% values$select_species) %>% dplyr::select_if(sumfun)
    # Filter by watershed
    df <- df %>% filter(BaseUnit.Watershed %in% values$select_watershed) %>% dplyr::select_if(sumfun)
    # Filter by FAZ
    df <- df %>% filter(FAZ %in% values$select_FAZ) %>% dplyr::select_if(sumfun)
    # Filter by Management Timing
    df <- df %>% filter(Management.Timing %in% values$select_management_timing) %>% dplyr::select_if(sumfun)
    # Filter by CU
    df <- df %>% filter(Base.Unit.CU.ShortName %in% values$select_CUs) %>% dplyr::select_if(sumfun)
    # calculate change in metric values if "change" selected
    if(values$select_change == "Change" ){                     ########## WILL NEED TO SET THIS UP SO IT UPDATES METRICS AUTOMATICALLY WITHOUT CHANGING THIS - LOOK TO METRICS FILE FOR LIST OF NAMES
      func <- function(x){x-dplyr::lag(x, default=dplyr::first(x))}
      
      df <- df %>% group_by(Base.Unit.CU.ShortName) %>%
            filter(Year %in% c(input$select_changeyear_1, input$select_changeyear_2)) %>%
            arrange(Year, .by_group=TRUE) %>%
            mutate(WSP.numeric = as.numeric(WSP.status)) %>%
            dplyr::mutate_at(.vars = vars(Recent.Total, Lower.Ratio, Upper.Ratio, LongTerm.Ratio, ShortTerm.Trend, Recent.ER, WSP.numeric), .funs= func) %>%
            filter(Year== max(Year)) %>%
            select(-WSP.status)
    } else { # use annual values
      df <- df %>% filter(Year %in% values$select_year)
    }
    if (!is.null(df)) {df <- df %>% select(-Year)}
    
    # remove any metrics and categories the user doesn't want to see
    # make this general by keeping everything that's not explicitly excluded
    drops <- as.character(CUMetrics[!(CUMetrics %in% values$select_metrics)])
    df <- df %>% select(-drops)
  })
  
  # -- some helper functions and structures for nested filtering 
  # a list of data attributes and corresponding selector input widgets
  inputIds <- list('Base.Unit.Species' = 'select_species',
                   'BaseUnit.Watershed' = 'select_watershed',
                   'FAZ' = 'select_FAZ',
                   'Management.Timing' = 'select_management_timing',
                   'Base.Unit.CU.ShortName' = 'select_CUs')
  
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
  # associated with thegiven data attributes  
  getFilteredDF <- function(attribs, df) {
    selection <- rep(T, nrow(df))
    for (a in attribs) {
      selection <- selection & (df[, a] %in% values[[inputIds[[a]]]])
    }
    df[selection, ]
  }
  
  # -- logic for nested filtering of data:
  # all other filters are limited to what's available for the selected species
  observeEvent(values$select_species, {
    df <- getFilteredDF(c('Base.Unit.Species'), data.start)
    updatePickerInputs(c('BaseUnit.Watershed', 'FAZ', 'Management.Timing', 'Base.Unit.CU.ShortName'), df) 
  })
  
  # watershed limits what FAZs and CUs are available
  observeEvent(values$select_watershed, {
    df <- getFilteredDF(c('Base.Unit.Species', 'BaseUnit.Watershed', 'Management.Timing'), data.start)
    updatePickerInputs(c('FAZ', 'Base.Unit.CU.ShortName'), df) 
  })
  
  # FAZ an Management Timing limit what CUs are available
  observeEvent({
    values$select_FAZ
    values$select_management_timing
  }, {
    df <- getFilteredDF(c('Base.Unit.Species', 'BaseUnit.Watershed','Management.Timing', 'FAZ'), data.start)
    updatePickerInputs(c('Base.Unit.CU.ShortName'), df) 
  })
  

  observeEvent(                            # will need to update selections once we have more than 2 years so both cannot be same year
    {input$select_changeyear_1},{ 
      updateSelectInput(session, "select_changeyear_2", choices=unique(as.character( unique(data.start$Year[data.start$Year > input$select_changeyear_1]))), 
                        selected = unique(as.character(data.start$Year[data.start$Year > input$select_changeyear_1]))[1] )  
    })    
  
  observeEvent(                            # will need to update selections once we have more than 2 years so both cannot be same year
    {input$select_changeyear_2},{ 
      updateSelectInput(session, "select_changeyear_1", choices=unique(as.character( unique(data.start$Year[data.start$Year < input$select_changeyear_2]))), 
                        selected = unique(as.character(data.start$Year[data.start$Year < input$select_changeyear_2]))[1] )  
    }) 
  
  
  #------------------- Parallel Coordinate Plot ------------------
  
  # Create data for the parallel plot (add row names and reorder columns and rows)
  data.par <- reactive({
    df <- as.data.frame(data.filtered())   
    rownames(df) <- df[,1]  
    df <- df %>% select(-dplyr::one_of("Base.Unit.CU.ShortName", "Base.Unit.Species", "BaseUnit.Watershed")) %>%  # one_of allows you to provide a list including names that may not be there
      select(-Management.Timing, Management.Timing) %>%
      select(-FAZ, FAZ)
    # Must re-order rows so most full rows are first and also have a full row as the first (no NAs)
    df[order(rowSums(is.na(df))),]
  })
  
  # create a shared dataset for use with crosstalk
  sharedDS <- SharedData$new(data.par, group="CUmetrics")
  
  # create dimensions list with auxiliary information on numeric metrics to pass on to parcoords
  # each element in dims is a list with a set of parameters specific to dims[[metric]], where 'metric'
  # is one of the metrics included in the parcoords dataset
  dims <- reactive({
    dataset <- data.par()
    metrics <- names(dataset)
    names(metrics) <- metrics
    lapply(metrics, 
           function(m) {
             d <- list() # add any information on metric m here that we want to pass on to javascript
             # if there is a checkbox for this dim; allow it to set visibility, otherwise make it always visible
             d[['hide']] <- ifelse (any(names(input) == sId("visible", m)), !input[[sId("visible", m)]], FALSE) 
             d[['title']] <- getLabel(m)
             if (m %in% numericMetrics) {
               d[['nullValue']] <- median(dataset[, m], na.rm = T) # change this to "top" or "bottom" to show nulls above or below chart
               d[['min']] <- min(dataset[, m], na.rm = T)
               d[['max']] <- max(dataset[, m], na.rm = T)
               d[['info']] <- metricInfo[[m]]
                if (sId("yrange", m) %in% names(input)) { # if there is an input widget for this dim, allow for it to set the ylims
                 d[['ymin']] <- input[[sId("yrange", m)]][1]
                 d[['ymax']] <- input[[sId("yrange", m)]][2]
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
 
  observeEvent({input$reset_brush}, {sharedDS$selection(NULL)})
  
  output$parcoords <- renderParcoords({ parcoords(data=sharedDS,
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
  output$parcoordsControls <- renderUI({
    metrics <- names(data.par())
    names(metrics) <- metrics 
    d <- dims()
    # control widgets for categorical metrics
    catWidgets <- lapply(metrics[!(metrics %in% numericMetrics)], 
                         function(m) { 
                           column(2,tags$div(title=metricInfo[[m]],
                                             checkboxInput(inputId = sId("visible", m), 
                                                           label=m,
                                                           value=!d[[m]][['hide']], width='20px'))) })
    # control widgets for numerical metrics
    numWidgets <- lapply(metrics[metrics %in% numericMetrics], 
                         function(m) { 
                           column(2,tags$div(title=metricInfo[[m]],
                                             checkboxInput(inputId = sId("visible", m), 
                                                           label=m,
                                                           value=!d[[m]][['hide']], width='20px'),
                                             sliderInput(inputId = sId("yrange", m),
                                                         label = NULL,
                                                         min = d[[m]][['min']],
                                                         max = d[[m]][['max']],
                                                         value = c(d[[m]][['ymin']],d[[m]][['ymax']]), 
                                                         width='80px')))})
    fluidRow(do.call(tagList, c(numWidgets, catWidgets)))})

  
  brushed.data <- reactive({
    #    if(length(input$parcoords_brushed_row_names)>0){
    #      df <- data.filtered() %>% filter(Base.Unit.CU.ShortName %in% input$parcoords_brushed_row_names)
    if (any(sharedDS$selection())) {
      df <- data.filtered() %>% filter(Base.Unit.CU.ShortName %in% row.names(sharedDS$data()[sharedDS$selection(),]))
    }
    else{df <- data.filtered()}
    df
  })
  
  observeEvent({values$select_change}, {
    df <- data.par()
    for (m in names(df)) {
      if (sId("yrange", m) %in% names(input)) {
        updateSliderInput(session, sId("yrange", m), 
                          value = c(min(df[, m], na.rm=T),max(df[, m], na.rm=T)))
      }
    }
  })
  
  # scale parcoords graph axes to current selection 
  observeEvent({input$scale_to_selected}, {
      df <- brushed.data()
      if (nrow(df) > 0) {
        for (m in names(df)) {
          if (sId("yrange", m) %in% names(input)) { # adjust the sliders so ylims correspond to range of selected data
            updateSliderInput(session, sId("yrange", m), 
                              value = c(min(df[, m], na.rm=T),
                                        max(df[, m], na.rm=T)))
          }
        }
      }
    })
  
  #------------------- Radar Plots ------------------
  
  # update available selections for the radar plot metrics
  currentRadarMetricOpts <- reactive({
      return(names(data.filtered())[names(data.filtered()) %in% radarMetricOpts])
  })
  
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
    p <- Polygon(cartesian(c(m, m[1]))) # Polygon expects the first point to be repeated at the end
    p@area # Polygon is an R4 class
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
  
  observe({ 
    updateSelectInput(session, "radar_select_metric_1", choices=currentRadarMetricOpts(), selected=currentRadarMetricOpts()[1])
  })
  
  observeEvent({
    input$radar_select_metric_1
    currentRadarMetricOpts
  }, {
    choices <- currentRadarMetricOpts()[!(currentRadarMetricOpts() %in% input$radar_select_metric_1)]
    updateSelectInput(session, "radar_select_metric_2", choices = choices, selected = choices[1])
  })
  
  observeEvent({
    input$radar_select_metric_1
    input$radar_select_metric_2
    currentRadarMetricOpts
  }, {
    choices <- currentRadarMetricOpts()[!(currentRadarMetricOpts() %in% c(input$radar_select_metric_1, input$radar_select_metric_2))]
    updateSelectInput(session, "radar_select_metric_3", choices=choices, selected=choices[1])
  })
  
  observeEvent({
    input$radar_select_metric_1
    input$radar_select_metric_2
    input$radar_select_metric_3
  }, {
    updateSelectInput(session, "radar_ranking", 
                      choices=c("Area", input$radar_select_metric_1, input$radar_select_metric_2, input$radar_select_metric_3), 
                      selected = "Area")
  })
  
  # Pull selected metrics data
  radar_metrics_subset <- reactive({
    req(input$radar_select_metric_1, input$radar_select_metric_2, input$radar_select_metric_3, input$radar_ranking)
    df <- brushed.data() %>% select(dplyr::one_of("Base.Unit.CU.ShortName", input$radar_select_metric_1,
                                                  input$radar_select_metric_2, input$radar_select_metric_3))
    # Filter our CUs with fewer than 3 metrics for Radar plot
    df <-  df[rowSums(!is.na(df)) >= 4, ]             
    # rescale
    df <- data.frame(lapply(df, ez.rescale02))
    # calc areas
    df$Area <- apply(df, 1, function(x){calcArea(as.numeric(x[2:length(x)]))})
    # sort
    df <- df[order(df[,input$radar_ranking], decreasing=T), ]
    CUs <- as.character(df$Base.Unit.CU.ShortName)
    df$Base.Unit.CU.ShortName <- ordered(CUs, levels = CUs)
    df
   })
  
  generateRadarPlot <- function(df, faceted = T) {
    # Long format
    df.long <- tidyr::gather(df, metricName, metricValue,-"Base.Unit.CU.ShortName")
    df.long$metricName <- sapply(df.long$metricName, getLabel)
    p <- ggplot(df.long,  aes(x = metricName, y = metricValue,
                              group=Base.Unit.CU.ShortName,
                              color=Base.Unit.CU.ShortName,
                              fill=Base.Unit.CU.ShortName,
                              linetype=Base.Unit.CU.ShortName)) + 
                geom_polygon(aes(), alpha=0.4, size = 1, show.legend = FALSE) +  xlab("") + ylab("") + coord_radar() +
                scale_linetype_manual(values=rep("solid",nlevels(df.long$Base.Unit.CU.ShortName)))
  
    if(faceted){
        p <- p + theme(strip.text.x = element_text(size = rel(1)),
                       axis.ticks.x = element_blank(),
                       axis.text.x = element_text(size = rel(0.5)),
                       axis.ticks.y = element_blank(),
                       axis.text.y = element_blank()) +
                 guides(color = "none") +
                 facet_wrap(~Base.Unit.CU.ShortName)
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
    cols <- c("Base.Unit.CU.ShortName", "Area", names(df)[!(names(df) %in% c("Base.Unit.CU.ShortName", "Area"))])
    df.reorder <- df[, cols]
    names(df.reorder) <- sapply(names(df.reorder), getLabel)
    output$radarAreaTable <- DT::renderDataTable({DT::datatable(df.reorder)})
    df$Area <- NULL
    output$radarPlot <- renderPlot({generateRadarPlot(df, input$radar_faceted)})
  })
  
  
  #------------------- All Data Tab ------------------
  
  
  output$AllData <- DT::renderDataTable({
    colnames <- as.character(sapply(names(data.start), getLabel))
    DT::datatable(data.start, colnames=colnames)
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadAllData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {write.csv(data.start, file, row.names = FALSE)}
  )
  
  
  #------------------- Extracted Data Box ------------------
  
  # Show the filtered data in a table.
  # Ideally, we'd just use crosstalk here, i.e.,
  # output$SelectedData <-  DT::renderDataTable({DT::datatable(sharedDS)}, server=FALSE)
  # but DT shows selected values (interprets selection as filter?), instead of 
  # showing full dataset with selected values highlighted. Work around this by using plain
  # shiny for data selection in table for now.
  output$SelectedData <- DT::renderDataTable({
    sel <- isolate(selectedRows())
    colnames <- as.character(sapply(names(data.par()), getLabel))
    if (!is.null(sel)) {
      datatable(data.par(), selection=list(selected=sel), colnames=colnames)
    } else {
      datatable(data.par(), colnames=colnames)
    }
   }, server=FALSE)
  
  # update datatable in response to various events
  proxySelectedData <- dataTableProxy('SelectedData')
  
  # use this to get selection of rows 'shiny-style', i.e., as indices of selected rows
  selectedRows <- reactive({
    if(!is.null(sharedDS$selection())) {
      return(which(sharedDS$selection()))
    } else {
      return(NULL)
    }
  })
  
  observeEvent(sharedDS$selection(), {
    proxySelectedData %>% selectRows(selectedRows())},
               ignoreNULL = FALSE) # make sure this handler fires when selection is reset to NULL
  
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
  
  # set crosstalk selection in response to Shiny selection
  observeEvent({input$SelectedData_rows_selected}, {
    sharedDS$selection(value = makeBoolVect(input$SelectedData_rows_selected, 
                                            nrow(isolate(sharedDS$data()))))
  })
  

  
  # Downloadable csv of selected dataset ----
  output$downloadSelectedData <- downloadHandler(
    filename = "selection.csv", 
    content = function(file) {
      selected <- sharedDS$selection()
      if(is.null(sharedDS$selection())) {
        write.csv(sharedDS$data(), file, row.names = TRUE)
      } else {
        write.csv(sharedDS$data()[sharedDS$selection(), ], file, row.names = TRUE)
      }
    })
  
  
  
  #------------------- Summary Plots Box ------------------
  
  # Summary Plots tab
  summary.prep <- function(variable=variable, type=input$select_type, change=values$select_change){ # type = "Proportion" or "Number"
    #   subset_data <-subset(data.start, !duplicated(Base.Unit.CU.ShortName) )
    subset_data <- data.filtered()
    brushed_data <- brushed.data()
    
    if(variable == "Recent.ER"){
      if(change == "Annual"){
        breaks <- c( 0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
        names <- c("Below 10%","10-20%","20-30%","30%-40%","40-50%", "50%-60%","60-70%","70%-80%","80-90%","Above 90%")
      }
      if(change =="Change"){
        breaks <- c(-1, -0.1, -0.05, -0.01,0.01, 0.05, 0.1, 1)
        names <- c(">10% decrease", "5%-10% decrease", "0-5% decrease","No Change", "0-5% increase", "5-10% increase",">10 increase")
      }
      subset_data[,variable] <- cut(subset_data[,variable], breaks=breaks, labels=names)
      brushed_data[,variable] <- cut(brushed_data[,variable], breaks=breaks, labels=names)
    }
    
    subset_data[,variable] <- factor(subset_data[,variable], exclude=NULL)
    # subset_data[,variable] <- addNA(subset_data[,variable], ifany=TRUE)
    # brushed_data[,variable] <- addNA(brushed_data[,variable], ifany=TRUE)
    y <- factor(levels(subset_data[,variable]), ordered=TRUE, levels=levels(subset_data[,variable]), exclude=NULL)
    total <- as.vector(table(subset_data[,variable], useNA="ifany"))
    
    brush_full_selection <- factor(brushed_data[,variable], levels = levels(subset_data[,variable]), exclude=NULL)
    selected <- as.vector(table(brush_full_selection,useNA = "ifany"))
    not.selected <- total-selected
    perc <- selected/total
    
    if(type == "Proportion")  data.sum <- data.frame(y, selected=perc, unselected =(1-perc), text=c(paste("Total # of CUs:",as.character(total))))
    if(type =="Number")       data.sum <- data.frame(y, selected, unselected=not.selected, text=c(paste("Total # of CUs:",as.character(total))))
    data.sum$y <- forcats::fct_explicit_na(data.sum$y, "Unknown")
    
    # Create plot
    p <- plot_ly(data.sum, x = ~selected, y = ~y, text=~text, type = 'bar', orientation = 'h', name = 'Selected', 
                 marker = list(color = "darkred")) %>% 
      add_trace(x = ~unselected, name = 'Unselected', 
                marker = list(color = "rgba(128,128,128,0.6)")) %>%        # last number is transparenct;  colours for plotly @ https://reeddesign.co.uk/test/namedcolors.html
      layout(barmode = 'stack', xaxis = list(title = c(paste(input$selected_type, "of CUs"))), 
             yaxis = list(title = variable), margin = list(l = 130, r = 50, b = 50, t = 50, pad = 4))
    
  }
  
  # Percentage.Plots <- function(variable="Management.Timing"){
  observeEvent({
    brushed.data()
    input$select_type},{
      p.1 <- summary.prep(variable="Management.Timing", type=input$select_type, change=values$select_change)
      
      output$summaryPlot_MT <- renderPlotly({p.1})
      
      p.2 <- summary.prep(variable="FAZ", type=input$select_type,change=values$select_change)
      output$summaryPlot_FAZ <- renderPlotly({p.2})
      
      if(values$select_change=="Annual") p.3 <- summary.prep(variable="WSP.status", type=input$select_type, change=values$select_change)
      if(values$select_change=="Change") p.3 <- summary.prep(variable="WSP.numeric", type=input$select_type, change=values$select_change)
      output$summaryPlot_WSP <- renderPlotly({p.3})
      
      p.4 <-  summary.prep(variable="Recent.ER", type=input$select_type,change=values$select_change)
      
      output$summaryPlot_ER <- renderPlotly({p.4})
    }
  )
  
  #------------------- Map  ------------------
  
  # create a shared dataset for use with crosstalk, with labels and lat-long info attached,
  # and linked to sharedDS used with parcoords
  
  data.spatial <- reactive({withLatLong(withLabels(data.par()))})
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

  #-------------------  Summary  ------------------
  
  output$summary.Management.Timing <- renderPlotly({dotHistogram(sharedDS, "Management.Timing")})
  output$summary.FAZ <- renderPlotly({dotHistogram(sharedDS, "FAZ")})
  output$summary.WSP.Status <- renderPlotly({dotHistogram(sharedDS, "WSP.status")})
  #output$summary.Recent.ER <- renderPlotly({dotHistogram(sharedDS, "Recent.ER")})
  
  #-------------------  CU selection flow  ------------------
  
  output$filters <- renderUI({
    pickerOptsSingleSelect <- list(`show-tick`=TRUE)
    pickerOptsMultiSelect <- list(`show-tick`=TRUE, `actions-box`=TRUE, `selected-text-format`='count')
    wellPanelStyle <- "background: white"
    yrs <- unique(sort(as.numeric(data.start$Year)))
    tagList(
      fluidRow(
        column(width=5,
               wellPanel(style = wellPanelStyle, tags$b("Step1:",  "Filter your data"), tags$hr(),
                        fluidRow(column(width=5, tags$div('By Species:')),
                                 column(width=7, pickerInput(inputId="select_species",					 
                                                             label=NULL,
                                                             choices=unique(as.character(data.start$Base.Unit.Species)),
                                                             selected=unique(as.character(data.start$Base.Unit.Species))[1],
                                                             multiple=FALSE,
                                                             options=pickerOptsSingleSelect))),
                        fluidRow(column(width=5, tags$div('By Watershed:')),
                                 column(width=7, pickerInput(inputId="select_watershed",					 
                                                             label=NULL,
                                                             choices=unique(as.character(data.start$BaseUnit.Watershed)),
                                                             selected=unique(as.character(data.start$BaseUnit.Watershed)),
                                                             multiple=TRUE,
                                                             options=pickerOptsMultiSelect))),
                       fluidRow(column(width=5, tags$div('By FAZ:')),
                                column(width=7, pickerInput(inputId="select_FAZ",					 
                                                            label=NULL,
                                                            choices=unique(as.character(data.start$FAZ)),
                                                            selected=unique(as.character(data.start$FAZ)),
                                                            multiple=TRUE,
                                                            options=pickerOptsMultiSelect))),
                       fluidRow(column(width=5, tags$div('By Management Timing:')),
                                column(width=7, pickerInput(inputId="select_management_timing",					 
                                                            label=NULL,
                                                            choices=unique(as.character(data.start$Management.Timing)),
                                                            selected=unique(as.character(data.start$Management.Timing)),
                                                            multiple=TRUE,
                                                            options=pickerOptsMultiSelect))),
                       fluidRow(column(width=5, tags$div('By CU:')),
                                column(width=7, pickerInput(inputId="select_CUs",					 
                                                            label=NULL,
                                                            choices=unique(as.character(data.start$Base.Unit.CU.ShortName)),
                                                            selected=unique(as.character(data.start$Base.Unit.CU.ShortName)),
                                                            multiple=TRUE,
                                                            options=pickerOptsMultiSelect)))
                                )),
        column(width=3, 
               wellPanel(style = wellPanelStyle, tags$b("Step2:", "Select metrics of interest"), tags$hr(),
                         fluidRow(
                                column(width=12, pickerInput(inputId="select_metrics", 
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
                                column(width=6, radioButtons( "select_change",
                                                              label="",
                                                              choices = list("Single year" = "Annual", "Change over time" ="Change"),
                                                              selected="Annual") ),
                                column(width=6, conditionalPanel("input.select_change == 'Annual'",
                                                                 pickerInput( inputId="select_year",					 
                                                                              label="",
                                                                              choices = yrs,
                                                                              selected = yrs[1])),
                                                conditionalPanel("input.select_change == 'Change'",
                                                                 pickerInput( inputId="select_changeyear_1",					 
                                                                              label="Initial Year:",
                                                                              choices = yrs[1:(length(yrs)-1)],  # choices do not include the last year
                                                                              selected= yrs[1]),
                                                                 pickerInput( inputId="select_changeyear_2",					 
                                                                              label="Last Year:",
                                                                              choices = yrs[2:length(yrs)],      # choices do not include the first year
                                                                              selected = yrs[length(yrs)])) )
                        )))

      ))
  })
  
  output$leafletMap <- renderUI({leafletOutput("CUmap", height = 500)})
 
  output$parcoordsPlot <- renderUI({ 
    tagList( tags$div('style' = "text-align:right;", 
                       actionButton(inputId = "reset_brush",
                                    label="Reset Brushing",icon("paper-plane"), 
                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4, height:70px; width:180px; font-size: 130%"),
                       actionButton(inputId = "scale_to_selected",
                                    label="Scale to Selected",icon("search-plus"), 
                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4, height:70px; width:180px; font-size: 130%")),
                   
              parcoordsOutput("parcoords", height="600px"),           # 400px is defaultheight
              uiOutput("parcoordsControls"))
  })
             
  output$data <- renderUI({ 
    tagList(tags$div('style' = "text-align:right;", downloadButton("downloadSelectedData", "Download")),
            DT::dataTableOutput("SelectedData", width="50%"))
    })
    
  output$summary <- renderUI({
    tagList( 
      fluidRow(
        column(width=4, tags$div(h4("Management Timing"),
                                 plotlyOutput("summary.Management.Timing", height=200))),
        column(width=4, tags$div(h4("Freshwater Adaptive Zone"),
                                 plotlyOutput("summary.FAZ", height=200))),
        column(width=4, tags$div(h4("WSP Status"),
                                 plotlyOutput("summary.WSP.Status"), height=200))))
 #       column(width=6, tags$div(h4("Exploitation Rate"),
 #                                plotlyOutput("summary.Recent.ER")))))
  })
  
  output$radarBox <- renderUI({
    tagList(
      h2("Radar plots of selected data"),
      h5("CU metrics are plotted in proportion to each other. Metric scores have been inverted so that larger triangles depict lower scores. Only CUs with all 3 metrics are shown."),
      br(),
      h4("Select metrics for radar plots:"),
      fluidRow(
        column(width=4,
               selectInput(inputId = "radar_select_metric_1",
                           label = "",
                           choices = radarMetricOpts,
                           selected = radarMetricOpts[1],
                           multiple=FALSE)),
        column(width=4,
               selectInput(inputId = "radar_select_metric_2",
                           label = "",
                           choices = radarMetricOpts,
                           selected = radarMetricOpts[2],
                           multiple=FALSE)),
        column(width=4,
               selectInput(inputId = "radar_select_metric_3",
                           label = "",
                           choices = radarMetricOpts,
                           selected = radarMetricOpts[3],
                           multiple=FALSE))),
      fluidRow(
        column(width=3, 
               checkboxInput(inputId = "radar_faceted",
                             label = "Select faceting:",
                             value = TRUE)),
        column(width=3,
               selectInput(inputId = "radar_ranking",
                           label = "Order by",
                           choices = c("Area"),
                           selected = c("Area"),
                           multiple=FALSE))),
      plotOutput("radarPlot", height="550px", width="550px"),
      tags$div(style = 'overflow-x: scroll', DT::dataTableOutput("radarAreaTable", width="70%"))
    )
  })
  
} # end server function
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
#                                                     input$select_metric,
#                                                     ":",
#                                                     toString(incomplete()$Base.Unit.CU.ShortName) )
#                      })
#     }
# })
# # 
