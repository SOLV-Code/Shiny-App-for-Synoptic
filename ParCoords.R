#------------------- Parallel Coordinate Plot ------------------


# Create data for the parallel plot (reorder columns and rows)
parcoords.data <- reactive({
  df <- data.filtered()
  # sort the CUs (rows) in the data according to specified sort criteria
  for (sortKey in rev(ParcoordsCUOrder[ParcoordsCUOrder %in% names(df)])) {
    df <- df[order(df[, sortKey]), , drop=F]
  }
  # replace 'NA' with true NAs in factors
  # replace Inf with true NAs in numeric metrics
  for (n in names(df)) {
    if (is.factor(df[, n])) {
        old <- df[, n]
        df[!is.na(df[, n]) & df[ ,n] == 'NA', n] <- NA
        df[ ,n] <- factor(as.character(df[ ,n]), levels=levels(old)[levels(old) != 'NA'], ordered=is.ordered(old), exclude=NA)
    }
    if (is.numeric(df[, n])) {
      df[!is.finite(df[, n]), n] <- NA
    }
  }
  # identify all-NA columns and columns without contrast, i.e. columns where all values are the same
  colsToDrop <- apply(df, 2, function(c) {all(is.na(c)) || all(c == c[1])}) 
  names(colsToDrop) <- names(df)
  colsToDrop[is.na(colsToDrop)] <- FALSE # don't drop columns for which the function returned NA; these are columns that have a mix of NAs and data
  # clean up, add CU_ID and pretty row names
  df <- arrangeColumns(df, colOrder=ParcoordsMetricOrder, hide=c(names(df)[colsToDrop], ParcoordsDrop))
  df$CU_ID <- row.names(df)
  row.names(df) <- unlist(lapply(row.names(df), getCUname))
  
  # add colorAttrib column; this will always be hidden
  if (colorCtrl.colorScheme() %in% names(data.filtered())) {
    df$colorAttrib <-  unlist(lapply(df$CU_ID, function(cu) {
      data.filtered()[cu, colorCtrl.colorScheme()]})) 
  }
  else if (colorCtrl.colorScheme() %in% names(data.CU.Lookup.filtered())) {
    df$colorAttrib <- unlist(lapply(df$CU_ID, function(cu) {
      data.CU.Lookup.filtered()[data.CU.Lookup.filtered()$CU_ID == cu, colorCtrl.colorScheme()][1]}))
  }
  if (!is.data.frame(df)) {df <- NULL}
  df
})

# create a shared dataset for use with crosstalk, to get around issue with 
# brushing when parcoords is called with a reactive pre-selection
parcoords.sharedDS <- SharedData$new(parcoords.data, group="parcoords")

# keep track of axis settings
parcoords.axisSettings <- reactiveValues()

observeEvent(parcoords.data(), {
  df <- parcoords.data()
  metrics <- names(df)
  hidden <- ifelse(metrics %in% c(ParcoordsHideOnInit, 'colorAttrib'), TRUE, FALSE)
  names(hidden) <- metrics
  lapply(metrics, function(m) {
      parcoords.axisSettings[[sId("parcoords_visible", m)]] <- !hidden[m]
      observeEvent(input[[sId("parcoords_visible", m)]], {
        parcoords.axisSettings[[sId("parcoords_visible", m)]] <- input[[sId("parcoords_visible", m)]]
      }, ignoreNULL = T, ignoreInit = T)
  })
  lapply(metrics[metrics %in% numericMetrics(df)], function(m) {
      parcoords.axisSettings[[sId("parcoords_yrange", m)]] <- c(min(df[, m], na.rm = T), max(df[, m], na.rm = T))
      observeEvent(input[[sId("parcoords_yrange", m)]], {
        parcoords.axisSettings[[sId("parcoords_yrange", m)]] <- input[[sId("parcoords_yrange", m)]]
      }, ignoreNULL = T, ignoreInit = T)
    })
})

# create dimensions list with auxiliary information on numeric metrics to pass on to parcoords
# each element in dims is a list with a set of parameters specific to dims[[metric]], where 'metric'
# is one of the metrics included in the parcoords dataset
parcoords.dims <- reactive({
  dataset <- parcoords.data()
  metrics <- names(dataset)
  names(metrics) <- metrics
  lapply(metrics, 
         function(m) {
           d <- list() # add any information on metric m here that we want to pass on to javascript
           # if there is a checkbox for this dim; allow it to set visibility, otherwise make it always visible
           d[['hide']] <- ifelse (any(names(parcoords.axisSettings) == sId("parcoords_visible", m)), !parcoords.axisSettings[[sId("parcoords_visible", m)]], FALSE) 
           # color special: always keep hidden
           if (m == 'colorAttrib') d[['hide']] <- T 
           d[['title']] <- GetLabel(m)
           if (m %in% numericMetrics(dataset)) {
             if (nrow(dataset) > 0 && any(!is.na(dataset[, m]))) {
               #d[['nullValue']] <- 'bottom' # median(dataset[, m], na.rm = T) # change this to "top" or "bottom" to show nulls above or below chart
               d[['min']] <- min(dataset[, m], na.rm = T)
               d[['max']] <- max(dataset[, m], na.rm = T)
               d[['info']] <- MetricInfo[[m]]
               # Note: parcoords uses first data row to auto-detect data-type. It fails if there are any nas in the first row.
               # to avoid this issue, explicitly specify the data type for each dim here
               d[['type']] <- 'number' 
             } else {
               d[['nullValue']] <- d[['min']] <- d[['max']] <- 0
               d[['info']] <- 'no data values available'
             }
             if (sId("parcoords_yrange", m) %in% names(parcoords.axisSettings)) { # if there is an input widget for this dim, allow for it to set the ylims
               d[['ymin']] <- parcoords.axisSettings[[sId("parcoords_yrange", m)]][1]
               d[['ymax']] <- parcoords.axisSettings[[sId("parcoords_yrange", m)]][2]
             } else { # ylims not under user control
               d[['ymin']] <- d[['min']]
               d[['ymax']] <- d[['max']]
             }
           } else {
             d[['type']] <- 'string'
            # d[['nullValue']] <- 'bottom' # getmode(dataset[ ,m])
             if (is.factor(dataset[ ,m])) {
               # maintain the order of values in the parcoords plot
               d[['ordering']] <- levels(dataset[ ,m])
             }
           }
           d
         })
})

# assemble a javascript function that returns appropriate color values as specified in
# ColorPalette:
# function(x) {
#   var colPal = { category1: '#xxxxxx', category2: '#yyyyyy', category3: '#zzzzzz'}
#   return(colPal[x])
#}
parcoords.makeColorFunc <- function(colPal) {
  paste0("function(x) {var colPal = {", 
         paste0(unlist(lapply(names(colPal), function(n) {
           paste0(n , ": '", colPal[[n]], "'")})), collapse=', '),
         "}; return(colPal[x]);}")
}

output$parcoords_Plot <- parcoordsSoS::renderParcoords({ 
  p <- try({scheme <- colorCtrl.colorScheme() 
            if ('colorAttrib' %in% names(parcoords.data()))
              color <- list(colorScale=htmlwidgets::JS(parcoords.makeColorFunc(colorCtrl.getColors(scheme))), 
                            colorBy="colorAttrib")
            else
              color <- '#000000'
           # print(parcoords.data())
           # print(color)
            parcoordsSoS::parcoords(data = parcoords.sharedDS,
                                    autoresize = TRUE,
#                                   color = list(colorScale=htmlwidgets::JS("d3.scale.category10()"), colorBy="Species"),
                                    color = color,
                                    rownames = T,
                                    alpha = 0.6, 
                                    alphaOnBrushed = 0,
                                    brushMode = "1D-axes-multi",
                                    brushPredicate = "and",
                                    reorderable = TRUE, 
                                    margin = list(top = 50, 
                                                  bottom = 50, 
                                                  left= 200, 
                                                  right = 50),
                                    dimensions = parcoords.dims(),
                                    #selectedRows = data.currentSelection[['CUs']], #this works, but makes it impossible to brush more than one CU at a time
                                    nullValueSeparator = "nullValue",
                                    dimensionTitleRotation = ParcoordsLabelRotation)})
  if (inherits(p, "try-error")) {
    print('parcoords call failed!')
    NULL
  } else {
    p
  }
})

# make a togglel widget for turning an axis on and off
parcoords.makeToggleSwitch <- function(m) { 
  if (parcoords.dims()[[m]][['hide']])
    div_class <- 'sidebar-checkbox-inactive'
  else
    div_class <- 'sidebar-checkbox-active'
  tags$div(prettySwitch(inputId = sId("parcoords_visible", m),
                        label = GetLabel(m),
                        status = 'primary',
                        fill = TRUE,
                        value = parcoords.axisSettings[[sId("parcoords_visible", m)]]),
           class = div_class, 
           title = MetricInfo[[m]])
}

# make a control widget for a numeric metric
parcoords.makeNumCtrWidget <- function(df, m) { 
    toggleSwitch <- parcoords.makeToggleSwitch(m) 
    if (parcoords.dims()[[m]][['hide']]) 
      return(tags$div(class = 'sidebar-input-container-plain', toggleSwitch))
    else {
      digits <- 2
      if (m %in% names(ParcoordsRound)) digits <- ParcoordsRound[[m]]
      minVal <- round(min(df[, m], na.rm = T), digits)
      maxVal <- round(max(df[, m], na.rm = T), digits)
      tags$div(class = 'sidebar-input-container-box', 
               toggleSwitch,
               sliderInput(inputId = sId("parcoords_yrange", m),
                           min = minVal,
                           max = maxVal,
                           label = NULL,
                           round = -digits,
                           value = c(max(round(parcoords.axisSettings[[sId("parcoords_yrange", m)]][1], 
                                               digits = digits),
                                         minVal),
                                     min(round(parcoords.axisSettings[[sId("parcoords_yrange", m)]][2],
                                               digits = digits),
                                         maxVal))))
    }
}
  
output$parcoords_Controls <- renderUI({
  df <- parcoords.data()
  metrics <- names(parcoords.data())
  names(metrics) <- metrics
  metrics <- metrics[!(metrics %in% c('CU_ID', 'colorAttrib'))] # always hide CU_ID and colorAttrib
  # control widgets
  ctrlWidgets <- lapply(metrics, function(m) {
    if (m %in% numericMetrics(df))
      parcoords.makeNumCtrWidget(df, m)
    else
      tags$div(class = 'sidebar-input-container-plain',  parcoords.makeToggleSwitch(m))
  })
  tags$div(tagList(ctrlWidgets)) 
})

# scale parcoords graph axes to current selection, 
# by setting sliders and thereby triggering corresponding changes in dims()
observeEvent({input$parcoords_scale_to_selected}, {
  df <- parcoords.sharedDS$origData()
  df <- df[df$CU_ID %in% parcoords.getParcoordsSelection(), ]
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

parcoords.getParcoordsSelection <- function() {
  if (is.null(parcoords.sharedDS$selection())) 
     NULL
  else 
    parcoords.sharedDS$origData()$CU_ID[parcoords.sharedDS$selection()]
}

parcoords.setParcoordsSelectionFromCurrentSelection <- function() {
  if(!setequal(data.currentSelection[['CUs']], parcoords.getParcoordsSelection())) {
    selected <- parcoords.sharedDS$origData()$CU_ID %in% data.currentSelection[['CUs']]
    parcoords.sharedDS$selection(selected)
  }
}

    
# set selection on brushing
# this will try to maintain the current shared data selection if
# data.currentSelection is changed somewhere else. Don't let it unless
# parcoords panel is currently the open one.
observeEvent(parcoords.sharedDS$selection(), {
  if (!is.null(input$UIPanels) && input$UIPanels == 'Parcoords') {
    selectedCUs <- parcoords.getParcoordsSelection()
    #print('parcoords.sharedDS$selection() triggered')
    if (!setequal(selectedCUs, data.currentSelection[['CUs']])) 
      data.setSelectionByCU(selectedCUs, widget="parcoords")
  }
}, ignoreInit = T, ignoreNULL = F)

#observeEvent(input$parcoords_reset_brush, {
#  #print('input$parcoords_reset_brush triggered')
#  parcoords.sharedDS$selection(NULL)
#  df <- parcoords.data()
#  for (m in names(df)) {
#    if (sId("parcoords_yrange", m) %in% names(input)) {
#      updateSliderInput(session, sId("parcoords_yrange", m), 
#                        value = c(min(df[, m], na.rm=T),max(df[, m], na.rm=T)))
#   }
# }
#}, ignoreInit = T)

# set the crosstalk selection in response to changes in the current selection 
# (e.g., in response to user clicking clear highlighting button)
# only do this if parcoords widget is currently the one in focus
observeEvent(data.currentSelection[['CUs']], {
  if (!is.null(input$UIPanels) && input$UIPanels == 'Parcoords') 
    parcoords.setParcoordsSelectionFromCurrentSelection()
}, ignoreNULL = F, ignoreInit = T)

# things to do when Parcoords panel is opened
observeEvent(input$UIPanels, {
  if (!is.null(input$UIPanels) && input$UIPanels == 'Parcoords') {
    showInfoPane(uiOutput("parcoords_Controls"))
    parcoords.setParcoordsSelectionFromCurrentSelection()
  }
}, ignoreInit = T)


output$box_Parcoords <- renderUI({ 
  tagList( tags$div('style' = "text-align:right;", 
                    actionButton(inputId = "how_to_video",
                                  label="How do I use this chart?",icon("question"), 
                                  style=ButtonStyle,
                                  onclick = "window.open('http://michaelbarrus.com/parallel-coordinates', '_blank')")),
           parcoordsSoS::parcoordsOutput("parcoords_Plot", 
                                         width="100%", 
                                         height="600px"))          # 400px is default height
})
