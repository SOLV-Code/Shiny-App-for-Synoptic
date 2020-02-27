#------------------- Parallel Coordinate Plot ------------------


# Create data for the parallel plot (reorder columns and rows)
data.parcoords <- reactive({
  df <- data.filtered()
  # sort the CUs (rows) in the data according to specified sort criteria
  for (sortKey in rev(ParcoordsCUOrder[ParcoordsCUOrder %in% names(df)])) {
    df <- df[order(df[, sortKey]), , drop=F]
  }
  # replace 'NA' with true NAs in factors
  for (n in names(df)) {
    if (is.factor(df[, n])) {
        old <- df[, n]
        df[!is.na(df[, n]) & df[ ,n] == 'NA', n] <- NA
        df[ ,n] <- factor(as.character(df[ ,n]), levels=levels(old)[levels(old) != 'NA'], ordered=is.ordered(old), exclude=NA)
    }
  }
  # identify all-NA columns and columns without contrast, i.e. columns where all values are the same
  colsToDrop <- apply(df, 2, function(c) {all(is.na(c)) || all(c == c[1])}) 
  colsToDrop[is.na(colsToDrop)] <- FALSE # don't drop columns for which the function returned NA; these are columns that have a mix of NAs and data
  df <- arrangeColumns(df, colOrder=ParcoordsMetricOrder, hide=names(df)[colsToDrop])
  df$CU_ID <- row.names(df)
  row.names(df) <- unlist(lapply(row.names(df), getCUname))
  if (!is.data.frame(df)) {df <- NULL} 
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
           # CU_ID special: always keep hidden
           #if (m == 'CU_ID') d[['hide']] <- T
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
             if (sId("parcoords_yrange", m) %in% names(input)) { # if there is an input widget for this dim, allow for it to set the ylims
               d[['ymin']] <- input[[sId("parcoords_yrange", m)]][1]
               d[['ymax']] <- input[[sId("parcoords_yrange", m)]][2]
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

output$parcoords_Plot <- parcoordsSoS::renderParcoords({ 
  p <- try({parcoordsSoS::parcoords(data=sharedDS.parcoords,
                            autoresize=TRUE,
                            color= list(colorScale=htmlwidgets::JS("d3.scale.category10()"), colorBy="Species"),
                            rownames=T,
                            alpha=0.6, 
                            alphaOnBrushed = 0,
                            brushMode="1D-axes-multi",
                            brushPredicate="and",
                            reorderable = TRUE, 
                            dimensions=dims(),
                            #selectedRows = data.currentSelection[['CUs']], #this works, but makes it impossible to brush more than one CU at a time
                            nullValueSeparator="nullValue",
                            dimensionTitleRotation=ParcoordsLabelRotation)})
  if (inherits(p, "try-error")) {
    print('parcoords call failed!')
    NULL
  } else {
    p
  }
})

# Create a block with miscellaneous controls for the parcoords plot
output$parcoords_Controls <- renderUI({
    df <- data.parcoords()
    metrics <- names(data.parcoords())
    names(metrics) <- metrics
    hidden <- ifelse(metrics %in% ParcoordsHideOnInit, TRUE, FALSE)
    names(hidden) <- metrics
    # control widgets for categorical metrics
    catWidgets <- lapply(metrics[!(metrics %in% numericMetrics(df))], 
                         function(m) { 
                           tags$tr(
                            tags$td(tags$div(title=MetricInfo[[m]], 
                                             style='width:70px; font-size:11px; padding-left:5px;', 
                                             GetLabel(m))),
                            tags$td(tags$div(title=MetricInfo[[m]], style='width:120px;', '')),
                            tags$td(tags$div(style = 'width: 20px;', 
                                             checkboxInput(inputId = sId("parcoords_visible", m), 
                                                  label=NULL, #GetLabel(m),
                                                  value=!(hidden[m])))))})
    # control widgets for numerical metrics
    numWidgets <- lapply(metrics[metrics %in% numericMetrics(df)], 
                         function(m) { 
                           minVal <- min(df[, m], na.rm = T)
                           maxVal <- max(df[, m], na.rm = T)
                           tags$tr(
                             tags$td(tags$div(title=MetricInfo[[m]], 
                                              style='width:70px; font-size:11px; padding-left:5px; white-space:normal; word-break:break-all;', 
                                              GetLabel(m))),
                           tags$td(tags$div(style = 'width: 120px;',
                                            sliderInput(inputId = sId("parcoords_yrange", m),
                                                label = NULL,
                                                min = minVal,
                                                max = maxVal,
                                                value = c(minVal, maxVal)))),
                          tags$td(tags$div(style = 'width: 20px;'),
                                  checkboxInput(inputId = sId("parcoords_visible", m), 
                                                           label=NULL, #GetLabel(m),
                                                           value=!hidden[m])))})
    tagList(tags$div(style='padding-left:5px;', tags$b("Adjust axes:")), 
            tags$div(style='line=height: 1;', 
              tags$table(do.call(tagList, c(numWidgets, catWidgets)))))
  })

# reset parcoords graph, 
# by resetting sliders and thereby triggering corresponding changes in dims()
observeEvent({input$parcoords_reset_brush}, {
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
    selectedCUs <- sharedDS.parcoords$origData()$CU_ID[sharedDS.parcoords$selection()]
  }
  #print('sharedDS.parcoords$selection() triggered')
  data.setSelection(selectedCUs, type='CUs', widget="parcoords")
}, ignoreInit = T, ignoreNULL = F)

observeEvent(input$parcoords_reset_brush, {
  #print('input$parcoords_reset_brush triggered')
  data.setSelection(NULL, type='CUs', widget="parcoords")
  sharedDS.parcoords$selection(NULL)
}, ignoreInit = T)

# set the crosstalk selection in response to changes in the current selection
observeEvent(data.currentSelection[['CUs']], {
  if (data.selectedBy[['CUs']] != "parcoords" && data.selectedBy[['CUs']] != "none") {
    #print('in observeEvent(data.currentSelection[[CUs]] for crosstalk: current selection is ')
    #print(data.currentSelection[['CUs']])
    selected <- sharedDS.parcoords$origData()$CU_ID %in% data.currentSelection[['CUs']]
    #print(selected)
    sharedDS.parcoords$selection(selected)
  }
}, ignoreNULL = F, ignoreInit = T)

# things to do when Parcoords panel is opened
observeEvent({input$UIPanels
             input$dataUnit}, {
  if (!is.null(input$UIPanels) && input$UIPanels == 'Parcoords' && input$dataUnit == 'CUs') {
    showInfoPane(uiOutput("parcoords_Controls"))
  }
}, ignoreInit = T)


output$box_Parcoords <- renderUI({ 
  if (input$dataUnit == 'CUs') {
    tagList( tags$div('style' = "text-align:right;", 
                      actionButton(inputId = "parcoords_reset_brush",
                                   label="Reset Highlighting",icon("paper-plane"), 
                                   style=ButtonStyle),
                      #actionButton(inputId = "parcoords_scale_to_selected",
                      #             label="Scale to Selected",icon("search-plus"), 
                      #             style=ButtonStyle)),
                      #This is where the "how to" button will be created"
                      actionButton(inputId = "how_to_video",
                                   label="How do I use this chart?",icon("question"), 
                                   style=ButtonStyle,
                                   onclick ="window.open('http://michaelbarrus.com/parallel-coordinates', '_blank')")),
             parcoordsSoS::parcoordsOutput("parcoords_Plot", height="600px"))           # 400px is defaultheight
             #uiOutput("parcoords_Controls"))
  } else {
    tags$div('no metrics available')
  }
})
