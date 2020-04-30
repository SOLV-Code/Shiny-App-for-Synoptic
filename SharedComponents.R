# ----------------- Shared data structures used in several widgets ---------------

# create a data frame analogous to data.CU.Metrics, but with status metrics coverted to numeric and 
# metric values replaced by change in metric values 
data.getChangeMetricsData <- function() {
  df <- data.CU.Metrics[order(data.CU.Metrics$CU_ID, data.CU.Metrics$Year), ]
  CUs <- intersect(df[df$Year == filter$changeyear_2, 'CU_ID'], df[df$Year == filter$changeyear_1, 'CU_ID'])
  # convert status metrics from factor to numeric
  df[ , data.CU.MetricsSeries.StatusMetricNames] <- as.data.frame(lapply(data.CU.MetricsSeries.StatusMetricNames, 
                                                                         function(m) {as.numeric(df[ , m])}))
  # replace metrics values with change in values between selected years
  metrics <- c(data.CU.MetricsSeries.MetricNames, data.CU.MetricsSeries.StatusMetricNames)
  df.yr1 <- df[df$Year == filter$changeyear_1 & df$CU_ID %in% CUs, metrics]
  df.yr2 <- df[df$Year == filter$changeyear_2 & df$CU_ID %in% CUs, metrics]
  df <- df[df$Year == filter$changeyear_2 & df$CU_ID %in% CUs, ]
  df[ , metrics] <- df.yr2 - df.yr1
  df
}

# filter data by row, based on filter criteria
data.applyFilters <- function(df) {
  selection <- rep(T, nrow(df))
  for (a in FilterAttributes) {
    selection <- selection & (as.character(df[, a]) %in% filter[[a]])
  }
  df <- df[selection, , drop=F]
  if (nrow(df) > 0) row.names(df) <- df$CU_ID
  # remove any metrics and categories the user doesn't want to see
  # make this general by keeping everything that's not explicitly excluded
  drops <- FilterMFAttributes[!(FilterMFAttributes %in% filter$metrics)]
  # if a metric is dropped, also drop the associated Status metric
  for (m in unique(data.CU.MetricsSeries.MetricNames)) if(!any(grepl(m, filter$metrics))) {
    drops <- c(drops, m, paste(m, 'Status', sep='.'))
  }
  # additional columns that are no longer needed and can be dropped now 
  drops <- c(drops, "CU_ID", "Year", SingleChoice)
  df[ ,drops] <- NULL
  as.data.frame(df) # make sure this remains a data frame, even if there is only one row or column left at this point
}

# generate a metrics dataset that has all the filters applied 
data.getFilteredData <- function() {
  # calculate change in metric values if "change" selected
  if(filter$change == "Change" ){   
    data.applyFilters(data.getChangeMetricsData())
  } else { # use annual values
    data.applyFilters(data.CU.Metrics[data.CU.Metrics$Year == filter$year, ])
  }
}

data.getFilteredCULookupTable <- function() {
  df <- data.CU.Lookup
  selection <- rep(T, nrow(df))
  for (a in FilterAttributes[FilterAttributes %in% names(df)]) {
    df.a <- as.character(df[, a])
    df.a[is.na(df.a)] <- "NA"
    selection <- selection & (df.a %in% filter[[a]])
  }
  df[selection, , drop=F]
}

# This dataset has all filters applied.
# One row per CU, either original metric values or change in metric values (if change selected)
# row.names set to CU ID
data.filtered <- reactiveVal(data.CU.Metrics[data.CU.Metrics$Year == max(data.CU.Metrics$Year), ])
# equivalent to data.CU.lookup, but with filters applied
data.CU.Lookup.filtered <- reactiveVal(data.CU.Lookup)
# keep track of current CUs within filter
data.currentCUs <- reactive({
  if (!is.null(data.CU.Lookup.filtered()) && nrow(data.CU.Lookup.filtered()) > 0)
    unique(data.CU.Lookup.filtered()$CU_ID)
  else NULL
})

# equivalent to data.pop.Lookup, but with filters applies
data.Pop.Lookup.filtered <- reactive({
  df <- data.Pop.Lookup
  df <- df[df$CU_ID %in% data.currentCUs(), ]
})

# keep track of current populations within filtered data
data.currentPops <- reactive({
  if (!is.null(data.Pop.Lookup.filtered()) && nrow(data.Pop.Lookup.filtered()) > 0) 
    data.Pop.Lookup.filtered()$Pop_UID  
  else
    NULL
})

observeEvent({filterChanged()
  data.isFrozen()}, {
    if (!data.isFrozen())  {
      data.filtered(data.getFilteredData())
      data.CU.Lookup.filtered(data.getFilteredCULookupTable())
    }
  }, ignoreInit = FALSE)


# ------------------ Selection (highlighting) ---------------------

# keep track of the current selection
data.currentSelection <- reactiveValues()
data.currentSelection[['CUs']] <- NULL
data.currentSelection[['Pops']] <- NULL

# keep track of the last widget that set the selection
data.selectedBy <- reactiveValues()
data.selectedBy[['CUs']] <- 'none'
data.selectedBy[['Pops']] <- 'none'

# Same as data.filtered(), but all rows removed that aren't currently selected
data.selected <- reactive({data.filtered()[data.currentSelection[['CUs']], , drop=F]})

# test whether the current selection contains anything
data.currentSelectionEmpty <- function(type) {length(data.currentSelection[[type]]) == 0L}

# set the data selection 
data.setSelection <- function(sel, type, widget) {
  #cat(widget, ": Setting selection for ", type, " to :")
  #print(sel)
  data.currentSelection[[type]] <- sel
  data.selectedBy[[type]] <- widget
}

# set selection for CUs and also associated sites 
data.setSelectionByCU <- function(sel, widget) {
  #cat(widget, ": Setting selectionByCU to :")
  #print(sel)
  data.currentSelection[['CUs']] <- sel
  data.currentSelection[['Pops']] <- unlist(lapply(sel, getPopsForCUs))
  data.selectedBy[['CUs']] <- widget
  data.selectedBy[['Pops']] <- widget
}
# add to the current selection
data.addToSelection <- function(sel, type, widget) {
  #cat(widget, ": adding to selection of ", type, ":")
  #print(sel)
  if (!data.currentSelectionEmpty(type)) {
    sel <- union(data.currentSelection[[type]], sel)
  } 
#  print('setting current selection to ')
#  print(sel)
  data.currentSelection[[type]] <- sel
  data.selectedBy[[type]] <- widget
}

# remove from the current selection
data.removeFromSelection <- function(sel, type, widget) {
  #cat(widget, ": removing from selection of ", type, ":")
  #print(sel)
  if (any(!(data.currentSelection[[type]] %in% sel))) {
    sel <- data.currentSelection[[type]][!(data.currentSelection[[type]] %in% sel)]
  } else {
    sel <- NULL 
  }
  data.currentSelection[[type]] <- sel
  data.selectedBy[[type]] <- widget
}

data.isSelected <- function(sel, type) {sel %in% data.currentSelection[[type]]}

data.showPops <- reactiveVal(FALSE)
observeEvent(input$sidebarMenu_showPops, {data.showPops(input$sidebarMenu_showPops)})
             
observeEvent(input$sidebarMenu_clearHighlighting,{
  data.setSelection(NULL, type='CUs', widget="clearHighlighting_button")
  data.setSelection(NULL, type='Pops', widget="clearHighlighting_button")
})

# --------------------------- freeze/unfreeze data -------------------------

# keep track of whether data is frozen to a snapshot,
# i.e., by a request from the user to constrain the dataset to the current selection
data.isFrozen <- reactiveVal(value=FALSE)
observeEvent(input$sidebarMenu_freezeDataToHighlighted, {
  data.isFrozen(TRUE)
  data.filtered(data.filtered()[data.currentSelection[['CUs']], ])
  data.CU.Lookup.filtered(data.CU.Lookup.filtered()[data.CU.Lookup.filtered()$CU_ID %in% data.currentSelection[['CUs']], ])
  data.setSelection(NULL, type='CUs', widget="freezeDataToHighlighted_button")
  data.setSelection(NULL, type='Pops', widget="freezeDataToHighlighted_button")
})

observeEvent(data.currentSelection[['CUs']], {
  if (data.currentSelectionEmpty('CUs'))
    updateButton(session, "sidebarMenu_freezeDataToHighlighted", disabled = TRUE)
  else {
    updateButton(session, "sidebarMenu_freezeDataToHighlighted", disabled = FALSE)
  }
}, ignoreNULL = FALSE)

observeEvent(data.isFrozen(), {
  if (data.isFrozen())
    updateButton(session, "sidebarMenu_resetDataToFilter", disabled = FALSE)
  else
    updateButton(session, "sidebarMenu_resetDataToFilter", disabled = TRUE)
  
})

observeEvent(input$sidebarMenu_resetDataToFilter, data.isFrozen(FALSE))

# ------------------- Info pane --------------------------

# add and remove information from the Info Pane

sidebarUI <- reactiveVal("")
populateModal <- function() {
  tags$div(id='insertMarkInfoPaneModal', sidebarUI())
}
observeEvent(input$popup, {showModal(modalDialog(populateModal(), size='l'))})

clearInfoPane <- function() {
  removeUI(selector = '#InfoPane', multiple=TRUE)
}

# add ui content to the info pane w/o clearing what's already there
addToInfoPane <- function(ui) {
  sidebarUI(ui)
  insertUI(selector = '#insertMarkInfoPane', # insert location
           where = "afterEnd",
           # wrap element in a div and give it it's css class name
           # this is so we can refer to it when it is time to remove it.
           ui = tags$div(ui, id='InfoPane'))
  insertUI(selector = '#insertMarkInfoPaneModal', # insert location
           where = "afterEnd",
           # wrap element in a div and give it it's css class name
           # this is so we can refer to it when it is time to remove it.
           ui = tags$div(ui, id='InfoPane'))
}

# Get rid of any existing info pane content before putting up new content.
# Shiny doesn't always pick up mouseout events, 
# so clearing the info pane on mouseout doesn't work reliably. 
# Also, it's actually sometimes handy to have the pane stay available after the mouse is moved out
showInfoPane <- function(ui) {
  clearInfoPane()
  addToInfoPane(ui)
}

# observeEvent(input$dataUnit, {
#   if (input$dataUnit == 'CUs')
#     data.setSelection(NULL, type='Pops', widget="dataUnit_toggle")
#   else
#     data.setSelection(NULL, type='CUs', widget="dataUnit_toggle")
#   clearInfoPane()
#   })

# -------------------------- sparklines and status summary -----------------------

# extract attribute 'attrib' from df and create a vector of attrib values to use for creating a sparkline, 
# padding with NAs if needed to fill the range of years (either explicity specified in args, or taken from column 'Year')
spark.makeSparklineData <- function(df, attrib, minYr=NULL, maxYr=NULL) {
  df <- df[, c('Year', attrib)]
  row.names(df) <- as.character(df$Year)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (is.null(minYr)) minYr <- min(df$Year, na.rm=T)
  if (is.null(maxYr)) maxYr <- max(df$Year, na.rm=T)
  yrs <- c(minYr:maxYr)
  out <- rep(NA, length(yrs))
  names(out) <- as.character(yrs)
  out[row.names(df)] <- df[ ,attrib]
  out
}

# create a sparkline, given a data frame with a year column and a time series column specified in the 'DataType' filter attribute 
spark.makeSparkline <- function(df, minYr=NULL, maxYr=NULL, attribs) {
  spark <- tags$div(style=(attribs$style$missingTS), attribs$missingTSText)
  if ((nrow(df) > 0) && (filter$DataType %in% names(df))) {
    ts <- filter$DataType
    df <- as.numeric(spark.makeSparklineData(df, ts, minYr, maxYr))
    if (!all(is.na(df))) {
      ID <- as.character(runif(1))
      output[[ID]] <- renderSparkline({sparkline(df, 
                                                 width=attribs$chartWidth,
                                                 height=attribs$chartHeight,
                                                 #lineWidth=attribs$lineWidth,
                                                 lineColor=attribs$lineColor, 
                                                 fillColor=attribs$fillColor
                                                 )})
      spark <- tags$div(class=attribs$sparkCanvas, sparklineOutput(ID))
    }
  }
  spark
}

# table row for a table with sparklines 
spark.makeSparklineTableRow <- function(df, labels, attribs, minYr, maxYr) {
 tagList(lapply(names(labels)[names(labels) %in% attribs$labelAttribs], function(l) {
                    tags$td(labels[l], style=attribs$styles[[l]])
                  }), 
          tags$td(spark.makeSparkline(df, minYr=minYr, maxYr=maxYr, attribs=attribs)))
}

# table row for a table with sparklines from population data
spark.makePopSparklineTableRow <- function(p, attribs) {
  df <- data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID == p[['Pop_UID']], ]
  if (!is.na(p[['tsName']])) {
    labels <- list(Pop_UID = p[['Pop_UID']], TS_Name = p[['tsName']], 
                Pop_ID = get_Pop_ID_From_Pop_UID(p[['Pop_UID']]), CU_ID = get_CU_ID_From_Pop_UID(p[['Pop_UID']]))
    df <- df[df$TS_Name == p[['tsName']], ]
    
  } else {
    labels <- list(Pop_UID = p[['Pop_UID']], TS_Name = p[['pName']],
                Pop_ID = get_Pop_ID_From_Pop_UID(p[['Pop_UID']]), CU_ID = get_CU_ID_From_Pop_UID(p[['Pop_UID']]))
  }
  # allow for blank label columns
  labels <- lapply(attribs$labelAttribs, function(l) {if (l %in% names(labels)) labels[l] else ''})
  names(labels) <- attribs$labelAttribs
  
  # workaround for messy data with multiple time series
  if (any(duplicated(df$Year))) {
    duplYrs <- df$Year[duplicated(df$Year)]
    sampleRows <- df[df$Year %in% duplYrs, ]
    sampleRows <- sampleRows[order(sampleRows$Year), ]
    cat('Found multiple time series for ', p, '!\n')
    print(sampleRows)
    # find the attributes by which the series differ
    attrs <- unlist(lapply(names(sampleRows), function(a) {if (length(unique(sampleRows[, a])) < length((sampleRows[, a]))) a else NA}))
    attrs <- attrs[!is.na(attrs)]
    tagList(lapply(names(labels), function(l) {tags$td(labels[l], style=attribs$styles[[l]])}),  
            tags$td(paste0('multiple time series found!')))
  }
  else 
    spark.makeSparklineTableRow(df, labels=labels, attribs=attribs,
                              minYr=min(data.Pop.Lookup$DataStartYear, na.rm=T), 
                              maxYr=max(data.Pop.Lookup$DataEndYear, na.rm=T))
}

# table row for a table with sparklines from CU data
spark.makeCUSparklineTableRow <- function(CU, attribs) {
  df <- data.CU.TimeSeries[data.CU.TimeSeries$CU_ID == CU, ]
  # allow for blank label columns
  labels <- lapply(attribs$labelAttribs, function(l) {if (l %in% names(df)) df[1, l] else ''})
  names(labels) <- attribs$labelAttribs
  # put the labels in td tags
  labels <- lapply(names(labels)[names(labels) %in% attribs$labelAttribs], function(l) {
                    tags$td(labels[l], style=attribs$styles[[l]])})
  sparkline <- tags$td( spark.makeSparkline(df=df, 
                                            minYr=min(data.CU.Lookup$DataStartYear, na.rm=T), 
                                            maxYr=max(data.CU.Lookup$DataEndYear, na.rm=T),
                                            attribs=attribs))
  metrics <- lapply(MapLabelMetrics[MapLabelMetrics %in% filter$metrics], function(m) {
                    spark.makeCUTableMetricsEntries(m, CU, attribs)})
  tagList(labels, sparkline, metrics)
}

# given a list of pop UIDs, put together a list of pairs of the form (Pop_UID, Pop_Name) for input to makePopSparklineTableRow
# this is needed since there are sometimes multiple time series for the same Pop_UID (all of which are concatenated in
# the 'tsNames' field in data.Pop.Lookup)
spark.getPopsWithTSNames <- function(pops) {
  p.name <- unlist(lapply(pops, function(p) {
    tsNames <- strsplit(data.Pop.Lookup[data.Pop.Lookup$Pop_UID == p, 'tsNames'], ':')[[1]]
    paste(p, tsNames, sep=':')
  }))
  out <- lapply(p.name, function(p) {
    pp <- strsplit(p, ':')[[1]]
    c('Pop_UID' = pp[1], 'tsName' = pp[2], 'pName' = data.Pop.Lookup[pp[1], 'Pop_Name'])
  })
  names(out) <- pops
  out
}


# make a sparkline table, given a list of Pop_UIDs
spark.makePopSparklineTable <- function(pops, mode='sidebar', CUheader = 'full') {
  # a lot of populations don't have time series data; put the ones that do first here
  pops <- pops[order(data.Pop.Lookup[pops, 'HasTimeSeriesData'], decreasing = T)]
  attribs <- popTableAttribs[[mode]]
  headerRow <- tagList(lapply(attribs$labelAttribs, function(l) {tags$td('')}),  # label columns
                       tags$td(''))                                              # sparkline column
  if (mode == 'full') # add columns for metrics
     headerRow <- tagList(headerRow,                                              
                         lapply(MapLabelMetrics[MapLabelMetrics %in% filter$metrics], function(m) { # metric columns
                           tagList(tags$td(GetLabel(m), style='font-weight: bold;'), tags$td(''))}))
  CUs <- unique(data.Pop.Lookup[pops, 'CU_ID'])
  blanks <- tagList(lapply(1:(2*length(MapLabelMetrics[MapLabelMetrics %in% filter$metrics])), function(i) {tags$td('')}))
  tags$div(class = paste0(mode, '-sparkline-box'),
           tags$table(tags$tr(headerRow), 
             tagList(lapply(CUs, function(cu) {
                      popRows <- lapply(spark.getPopsWithTSNames(pops[data.Pop.Lookup[pops, 'CU_ID'] == cu]), 
                                        function(p, attribs) {
                                            tableRow <- spark.makePopSparklineTableRow(p, attribs)
                                            if (mode == 'full') tableRow <- tagList(tableRow, blanks)
                                            tags$tr(tableRow) }, attribs)
                      if (CUheader == 'full') {
                        popsInCU <- data.Pop.Lookup[data.Pop.Lookup$CU_ID == cu, 'Pop_UID']
                        if (all(popsInCU %in% pops)) 
                          CUattribs <- popTableAttribsCUHeader[[mode]][['complete']] 
                        else 
                          CUattribs <- popTableAttribsCUHeader[[mode]][['partial']] 
                        CUheader <- tags$tr(spark.makeCUSparklineTableRow(cu, CUattribs))
                        tagList(CUheader, popRows)
                      } else if (CUheader == 'labelOnly') {
                        CUheader <- tags$tr(tags$td(cu), tags$td(data.CU.Lookup[data.CU.Lookup$CU_ID == cu, 'CU_Name']))
                        tagList(CUheader, popRows)
                      }
                      else 
                        tagList(popRows)
                     }))))
}

# make a sparkline table, given a list of CU_IDs
spark.makeCUSparklineTable <- function(CUs, mode='sidebar') {
  attribs <- CUTableAttribs[[mode]]
  headerRow <- tags$tr(tagList(lapply(attribs$labelAttribs, function(l) {tags$td('')}),
                               tags$td(''),
                               lapply(MapLabelMetrics[MapLabelMetrics %in% filter$metrics], function(m) {
                                      tagList(tags$td(GetLabel(m), style='font-weight: bold;'), tags$td(''))})))
  sparklines <- lapply(CUs, function(cu, attribs) {tags$tr(spark.makeCUSparklineTableRow(cu, attribs))}, attribs)                              
  tags$div(class = paste0(mode, '-sparkline-box'),
           tags$table(tagList(headerRow, sparklines)))
}

# css snipped for showing an arrow rotated by degrees from the horizonal
makeArrow <- function(degrees) {
  cssStyle <- "-ms-transform:rotate(xxdeg); -webkit-transform:rotate(xxdeg); -moz-transform:rotate(xxdeg); -o-transform:rotate(xxdeg);"
  cssStyle <- gsub('xx', as.character(degrees), cssStyle)
  paste("<div style='", cssStyle, "'>&rarr;</div>", sep="")
}

# get the polar angle for representing the given change in y over an x distance of 1
polarAngle <- function(dy) {atan2(dy, 1) * 180 / pi}

# set style for text and background conditional on status
getMetricTextStyle <- function(status) {
  switch(status,
         Red = 'background-color: #ff0000; color: #000000;',
         Amber = 'background-color: #ff9900; color: #000000;',
         Green = 'background-color: #00dd00; color: #000000;',
         'NA' = ''
  )
}


# create tds with information on the given metric
spark.makeCUTableEntry <- function(metric, end, start, attribs) {
  style <- getMetricTextStyle(end['Status'])
  if (is.na(end['Value'])) value <- 'NA'
  else value <- as.character(round(as.numeric(end['Value']), 2))
  if (is.null(start) || is.na(start['Value']) || start['Value'] == 'NA' || !(abs(as.numeric(start['Value'])) > 0)) 
    change <- ''
  else 
    change <- HTML(makeArrow(polarAngle((as.numeric(end['Value']) - as.numeric(start['Value']))/as.numeric(start['Value']))))
  tagList(tags$td(style=paste(style, attribs$metricCellValue), value), 
          tags$td(style=paste(style, attribs$metricCellArrow), change))
}

# get together the information needed to output information for the given metrics
# the pane shows arrows indicating the direction and magnitude of change, either 
# over the period from change year 1 to change year 2, or over the period leading
# up to the selected year.
spark.makeCUTableMetricsEntries <- function(metrics, CU, attribs) {
  if (filter$change == "Change") {
    endYear <- filter$changeyear_2
    startYear <- filter$changeyear_1
  } else {
    endYear <- filter$year
    if (length(data.CU.Metrics[data.CU.Metrics$CU_ID == CU, 'Year']) > 0 && 
        filter$year > min(data.CU.Metrics[data.CU.Metrics$CU_ID == CU, 'Year'])) {
      startYear <- as.numeric(filter$year) - 1
      # count back if the current startYear isn't in the dataset
      while(!(startYear %in% data.CU.Metrics[data.CU.Metrics$CU_ID == CU, 'Year'])) startYear <- startYear - 1
    } else {
      startYear <- NULL
    }
  } 
  tagList(lapply(metrics, function(m) {
    end <- c( Value = data.CU.Metrics[paste(CU, filter$DataType, endYear, sep="."), m],
              Status = as.character(data.CU.Metrics[paste(CU, filter$DataType, endYear, sep="."), paste0(m, '.Status')]))
    if (is.null(startYear)) start <- NULL
    else start <- c( Value = data.CU.Metrics[paste(CU, filter$DataType, startYear, sep="."), m],
                     Status = as.character(data.CU.Metrics[paste(CU, filter$DataType, startYear, sep="."), paste0(m, '.Status')]))
    spark.makeCUTableEntry(m, start = start, end = end, attribs)
  }))
}


