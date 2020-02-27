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
  drops <- c(drops, "CU_ID", "Year", FilterSingleChoiceAttributes)
  df[ ,drops] <- NULL
  as.data.frame(df) # make sure this remains a data frame, even if there is only one row or column left at this point
}

# This dataset has all filters applied.
# One row per CU, either original metric values or change in metric values (if change selected)
# row.names set to CU ID
data.filtered <- reactive({
  # calculate change in metric values if "change" selected
  if(filter$change == "Change" ){   
    data.applyFilters(data.getChangeMetricsData())
  } else { # use annual values
    data.applyFilters(data.CU.Metrics[data.CU.Metrics$Year == filter$year, ])
  }
})

# This dataset has all filters applied.
# One row per CU, either original metric values or change in metric values (if change selected)
# row.names set to CU ID
data.CU.filtered <- reactive({
  # calculate change in metric values if "change" selected
  if(filter$change == "Change" ){   
    data.applyFilters(data.getChangeMetricsData())
  } else { # use annual values
    data.applyFilters(data.CU.Metrics[data.CU.Metrics$Year == filter$year, ])
  }
})

data.CU.Lookup.filtered <- reactive({
  df <- data.CU.Lookup
  selection <- rep(T, nrow(df))
  for (a in FilterAttributes[FilterAttributes %in% names(df)]) {
    df.a <- as.character(df[, a])
    df.a[is.na(df.a)] <- "NA"
    selection <- selection & (df.a %in% filter[[a]])
  }
  df <- df[selection, , drop=F]
})

data.Pop.Lookup.filtered <- reactive({
  df <- data.Pop.Lookup
  df <- df[df$CU_ID %in% data.CU.Lookup.filtered()$CU_ID, ]
})

# keep track of current CUs within filter
data.currentCUs <- reactive({
  if (!is.null(data.CU.Lookup.filtered()) && nrow(data.CU.Lookup.filtered()) > 0)
    unique(data.CU.Lookup.filtered()$CU_ID)
  else NULL
})

# keep track of current populations within filtered data
data.currentPops <- reactive({
  if (!is.null(data.Pop.Lookup.filtered()) && nrow(data.Pop.Lookup.filtered()) > 0) 
    data.Pop.Lookup.filtered()$Pop_UID  
  else
    NULL
})

getPopsForCU <- function(CU) {data.Pop.Lookup$Pop_UID[data.Pop.Lookup$CU_ID == CU]}

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
#  cat(widget, ": Setting selection for ", type, " to :")
#  print(sel)
  data.currentSelection[[type]] <- sel
  data.selectedBy[[type]] <- widget
}

# add to the current selection
data.addToSelection <- function(sel, type, widget) {
#  cat(widget, ": adding to selection of ", type, ":")
#  print(sel)
  if (!data.currentSelectionEmpty(type)) {
    sel <- union(data.currentSelection[[type]], sel)
  } 
#  print('setting current selection to ')
#  print(sel)
  data.currentSelection[[type]] <- sel
  data.selectedBy[[type]] <- widget
}

# remove CUs from the current selection
data.removeFromSelection <- function(sel, type, widget) {
#  cat(widget, ": removing from selection of ", type, ":")
#  print(sel)
  if (any(!(data.currentSelection[[type]] %in% sel))) {
    sel <- data.currentSelection[[type]][!(data.currentSelection[[type]] %in% sel)]
  } else {
    sel <- NULL 
  }
  data.currentSelection[[type]] <- sel
  data.selectedBy[[type]] <- widget
}

observeEvent(input$sidebarMenu_clearHighlighting,{
  data.setSelection(NULL, type='CUs', widget="clearHighlighting_button")
  data.setSelection(NULL, type='Pops', widget="clearHighlighting_button")
})

# add and remove information from the Info Pane

clearInfoPane <- function() {
  removeUI(selector = '#InfoPane', multiple=TRUE)
}

# add ui content to the info pane w/o clearing what's already there
addToInfoPane <- function(ui) {
  insertUI(selector = '#insertMarkInfoPane', # insert location 
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

observeEvent(input$dataUnit, {
  if (input$dataUnit == 'CUs')
    data.setSelection(NULL, type='Pops', widget="dataUnit_toggle")
  else
    data.setSelection(NULL, type='CUs', widget="dataUnit_toggle")
  clearInfoPane()
  })
