# -- Code associated with the DataFilters box and associated filtering logic  --------------------------  

# -- list of reactiveValues for storing the filters in
filter <- reactiveValues()
filterChanged <- reactiveVal()

setFilter <- function(field, val) {
  filter[[field]] <- val
  filterChanged(runif(1))
}

for (a in FilterAttributes[FilterAttributes %in% names(data.CU.Metrics)]) {
  local({
    lc_a <- a
    if (lc_a %in% SingleChoice) 
      filter[[lc_a]] <- unique(as.character(data.CU.Metrics[, lc_a]))[1]
    else
      filter[[lc_a]] <- unique(as.character(data.CU.Metrics[, lc_a]))
    observeEvent(input[[sId("dataFilters", lc_a)]], 
                 setFilter(lc_a, input[[sId("dataFilters", lc_a)]]), ignoreNULL = F, ignoreInit = T)
  })
}

# -- Year selection: set to most recent year as default
filter$year <- max(data.CU.Metrics$Year)
observeEvent(input$dataFilters_year, setFilter('year', input$dataFilters_year))
# Select metrics and attributes to show
filter$metrics <- c(data.CU.MetricsSeries.MetricNames, FilterMFAttributes) 
observeEvent(input$dataFilters_metrics, setFilter('metrics', input$dataFilters_metrics))
# Toggle between annual snapshot (default) and change from a baseline year
filter$change <- "Annual"
observeEvent(input$dataFilters_change, setFilter('change', input$dataFilters_change))
# If showing change from baseline year, select the two years to use (baseline and year to calculate change from baseline)
filter$changeyear_1 <- data.CU.Metrics.Years[length(data.CU.Metrics.Years) - 1]
observeEvent(input$dataFilters_changeyear_1, setFilter('changeyear_1', input$dataFilters_changeyear_1))
filter$changeyear_2 <- data.CU.Metrics.Years[length(data.CU.Metrics.Years)]
observeEvent(input$dataFilters_changeyear_2, setFilter('changeyear_2', input$dataFilters_changeyear_2))

# -- some helper functions and structures for nested filtering 

# given a list of data attributes and a data frame, 
# update the associated selector widgets to show the choices corresponding
# to the values available in the data frame for the given attributes
updatePickerInputs <- function(attribs, df) {
  for (a in attribs[attribs %in% names(df)]) {
    choices <- GetNamedChoices(a, df)
    updatePickerInput(session, sId("dataFilters", a), choices=choices, selected=choices)
  }
}

# given a list of data attributes and a data frame, 
# filter data frame based on the set of selector values
# associated with the given data attributes  
getFilteredDF <- function(attribs, df) {
  selection <- rep(T, nrow(df))
  for (a in attribs[attribs %in% names(df)]) {
    selection <- selection & (df[, a] %in% filter[[a]])
  }
  df[selection, ]
}

# -- logic for nested filtering of data:
# Data type selection: 
observeEvent(filter$DataType, {
  df <- getFilteredDF(c('DataType'), data.CU.Metrics)
  updatePickerInputs(c('Species', 'FAZ', 'Area', 'RunTiming', 'LifeHistory', 'CU_ID'), df) 
}, ignoreNULL = FALSE)

# -- logic for nested filtering of data:
# all other filters are limited to what's available for the selected species
observeEvent(filter$Species, {
  df <- getFilteredDF(c('Species'), data.CU.Metrics)
  updatePickerInputs(c('FAZ', 'Area', 'RunTiming', 'LifeHistory', 'CU_ID'), df) 
}, ignoreNULL = FALSE)

# -- Area limits what FAZs and CUs are available
observeEvent(filter$Area, {
  df <- getFilteredDF(c('Species', 'Area', 'RunTiming', 'LifeHistory'), data.CU.Metrics)
  updatePickerInputs(c('FAZ', 'CU_ID'), df) 
}, ignoreNULL = FALSE)

# -- FAZ, RunTiming, and LifeHistory limit what CUs are available
observeEvent({
  filter$FAZ
  filter$RunTiming
  filter$LifeHistory
}, {
  df <- getFilteredDF(c('Species', 'Area','RunTiming', 'LifeHistory', 'FAZ'), data.CU.Metrics)
  updatePickerInputs(c('CU_ID'), df) 
}, ignoreNULL = FALSE)

# -- Years available for changeyear_1 are dependent on selection of changeyear_2 and vice versa
observeEvent(                            
  {input$dataFilters_changeyear_1},{ 
    choices <- data.CU.Metrics.Years[as.numeric(data.CU.Metrics.Years) > as.numeric(input$dataFilters_changeyear_1)]
    updatePickerInput(session, "dataFilters_changeyear_2", 
                      choices=choices, 
                      selected=choices[length(choices)])  
  })    

observeEvent(                            
  {input$dataFilters_changeyear_2},{ 
    choices <- data.CU.Metrics.Years[as.numeric(data.CU.Metrics.Years) < as.numeric(input$dataFilters_changeyear_2)]
    updatePickerInput(session, "dataFilters_changeyear_1", 
                      choices=choices, 
                      selected=choices[length(choices)] )  
  }) 

# Add tooltip to checkbox input. Borrowed from
#https://stackoverflow.com/questions/36670065/tooltip-in-shiny-ui-for-help-text
makeCheckboxTooltip <- function(checkboxValue, buttonLabel, buttonId, Tooltip){
  tags$script(HTML(paste0("
                            $(document).ready(function() {
                            var inputElements = document.getElementsByTagName('input');
                            for(var i = 0; i < inputElements.length; i++) {
                            var input = inputElements[i];
                            if(input.getAttribute('value') == '", checkboxValue, "' && input.getAttribute('value') != 'null') {
                            var button = document.createElement('button');
                            button.setAttribute('id', '", buttonId, "');
                            button.setAttribute('type', 'button');
                            button.setAttribute('class', 'btn action-button btn-inverse btn-xs');
                            button.style.float = 'right';
                            button.appendChild(document.createTextNode('", buttonLabel, "'));
                            
                            input.parentElement.parentElement.appendChild(button);
                            shinyBS.addTooltip('", buttonId, "', \"tooltip\", {\"placement\": \"right\", \"trigger\": \"click\", \"title\": \"", Tooltip, "\"})
                            console.log(button)
                            };
                            }
                            });
                            ")))
}

output$box_DataFilters <- renderUI({
  metricChoices <- list(
    Metrics = data.CU.MetricsSeries.MetricNames, #FilterMFMetrics[FilterMFMetrics %in% names(data.CU.Metrics)],
    Attributes = FilterMFAttributes[FilterMFAttributes %in% names(data.CU.Metrics)])
  names(metricChoices$Metrics) = as.character(lapply(metricChoices$Metrics, GetLabel))
  names(metricChoices$Attributes) = as.character(lapply(metricChoices$Attributes, GetLabel))
  allMetricChoices <- c(metricChoices$Metrics, metricChoices$Attributes)
  metricHelp <- lapply(as.character(allMetricChoices), function(m) {
    makeCheckboxTooltip(checkboxValue = m,
                        buttonLabel = "ⓘ",
                        buttonId = sId("dataFiltersMetricHelp", which(as.character(allMetricChoices) == m)),
                        Tooltip = htmlEscape(MetricInfo[[m]], attribute=TRUE))
  })
  
  tagList(
    fluidRow( 
      column(width=4,
             wellPanel(style = WellPanelStyle, tags$b("Step1:",  "Filter data"), tags$hr(),
                       lapply(FilterAttributes[FilterAttributes %in% names(data.CU.Metrics)], function(attrib) {
                         choices <- GetNamedChoices(attrib, data.CU.Metrics)
                         if (attrib %in% SingleChoice) { 
                           picker <- pickerInput(inputId=sId("dataFilters", attrib),					 
                                                 label=NULL,
                                                 choices=choices,
                                                 selected=choices[1],
                                                 multiple=FALSE,
                                                 options=PickerOptsSingleSelect)
                         } else {
                           picker <- pickerInput(inputId=sId("dataFilters", attrib),					 
                                                 label=NULL,
                                                 choices=choices,
                                                 selected=choices,
                                                 multiple=TRUE,
                                                 options=PickerOptsMultiSelect)
                         }
                         fluidRow(column(width=5, tags$div(paste("By ", GetLabel(attrib), ":", sep=""))),
                                  column(width=7, picker))
                       }))),
      column(width=4, 
             wellPanel(style = WellPanelStyle, tags$b("Step2:", "Select the metrics and additional data you are interested in seeing"), tags$hr(),
                       fluidRow(
                         column(width=12, 
                                checkboxGroupInput(inputId="dataFilters_metrics", label = "", 
                                                   choices=allMetricChoices,
                                                   selected=as.character(allMetricChoices)),
                                metricHelp)
                       ))),
      column(width=4, 
             wellPanel(style = WellPanelStyle, tags$b("Step3:", "Show data for a single year, or change between years?"), tags$hr(),
                       fluidRow(
                         column(width=6, radioButtons( "dataFilters_change",
                                                       label="",
                                                       choices = list("Single year" = "Annual", "Change over time" ="Change"),
                                                       selected="Annual") ),
                         column(width=6, conditionalPanel("input.dataFilters_change == 'Annual'",
                                                          pickerInput( inputId="dataFilters_year",					 
                                                                       label="",
                                                                       choices = as.character(data.CU.Metrics.Years),
                                                                       selected = as.character(data.CU.Metrics.Years[length(data.CU.Metrics.Years)]))),
                                conditionalPanel("input.dataFilters_change == 'Change'",
                                                 pickerInput( inputId="dataFilters_changeyear_1",					 
                                                              label="Initial Year:",
                                                              choices = as.character(data.CU.Metrics.Years[1:(length(data.CU.Metrics.Years)-1)]),  # choices do not include the last year
                                                              selected= as.character(data.CU.Metrics.Years[length(data.CU.Metrics.Years)-1])),
                                                 pickerInput( inputId="dataFilters_changeyear_2",					 
                                                              label="Last Year:",
                                                              choices = as.character(data.CU.Metrics.Years[2:length(data.CU.Metrics.Years)]),      # choices do not include the first year
                                                              selected = as.character(data.CU.Metrics.Years[length(data.CU.Metrics.Years)]))) )
                       )))
      
    ))
})

# things to do when the Filter panel is opened
observeEvent(input$UIPanels, {
  if (input$UIPanels == 'Filter') clearInfoPane()
})
