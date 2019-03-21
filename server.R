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
                      "sp",
                      "leaflet",
                      "shinyBS")
# 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


#--------- ---------- Helper functions ------------------



# assemble the id of a shiny input widget or a variable name from a prefix and a postfix, e.g. widget.1 
sId <- function(pre, post) {paste(pre, post, sep="_")}

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

# ------------- Data filtering --------------------------  

  filter <- reactiveValues()
  for (a in FilterAttributes) {
    local({
      lc_a <- a
      filter[[lc_a]] <- unique(as.character(data.start[, lc_a]))
      observeEvent(input[[sId("dataFilters", lc_a)]], {filter[[lc_a]] <- input[[sId("dataFilters", lc_a)]]}, ignoreNULL = F, ignoreInit = F)
    })
  }

  filter$year <- max(data.start$Year)
  observeEvent(input$dataFilters_year, {filter$year <- input$dataFilters_year})
  filter$change <- "Annual"
  observeEvent(input$dataFilters_change, {filter$change <- input$dataFilters_change})
  filter$metrics <- c(FilterMFMetrics, FilterMFAttributes)
  observeEvent(input$dataFilters_metrics, {filter$metrics <- input$dataFilters_metrics})
  filter$changeyear_1 <- data.years[length(data.years) - 1]
  observeEvent(input$dataFilters_changeyear_1, {filter$changeyear_1 <- input$dataFilters_changeyear_1})
  filter$changeyear_2 <- data.years[length(data.years)]
  observeEvent(input$dataFilters_changeyear_2, {filter$changeyear_2 <- input$dataFilters_changeyear_2})
  
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
  # all other filters are limited to what's available for the selected species
  observeEvent(filter$Base.Unit.Species, {
    df <- getFilteredDF(c('Base.Unit.Species'), data.start)
    updatePickerInputs(c('BaseUnit.Watershed', 'FAZ', 'Management.Timing', 'Base.Unit.CU.ShortName'), df) 
  }, ignoreNULL = FALSE)
  
  # watershed limits what FAZs and CUs are available
  observeEvent(filter$BaseUnit.Watershed, {
    df <- getFilteredDF(c('Base.Unit.Species', 'BaseUnit.Watershed', 'Management.Timing'), data.start)
    updatePickerInputs(c('FAZ', 'Base.Unit.CU.ShortName'), df) 
  }, ignoreNULL = FALSE)
  
  # FAZ an Management Timing limit what CUs are available
  observeEvent({
    filter$FAZ
    filter$Management.Timing
  }, {
    df <- getFilteredDF(c('Base.Unit.Species', 'BaseUnit.Watershed','Management.Timing', 'FAZ'), data.start)
    updatePickerInputs(c('Base.Unit.CU.ShortName'), df) 
  }, ignoreNULL = FALSE)
  

  observeEvent(                            
    {input$dataFilters_changeyear_1},{ 
      choices <- data.years[as.numeric(data.years) > as.numeric(input$dataFilters_changeyear_1)]
      updatePickerInput(session, "dataFilters_changeyear_2", 
                        choices=choices, 
                        selected=choices[1])  
    })    
  
  observeEvent(                            # will need to update selections once we have more than 2 years so both cannot be same year
    {input$dataFilters_changeyear_2},{ 
      choices <- data.years[as.numeric(data.years) < as.numeric(input$dataFilters_changeyear_2)]
      updatePickerInput(session, "dataFilters_changeyear_1", 
                        choices=choices, 
                        selected=choices[1] )  
    }) 
  
  output$box_DataFilters <- renderUI({
    metricChoices <- list(
       Metrics = FilterMFMetrics[FilterMFMetrics %in% names(data.start)],
       Attributes = FilterMFAttributes[FilterMFAttributes %in% names(data.start)])
    names(metricChoices$Metrics) = as.character(lapply(metricChoices$Metrics, GetLabel))
    names(metricChoices$Attributes) = as.character(lapply(metricChoices$Attributes, GetLabel))
    allMetricChoices <- c(metricChoices$Metrics, metricChoices$Attributes)
    metricHelp <- lapply(as.character(allMetricChoices), function(m) {
                          makeCheckboxTooltip(checkboxValue = m,
                                 buttonLabel = "?",
                                 buttonId = sId("dataFiltersMetricHelp", which(as.character(allMetricChoices) == m)),
                                 Tooltip = htmlEscape(MetricInfo[[m]], attribute=TRUE))
              })

    tagList(
      fluidRow(
        column(width=4,
               wellPanel(style = WellPanelStyle, tags$b("Step1:",  "Filter your data"), tags$hr(),
                         lapply(FilterAttributes[FilterAttributes %in% names(data.start)], function(attrib) {
                                  choices <- GetNamedChoices(attrib, data.start)
                                  if (attrib %in% FilterSingleChoiceAttributes) { 
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
               wellPanel(style = WellPanelStyle, tags$b("Step2:", "Select metrics and/or attributes of interest"), tags$hr(),
                         fluidRow(
                           # column(width=12, pickerInput(inputId="dataFilters_metrics",
                           #                              label="",
                           #                              choices=metricChoices,
                           #                              selected=as.character(unlist(metricChoices)),
                           #                              multiple=TRUE,
                           #                              options=PickerOptsMultiSelect))
                            column(width=12, 
                                   checkboxGroupInput(inputId="dataFilters_metrics", label = "", 
                                                      choices=allMetricChoices,
                                                      selected=as.character(allMetricChoices)),
                                   metricHelp)
                         ))),
        column(width=4, 
               wellPanel(style = WellPanelStyle, tags$b("Step3:", "Show analysis for a single year, or change between years?"), tags$hr(),
                         fluidRow(
                           column(width=6, radioButtons( "dataFilters_change",
                                                         label="",
                                                         choices = list("Single year" = "Annual", "Change over time" ="Change"),
                                                         selected="Annual") ),
                           column(width=6, conditionalPanel("input.dataFilters_change == 'Annual'",
                                                            pickerInput( inputId="dataFilters_year",					 
                                                                         label="",
                                                                         choices = data.years,
                                                                         selected = data.years[length(data.years)])),
                                  conditionalPanel("input.dataFilters_change == 'Change'",
                                                   pickerInput( inputId="dataFilters_changeyear_1",					 
                                                                label="Initial Year:",
                                                                choices = data.years[1:(length(data.years)-1)],  # choices do not include the last year
                                                                selected= data.years[length(data.years)-1]),
                                                   pickerInput( inputId="dataFilters_changeyear_2",					 
                                                                label="Last Year:",
                                                                choices = data.years[2:length(data.years)],      # choices do not include the first year
                                                                selected = data.years[length(data.years)])) )
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
    if(filter$change == "Change" ){   
        calc.change <- function(x){x-dplyr::lag(x, default=dplyr::first(x))}
        df <- df %>% group_by(Base.Unit.CU.ShortName) %>%
              filter(Year %in% c(input$dataFilters_changeyear_1, input$dataFilters_changeyear_2)) %>%
              arrange(Year, .by_group=TRUE) %>%
              mutate(WSP.numeric = as.numeric(WSP.status)) 
        df <- df %>% dplyr::mutate_at(.vars=numericMetrics(df), .funs=calc.change) %>%
              filter(Year== max(Year)) %>%
              select(-WSP.status)
        if (!("WSP.status" %in% filter$metrics)) { # don't keep WSP.numeric column unless WSP.status is of interest
          df <- df %>% select(-WSP.numeric)
        }
    } else { # use annual values
      df <- df %>% filter(Year %in% filter$year)
    }
    df <- as.data.frame(df)
    row.names(df) <- df$Base.Unit.CU.ShortName
    
    # filter data by row, based on filter criteria 
    selection <- rep(T, nrow(df))
    for (a in FilterAttributes) {
      selection <- selection & df[, a] %in% filter[[a]]
    }
    df <- df[selection, , drop=F]
    # remove any metrics and categories the user doesn't want to see
    # make this general by keeping everything that's not explicitly excluded
    drops <- c(FilterMFMetrics[!(FilterMFMetrics %in% filter$metrics)], 
               FilterMFAttributes[!(FilterMFAttributes %in% filter$metrics)])
    # additional columns that are no longer needed and can be dropped now
    
    drops <- c(drops, "Year", "Base.Unit.CU.ShortName")
    df[ ,drops] <- NULL
    as.data.frame(df)
  })
 
  # keep track of the current selection
  data.currentSelection <- reactiveVal(NULL)
  
  # keep track of the last widget that set the selection
  data.selectedBy <- reactiveVal("none")
  
  # Same as data.filtered(), but all rows removed that aren't currently selected
  data.selected <- reactive({data.filtered()[data.currentSelection(), , drop=F]})
  
  # set the data selection 
  data.setSelection <- function(CUs, widget) {
    data.currentSelection(CUs)
    data.selectedBy(widget)
  }
  
  # add CUs to the current selection
  data.addToSelection <- function(CUs, widget) {
    if (is.null(data.currentSelection()) || length(data.currentSelection()) == 0) {
      sel <- CUs
    } else {
      sel <- union(data.currentSelection(), CUs)
    } 
    data.currentSelection(sel)
    data.selectedBy(widget)
  }
  
  # remove CUs from the current selection
  data.removeFromSelection <- function(CUs, widget) {
    if (any(!(data.currentSelection() %in% CUs))) {
      sel <- data.currentSelection()[!(data.currentSelection() %in% CUs)]
    } else {
      sel <- NULL 
    }
    data.currentSelection(sel)
    data.selectedBy(widget)
  }
  
  observeEvent(input$sidebarMenu_clearSelection,{data.setSelection(NULL, "clearSelection_button")})
  
  #------------------- Attribute and Metric Range Selector  ------------------
  
  sliderVals <- function(vals) {
    if (any(!is.na(vals))) {
       list(min=min(vals, na.rm=T) , max=max(vals, na.rm=T))
    } else {
      NULL
    }
  }
  
  # generate a slider widget for the given metric
  slider  <- function(m, sels, df) {
    s <- sliderVals(df[ ,m])
    if (length(sels) == 0) {
      s.sel <- s
    } else {
      s.sel <- sliderVals(df[sels ,m])
    }
    if (!is.null(s)) {
      id <- sId("dataSelectors", m)
      idNA <- paste(id, "includeNAs", sep='_')
      observeEvent({input[[id]]
                    input[[idNA]]}, 
                    {setSelection()},
                   ignoreNULL = FALSE, ignoreInit = T)
      # need to set step size and make the slider & selection one step bigger than data values, 
      # otherwise the boundary values used to define the range may end up selecting out extreme data points 
      sliderInput(inputId = id,
                  label = NULL, #GetLabel(m),
                  min = s$min-0.001,
                  max = s$max+0.001,
                  step=0.001,
                  value=c(s.sel$min-0.001, s.sel$max+0.001))
    } else {
      NULL
    }
  }
  
  resetSlider  <- function(m, sels, df) {
    s <- sliderVals(df[ ,m])
    s.sel <- sliderVals(df[sels ,m])
    id <- sId('dataSelectors', m)
    if (!is.null(s) && id %in% names(input)) {
      updateSliderInput(session, inputId = id, min=s$min, max=s$max, value=c(s.sel$min-0.001, s.sel$max+0.001))
    }
  }
  
  inRange <- function(v, range, includeNAs) {
    if (includeNAs) {
      is.na(v) | (v >= range[1] & v <= range[2])
    } else {
      !is.na(v) & (v >= range[1] & v <= range[2])
    }
  }
  
  naCheckbox <- function(m) {
    id <- paste(sId("dataSelectors", m), "includeNAs", sep='_')
    checkboxInput(id, label="NAs", value=T)
  }
  
  drawSelectorWidgets <- function() {
    renderUI({
      df <- data.filtered()
      df$Base.Unit.CU.ShortName <- row.names(df)
      sel <- row.names(df) # initially, have everything 'on'. Let this be modified if/when a selection is set
      tagList(
        tags$b("Use these widgets to generate a selection of CUs 'of concern' that can be compared to the full (filtered) dataset"),
        fluidRow(
          column(width=5,
                wellPanel(style = WellPanelStyle, 
                          fluidRow(column(width=12, tags$h4("Select by Attribute"))),
                          tags$hr(),
                          fluidRow(lapply(SelectAttributes[SelectAttributes %in% names(df)], 
                                         function(m) {
                                            id <- sId("dataSelectors", m)
                                            observeEvent(input[[id]], {setSelection()}, ignoreNULL = F, ignoreInit = T)
                                            pickerInput(inputId=sId("dataSelectors", m),					 
                                                        label=GetLabel(m),
                                                        choices=GetNamedChoices(m, df),
                                                        selected=GetNamedChoices(m, df[sel, ]),
                                                        multiple=TRUE,
                                                        options=PickerOptsMultiSelect)
                                        }))),
                fluidRow(
                  column(width=6, actionButton("dataSelectors_resetSelection",label="Reset", style=ButtonStyle)))),
          column(width=7, 
               wellPanel(style = WellPanelStyle, 
                         fluidRow(column(width=7, tags$h4("Select by Metric Range"))),
                         tags$hr(),
                         fluidRow(lapply(numericMetrics(df), 
                                         function(m) {
                                           column(width=12, 
                                                  fluidRow(column(width=5, tags$b(GetLabel(m)), `style` = "line-height:50px;"),
                                                           column(width=2, naCheckbox(m)),
                                                           column(width=5, slider(m, sel, df))))
                                                      }))))
        ))
    })
  }
  
  output$box_DataSelectors <- drawSelectorWidgets()
  

  #update with current selection
  observeEvent(data.currentSelection(), {
    if (is.null(data.currentSelection()) || length(data.currentSelection()) == 0) {
       output$box_DataSelectors <- drawSelectorWidgets()
    } else {
      df <- data.filtered()
      df$Base.Unit.CU.ShortName <- row.names(df)
      sel <- data.currentSelection()
      if (is.null(sel)) sel <- row.names(df)
      for (a in SelectAttributes[SelectAttributes %in% names(df)]) {
        choices <- GetNamedChoices(a, df)
        selected <- GetNamedChoices(a, df[sel, ])
        updatePickerInput(session, sId("dataSelectors", a), selected=selected, choices=choices)
      }
      for (m in numericMetrics(data.filtered())) {
        resetSlider(m, sel, df)
        updateCheckboxInput(session, sId('dataSelectors_IncludeNAs', m), label="NAs", value=T)
      }
    }
  })
    
  setSelection <- function() {
    if (any(grepl("dataSelectors", names(input)))) {
      df <- data.filtered()
      df$Base.Unit.CU.ShortName <- row.names(df)
      sel <- rep(T, nrow(df))
      for (a in SelectAttributes[SelectAttributes %in% names(df)]) {
        id <- sId("dataSelectors", a)
        if (!is.null(input[[id]])) 
          sel <- sel & (df[ ,a] %in% input[[id]])
        else {
          sel <- rep(F, nrow(df))
        }
      }
      for (m in numericMetrics(df)) {
        id <- sId('dataSelectors', m)
        if (!is.null(input[[id]])) 
          sel <- sel & (inRange(df[ ,m], input[[id]], input[[paste(id, "includeNAs", sep='_')]]))
      }
      # only set current selection if the user has modified something
      if (!((length(data.currentSelection()) == 0 && all(sel)) || 
            setequal(data.currentSelection(), row.names(df)[sel]))) {
        data.setSelection(row.names(df)[sel], "dataSelectors")
      }
    }
  }

  observeEvent(input$dataSelectors_resetSelection, {
    data.setSelection(NULL, "dataSelectors")
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
      tr <- tags$tr(tags$td(GetLabel(metric)), 
                    tags$td(as.character(round(as.numeric(end), 2))), 
                    tags$td(''))
    } else {
      if (is.na(start) || is.na(end)) {
        tr <- tags$tr(tags$td(GetLabel(metric)), 
                      tags$td('NA'), 
                      tags$td(''))
      } else {
        change <- (end - start)/start
        tr <- tags$tr(tags$td(GetLabel(metric)), 
                      tags$td(as.character(round(as.numeric(end), 2))), 
                      tags$td(HTML(makeArrow(polarAngle(change)))))
      }
    }
    tr
  }
  
  # create an info pane for the given CU
  makePopup <- function(CU, endDf, startDf=NULL, metrics=MapLabelMetrics) {
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
  
  makePopupCol <- function(dfEnd, dfStart=NULL, metrics=MapLabelMetrics) {
    sapply(row.names(dfEnd), function(CU) {
      gsub("[\n]", "", as.character(makePopup(CU, dfEnd, dfStart, metrics)))
    })
  }
  
  fishIcons <- leaflet::iconList(
    'red-fish' = makeIcon("fish-red.png", "fish-red.png", iconHeight=24, iconWidth=24),
    'black-fish' = makeIcon("fish-black.png", "fish-black.png", iconHeight=24, iconWidth=24)
  )
  
  # marks up the spatial data frame to highlight currently selected items
  withSpatialSelection <- function(df) {
    # current selection, to be used as a group in leaflet
    if (!is.null(df)) {
      df$selected <- rep('not selected', nrow(df))
      df$selected[df$Base.Unit.CU.ShortName %in% isolate(data.currentSelection())] <- 'selected'
      # icons
      df$icon <- ifelse(df$selected == 'selected', 'red-fish', 'black-fish')
      df
    }
  }
  
  data.spatial <- reactive({
    df <- data.filtered()
    if (is.data.frame(df) && nrow(df) > 0) {
      # attach the information in the lookup table
      df$Base.Unit.CU.ShortName <- row.names(df)
      df <- merge(df, data.spatialLookup, all.x=T, all.y=F, by=c("Base.Unit.CU.ShortName"))
      row.names(df) <- df$Base.Unit.CU.ShortName
      # put together a information pane for each CU, to be shown on mouse-over on the map
      if (filter$year == data.years[1]) {
        df$popup <- makePopupCol(df, NULL)
      } else {
        df$popup <- makePopupCol(df, data.by.year[[data.years[1]]][row.names(df), ])
      }
      # add selection info
      df <- withSpatialSelection(df) 
      # CU polyons
      CUpolys <- sp::merge(data.CUpolygons, df, by=c("CU_INDEX"), all.x=FALSE, all.y=FALSE) 
      # groups for map; use species, and/or management timing if Fraser sox selected
      CUpolys$grp <- as.character(MapLevelLabels[['Base.Unit.Species']][as.character(CUpolys$Base.Unit.Species)])
      if (("Management.Timing" %in% names(CUpolys)) && any(!is.na(CUpolys$Management.Timing)) ) {
        fraserSox <- CUpolys$Base.Unit.Species == "SK" & CUpolys$BaseUnit.Watershed == "Fraser"
        CUpolys$grp[which(fraserSox)] <-as.character(MapLevelLabels[['Management.Timing']][as.character(CUpolys$Management.Timing[fraserSox])])
      }
      CUpolys$grp <- factor(CUpolys$grp, levels <- c(as.character(MapLevelLabels[['Management.Timing']]),
                                                     as.character(MapLevelLabels[['Base.Unit.Species']])))
      # jitter the marker locations so all will be visible
      CUpolys$latitude <- jitter(CUpolys$latitude, factor=0.050)
      CUpolys$longitude <- jitter(CUpolys$longitude, factor=0.050)
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
                   data.removeFromSelection(CU, "map")
                   icon <- fishIcons['black-fish']
                 } else {
                   data.addToSelection(CU, "map")
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
    if (!is.null(df)) {
      CUmapProxy %>% addMarkers(data=df, lng=~longitude, lat=~latitude, 
                                layerId = ~Base.Unit.CU.ShortName, 
                                icon = ~fishIcons[icon],
                                group = ~selected,
                                label = ~lapply(popup, HTML))
    }
  })
  
  output$box_LeafletMap <- renderUI({leafletOutput("CUmap", height = 500)})
  
  
  #------------------- Parallel Coordinate Plot ------------------
  
  # Create data for the parallel plot (reorder columns and rows)
  data.parcoords <- reactive({
     df <- arrangeColumns(data.filtered(), colOrder=ParcoordsMetricOrder)
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
             d[['title']] <- GetLabel(m)
             if (m %in% numericMetrics(dataset)) {
               if (nrow(dataset) > 0 && any(!is.na(dataset[, m]))) {
                  d[['nullValue']] <- median(dataset[, m], na.rm = T) # change this to "top" or "bottom" to show nulls above or below chart
                  d[['min']] <- min(dataset[, m], na.rm = T)
                  d[['max']] <- max(dataset[, m], na.rm = T)
                  d[['info']] <- MetricInfo[[m]]
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
 
 
  output$parcoords_Plot <- parcoords::renderParcoords({ p <- try(
                                                parcoords::parcoords(data=sharedDS.parcoords,
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
                           column(2,tags$div(title=MetricInfo[[m]],
                                             checkboxInput(inputId = sId("parcoords_visible", m), 
                                                           label=GetLabel(m),
                                                           value=!d[[m]][['hide']], width='20px'))) })
    # control widgets for numerical metrics
    numWidgets <- lapply(metrics[metrics %in% numericMetrics(df)], 
                         function(m) { 
                           column(2,tags$div(title=MetricInfo[[m]],
                                             checkboxInput(inputId = sId("parcoords_visible", m), 
                                                           label=GetLabel(m),
                                                           value=!d[[m]][['hide']], width='20px'),
                                             sliderInput(inputId = sId("parcoords_yrange", m),
                                                         label = NULL,
                                                         min = d[[m]][['min']],
                                                         max = d[[m]][['max']],
                                                         value = c(d[[m]][['ymin']],d[[m]][['ymax']]), 
                                                         width='80px')))})
    tagList(tags$b("Adjust axes:"),
            fluidRow(do.call(tagList, c(numWidgets, catWidgets))))
  })

  # reset parcoords graph, 
  # by resetting sliders and thereby triggering corresponding changes in dims()
  observeEvent({
    input$parcoords_reset_brush
    filter$change
    filter$metrics}, {
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
    data.setSelection(selectedCUs, "parcoords")
  }, ignoreInit = T)
  
  observeEvent(input$parcoords_reset_brush, {
    data.setSelection(NULL, "parcoords")
    sharedDS.parcoords$selection(NULL)
  }, ignoreInit = T)
  
  # set the crosstalk selection in response to changes in the current selection
  observeEvent(data.currentSelection(), {
    if (data.selectedBy() != "parcoords" && data.selectedBy() != "none") {
      selected <- row.names(sharedDS.parcoords$origData()) %in% data.currentSelection()
      sharedDS.parcoords$selection(selected)
    }
  }, ignoreNULL = F, ignoreInit = T)
  
  output$box_Parcoords <- renderUI({ 
    tagList( tags$div('style' = "text-align:right;", 
                      actionButton(inputId = "parcoords_reset_brush",
                                   label="Reset Brushing",icon("paper-plane"), 
                                   style=ButtonStyle),
                      actionButton(inputId = "parcoords_scale_to_selected",
                                   label="Scale to Selected",icon("search-plus"), 
                                   style=ButtonStyle)),
             
    #         tags$div(`class` = 'scroll-container', `style`="width: 500px; height=600px; overflow: auto; border: 1px solid;",
                      parcoords::parcoordsOutput("parcoords_Plot", height="600px"),           # 400px is defaultheight
             uiOutput("parcoords_Controls"))
  })
  
  #------------------- Selected Data Box ------------------
  
  # Show the filtered data in a table.
  # Note that using crosstalk here doesn't produce the desired result, since 
  # DT shows only selected values (interprets selection as filter?), instead of 
  # showing full dataset with selected values highlighted. 
  output$selectedData_Table <- DT::renderDataTable({
    selectedCUs <- isolate(data.currentSelection()) # only want to re-render this if data.filtered changes; selection handled by using proxy
    sel <- which(row.names(data.filtered()) %in% selectedCUs)
    colnames <- as.character(sapply(names(data.filtered()), GetLabel))
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
    data.setSelection(CUs, "datatable")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Downloadable csv of selected dataset
  output$selectedData_Download <- downloadHandler(
    filename = "selection.csv", 
    content = function(file) {
      if(length(data.currentSelection() == 0L)) {
        write.csv(data.filtered(), file, row.names = TRUE)
      } else {
        write.csv(data.selected(), file, row.names = TRUE)
      }
    })
  
  output$box_SelectedDataTable <- renderUI({ 
    tagList(tags$div('style' = "text-align:right;", downloadButton("selectedData_Download", "Download")),
            DT::dataTableOutput("selectedData_Table", width="50%"))
  })
  
  
  
  #------------------- Radar Plots ------------------
  
  # update available selections for the radar plot metrics
  currentRadarMetricOpts <- reactive({ names(data.filtered())[names(data.filtered()) %in% RadarMetricOpts] })
  
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
    df.long$metricName <- sapply(df.long$metricName, GetLabel)
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
      names(dfForTable) <- sapply(names(dfForTable), GetLabel)
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
    if (is.factor(values)) {
      bins <- levels(values)
      if (any(is.na(values))) bins[length(bins) + 1] <- 'NA'
    } else {
      bins <- levels(factor(values, exclude=NULL))
      bins[is.na(bins)] <- 'NA'
    }
    n.bins <- length(bins)
    yf <- factor(bins, levels=bins, ordered=T)
    levels(yf) <- sapply(bins, GetLabel)
    dfbars <- data.frame(x.sel=rep(0, n.bins), x.sel.l=rep("", n.bins), 
                         x.notsel=rep(0, n.bins), x.notsel.l=rep("", n.bins),
                         y=yf, stringsAsFactors = F)
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
      if (any(sels)) df$x[sels] <- 1:n.sels  # first show the selected CUs
      if (any(not.sels)) df$x[not.sels] <- (n.sels+1):(n.sels+n.notsels) # now the unselected
      dfbars[i, c('x.sel', 'x.notsel')] <- c(n.sels, n.notsels)
      dfbars[i, c('x.sel.l', 'x.notsel.l')] <- c(paste(n.sels, " CUs"), paste(n.notsels, " CUs"))
    }
    
    # 'dot' plot
    p.dots <- plotly::plot_ly(df, x=~x, y=~y, height = 200, type="scatter", mode="markers", text=~label, hoverinfo="text",
                    marker = list(size = 10,
                                  color = ~color,
                                  line = list(color = 'rgba(0, 0, 0, .8)', width = 2))) %>%
              plotly::layout(yaxis = list(title="", 
                                          zeroline = FALSE, 
                                          tickvals = 1:length(bins), 
                                          ticktext=sapply(bins, GetLabel), 
                                          showgrid=FALSE),
                             xaxis = list(visible = FALSE))
    # barplot
    p.bars <- plotly::plot_ly(dfbars, y=~y, x=~x.sel, height = 200, type="bar", orientation='h',
                              name="selected", text=~x.sel.l, hoverinfo="text", 
                              marker = list( color = 'red',
                                             line = list(color = 'rgba(0, 0, 0, .8)', width = 2))) %>%
              plotly::add_trace(x = ~x.notsel, 
                                name = "not selected", text=~x.notsel.l, hoverinfo="text",
                                marker = list( color = 'white',
                                                line = list(color = 'rgba(0, 0, 0, .8)', width = 2))) %>%
              plotly::layout(yaxis = list(title="", zeroline = FALSE, showgrid=FALSE),
                             xaxis = list(visible = FALSE),
                             barmode = 'stack',
                             showlegend=F)
    list(dots=p.dots, bars=p.bars)
  }
  
  histoSummaries <- reactive({
    intervalInfo <- HistoCustomInfo[[filter$change]]
    plots <- list()
    summaryAttribs <- HistoSummaryAttribs[HistoSummaryAttribs %in% names(data.filtered())]
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
    input$summary_type
    }, {
      summaries <- histoSummaries()
      for(a in names(summaries)) {
        local({ 
           lc_a <- a
           output[[sId('summary', lc_a)]] <- plotly::renderPlotly({summaries[[lc_a]][[input$summary_type]]}) 
        })
      }
  })
  

  # build the UI widgets
  output$box_HistoSummary <- renderUI({
    if (nrow(data.filtered()) < HistoMaxDots) type <- "dots" else type <- "bars"
    cntrl <- radioGroupButtons("summary_type", label = NULL,
                                choices = c("dots", "bars"),
                                selected = type,
                                status = "primary",
                                size = "normal", 
                                justified = TRUE,
                                individual = FALSE)
    summaryAttribs <- HistoSummaryAttribs[HistoSummaryAttribs %in% names(data.filtered())]
    plots <- lapply(summaryAttribs, function(a) {
              column(width=4, tags$div(title=MetricInfo[[a]],
                                       h4(GetLabel(a)),
                                       plotly::plotlyOutput(sId('summary', a), height=200)))})
  
    tagList(fluidRow(column(width=3, cntrl)),
            fluidRow(do.call(tagList, plots)))
  })
  
  #------------------- All Data Tab ------------------
  
  
  output$allData_Table <- DT::renderDataTable({
    colnames <- as.character(sapply(names(data.start), GetLabel))
    DT::datatable(data.start, colnames=colnames)
  })
  
  # Downloadable csv of selected dataset
  output$allData_Download <- downloadHandler(  
    filename = "data.csv",
    content = function(file) {write.csv(data.start, file, row.names = FALSE)}
  )
  
  
  
} # end server function
#   


