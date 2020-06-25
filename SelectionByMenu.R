# ------------------- Attribute and Metric Selector Menu ------------------

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
    updateSliderInput(session, inputId = id, min=s$min-0.001, max=s$max+0.001, value=c(s.sel$min-0.001, s.sel$max+0.001))
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
    df$CU_ID <- row.names(df)
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
observeEvent(data.currentSelection[['CUs']], {
  if (data.currentSelectionEmpty('CUs')) {
    output$box_DataSelectors <- drawSelectorWidgets()
  } else {
    df <- data.filtered()
    df$CU_ID <- row.names(df)
    sel <- data.currentSelection[['CUs']]
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
}, ignoreNULL = F)

setSelection <- function() {
  if (any(grepl("dataSelectors", names(input)))) {
    df <- data.filtered()
    df$CU_ID <- row.names(df)
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
    if (!((length(data.currentSelection[['CUs']]) == 0 && all(sel)) || 
          setequal(data.currentSelection[['CUs']], row.names(df)[sel]))) {
      data.setSelection(row.names(df)[sel], type='CUs', widget="dataSelectors")
    }
  }
}

observeEvent(input$dataSelectors_resetSelection, {
  data.setSelection(NULL, type='CUs', widget="dataSelectors")
})

# things to do when the Select panel is opened
observeEvent(input$UIPanels, {
  if (input$UIPanels == 'Select') clearInfoPane()
})