#------------------- Table and Download Box ------------------

table.dataType <- reactiveVal('CUs')
observeEvent(input$table_DataType, {
  if (input$table_DataType == 'CUs') table.dataType('CUs') else table.dataType('Pops')})
table.downloadType <- reactiveVal('Table')
observeEvent(input$table_DownloadType, {table.downloadType(input$table_DownloadType)})
table.selectionOnly <- reactiveVal(FALSE)
observeEvent(input$table_SelectionOnly, {table.selectionOnly(input$table_SelectionOnly)})
observeEvent(data.showPops(), {if (data.showPops()) table.dataType('Pops') else table.dataType('CUs')})

table.tableData <- reactive({
  if (table.dataType() == 'CUs') {
    df <- data.filtered()
    df2 <- unique(data.CU.Lookup.filtered()[data.CU.Lookup.filtered()$CU_ID %in% row.names(df), c(DataTable.CULookupAttribsToInclude, 'CU_ID')])
    row.names(df2) <- df2$CU_ID
    df2 <- df2[row.names(df), DataTable.CULookupAttribsToInclude, drop=F]
    df <- cbind(df2, df)[, DataTable.MetricCols]
  } else if (table.dataType() == 'Pops') {
    df <- data.Pop.Lookup.filtered()[, DataTable.ColsPop]
  }
  df[order(row.names(df)), ]
})

# Show the filtered data in a table.
# Note that using crosstalk here doesn't produce the desired result, since
# DT shows only selected values (interprets selection as filter?), instead of
# showing full dataset with selected values highlighted.
output$table_Table <- DT::renderDataTable({
  df <- fixForDisplay(table.tableData(), DataTable.Round)
  selected <- isolate(data.currentSelection[[table.dataType()]]) # don't want to re-render this if selection changes; selection handled by using proxy
  sel <- which(row.names(df) %in% selected)
  colNames <- as.character(sapply(names(df), GetLabel))
  if (!data.currentSelectionEmpty(table.dataType())) {
   datatable(df, selection=list(selected=sel, mode='multiple', target='row'), colnames=colNames, options=list(paging=FALSE))
  } else {
   datatable(df, colnames=colNames, options=list(paging=FALSE))
  }
}, server=FALSE)

table.getCurrentTableSelection <- function() {
    row.names(table.tableData())[input$table_Table_rows_selected]
}

table.setTableSelectionFromCurrentSelection <- function() {
  sel <- table.getCurrentTableSelection()
  if (table.dataType() == 'CUs' && !setequal(sel, data.currentSelection[['CUs']])) {
    dataTableProxy('table_Table') %>% selectRows(which(row.names(table.tableData()) %in% data.currentSelection[['CUs']]))
  }
  else if (table.dataType() == 'Pops' && !setequal(sel, data.currentSelection[['Pops']]))
    dataTableProxy('table_Table') %>% selectRows(which(row.names(table.tableData()) %in% data.currentSelection[['Pops']]))
}

# set selection in table in response to change in global selection (e.g., because user clicked 'clear highlighting' button)
# only do this if the table is currently open
observeEvent({data.currentSelection[['CUs']]
             data.currentSelection[['Pops']]}, {
  if (!is.null(input$UIPanels) && input$UIPanels == 'Table') table.setTableSelectionFromCurrentSelection()
  }, ignoreNULL = FALSE) # make sure this handler fires when selection is reset to NULL

# set global selection
# don't let table change global selection unless table is currently being worked on
observeEvent({input$table_Table_rows_selected}, {
  if (!is.null(input$UIPanels) && input$UIPanels == 'Table') {
    sel <- row.names(table.tableData())[input$table_Table_rows_selected]
    if (table.dataType() == 'CUs') {
      if (!setequal(sel, data.currentSelection[['CUs']]))
        data.setSelection(sel, type='CUs', widget="datatable")
    }
    else if (table.dataType() == 'Pops') {
      if (!setequal(sel, data.currentSelection[['Pops']]))
        data.setSelection(sel, type='Pops', widget="datatable")
    }
  }
}, ignoreNULL = FALSE, ignoreInit = TRUE)


observeEvent(input$table_DownloadMenu, {
  if (table.dataType() == 'CUs') units <- 'CUs'
  else units <- 'sites'
  showModal(modalDialog(
    radioButtons(inputId= 'table_DownloadType', label= "Dataset to download",
                 selected = 'Table', inline = TRUE,
                 choiceNames = c('Table as shown', 'Time series data'), choiceValues = c('Table', 'TS')),
    radioButtons(inputId= 'table_SelectionOnly', label = paste0(units, ' to include'),
                 selected = 'all', inline = TRUE,
                 choiceNames = c(paste0('all ', units, ' in table'),
                                 paste0('highlighted ', units, ' only')),
                 choiceValues = c('all', 'selectedOnly')),
    tags$div(id='insertMarkForAdditionalContent'),
    downloadButton("table_Download", "Download", inline=TRUE),
    easyClose = TRUE,
    footer = NULL,
    size = 's'
  ))
})

getDownloadFilename <- function() {
  if (table.dataType() == 'CUs')
    units <- 'CUs'
  else
    units <- 'sites'
  if (table.selectionOnly() == 'selectedOnly')
    f <- paste0("highlighted_", units, '_', table.downloadType(), ".csv")
  else
    f <- paste0(units, '_', table.downloadType(), ".csv")
  f
}

# Downloadable csv of selected dataset
output$table_Download <- downloadHandler(filename = getDownloadFilename,
                                                content = function(file) {
                                                  if (table.downloadType() == 'Table')
                                                    df <- table.tableData()
                                                  else if (table.downloadType() == 'TS') {
                                                    if (table.dataType() == 'CUs') {
                                                      df <- data.CU.TimeSeries[data.CU.TimeSeries$CU_ID %in% data.currentCUs(), DataTable.TScolsCU]
                                                    }
                                                    else {
                                                      df <- data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID %in% data.currentPops(), DataTable.TScolsPop]
                                                    }
                                                  }
                                                  if (table.selectionOnly() == 'selectedOnly') { # download only selection
                                                    if (table.downloadType() == 'Table')
                                                      df <- df[row.names(df) %in% data.currentSelection[[table.dataType()]], ]
                                                    else if (table.downloadType() == 'TS') {
                                                      if (table.dataType() == 'CUs')
                                                        df <- df[df$CU_ID %in% data.currentSelection[['CUs']], ]
                                                      else
                                                        df <- df[df$Pop_UID %in% data.currentSelection[['Pops']], ]
                                                    }
                                                  }
                                                  # had to keep Pop_UID column to filter by - if still present, drop it now ...
                                                  if ('Pop_UID' %in% names(df)) df$Pop_UID <- NULL
                                                  write.csv(df, file, row.names = TRUE)
                                                  removeModal()
                                                })

# warn about missing data before downloading
observeEvent({table.downloadType()
  table.selectionOnly()}, {
    removeUI(selector='#Alert')
    alert <- NULL
    if (table.downloadType() == 'TS') {
      if (table.dataType() == 'CUs') {
        CUs <- data.currentCUs()
        if (table.selectionOnly() == 'selectedOnly')
          CUs <- CUs[CUs %in% data.currentSelection[['CUs']]]
        CUsWithoutTS <- unlist(lapply(CUs, function(cu) {
          !(cu %in% data.CU.TimeSeries$CU_ID) || all(is.na(data.CU.TimeSeries[data.CU.TimeSeries$CU_ID == cu, ]))
        }))
        if (any(CUsWithoutTS))
          alert <- paste0("No time series data available for ", paste(CUs[CUsWithoutTS], collapse = ', '))
      }
      else { # table.dataType() == "Pops"
        pops <- data.currentPops()
        if (table.selectionOnly() == 'selectedOnly')
          pops <- pops[pops %in% data.currentSelection[['Pops']]]
        popsWithoutTS <- unlist(lapply(pops, function(p) {
          !(p %in% data.Pop.TimeSeries$Pop_UID) || all(is.na(data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID == p, ]))
        }))
        popNames <- unlist(lapply(pops[popsWithoutTS], function(p) {getPopNameShort(p)}))
        if (any(popsWithoutTS))
          alert <- paste0("No time series data available for ", paste(popNames, collapse = ', '))
      }
    }
    if(!is.null(alert)) {
      insertUI(selector = '#insertMarkForAdditionalContent', # insert location
               where = "afterEnd",
               ui = tags$div(id = 'Alert', tags$b('Alert!'), alert, tags$hr()))
    }
  })

output$box_Table <- renderUI({
  dataTypeToggle <- radioButtons(inputId = 'table_DataType',
                                 choices = c('CUs', 'Sites'),
                                 selected = if (table.dataType() == 'CUs') 'CUs' else 'Sites',
                                 label = NULL,
                                 inline = TRUE,
                                 width = NULL)
  downloadButton <- actionButton("table_DownloadMenu",  label="Download", icon=icon('download'))
  if (data.showPops())
    header <- div(class='dataTableHead', fluidRow(column(3, dataTypeToggle), column(3, downloadButton, offset=6)))
  else
    header <- div(class='dataTableHead', fluidRow(column(3, downloadButton, offset=9)))
  tagList(header, tags$div(DT::dataTableOutput("table_Table", width="50%")))
})


# things to do when the Table panel is opened
observeEvent(input$UIPanels, {
  if (!is.null(input$UIPanels) && input$UIPanels == 'Table') {
    clearInfoPane()
    table.setTableSelectionFromCurrentSelection()
  }
})
