#------------------- Table and Download Box ------------------

tableData <- reactive({
  if (input$dataUnit == 'CUs') {
    df <- data.filtered()
  } else if (input$dataUnit == "Pops") {
    df <- data.Pop.Lookup[data.Pop.Lookup$CU_ID %in% row.names(data.filtered()), ]
  }
  # get rid of trailing zero segments for display
  if ('FWA_WATERSHED_CODE' %in% names(df))
    df$FWA_WATERSHED_CODE <- unlist(lapply(df$FWA_WATERSHED_CODE, strip))
  df <- df[order(row.names(df)), ]
})

# Show the filtered data in a table.
# Note that using crosstalk here doesn't produce the desired result, since 
# DT shows only selected values (interprets selection as filter?), instead of 
# showing full dataset with selected values highlighted. 
output$selectedData_Table <- DT::renderDataTable({
  df <- tableData()
  selected <- isolate(data.currentSelection[[input$dataUnit]]) # don't want to re-render this if selection changes; selection handled by using proxy
  sel <- which(row.names(df) %in% selected)
  colnames <- as.character(sapply(names(df), GetLabel))
  if (!is.null(sel)) {
    datatable(df, selection=list(selected=sel), colnames=colnames)
  } else {
    datatable(df, colnames=colnames)
  }
}, server=FALSE)

observeEvent(data.currentSelection[['CUs']], {
  sel <- which(row.names(tableData()) %in% data.currentSelection[['CUs']])
  dataTableProxy('selectedData_Table') %>% selectRows(sel)
}, ignoreNULL = FALSE) # make sure this handler fires when selection is reset to NULL

observeEvent(data.currentSelection[['Pops']], {
  sel <- which(row.names(tableData()) %in% data.currentSelection[['Pops']])
  dataTableProxy('selectedData_Table') %>% selectRows(sel)
}, ignoreNULL = FALSE) # make sure this handler fires when selection is reset to NULL

# set global selection 
observeEvent({input$selectedData_Table_rows_selected}, {
  sel <- row.names(tableData())[input$selectedData_Table_rows_selected]
  if (input$dataUnit == 'CUs')
    data.setSelection(sel, type='CUs', widget="datatable")
  else if (input$dataUnit == 'Pops')
    data.setSelection(sel, type='Pops', widget="datatable")
}, ignoreNULL = FALSE, ignoreInit = TRUE)


observeEvent(input$selectedData_DownloadMenu, {
  if (input$dataUnit == 'CUs') units <- 'CUs'
  else units <- 'populations' 
  showModal(modalDialog(
    radioButtons(inputId= 'selectedData_DownloadType', label= "Dataset to download", 
                 selected = 'Table', inline = TRUE,
                 choiceNames = c('Table as shown', 'Time series data'), choiceValues = c('Table', 'TS')),
    radioButtons(inputId= 'selectedData_SelectionOnly', label = paste0(units, ' to include'), 
                 selected = 'all', inline = TRUE,
                 choiceNames = c(paste0('all ', units, ' in table'),
                                 paste0('highlighted ', units, ' only')),
                 choiceValues = c('all', 'selectedOnly')),
    tags$div(id='insertMarkForAdditionalContent'),
    downloadButton("selectedData_Download", "Download", inline=TRUE),
    easyClose = TRUE,
    footer = NULL,
    size = 's'
  ))
})

getDownloadFilename <- function() {
  if (input$dataUnit == 'CUs') 
    units <- 'CUs'
  else 
    units <- 'populations' 
  if (input$selectedData_SelectionOnly == 'selectedOnly')
    f <- paste0("highlighted_", units, '_', input$selectedData_DownloadType, ".csv")
  else
    f <- paste0(units, '_', input$selectedData_DownloadType, ".csv")
  f
}

# Downloadable csv of selected dataset
output$selectedData_Download <- downloadHandler(filename = getDownloadFilename, 
                                                content = function(file) {
                                                  if (input$selectedData_DownloadType == 'Table') 
                                                    df <- tableData()
                                                  else if (input$selectedData_DownloadType == 'TS') {
                                                    if (input$dataUnit == 'CUs') {
                                                      CUs <- row.names(data.filtered())
                                                      df <- data.CU.TimeSeries[data.CU.TimeSeries$CU_ID %in% CUs, c('CU_ID', 'CU_Name', 'Species', 'Year', filter[['DataType']])]
                                                    }
                                                    else {
                                                      pops <- data.Pop.Lookup[data.Pop.Lookup$CU_ID %in% row.names(data.filtered()), 'Pop_UID']
                                                      df <- data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID %in% pops, c('DataSet', 'Year', 'Pop_ID', 'Pop_Name', 'CU_ID', 'CU_Name', filter[['DataType']])]
                                                    }
                                                  }
                                                  if (input$selectedData_SelectionOnly == 'selectedOnly') { # download only selection
                                                    if (input$selectedData_DownloadType == 'Table') 
                                                      df <- df[row.names(df) %in% data.currentSelection[[input$dataUnit]], ]
                                                    else if (input$selectedData_DownloadType == 'TS') {
                                                      if (input$dataUnit == 'CUs') 
                                                        df <- df[df$CU_ID %in% data.currentSelection[['CUs']], ]
                                                      else 
                                                        df <- df[df$Pop_UID %in% data.currentSelection[['Pops']], ]
                                                    }
                                                  }
                                                  write.csv(df, file, row.names = TRUE)
                                                  removeModal()
                                                })

# warn about missing data before downloading
observeEvent({input$selectedData_DownloadType
  input$selectedData_SelectionOnly}, {
    removeUI(selector='#Alert')
    alert <- NULL
    if (input$selectedData_DownloadType == 'TS') {
      if (input$dataUnit == 'CUs') {
        CUs <- row.names(data.filtered())
        if (input$selectedData_SelectionOnly == 'selectedOnly') 
          CUs <- CUs[CUs %in% data.currentSelection[['CUs']]]
        CUsWithoutTS <- unlist(lapply(CUs, function(cu) {
          !(cu %in% data.CU.TimeSeries$CU_ID) || all(is.na(data.CU.TimeSeries[data.CU.TimeSeries$CU_ID == cu, filter[['DataType']]]))
        }))
        if (any(CUsWithoutTS)) 
          alert <- paste0("No time series data available for ", paste(CUs[CUsWithoutTS], collapse = ', '))
      }
      else {
        pops <- data.Pop.Lookup[data.Pop.Lookup$CU_ID %in% row.names(data.filtered()), 'Pop_UID']
        if (input$selectedData_SelectionOnly == 'selectedOnly') 
          pops <- pops[pops %in% data.currentSelection[['Pops']]]
        popsWithoutTS <- unlist(lapply(pops, function(p) {
          !(p %in% data.Pop.TimeSeries$Pop_UID) || all(is.na(data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID == p, filter[['DataType']]]))
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

output$box_SelectedDataTable <- renderUI({ 
  tagList(tags$div('style' = "text-align:right;", 
                   actionButton("selectedData_DownloadMenu",  label="Download", icon=icon('download'))),
          DT::dataTableOutput("selectedData_Table", width="50%"))
})

# things to do when the Table panel is opened
observeEvent(input$UIPanels, {
  if (input$UIPanels == 'Table') clearInfoPane()
})
