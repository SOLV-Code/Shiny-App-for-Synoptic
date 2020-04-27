tsPlots.makeCUtable <- function(CUs) {
  if (length(CUs) > 0)
    tagList(
      tags$div(class='full-sparkline-box-header', 'CUs currently highlighted'),
      spark.makeCUSparklineTable(CUs, mode='full')
    )
  else
    tags$div('There are currently no CUs highlighted')
}

tsPlots.makePopTable <- function(pops) {
  if (length(pops) > 0) 
    tagList(tags$div(class='full-sparkline-box-header', 'Sites currently highlighted'), 
            spark.makePopSparklineTable(pops, mode='full'))
  else
    tags$div('There are currently no CUs highlighted')
}

output$box_TSPlots <- renderUI({
  p <- tsPlots.makeCUtable(data.currentSelection[['CUs']])
  if (data.showPops() && !data.currentSelectionEmpty('Pops'))
    p <- tagList(p, tags$hr(), tsPlots.makePopTable(data.currentSelection[['Pops']]))
  p})

# things to do when TSPlots panel is opened
observeEvent(input$UIPanels, {
  if (!is.null(input$UIPanels) && input$UIPanels == 'TSPlots') {
    clearInfoPane()
  }
}, ignoreInit = T)
