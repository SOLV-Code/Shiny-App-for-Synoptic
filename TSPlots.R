tsPlots.makeCUtable <- function(CUs) {
  tagList(
    tags$div(class='full-sparkline-box-header', 'CUs currently highlighted'),
    spark.makeCUSparklineTable(CUs, mode='full')
  )
}

tsPlots.makePopTable <- function(pops) {
  p <- tags$div(class='full-sparkline-box-header', 'Sites currently highlighted')
  if (length(pops) > 0) {
    p <- tagList(p, spark.makePopSparklineTable(pops, mode='full'))
  }
  p
}


output$box_TSPlots <- renderUI({
    p <- tsPlots.makeCUtable(data.currentSelection[['CUs']])
    if(data.showPops()) 
      p <- tagList(p, tags$hr(), tsPlots.makePopTable(data.currentSelection[['Pops']]))
    p})

# things to do when TSPlots panel is opened
observeEvent(input$UIPanels, {
  if (!is.null(input$UIPanels) && input$UIPanels == 'TSPlots') {
    clearInfoPane()
  }
}, ignoreInit = T)
