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
      values <- cut(values, breaks=hist(values)$breaks)
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
                   xaxis = list(visible = FALSE)) %>% 
    plotly::config(displayModeBar = F)
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
                   showlegend=F) %>% 
    plotly::config(displayModeBar = F)
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
      selectedCUs <- CUs %in% data.currentSelection[['CUs']]
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
  data.currentSelection[['CUs']]
  input$summary_type
}, {
  summaries <- histoSummaries()
  for(a in names(summaries)) {
    local({ 
      lc_a <- a
      output[[sId('summary', lc_a)]] <- plotly::renderPlotly({summaries[[lc_a]][[input$summary_type]]}) 
    })
  }
}, ignoreNULL = F)


# build the UI widgets
output$box_HistoSummary <- renderUI({
  if (input$dataUnit == 'CUs') {
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
                               shinycssloaders::withSpinner(plotly::plotlyOutput(sId('summary', a), height=200))))})
    tagList(fluidRow(column(width=3, cntrl)),
            fluidRow(do.call(tagList, plots)))
  } else { # intput$dataUnit == 'Populations'
    # nothing to show for populations 
    tags$div('no metrics available')
  }
})

# things to do when the Histogram panel is opened
observeEvent(input$UIPanels, {
  if (input$UIPanels == 'Histogram') clearInfoPane()
})