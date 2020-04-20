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
}, ignoreNULL = F)

observeEvent(input$radar_select_metrics, {
  updatePickerInput(session, "radar_ranking", 
                    choices=c("Area", input$radar_select_metrics), 
                    selected = "Area")
})


# Pull selected metrics data
radar_metrics_subset <- reactive({
  req(input$radar_select_metrics)
  if (data.currentSelectionEmpty('CUs')) {
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
  data.currentSelection[['CUs']]
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
    shinycssloaders::withSpinner(plotOutput("radar_Plot", height="550px", width="550px")),
    tags$div(style = 'overflow-x: scroll', DT::dataTableOutput("radar_AreaTable", width="70%"))
  )
})

# things to do when the Radar panel is opened
observeEvent(input$UIPanels, {
  if (input$UIPanels == 'Radar') clearInfoPane()
})
