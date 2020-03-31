#-------------------Leaflet Map for CU and Population Selection  ------------------

# state information for controls; can't use shiny inputs directly for these, 
# because the inputs don't exist until modal menus are opened
mapCtrl.isVisible <- reactiveValues(CUMarkers = TRUE, CUPolygons=FALSE, PopMarkers=TRUE, Streams=TRUE)
mapCtrl.selectionMode <- reactiveVal(value = 'CUs', label = 'selectionMode')
mapCtrl.colorScheme <- reactiveVal(value = 'Species', label = 'colorScheme')
mapCtrl.colorOpts <- reactiveVal(value = c('Species', paste0(MapLabelMetrics, '.Status')), label = 'colorOpts')
mapCtrl.CUmarkerHoverHighlights <- reactiveVal(value = c('Polygon', 'Pops'), label = 'CUmarkerHoverHighlights')
mapCtrl.CUpolyHoverHighlights <- reactiveVal(value = c('Polygon', 'Pops'), label = 'CUpolyHoverHighlights')
mapCtrl.CUstreamHoverHighlights <- reactiveVal(value = c('Marker','Pops'), label = 'CUstreamHoverHighlights')

# -- Functions to do with creating 'popup' information panes to be shown on mouseover

# some helper functions for putting together information to be shown in the popover panes

# get the list of CUs associated with the given stream segment
getCUsFromStreamSeg <- function(seg) {
  CUs <- unpack(data.Streams$CUsSelectable[data.Streams$code == seg]) 
  CUs[CUs %in% data.currentCUs()]
}

# get the list of populations associated with the given stream segment
getPopsFromStreamSeg <- function(seg) {
  pops <- unpack(data.Streams$PopsSelectable[data.Streams$code == seg]) 
  pops[pops %in% data.currentPops()]
}
# css snipped for showing an arrow rotated by degrees from the horizonal
makeArrow <- function(degrees) {
  cssStyle <- "-ms-transform:rotate(xxdeg); -webkit-transform:rotate(xxdeg); -moz-transform:rotate(xxdeg); -o-transform:rotate(xxdeg);"
  cssStyle <- gsub('xx', as.character(degrees), cssStyle)
  paste("<div style='", cssStyle, "'>&rarr;</div>", sep="")
}

# get the polar angle for representing the given change in y over an x distance of 1
polarAngle <- function(dy) {atan2(dy, 1) * 180 / pi}

# set style for text and background conditional on status
getMetricTextStyle <- function(status) {
  switch(status,
         Red = 'background-color: #ff0000; color: #000000;',
         Amber = 'background-color: #ff9900; color: #000000;',
         Green = 'background-color: #00dd00; color: #000000;',
         'NA' = ''
  )
}

# create one row with information on the given metric
map.makeCUPopupTableRow <- function(metric, end, start) {
  style <- getMetricTextStyle(end['Status'])
  label <- GetLabel(metric)
  if (is.na(end['Value'])) value <- 'NA'
  else value <- as.character(round(as.numeric(end['Value']), 2))
  if (is.null(start) || is.na(start['Value']) || start['Value'] == 'NA' || !(abs(as.numeric(start['Value'])) > 0)) change <- ''
  else change <- HTML(makeArrow(polarAngle((as.numeric(end['Value']) - as.numeric(start['Value']))/as.numeric(start['Value']))))
  tags$tr(tags$td(style=style, label),
          tags$td(style=style, value), 
          tags$td(style=style, change))
}

# get together the information needed to output information for the given metric
# the pane shows arrows indicating the direction and magnitude of change, either 
# over the period from change year 1 to change year 2, or over the period leading
# up to the selected year.
map.makeCUPopupMetricRow <- function(m, CU) {
  if (filter$change == "Change") {
    endYear <- filter$changeyear_2
    startYear <- filter$changeyear_1
  } else {
    endYear <- filter$year
    if (length(data.CU.Metrics[data.CU.Metrics$CU_ID == CU, 'Year']) > 0 && 
        filter$year > min(data.CU.Metrics[data.CU.Metrics$CU_ID == CU, 'Year'])) {
        startYear <- as.numeric(filter$year) - 1
        # count back if the current startYear isn't in the dataset
        while(!(startYear %in% data.CU.Metrics[data.CU.Metrics$CU_ID == CU, 'Year'])) startYear <- startYear - 1
      } else {
        startYear <- NULL
      }
  } 
  end <- c( Value = data.CU.Metrics[paste(CU, filter$DataType, endYear, sep="."), m],
            Status = as.character(data.CU.Metrics[paste(CU, filter$DataType, endYear, sep="."), paste0(m, '.Status')]))
  if (is.null(startYear)) start <- NULL
  else start <- c( Value = data.CU.Metrics[paste(CU, filter$DataType, startYear, sep="."), m],
                   Status = as.character(data.CU.Metrics[paste(CU, filter$DataType, startYear, sep="."), paste0(m, '.Status')]))
  map.makeCUPopupTableRow(m, start = start, end = end)
}

# create a vector of values to use for creating a sparkline, padding with NAs if needed to fill the range of years
map.makeSparklineData <- function(df, attrib, minYr=NULL, maxYr=NULL) {
  df <- df[, c('Year', attrib)]
  row.names(df) <- as.character(df$Year)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (is.null(minYr)) minYr <- min(df$Year, na.rm=T)
  if (is.null(maxYr)) maxYr <- max(df$Year, na.rm=T)
  yrs <- c(minYr:maxYr)
  out <- rep(NA, length(yrs))
  names(out) <- as.character(yrs)
  out[row.names(df)] <- df[ ,attrib]
  out
}

# create a sparkline, given a data frame with a year column and a time series column specified in the 'DataType' filter attribute 
# and an id to be used as the shiny output id
map.makeSparkline <- function(df, id, minYr=NULL, maxYr=NULL) {
  default <- tags$div(style=('background-color: black; color: #b0b0b0'), '<no time series data>')
  if ((nrow(df) > 0) && (filter$DataType %in% names(df))) {
    ts <- filter$DataType
    df <- as.numeric(map.makeSparklineData(df, ts, minYr, maxYr))
    if (all(is.na(df))) default
    else {
      #ID <- sId(paste0('sparkline', ts), id)
      ID <- as.character(runif(1))
      output[[ID]] <- renderSparkline({sparkline(df)})
      tags$div(class='sparkline-canvas', 
               tags$style(HTML('canvas {width: 95% !important; height: auto !important; display: grid !important}')),
               sparklineOutput(ID))
    }
  } else default
}

# table row for a table with sparklines 
map.makeSparklineTableRow <- function(df, id, label, minYr, maxYr) {
  tags$tr(tags$td(label, style='width: 60px'), 
          tags$td(map.makeSparkline(df, id, minYr=minYr, maxYr=maxYr)))
}

# table row for a table with sparklines from population data
map.makePopSparklineTableRow <- function(p) {
  df <- data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID == p['Pop_UID'], ]
  if (!is.na(p['tsName'])) {
    label <- p['tsName']
    df <- df[df$TS_Name == p['tsName'], ]
  } else {
    label <- p['pName']
  }
  # workaround for messy data with multiple time series
  if (any(duplicated(df$Year))) {
    duplYrs <- df$Year[duplicated(df$Year)]
    sampleRows <- df[df$Year %in% duplYrs, ]
    sampleRows <- sampleRows[order(sampleRows$Year), ]
    cat('Found multiple time series for ', p, '!\n')
    print(sampleRows)
    # find the attributes by which the series differ
    attrs <- unlist(lapply(names(sampleRows), function(a) {if (length(unique(sampleRows[, a])) < length((sampleRows[, a]))) a else NA}))
    attrs <- attrs[!is.na(attrs)]
    tags$tr(tags$td(label, style='width: 40px'), 
            tags$td(paste0('multiple time series found!')))
  }
  else 
    map.makeSparklineTableRow(df, id=paste(c(p['Pop_UID'], label), sep=':'), label=label, 
                              minYr=min(data.Pop.Lookup$DataStartYear, na.rm=T), 
                              maxYr=max(data.Pop.Lookup$DataEndYear, na.rm=T))
}

# table row for a table with sparklines from CU data
map.makeCUSparklineTableRow <- function(CU) {
  df <- data.CU.TimeSeries[data.CU.TimeSeries$CU_ID == CU, ]
  map.makeSparklineTableRow(df, id=CU, label=CU, 
                            minYr=min(data.CU.Lookup$DataStartYear, na.rm=T), 
                            maxYr=max(data.CU.Lookup$DataEndYear, na.rm=T))
}

# given a list of pop UIDs, put together a list of pairs of the form (Pop_UID, Pop_Name) for input to makePopSparklineTableRow
# this is needed since there are sometimes multiple time series for the same Pop_UID (all of which are concatenated in
# the 'tsNames' field in data.Pop.Lookup)
map.getPopsWithTSNames <- function(pops) {
  p.name <- unlist(lapply(pops, function(p) {
    tsNames <- strsplit(data.Pop.Lookup[data.Pop.Lookup$Pop_UID == p, 'tsNames'], ':')[[1]]
    paste(p, tsNames, sep=':')
  }))
  out <- lapply(p.name, function(p) {
    pp <- strsplit(p, ':')[[1]]
    c('Pop_UID' = pp[1], 'tsName' = pp[2], 'pName' = data.Pop.Lookup[pp[1], 'Pop_Name'])
  })
  names(out) <- pops
  out
}

# make a sparkline table, given a list of Pop_UIDs
map.makePopSparklineTable <- function(pops, header=NULL) {
  # a lot of populations don't have time series data; put the ones that do first here
  pops <- pops[order(data.Pop.Lookup[pops, 'HasTimeSeriesData'], decreasing = T)]
  tags$div(class = 'mouseover-box',
           if (!is.null(header)) tags$div(class = 'mouseover-box-header', header),
           tags$table(lapply(map.getPopsWithTSNames(pops), map.makePopSparklineTableRow)))
}

# make a sparkline table, given a list of CU_IDs
map.makeCUSparklineTable <- function(CUs, header=NULL) {
  tags$div(class = 'mouseover-box',
           if (!is.null(header)) tags$div(class = 'mouseover-box-header', header),
           tags$table(lapply(CUs, map.makeCUSparklineTableRow)))
}

# put together an information pane for each CU, to be shown on mouse-over on the map
map.makeCUPopup <- function(CU) {
  if (input$dataUnit == 'CUs') {
    df <- data.CU.TimeSeries[data.CU.TimeSeries$CU_ID == CU, ]
    tags$div(class = 'mouseover-box',
             tags$div(class = 'mouseover-box-header', getCUname(CU)),
             map.makeSparkline(df, CU),
             tags$table(lapply(MapLabelMetrics[MapLabelMetrics %in% filter$metrics], map.makeCUPopupMetricRow, CU)))
  } else {
    map.makePopSparklineTable(getPopsForCU(CU), header=getCUname(CU))
  }
}

# put together an information pane for each population, to be shown on mouse-over on the map
map.makePopPopup <- function(pop) {
  if (data.Pop.Lookup[pop, 'HasTimeSeriesData'] == 'Yes') {
    tsSparkline <- map.makePopSparklineTable(c(pop))
  } else {
    tsSparkline <- tags$div(style=('background-color: black; color: #b0b0b0'), '<no time series data>')
  }
  tags$div(class = 'mouseover-box',
           tags$div(class = 'mouseover-box-header', data.Pop.Lookup[pop, "Pop_Name"]),
           tsSparkline,
           tags$table(tags$tr(tags$td('ID: '), tags$td(data.Pop.Lookup[pop, "Pop_ID"])),
                      tags$tr(tags$td("CU:"), 
                              tags$td(data.CU.Lookup[data.CU.Lookup$CU_ID == data.Pop.Lookup[pop, "CU_ID"], 'CU_Name'][1]))))
}

# put together an information pane to be shown when user moves mouse over a stream segment
map.makeStreamPopup <- function(segCode) {
  if (input$dataUnit == 'CUs') {
    p <- 'no CUs on this stream segment match current filter criteria'
    CUs <- getCUsFromStreamSeg(segCode)
    if (length(CUs) > 0) 
      p <- map.makeCUSparklineTable(CUs)
      #p <- tags$div(class = 'mouseover-box-text', paste0(CUs,collapse=', '))
  }
  else if (input$dataUnit == 'Pops') {
    pops <- getPopsFromStreamSeg(segCode)
    p <- 'no populations on this stream segment match current filter criteria'
    if (length(pops) > 0) {
      df <- data.Pop.Lookup[data.Pop.Lookup$Pop_UID %in% pops, c('Pop_UID', 'CU_ID')]
      dl <- split(df$Pop_UID, df$CU_ID)
      p <- lapply(names(dl), function(cu) {map.makePopSparklineTable(dl[[cu]], header=cu)})
    }
  } 
  tags$div(class = 'mouseover-box',
           tags$div(class = 'mouseover-box-header', data.Streams$Name[data.Streams$code == segCode]), 
           p)
}

# -- Map element display --

# add markers to map
map.showMarkers <- function(map, df, pane, group, styleOpts) {
  addCircleMarkers(map, data=df, lng=~Lon, lat=~Lat, 
                   layerId = ~layer,
                   label = ~lapply(label, HTML),
                   options = pathOptions(pane = pane),
                   group = group,
                   radius = styleOpts$radius,
                   opacity = styleOpts$opacity,
                   fillColor = ~fillColor,
                   stroke = styleOpts$stroke,
                   fill = styleOpts$fill,
                   fillOpacity = styleOpts$fillOpacity,
                   weight = styleOpts$weight,
                   color = ~color
  )
}

# add polygons to map
map.showPolygons <- function(map, df, pane, group, styleOpts) {
  addPolygons(map=map, data=df, 
              layerId = ~layer,
              label = ~lapply(label, HTML),
              options = pathOptions(pane = pane),
              color = ~color,
              fillColor = ~fillColor,
              fillOpacity = styleOpts$fillOpacity,
              stroke=styleOpts$stroke,
              weight=styleOpts$weight,
              opacity= styleOpts$opacity,
              group=group)
} 

map.showStream <- function(map, segCode, df, layer, pane, group, styleOpts) {
  addPolylines(map=map, data=df[segCode, ],
               label = ~Name,
               color = styleOpts$color,
               weight = styleOpts$weight,
               opacity = styleOpts$opacity,
               layerId = layer,
               group = group,
               options = pathOptions(pane = pane))
}

# get the color to use for the given attribute values
map.getColor <- function(attribVals, override=NULL, scheme = mapCtrl.colorScheme()) {
  if (!is.null(override)) return(rep(override, length(attribVals)))
  attribVals <- as.character(attribVals)
  attribVals[is.na(attribVals)] <- 'NA'
  if (scheme %in% paste0(MapLabelMetrics, '.Status')) {
    unlist(lapply(attribVals, function(a) {as.character(ColorPalette[['Status']][a])}))
  } else {
    unlist(lapply(attribVals, function(a) {as.character(ColorPalette[[scheme]][a])}))
  }
}

map.CUMarkerData <- reactive({
  df <- unique.data.frame(data.CU.Lookup.filtered()[, c('CU_ID', MapAttribs)])
    if (nrow(df) > 0) {
      df$layer <- df$CU_ID
      df$label <- unlist(lapply(df$CU_ID, getCUname))
      df.m <- data.filtered()
      df.m$CU_ID <- row.names(df.m)
      metrics <- names(df.m)[grep('.Status', names(df.m))]
      df <- merge(df, df.m[ , c('CU_ID', metrics)], by=c("CU_ID"), all.x=T, all.y=F)
      if (mapCtrl.colorScheme() %in% names(df)) {
        df$color <- map.getColor(df[ , mapCtrl.colorScheme()], override=CUMarkerStyle.normal$color)
        df$fillColor <- map.getColor(df[ , mapCtrl.colorScheme()], override=CUMarkerStyle.normal$fillColor)
      }
      else {
        df$color <- rep('black', nrow(df))
        df$fillColor <- rep('#eeeeee', nrow(df))
      }
    }
  df
})

# Add markers to map that represent the CUs listed (or all CUs currently in the filter set if CUs = NULL)
map.showCUMarkers <- function(leafletMap, CUs=data.currentCUs(), styleOpts = CUMarkerStyle.normal, layer=NULL) {
  df <- map.CUMarkerData()[map.CUMarkerData()$CU_ID %in% CUs, ]
  if (nrow(df) > 0) {
    group <- 'CUMarkers'
    if(!is.null(layer)) {
      df$layer <- paste0(layer, '.', df$layer)
      group <- paste0(layer, '.', group)
    }
    if (!is.null(styleOpts$color))  df$color <- rep(styleOpts$color, nrow(df))
    if (!is.null(styleOpts$fillColor))  df$fillColor <- rep(styleOpts$fillColor, nrow(df))
    leafletMap <- map.showMarkers(leafletMap, df, pane=group, group=group, styleOpts=styleOpts)
  }
  leafletMap
}

map.popMarkerData <- reactive({
  df <- data.Pop.Lookup.filtered()
  if (nrow(df) > 0) {
    df.sp <- data.Pop.Spatial[data.Pop.Spatial$Pop_UID %in% df$Pop_UID, ]
    df.sp$label <- unlist(lapply(df.sp$Pop_UID, getPopName))
    df.sp$layer <- df.sp$Pop_UID
    if (mapCtrl.colorScheme() %in% names(df.sp)) {
      df.sp$color <- map.getColor(df.sp[ , mapCtrl.colorScheme()], override=PopMarkerStyle.normal$color)
      df.sp$fillColor <- map.getColor(df.sp[ , mapCtrl.colorScheme()], override=PopMarkerStyle.normal$fillColor)
    }
    else {
      df.sp$color <- rep('black', nrow(df.sp))
      df.sp$fillColor <- rep('white', nrow(df.sp))
    }
    df.sp
  } else {
    df 
  }
})

# add markers to map that represent the populations listed (or all populations currently in the filter set
# if Pops = NULL). Markers for pops currently selected appear highlighted
map.showPopMarkers <- function(leafletMap, pops=data.currentPops(), styleOpts = PopMarkerStyle.normal, layer=NULL) {
  df <- map.popMarkerData()[map.popMarkerData()$Pop_UID %in% pops, ]
  if (nrow(df) > 0) {
    group <- 'PopMarkers'
    if(!is.null(layer)) {
      df$layer <- paste0(layer, '.', df$layer)
      group <- paste0(layer, '.', group)
    }
    if (!is.null(styleOpts$color))  df$color <- rep(styleOpts$color, nrow(df))
    if (!is.null(styleOpts$fillColor))  df$fillColor <- rep(styleOpts$fillColor, nrow(df))
    leafletMap <- map.showMarkers(leafletMap, df, pane=group, group = group, styleOpts = styleOpts)
  }
  leafletMap
}

map.CUpolyData <- reactive({
  df.poly <- data.CU.Spatial[data.CU.Spatial$CU_ID %in% data.currentCUs(), ]
  if (nrow(df.poly) > 0) {
    df.poly <- sp::merge(df.poly, map.CUMarkerData(), by=c("CU_ID"), all.x=T, all.y=F)
    if (!is.null(CUPolyStyle.normal$color)) df.poly$color <- rep(CUPolyStyle.normal$color, nrow(df.poly))
    if (!is.null(CUPolyStyle.normal$fillColor)) df.poly$fillColor <- rep(CUPolyStyle.normal$fillColor, nrow(df.poly))
  }
  df.poly
})

# Add CU boundaries for the CUs listed (or for all CUs currently in the filter if CUs = NULL) 
# boundaries for CUs currently selected appear highlighted
map.showCUPolys <- function(leafletMap, CUs=data.currentCUs(), styleOpts = CUPolyStyle.normal, layer=NULL) {
  df <- map.CUpolyData()[map.CUpolyData()$CU_ID %in% CUs, ]
  if (nrow(df) > 0) {
    group <- 'CUPolygons'
    if(!is.null(layer)) {
      df$layer <- paste0(layer, '.', df$layer)
      group <- paste0(layer, '.', group)
    }
    if (!is.null(styleOpts$color))  df$color <- rep(styleOpts$color, nrow(df))
    if (!is.null(styleOpts$fillColor))  df$fillColor <- rep(styleOpts$fillColor, nrow(df))
    for (sp in unique(df$Species)) {
      leafletMap <- map.showPolygons(leafletMap, df[df$Species == sp, ], 
                                     pane = paste0(group, '.', sp), group = group, styleOpts = styleOpts)
    }
  }
  leafletMap
}

# hide a group and associated selection group
hide <- function(map, group) {
  map %>% hideGroup(map, group) %>% hideGroup(map, paste0('selected.', group))
}

# -- Leaflet map rendering 

output$CUmap <- renderLeaflet({
  leafletOutput <- try({
    leafletMap <- leaflet(options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
      addProviderTiles(providers$CartoDB.Positron) 
    leafletMap <- addMiniMap(leafletMap,
                             tiles = providers$CartoDB.Positron,
                             zoomLevelOffset = -4,
                             toggleDisplay = TRUE)
    
    # set up custom z-panes for content; need this to control the order in which map elements are layered
    z <- 400 # 400 is the leaflet default overlay pane; start at pane 401 for custom panes
    # CU polygons
    for (i in 1:length(zPaneOrder)) {
      z <- z+1
      leafletMap <- addMapPane(leafletMap, name = paste0("CUPolygons.", zPaneOrder[i]), zIndex = z)
    }
    for (i in 1:length(zPaneOrder)) {
      z <- z+1
      leafletMap <- addMapPane(leafletMap, name = paste0("selected.CUPolygons.", zPaneOrder[i]), zIndex = z)
    }
    for (i in 1:length(zPaneOrder)) {
      z <- z+1
      leafletMap <- addMapPane(leafletMap, name = paste0("mouseover.CUPolygons.", zPaneOrder[i]), zIndex = z)
    }
    
    # stream segments
    z <- z + 1
    leafletMap <- addMapPane(leafletMap, name = "streams", zIndex = z)
    z <- z + 1
    leafletMap <- addMapPane(leafletMap, name = "mouseover.streams", zIndex = z)    
      
    # for (i in 1:max(data.Streams$StreamOrder)) {
    #   z <- z + 1
    #   leafletMap <- addMapPane(leafletMap, name = paste0("Order", i, "Streams"), zIndex = z)
    # }
    
    # CU markers
    z <- z + 1
    leafletMap <- addMapPane(leafletMap, name = "CUMarkers", zIndex = z)
    z <- z + 1
    leafletMap <- addMapPane(leafletMap, name = "selected.CUMarkers", zIndex = z)
    z <- z + 1
    leafletMap <- addMapPane(leafletMap, name = "mouseover.CUMarkers", zIndex = z)
    
    # Pop markers
    z <- z + 1
    leafletMap <- addMapPane(leafletMap, name = "PopMarkers", zIndex = z)
    z <- z + 1
    leafletMap <- addMapPane(leafletMap, name = "selected.PopMarkers", zIndex = z)
    z <- z + 1
    leafletMap <- addMapPane(leafletMap, name = "mouseover.PopMarkers", zIndex = z)
    
    # add the stream segments
    leafletMap <- addPolylines(leafletMap, 
                               color=StreamStyle.normal$color,
                               weight=StreamStyle.normal$weight,
                               opacity=StreamStyle.normal$opacity,
                               layerId = ~code,
                               group = "Streams",
                               label = ~Name,
#                                highlightOptions = highlightOptions(weight = 5, color="blue", bringToFront = TRUE),
                               data = data.Streams,
                               options = pathOptions(pane = 'streams'))

    # for (i in 1:nrow(data.Streams)) {
    #   leafletMap <- addPolylines(leafletMap, color="blue",
    #                              weight=1,
    #                              opacity=0.7,
    #                              layerId = ~code,
    #                              group = "Streams",
    #                              label = ~Name,
    #                              highlightOptions = highlightOptions(weight = 5, color="blue", bringToFront = TRUE),
    #                              data = data.Streams[i, ],
    #                              options = pathOptions(pane = paste0("Order", data.Streams$StreamOrder[i], "Streams")))
    # }
    # We only want to have the basemap drawn once here, not every time the data filter changes.
    # Since the various map display functions use reactive expressions dependent on filtering, 
    # make sure all calls to these function are isolated here
    isolate(leafletMap <- map.showCUMarkers(leafletMap))
    isolate(leafletMap <- map.showCUPolys(leafletMap))
    isolate(if(!data.currentSelectionEmpty('CUs')) {
      CUs <- data.currentSelection[['CUs']]
      leafletMap <- map.showCUMarkers(leafletMap, CUs=CUs, styleOpts = CUMarkerStyle.highlighted, layer='selected')
      leafletMap <- map.showCUPolys(leafletMap, CUs=CUs, styleOpts = CUPolyStyle.highlighted, layer='selected')
    })
    isolate(leafletMap <- map.showPopMarkers(leafletMap))
    isolate(if(!data.currentSelectionEmpty('Pops')) {
      leafletMap <- map.showPopMarkers(leafletMap, pops=data.currentSelection[['Pops']], styleOpts = PopMarkerStyle.highlighted, layer='selected')
    })
    # could use native layersCotrol here, but it works slightly differently from shiny modals in that the modal
    # disappears on mouseout, rather than on mouse click outside modal. 
    # Implement this with an easyButton and shiny modal instead to maintain consistent UI
    # add controls for drawing masks for selection
    leafletMap <- addDrawToolbar(leafletMap,
                                 targetGroup = "mask",
                                 rectangleOptions = T,
                                 polylineOptions = F,
                                 markerOptions = F,
                                 circleMarkerOptions = F,
                                 editOptions = F,
                                 circleOptions = F)
    leafletMap <- addEasyButton(leafletMap, 
                                easyButton(icon="fa-arrows", 
                                           title="Zoom to map",
                                           onClick=JS("function(btn, map){ 
                                                // Don't care about the value here. It just needs to change every time the button is clicked.
                                                Shiny.onInputChange('leaflet_button_zoom_out', Math.random());
                                             }")))
    leafletMap <- addEasyButton(leafletMap, 
                                easyButton(icon="fa-eye", 
                                           title="Show/hide layers",
                                           onClick=JS("function(btn, map){ 
                                              // Don't care about the value here. It just needs to change every time the button is clicked.
                                              Shiny.onInputChange('leaflet_button_layer_visibility', Math.random());
                                             }")))
    leafletMap <- addEasyButton(leafletMap, 
                                easyButton(icon="fa-paint-brush", 
                                           title="Set color scheme",
                                           onClick=JS("function(btn, map){ 
                                              // Don't care about the value here. It just needs to change every time the button is clicked.
                                              Shiny.onInputChange('leaflet_button_color_scheme', Math.random());
                                             }")))
    leafletMap <- addEasyButton(leafletMap, 
                                easyButton(icon="fa-cog", 
                                           title="Additional settings",
                                           onClick=JS("function(btn, map){ 
                                                console.log(map);
                                                // Don't care about the value here. It just needs to change every time the button is clicked.
                                                Shiny.onInputChange('leaflet_button_settings', Math.random());
                                             }")))
    
    # hide any groups that aren't supposed to be visible 
    # need to isolate these to avoid re-rendering of entire map when mapCtrl.isVisible changes
    for (group in c('CUMarkers', 'PopMarkers', 'CUPolygons')) {
      isolate(if (!mapCtrl.isVisible[[group]]) {
        leafletMap <- hideGroup(leafletMap, group)
        leafletMap <- hideGroup(leafletMap, paste0('selected.', group))
      })
    }
    isolate(if(!mapCtrl.isVisible[['Streams']])
      leafletMap <- hideGroup(leafletMap, 'Streams'))
  leafletMap
  })
  if (!inherits(leafletOutput, "try-error")) {
    leafletOutput
  } else {
    NULL
  }
})

output$box_LeafletMap <- renderUI({shinycssloaders::withSpinner(leafletOutput("CUmap", height = 500))})

# use this to make changes to the leaflet map without re-rendering the whole thing
CUmapProxy <- leafletProxy('CUmap')

# keeping track of initial view 
map.initCenter <- reactiveVal()
map.initZoom <- reactiveVal()

# input$CUmapCreated gets set by a custom event defined in customJSCode.js
# See the comment in customJSCode.js for how to get access to a widget just after
# it has been rendered into the dom, e.g., to modify/add style elements
observeEvent(input$CUmapCreated, {
  map.initCenter(input$CUmap_center)
  map.initZoom(input$CUmap_zoom)
  # javascript custom message handler defined in customJSCode.js
  # changes default titles on leaflet draw buttons 
  session$sendCustomMessage("fixDrawButtonTitles", 'CUmap')
})

# things to do when the Map panel is opened
observeEvent(input$UIPanels, {
  if (input$UIPanels == 'Map') clearInfoPane()
})

# -- Event handlers for EasyButtons -- 

observeEvent(input$leaflet_button_zoom_out, {
  CUmapProxy %>% setView(lng=map.initCenter()$lng, lat=map.initCenter()$lat, zoom=map.initZoom())
})

observeEvent(input$leaflet_button_layer_visibility, {
  showModal(modalDialog(
    title = "Layer Visibility",
    prettyToggle(inputId= 'map.CUMarkers.visible', value = mapCtrl.isVisible[['CUMarkers']],
                 label_on= 'CU Markers', label_off = 'CU Markers',
                 icon_on = icon("eye"), icon_off = icon("eye-slash"),
                 status_on = "info", status_off = "info",
                 outline = TRUE, plain = TRUE),
    prettyToggle(inputId= 'map.CUPolygons.visible', value = mapCtrl.isVisible[['CUPolygons']],
                 label_on= 'CU Boundaries', label_off = 'CU Boundaries',
                 icon_on = icon("eye"), icon_off = icon("eye-slash"),
                 status_on = "info", status_off = "info",
                 outline = TRUE, plain = TRUE),
    prettyToggle(inputId= 'map.PopMarkers.visible', value = mapCtrl.isVisible[['PopMarkers']],
                 label_on= 'Populations', label_off = 'Populations',
                 icon_on = icon("eye"), icon_off = icon("eye-slash"),
                 status_on = "info", status_off = "info",
                 outline = TRUE, plain = TRUE),
    prettyToggle(inputId= 'map.Streams.visible', value = mapCtrl.isVisible[['Streams']],
                 label_on= 'Streams', label_off = 'Streams',
                 icon_on = icon("eye"), icon_off = icon("eye-slash"),
                 status_on = "info", status_off = "info",
                 outline = TRUE, plain = TRUE ),
    easyClose = TRUE,
    footer = NULL,
    size = 's'
  ))
})

# set the visibility of a group in the map, based on the value of mapCtrl.isVisible[[group]]
setVisibility <- function(group) {
  if (mapCtrl.isVisible[[group]]) 
    CUmapProxy %>% showGroup(group) %>% showGroup(paste0('selected.', group))
  else 
    CUmapProxy %>% hideGroup(group) %>% hideGroup(paste0('selected.', group))
}

observeEvent(input$map.CUPolygons.visible, mapCtrl.isVisible[['CUPolygons']] <- input$map.CUPolygons.visible)
observeEvent(mapCtrl.isVisible[['CUPolygons']], setVisibility('CUPolygons'))
observeEvent(input$map.CUMarkers.visible, mapCtrl.isVisible[['CUMarkers']] <- input$map.CUMarkers.visible)
observeEvent(mapCtrl.isVisible[['CUMarkers']], setVisibility('CUMarkers'))
observeEvent(input$map.PopMarkers.visible, mapCtrl.isVisible[['PopMarkers']] <- input$map.PopMarkers.visible)
observeEvent(mapCtrl.isVisible[['PopMarkers']], setVisibility('PopMarkers'))
observeEvent(input$map.Streams.visible, mapCtrl.isVisible[['Streams']] <- input$map.Streams.visible)
observeEvent(mapCtrl.isVisible[['Streams']], setVisibility('Streams'))

observeEvent(input$leaflet_button_settings, {
  showModal(modalDialog(
    radioButtons(inputId = 'map.SelectBy', label = 'Clicking on stream selects: ', 
                 selected = mapCtrl.selectionMode(), inline = TRUE,
                 choiceNames = c('CUs', 'Populations'), choiceValues = c('CUs', 'Pops')),
    checkboxGroupInput(inputId= 'map.showOnCUmarkerHover', label = "On hover over CU marker: ", 
                       choiceNames = c('Highlight CU marker',
                                       'Highlight CU boundaries',
                                       'Highlight populations associated with CU',
                                       'Hide other map elements'),
                       choiceValues = c('Marker', 'Polygon', 'Pops', 'hideOthers'),
                       selected = mapCtrl.CUmarkerHoverHighlights(),
                       inline = FALSE, width = NULL),
    checkboxGroupInput(inputId= 'map.showOnCUpolyHover', label = "On hover over CU polygons: ", 
                       choiceNames = c('Highlight CU marker',
                                       'Highlight CU boundaries',
                                       'Highlight populations associated with CU',
                                       'Hide other map elements'),
                       choiceValues = c('Marker', 'Polygon', 'Pops', 'hideOthers'),
                       selected = mapCtrl.CUpolyHoverHighlights(),
                       inline = FALSE, width = NULL),
    checkboxGroupInput(inputId= 'map.showOnStreamHover', label = "On hover over stream segments: ", 
                       choiceNames = c('Highlight CU markers of CUs on stream',
                                       'Highlight CU boundaries of CUs on stream',
                                       'Highlight populations on stream',
                                       'Hide other map elements'),
                       choiceValues = c('Marker', 'Polygon', 'Pops', 'hideOthers'),
                       selected = mapCtrl.CUstreamHoverHighlights(),
                       inline = FALSE, width = NULL),
    easyClose = TRUE,
    footer = NULL,
    size = 's'
  ))
})

observeEvent(input$map.SelectBy, mapCtrl.selectionMode(input$map.SelectBy))
observeEvent(input$map.showOnCUmarkerHover, mapCtrl.CUmarkerHoverHighlights(input$map.showOnCUmarkerHover))
observeEvent(input$map.showOnCUpolyHover, mapCtrl.CUpolyHoverHighlights(input$map.showOnCUpolyHover))
observeEvent(input$map.showOnStreamHover, mapCtrl.CUstreamHoverHighlights(input$map.showOnStreamHover))

observeEvent(input$dataUnit, {
  mapCtrl.selectionMode(input$dataUnit)
  if (input$dataUnit == 'CUs') { # if dataUnit just got toggled to CUs, hide pop markers
    mapCtrl.isVisible[['PopMarkers']] <- FALSE
    opts <- mapCtrl.CUpolyHoverHighlights()
    mapCtrl.CUpolyHoverHighlights(opts[opts != 'Pops'])
    opts <- mapCtrl.CUmarkerHoverHighlights()
    mapCtrl.CUmarkerHoverHighlights(opts[opts != 'Pops'])
    opts <- mapCtrl.CUstreamHoverHighlights()
    mapCtrl.CUstreamHoverHighlights(opts[opts != 'Pops'])
  }
  else { # dataUnit just got toggled to Populations; make populations visible on map
    mapCtrl.isVisible[['PopMarkers']] <- TRUE
    opts <- mapCtrl.CUpolyHoverHighlights()
    mapCtrl.CUpolyHoverHighlights(unique(c(opts, 'Pops')))
    opts <- mapCtrl.CUmarkerHoverHighlights()
    mapCtrl.CUmarkerHoverHighlights(unique(c(opts, 'Pops')))
    opts <- mapCtrl.CUstreamHoverHighlights()
    mapCtrl.CUstreamHoverHighlights(unique(c(opts, 'Pops')))
  }
})

observeEvent(input$leaflet_button_color_scheme, {
  showModal(modalDialog(
    selectInput(inputId = 'map.colorScheme', label = 'Color by', 
                choices = mapCtrl.colorOpts(), selected = mapCtrl.colorScheme(), multiple = FALSE),      
    easyClose = TRUE,
    footer = NULL,
    size = 's'
  ))
}) 

observeEvent(input$map.colorScheme, mapCtrl.colorScheme(input$map.colorScheme))

# -- Hihglighting of map elements --

map.highlightStream <- function(segCode) {
  map.showStream(CUmapProxy, segCode, df=data.StreamsExtended, 
                 layer=paste0('mouseover', '.', segCode), 
                 pane='mouseover.streams', 
                 group='mouseover.streams', 
                 styleOpts = StreamStyle.highlighted)
}
  
# highlight markers of the given type ('CUs' or 'Pops')
map.highlightMarkers <- function(markers, type, highlightLayer='selected') {
  if (type == 'CUs') 
    map.showCUMarkers(CUmapProxy, markers, styleOpts = CUMarkerStyle.highlighted, layer=highlightLayer)
  else if (type == 'Pops') 
    map.showPopMarkers(CUmapProxy, markers, styleOpts = PopMarkerStyle.highlighted, layer=highlightLayer)
}

# un-highlight markers of the given type ('CUs' or 'Pops')
map.unhighlightMarkers <- function(markers=NULL, type, highlightLayer='selected') {
    if (type == 'CUs') group <- 'CUMarkers' else group <- 'PopMakers'
    if (is.null(markers)) {
      clearGroup(CUmapProxy, paste0(highlightLayer, '.', group))
    }
    else 
      lapply(markers, function(m) {
        removeMarker(CUmapProxy, paste0(highlightLayer, '.', m))})
}

# highlight polygons
map.highlightPolygons <- function(polys, highlightLayer='selected') {
  map.showCUPolys(CUmapProxy, polys, styleOpts = CUPolyStyle.highlighted, layer=highlightLayer)
}

map.unhighlightPolygons <- function(polys=NULL, highlightLayer='selected') {
  if (is.null(polys))
    clearGroup(CUmapProxy, paste0(highlightLayer, '.CUPolygons'))
  else 
    lapply(polys, function(p) {removeShape(CUmapProxy, paste0(highlightLayer, '.', p))})
}
  
# highlight map elements during a mouseover 
map.showMouseoverHighlights <- function(CUPolys=NULL, CUMarkers=NULL, PopMarkers=NULL) {
  if (!is.null(CUPolys)) 
    map.showCUPolys(CUmapProxy, CUs = CUPolys, styleOpts = CUPolyStyle.mouseover, layer='mouseover')
  if (!is.null(CUMarkers)) 
    map.showCUMarkers(CUmapProxy, CUs = CUMarkers, styleOpts = CUMarkerStyle.mouseover, layer='mouseover')
  if (!is.null(PopMarkers)) 
    map.showPopMarkers(CUmapProxy, pops = PopMarkers, styleOpts = PopMarkerStyle.mouseover, layer='mouseover')
}

# clear map elements that were highlighted due to a mouseover 
# if called w/o parameters, removes all mouseover highlights currently on map
map.clearMouseoverHighlights <- function(CUPolys=NULL, CUMarkers=NULL, PopMarkers=NULL, Streams=NULL) {
  if (is.null(CUPolys) && is.null(CUMarkers) && is.null(PopMarkers)) { # remove all mouseover highlights
    clearGroup(CUmapProxy, 'mouseover.CUMarkers')
    clearGroup(CUmapProxy, 'mouseover.PopMarkers')
    clearGroup(CUmapProxy, 'mouseover.CUPolygons')
    clearGroup(CUmapProxy, 'mouseover.streams')
  } else { # remove only specified mouseover highlights
    for (s in CUPolys) CUmapProxy %>% removeShape(paste0('mouseover', '.', s))
    for (m in c(CUMarkers, PopMarkers)) CUmapProxy %>% removeMarker(paste0('mouseover', '.', m))
    for (s in Streams) CUmapProxy %>% removeShape(paste0('mouseover', '.', s))
  }
}

# -- Event handlers for Marker events -- 
# strip layer information and extract the actual ID of a marker or shape
map.getID <- function(el) {gsub('selected.', '', gsub('mouseover.', '', el))}

# show mouseover highlights associated with the selected CUs contained in sel on map
# elementsToHighlight is a list analogous to mapCtrl.CUmarkerHoverHighlights 
# that controls what should be highlighted ('Marker', 'Pops', 'Polygon')
map.showCUMouseoverHighlights <- function(sel, elementsToHighlight = mapCtrl.CUmarkerHoverHighlights()) {
  if ('Marker' %in% elementsToHighlight) # highlight the marker associated with this CU 
    map.showMouseoverHighlights(CUMarkers = c(sel))
  if ('Pops' %in% elementsToHighlight) # show populations associated with this CU in addition to CU marker
    map.showMouseoverHighlights(PopMarkers = getPopsForCUs(sel))
  if ('Polygon' %in% elementsToHighlight) # show the boundaries associated with this CU
    map.showMouseoverHighlights(CUPolys = c(sel))
}

map.showPopMouseoverHighlights <- function(sel) {
  map.showMouseoverHighlights(PopMarkers=c(sel))
}

map.showExtendedStream <- function(code) {
  
}
# things that should occur when the user moves the mouse over a marker
observeEvent(input$CUmap_marker_mouseover, 
             { # mouseover events aren't always detected, so if there are residual highlighted markers around,
               # they will be on top and therefore block access to the actual marker underneath
               # Get rid of any residual mouseover highlights here before continuing
               # cat("Marker mouseover event observed for ", input$CUmap_marker_mouseover$id, "\n")
               map.clearMouseoverHighlights()
               sel <- map.getID(input$CUmap_marker_mouseover$id)
               InfoPane <- NULL
               if (sel %in% data.CUs) { # mouse is over a CU marker
                 map.showCUMouseoverHighlights(sel, mapCtrl.CUmarkerHoverHighlights())
                 InfoPane <- map.makeCUPopup(sel)
               } else if (sel %in% data.Pops) { # mouse is over a Pop marker
                 map.showPopMouseoverHighlights(sel)
                 InfoPane <- map.makePopPopup(sel)
               } else {
                 InfoPane <- tags$div(style='padding: 5px;', paste0('unknown marker type: ', sel))
               }
               if (!is.null(InfoPane)) showInfoPane(InfoPane)
             })

# things that should occur when the user moves the mouse away from a marker
observeEvent(input$CUmap_marker_mouseout, 
             {
               #cat("Marker mouseout event observed for ", input$CUmap_marker_mouseover$id, "\n")
               map.clearMouseoverHighlights()
             })

# things that should occur when the user moves the mouse over a shape (i.e., a CU polygon or a stream segment)
observeEvent(input$CUmap_shape_mouseover, 
             {# mouseover events aren't always detected, so if there are residual highlighted shapes around,
               # they will be on top and therefore block access to the shape underneath
               # get rid of any residual mouseover highlights before proceeding
               #cat("Shape mouseover event observed for ", input$CUmap_shape_mouseover$id, "\n")
               map.clearMouseoverHighlights()
               sel <- map.getID(input$CUmap_shape_mouseover$id) 
               InfoPane <- NULL
               if (sel %in% data.CUs) { # user hovering over a CU polygon
                 map.showCUMouseoverHighlights(sel, mapCtrl.CUpolyHoverHighlights())
                 InfoPane <- map.makeCUPopup(sel)
               } else if (sel %in% data.Watersheds) { # user hovering over a stream segment
                 if (mapCtrl.selectionMode() == 'CUs') {
                   # special treatment of Pops here: if user wants to see populations, they should only be the ones associated with
                   # the stream segment, not all the ones associated with the CUs that are associated with the stream segment
                   elementsToHighlight <- mapCtrl.CUstreamHoverHighlights()
                   map.showCUMouseoverHighlights(getCUsFromStreamSeg(sel), elementsToHighlight[elementsToHighlight != 'Pops'])
                   if ('Pops' %in% elementsToHighlight)
                     map.showPopMouseoverHighlights(getPopsFromStreamSeg(sel))
                 }
                 else if (mapCtrl.selectionMode() == 'Pops') 
                   map.showPopMouseoverHighlights(getPopsFromStreamSeg(sel))
                 InfoPane <- map.makeStreamPopup(sel)
                 map.highlightStream(sel)
               } else {
                 InfoPane <- tags$div(style='padding: 5px;', paste0('unknown shape type: ', sel))
               }
               if (!is.null(InfoPane)) showInfoPane(InfoPane)
             })

# things that should occur when the user moves the mouse away from a shape
observeEvent(input$CUmap_shape_mouseout, 
             {
               #cat("Shape mouseout event observed for " , input$CUmap_shape_mouseout$id, '\n') 
               map.clearMouseoverHighlights()
             })

# things that should occur when a marker is clicked on
observeEvent(input$CUmap_marker_click, 
             {
               sel <- map.getID(input$CUmap_marker_click$id)
               #cat("Marker click event observed for ", sel, "\n")
               str(input$CUmap_marker_click)
               if (sel %in% data.CUs)  { # user clicked on a CU marker
                 if (mapCtrl.selectionMode() == 'CUs') map.addToSelection(sel, 'CUs') 
                 else if (mapCtrl.selectionMode() == 'Pops')  map.addToSelection(getPopsForCUs(sel), 'Pops')
               }
               else if (sel %in% data.Pops)  { # user clicked on a Population marker
                 if (mapCtrl.selectionMode() == 'Pops') map.addToSelection(sel, 'Pops') 
               }
             })


# things that should occur when a shape (line or polygon) is clicked
observeEvent(input$CUmap_shape_click, 
             {
               sel <- map.getID(input$CUmap_shape_click$id)
               #cat("shape click event observed for shape ", sel, '\n')
               str(input$CUmap_shape_click)
               if (sel %in% data.Watersheds) { # user clicked on a stream segment
                 if (mapCtrl.selectionMode() == 'CUs') {
                    map.addToSelection(getCUsFromStreamSeg(sel), type='CUs')
                 } else if (mapCtrl.selectionMode() == 'Pops') {
                    map.addToSelection(getPopsFromStreamSeg(sel), type='Pops')
                 }
               }
             })

# -- Selection of CUs and populations  --

# update selection shown on map; type is the type of data items affected (CU or Pop)
map.updateSelection <- function(type) {
  map.unhighlightMarkers(markers=NULL, type)
  map.highlightMarkers(data.currentSelection[[type]], type)
  if (type == 'CUs') {
    map.unhighlightPolygons(polys=NULL)
    map.highlightPolygons(data.currentSelection[[type]])
  }
}

observeEvent(data.currentSelection[['CUs']], {map.updateSelection('CUs')}, ignoreNULL = F)
observeEvent(data.currentSelection[['Pops']], {map.updateSelection('Pops')}, ignoreNULL = F)

# add sel (an vector of ids) to current selection; type is the type of data items (CU or Pop)
map.addToSelection <- function(sel, type) {
  if (length(sel) > 0) {
    alreadySelected <- sel[sel %in% data.currentSelection[[type]]]
    if (setequal(sel, alreadySelected)) { # all markers in this list are already selected; toggle to unselect all
      data.removeFromSelection(sel, type=type, widget="map")
      map.unhighlightMarkers(sel, type)
      if (type == 'CUs') 
        map.unhighlightPolygons(sel)
    } else { # at least some markers in this list were not already selected; select all
      data.addToSelection(sel, type=type, widget="map")
      map.highlightMarkers(sel, type)
      if (type == 'CUs') 
        map.highlightPolygons(sel)
    } 
  }
}

# leaflet projection 
proj <- CRS("+proj=longlat +datum=WGS84")

# create a SpatialPolygons object from the coordinate list returned by leafletDraw events
makeSPpoly <- function(geomList, ID) {
  geom <- t(matrix(unlist(geomList), nrow=2)) # convert from list to matrix 
  SpatialPolygons(c(Polygons(c(Polygon(coords=geom, hole=F)), ID=as.character(ID))), proj4string = proj)
}

# given a SpatialPoints object and a SpatialPolygons object, identify which of the points are inside the polygon(s)
ptsInsidePoly <- function(pts, poly) {
  sel <- over(pts, poly)
  # over returns the index of the polygon each point is contained in, or NA for points not inside any of the polygons
  # convert this into a vector of booleans
  sel <- ifelse(is.na(sel), FALSE, TRUE)
}

# Event handler to deal with selection polygon (rectangle or polygon shape drawn by user)
# Gets called when the user finishes drawing the feature
observeEvent(input$CUmap_draw_new_feature, {
  id <- input$CUmap_draw_new_feature$properties$`_leaflet_id`
  geomList <- input$CUmap_draw_new_feature$geometry$coordinates[[1]]
  selPoly <- makeSPpoly(geomList, id)
  if(mapCtrl.selectionMode() == 'CUs') {
    df <- unique(data.CU.Lookup.filtered()[ , c("CU_ID", "Lat", "Lon")])
    pts <- SpatialPoints(data.frame(lng=df$Lon, lat=df$Lat), proj4string = proj)
    CUs <- df$CU_ID[ptsInsidePoly(pts, selPoly)] 
    map.addToSelection(CUs, 'CUs')
  } else if(mapCtrl.selectionMode() == 'Pops') {
    df <- data.Pop.Lookup.filtered()[ , c("Pop_UID", "Lat", "Lon")]
    pts <- SpatialPoints(data.frame(lng=df$Lon, lat=df$Lat), proj4string = proj)
    pops <- df$Pop_UID[ptsInsidePoly(pts, selPoly)] 
    map.addToSelection(pops, 'Pops')
  }
  # Remove selection polygon from map
  # Note that removeShape() won't work for shapes drawn with leafletDraw
  # Need to use custom js handler defined in www/customJSCode.js instead
  session$sendCustomMessage("removeSelectionPolygon", list(elid="CUmap", layerid=id))
})

# -- map updating on changes to filter or other changes to global settings --

# add dynamic map elements when filter changes; 
# don't render entire map again, since rendering of stream network takes a while
observeEvent({data.CU.Lookup.filtered()
  mapCtrl.colorScheme()
  input$dataUnit}, {
    CUmapProxy %>% clearGroup('CUMarkers') %>% clearGroup('CUPolygons') %>% clearGroup('PopMarkers')
    CUmapProxy %>% clearGroup('selected.CUMarkers') %>% clearGroup('selected.CUPolygons') %>% clearGroup('selected.PopMarkers')
    map.showCUMarkers(CUmapProxy)
    map.showCUPolys(CUmapProxy)
    if (!data.currentSelectionEmpty('CUs')) {
      map.highlightMarkers(data.currentSelection[['CUs']], 'CUs')
      map.highlightPolygons(data.currentSelection[['CUs']])
    }
    map.showPopMarkers(CUmapProxy)
    if (!data.currentSelectionEmpty('Pops')) {
      map.highlightMarkers(data.currentSelection[['Pops']], 'Pops')
    }
  }, ignoreInit=T)
