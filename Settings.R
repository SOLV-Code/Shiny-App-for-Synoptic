# 

# ----------- Data source customization -----------------

#  ** Data files contributed by SoS **

# CU metrics and status for multiple assessment years; also includes CU life history attributes and other descriptive attributes
metricsFile <- "data/METRICS_FILE_BY_CU.csv"            
# CU escapement time series
CUTimeSeriesFile <- "data/MERGED_FLAT_FILE_BY_CU.csv"
# escapement time series for individual sites
PopTimeSeriesFile <- "data/MERGED_FLAT_FILE_BY_POP.csv"

# ** Special-purpose data files created for this web application **

# Lookup table for matching of CU IDs used in SoS files to IDs used in Open Data maps; 
# also houses additional CU attributes extracted from the map data (e.g., Lat-lon)
CULookupFile <- 'data/CULookup.csv'
# Lookup table for matching of Pop IDs/names used in SoS files to IDs/names used in Open Data maps
PopLookupFile <- "data/PopLookup.csv"
# CU boundary polygons (extracted from the Open Data polygon coverages for the individual species)
CUBoundariesFile <- "data/CU_Boundaries.gpkg" 
# special-purpose stream network created by splitting BC Freshwater Atlas streams into segments and attaching CU and site information
StreamNetworkFile <- "data/Streams_simplified.gpkg" 
# similar to StreamNetworkFile, but the polyline associated with each segment is extended to include the full upstream network above the segment
ExtendedStreamNetworkFile <- "data/StreamsExtended_simplified.gpkg" 

# --------------------- Labels and help text --------------------

# Explanatory information for the different metrics, shown when user clicks info icon
MetricInfo <- list(
  CU_ID = "Conservation Unit",
  Species = "Species",
  RelAbd = "Spawner abundance relative to ?",
  AbsAbd = "Spawner Abundance",
  LongTrend = "Long-term trend",
  PercChange = "Percent change over ??",
  ProbDeclBelowLBM = "Probability of decline below lower benchmark",
  FAZ = "Freshwater Adaptive Zone"
)

# Add labels here for any names or categories that should be shown with 'pretty' labels
# Names and category labels not in this list will show 'as-is' in the UI
Labels <- list(Species = 'Species',
               AvGen = 'Average Generation Length',
               RunTiming = 'Run Timing',
               LifeHistory = 'Life History Traits',
               Sk = "Sockeye",
               Ck = "Chinook",
               Co = 'Coho',
               FAZ="Freshwater Adaptive Zone",
               Estu="Early Stuart", 
               Early_Summer="Early Summer", 
               Summer="Summer", 
               Late="Late",
               CU_ID='Conservation Unit',
               RelAbd = "RelAbd",
               AbsAbd = "AbsAbd",
               LongTrend = "LongTrend",
               PercChange = "PercChange",
               ProbDeclBelowLBM = "ProbDeclBelowLBM"
)

# Specifies the order in which values for various attributes should appear in the graphs and tables
AttribLevels <- list(
  Species = c('Sk', 'Ck', 'Co'),
  FAZ = c('LFR', 'FRCany', 'LILL', 'MFR', 'UFR', 'LTh', 'STh', 'NTh'),
  Area = c('Fraser_Lower', 'Fraser_Canyon', 'Fraser_Mid','Fraser_Upper', 'Fraser_Thompson_Lower', 'Fraser_Thompson'),
  RunTiming = c('Estu', 'Spring', 'Early_Summer', 'Summer', 'Late', 'Fall', 'NA'),
  LifeHistory = c('Ocean', 'Stream', 'River', 'Lake', 'NA'),
  AvGen = c('3', '4', '5', '?')
)

# ------------------------ UI customization -----------------

# ------------------ Shared styling ---------
BoxHeaderStatus = 'primary'
WellPanelStyle <- "background: white"
PickerOptsSingleSelect <- list(`show-tick`=TRUE)
PickerOptsMultiSelect <- list(`show-tick`=TRUE, `actions-box`=TRUE, `selected-text-format`='count')
ButtonStyle <- "color: #fff; background-color: #337ab7; border-color: #2e6da4, height:70px; font-size: 100%"

# ------------ Data Filtering UI --------------

# attribute filter customization
# the names of the attributes users may filter by, shown in this order
FilterAttributes <- c('DataType', 'Species', 'FAZ', 'Area', 'RunTiming', 'LifeHistory', 'AvGen', 'CU_ID')

# allow only a single choice for these attributes:
SingleChoice <- c('DataType') 

# metric selector customization
# the names of the metrics users may choose from
#FilterMFMetrics <- unique(data.CU.MetricSeries$Metric)

# the names of the attributes users may choose from
FilterMFAttributes <- c("Species", "FAZ", "Area", "RunTiming", "LifeHistory", "AvGen")

# attributes for which it doesn't make sense to let the user select whether they should be shown
FilterMFhiddenAttributes <- c("CU_ID", "DataType")

# ---------------- Data Selector UI ------------------

SelectAttributes <- c('Species',
                      'CU_ID',
                      'FAZ',
                      'Area',
                      'RunTiming',
                      'LifeHistory',
                      'AvGen',
                      'Status')


# ---------------- Parcoords UI ------------------
# show the following axes in parcoords, in the order specified here
ParcoordsMetricOrder <- c("FAZ",
                          "RelAbd", "RelAbd.Status", 
                          "AbsAbd", "AbsAbd.Status",
                          "LongTrend", "LongTrend.Status",
                          "PercChange", "PercChange.Satus",
                          "ProbDeclBelowLBM", "ProbDeclBelowLBM.Status",
                          "Species",
                          "RunTiming",
                          "LifeHistory",
                          "CU_ID")

# don't ever display these columns
ParcoordsDrop <-c("ProbDeclBelowLBM", "ProbDeclBelowLBM.Status")

# hide these columns initially (i.e., make them appear as unchecked by default)
ParcoordsHideOnInit <- c('CU_ID')

# sort CUs on y-axis by these attributes
ParcoordsCUOrder <- c("Species", "FAZ", "AbsAbd")

# rotation of axis labels for metric axes (in degrees from horizontal)
ParcoordsLabelRotation <- -15 

# ---------------- Historgram Summaries UI ------------------
# histogram summaries will be generated for these metrics/attributes in the order specified
HistoSummaryAttribs <- c("Area", "FAZ", "RelAbd.Status", "AbsAbd.Status", "LongTrend.Status", "PercChange.Status")

# the number of CUs afte which display switches to bars by default
HistoMaxDots <- 40

# this list specifies the information necessary to construct a histogram from a numeric metric 
HistoCustomInfo <- list(
  Annual = list(
    Recent.ER = list(
      breaks = c( 0,10,20,30,40,50,60,70,80,90,100),
      names = c("Below 10%","10-20%","20-30%","30%-40%","40-50%", "50%-60%","60-70%","70%-80%","80-90%","Above 90%")
    )
  ),
  Change = list(
    Recent.ER = list(
      breaks = c(-100, -10, -5, -1, 1, 5, 10, 100),
      names = c(">10% decr", "5%-10% decr", "0-5% decr","No Change", "0-5% incr", "5-10% incr",">10% incr")
    )
  ))

# --------------- Radar Plot UI ----------------

# the metrics offered as choices for the radar plot
RadarMetricOpts <- c("RelAbd.Status", "AbsAbd.Status", "LongTrend", "PercChange", "ProbDeclBelowLBM")

# -------------------- Map UI ______________________

# attributes from CU lookup table to pull into spatial data frame
MapAttribs <- c('Lat', 'Lon', 'Species', 'HasMetricsData', 'HasTimeSeriesData', "RunTiming",
                "LifeHistory")


# the metrics to include in the map labels (i.e., the metric information shown on clicking on a CU in the map)
MapLabelMetrics <-  c("RelAbd", "LongTrend", "PercChange")

# species and status metrics are offered as color themes by default. Add additional options here. 
# then add a corresponding entry in the list of color palettes below
AdditionalColorThemes <- c('RunTiming', 'LifeHistory', 'HasMetricsData', 'HasTimeSeriesData')

# palettes to use for the different color theme options
ColorPalette <- list(
  Species = c(Sk = '#8c1aff', Co = '#ff9900', Ck = '#009999'),
  Status = c(Red = '#ff0000', Amber = '#ffa500', Green = '#00ff00', 'NA' = '#858585'),
  StatusChange = c('-2'='#ff0000', '-1'='#fe7581', '0'='#cca6ce', '1'='#7578fe', '2'='#0000ff', 'NA' = '#858585'),
  HasMetricsData = c(Yes = '#105592', No = '#ff0000'),
  HasTimeSeriesData = c(Yes = '#105592', No = '#ff0000'),
  RunTiming = c(Estu = '#fd5c71', Spring = '#cb3f51', Early_Summer = '#b01f32', Summer = '#8c0e0e', Late='#76353e', Fall='#6b464b', 'NA' = '#bebebe'),
  LifeHistory = c(Ocean = '#5fd2bb', Stream = '#347266', River = '#d25587', Lake = '#9b1349', 'NA' = '#858585')
)


StreamStyle.normal <- list(
  color = 'blue',
  weight = 1,
  opacity = 0.7
)

StreamStyle.highlighted <- list(
  color = 'blue',
  weight = 3,
  opacity = 0.9
)

CUPolyStyle.normal <- list(
  fill = TRUE,
  fillOpacity = 0.3,
  stroke = TRUE,
  weight = 2,
  opacity = 0.6
)

CUPolyStyle.highlighted <- list(
  fill = TRUE,
  fillOpacity = 0.3,
  stroke = TRUE,
  weight = 3,
  opacity = 0.9
)

CUPolyStyle.mouseover <- list(
  fill = FALSE,
  fillOpacity = 0.3,
  stroke = TRUE,
  weight = 3,
  opacity = 0.9,
  color = 'red'
)

CUMarkerStyle.normal <- list(
  radius = 6,
  stroke = FALSE,
  weight = 3,
  opacity = 0.5,
  fill = TRUE,
  fillOpacity = 0.5
)

CUMarkerStyle.highlighted <- list(
  radius = 10,
  stroke = FALSE,
  weight = 2,
  opacity = 0.9,
  fill = TRUE,
  fillOpacity = 0.9
)

CUMarkerStyle.mouseover <- list(
  radius = 10,
  stroke = TRUE,
  weight = 2,
  opacity = 0.9,
  fill = FALSE,
  fillOpacity = 0.9,
  color = 'red'
)

PopMarkerStyle.normal <- list(
  radius = 4,
  stroke = TRUE,
  weight = 1,
  opacity = 0.5,
  fill = TRUE,
  fillOpacity = 0.5
)

PopMarkerStyle.highlighted <- list(
  radius = 5,
  stroke = TRUE,
  weight = 1,
  opacity = 0.9,
  fill = TRUE,
  fillOpacity = 0.8
)

PopMarkerStyle.mouseover <- list(
  radius = 4,
  stroke = TRUE,
  weight = 1,
  opacity = 0.9,
  fill = FALSE,
  fillOpacity = 0.8,
  color = 'red'
)

SpiderLegs <- list(
  color = 'black',
  weight = 1,
  opacity = 0.7
)

# CU boundaries and markers will be shown overlaid in this order, i.e., the species listed first will be on bottom
zPaneOrder <- c("Co", "Ck", "Sk")

# sparkline styling
popTableAttribs <- list()         # styling for table rows showing information for individual selected populations
popTableAttribsCUHeader <- list() # styling for the table row showing information for the CU containing the selected populations
CUTableAttribs <- list()          # styling for the table rows in a table that only summarizes CU information

# styling for time series and status summaries
# 'full' attributes are for display of the table in a collapsible panel
# 'sidebar' attributes are for showing the same information in the sidebar

popTableAttribs[['full']] <- list(
  labelAttribs = c('Pop_ID', 'TS_Name'),
  missingTSText = 'no time series data',
  styles =list(Pop_ID = '',
               TS_Name = '',
               missingTS = ''),
  sparkCanvas = 'full-canvas',
  lineColor = '#3333ff',
  fillColor = '#33ccff',
  lineWidth = '3px',
  chartWidth = '200px',
  chartHeight = '50px'
)

# styling for the CU entry that serves as a 'header' for the selected populations shown in the table
# use 'complete' styling when all populations within the CU are selected
# 'partial' styling when only some populations are selected
popTableAttribsCUHeader[['full']] <- list(
  complete = list(labelAttribs = c('CU_ID', 'CU_Name'),
                  missingTSText = 'no time series data',
                  styles =list(CU_ID = 'font-weight: bold;',
                               CU_Name = 'font-weight: bold;',
                               missingTS = ''),
                  sparkCanvas = 'full-canvas',
                  metricCell = 'full-metric',
                  lineColor = '#000000',          
                  fillColor = '#a9a9a9',  
                  lineWidth = '3px',
                  chartWidth = '200px',
                  chartHeight = '50px',
                  metricCellValue = 'padding-left: 10px;',
                  metricCellArrow = 'padding-right: 10px;'),
  partial = list(labelAttribs = c('CU_ID', 'CU_Name'),
                 missingTSText = 'no time series data',
                 styles =list(CU_ID = 'font-weight: bold;',
                              CU_Name = 'font-weight: bold;',
                              missingTS = ''),
                 sparkCanvas = 'full-canvas',
                 metricCell = 'full-metric',
                 lineColor = '#707070',          
                 fillColor = '#d0d0d0',
                 lineWidth = '3px',
                 chartWidth = '200px',
                 chartHeight = '50px',
                 metricCellValue = 'padding-left: 10px;',
                 metricCellArrow = 'padding-right: 10px;')
)


CUTableAttribs[['full']] <- list(
  labelAttribs = c('CU_ID', 'CU_Name'),
  missingTSText = 'no time series data',
  styles = list(CU_ID = 'padding: 2px;', 
                CU_Name = 'padding: 2px;',
                missingTS = ''),
  sparkCanvas = 'full-canvas',
  lineColor = '#000000',
  fillColor = '#a9a9a9',
  lineWidth = '3px',
  chartWidth = '200px',
  chartHeight = '50px',
  metricCellValue = 'padding-left: 10px;',
  metricCellArrow = 'padding-right: 10px;'
)

# 'sidebar' attributes are for display of the table in the side panel of the app

popTableAttribs[['sidebar']] <- list(
  labelAttribs = c('TS_Name'),
  missingTSText = 'no time series data',
  styles = list(TS_Name = 'width: 40px;',
                missingTS = 'background-color: black; color: #b0b0b0;'),
  sparkCanvas = 'sidebar-canvas',
  lineColor = '#1aa3ff',
  fillColor = '#99d6ff',
  lineWidth = '2px',
  chartWidth = '60px',
  chartHeight = '20px'
)

# styling for the CU entry that serves as a 'header' for the selected populations shown in the table
# use 'complete' styling when all populations within the CU are selected
# 'partial' styling when only some populations are selected
popTableAttribsCUHeader[['sidebar']] <- list(
  complete = list(labelAttribs = c('CU_ID'),
                  missingTSText = 'no time series data',
                  styles = list(CU_ID = 'width: 40px; font-weight: bold;',
                                missingTS = 'background-color: black; color: #b0b0b0;'),
                  sparkCanvas = 'sidebar-canvas',
                  lineColor = '#e0e0e0',
                  fillColor = '#f0f0f0',
                  lineWidth = '2px',
                  chartWidth = '60px',
                  chartHeight = '20px',
                  metricCellValue = 'padding-left: 1px;',
                  metricCellArrow = 'padding-right: 1px;'),
  partial = list(labelAttribs = c('CU_ID'),
                 missingTSText = 'no time series data',
                 styles = list(CU_ID = 'width: 40px; font-weight: bold;',
                               missingTS = 'background-color: black; color: #b0b0b0;'),
                 sparkCanvas = 'sidebar-canvas',
                 lineColor = '#d0d0d0',
                 fillColor = '#e0e0e0',  
                 lineWidth = '2px',
                 chartWidth = '60px',
                 chartHeight = '20px',
                 metricCellValue = 'padding-left: 1px;',
                 metricCellArrow = 'padding-right: 1px;')
)

CUTableAttribs[['sidebar']] <- list(
  labelAttribs = c('CU_ID'),
  missingTSText = 'no time series data',
  styles = list(CU_ID = 'width: 20px;',
                missingTS = 'background-color: black; color: #b0b0b0;'),
  sparkCanvas = 'sidebar-canvas',
  lineColor = '#f5f5f5',
  fillColor = '#f0f0f0',
  lineWidth = '2px',
  chartWidth = '60px',
  chartHeight = '20px',
  metricCellValue = 'padding-left: 1px;',
  metricCellArrow = 'padding-right: 1px;'
)