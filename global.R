# A note on UI and backend terminology: 
# The synoptic app implements what is essentially a two-stage selection process. 
# Users have the option to 'filter' the data, which has the effect of removing 
# any elements from the dataset that aren't included in the filter.
# Users also have the option to 'highlight' specific CUs and/or populations 
# for the purpose of comparing/contrasting these with the CUs/populations not
# highlighted.  
# In line with terminology used in Shiny widgets and software development more generally,
# in the code, the term 'selection' is used in conjunction with the data structures and
# algorithms used to implement the 'highlighting' process,
# (e.g., data.currentSelection() holds the currently highlighted CUs and populations etc)
# whereas the term 'highlighting' is used to refer to styling of graphical elements
# to emphasize them visually. Often, visual highlighting signals that the element is
# part of data.currentSelection(), but visual highlighting can be expressed through
# multiple styles, and also serves purposes other than communicating the contents of the
# data.currentSelection() to the user, such as showing the user which UI element they 
# are currently hovering over.
# Note on managing selection/highlighting:
# The set of currently selected CUs and populations is managed through a set of shiny reactive
# variables. However, DT:dataTables have their own mechanism of keeping track of highlighted rows,
# and to make brushing in parcoords work properly, it needs to keep track of its brushing
# and communicate through a SharedData object. Having these three mechanisms going at the same
# time can lead to situations where the mechanisms 'fight' to set/reset the selection to their
# stored version. Because of this, the event handlers that manage the update process need
# to be managed so updates to and from data table selection and parcoords selection are 
# only propagated when the respective widgets are in focus. 

library(xfun)
library(rgdal)
library(sp)
library(sf)

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
CUBoundariesFile <- "data/All_Species_CU_Boundaries_Fraser.gpkg"
# special-purpose stream network created by splitting BC Freshwater Atlas streams into segments and attaching CU and site information
StreamNetworkFile <- "data/StreamNetwork_Fraser.gpkg"
# similar to StreamNetworkFile, but the polyline associated with each segment is extended to include the full upstream network above the segment
ExtendedStreamNetworkFile <- "data/StreamNetwork_Fraser_extended.gpkg"
         
# ----- explanatory information for the different metrics ---------

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

# add labels here for any names or categories that should be shown with pretty labels
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
  
# Specify order in which values for various attributes should appear in the graphs and tables
AttribLevels <- list(
  Species = c('Sk', 'Ck', 'Co'),
  FAZ = c('LFR', 'FRCany', 'LILL', 'MFR', 'UFR', 'LTh', 'STh', 'NTh'),
  Area = c('Fraser_Lower', 'Fraser_Canyon', 'Fraser_Mid','Fraser_Upper', 'Fraser_Thompson_Lower', 'Fraser_Thompson'),
  RunTiming = c('Estu', 'Spring', 'Early_Summer', 'Summer', 'Late', 'Fall', 'NA'),
  LifeHistory = c('Ocean', 'Stream', 'River', 'Lake', 'NA'),
  AvGen = c('3', '4', '5', '?')
)

# ------------------------ UI customization -----------------

# ------------------ Common Styles ---------
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

# 'full' attributes are for display of the tabale in a collapsible panel

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
# --------------------- Helper functions for data display and restructuring ---------

# get full name of a CU, given the CU's ID
getCUname <- function(CU_ID) {paste0(data.CU.Lookup[data.CU.Lookup$CU_ID == CU_ID, 'CU_Name'][1], ' (', CU_ID, ')')}

# get full name of a population, given the population Pop_UID
getPopName <- function(Pop_UID) {paste0(data.Pop.Lookup[Pop_UID, 'CU_ID'], ': ', data.Pop.Lookup[Pop_UID, 'Pop_Name'], ' (', data.Pop.Lookup[Pop_UID, 'Pop_ID'], ')')}

# get name of a population, given the population Pop_UID
getPopNameShort <- function(Pop_UID) {data.Pop.Lookup[Pop_UID, 'Pop_Name']}

# get list of populations associated with the given CUs
getPopsForCUs <- function(CUs) {data.Pop.Lookup[data.Pop.Lookup$CU_ID %in% CUs, 'Pop_UID']}

# unpack a comma-separated list of items
unpack <- function(itemList) {
  if (is.null(itemList) || length(itemList) == 0 || itemList == '') {
    c()
  } else {
    strsplit(itemList, ',')[[1]]
  }
} 

# pass through a CU metrics table and add a 'labels' column
# CUlabels should be a named vector that specified the label for each CU
# if CUnames is given, uses the labels from CUnames
# otherwise assumes that the ds row names are to be used as the labels 
WithLabels <- function(ds, CUnames = NULL) {
  if (is.null(CUnames)) {
    ds$labels <- row.names(ds)
  } else {
    ds$labels <- CUnames[row.names(ds)]
  }
  return(ds)
}

# get the label for pretty printing, given the name of a metric, attribute, or attribute category
GetLabel <- function(m) {
  if (m %in% names(Labels)) {
    Labels[[m]]
  } else
  {
    m
  }
}

# get named choices for a metric or attribute, given the name of the metric 
# and a data frame with a column of values for that metric
GetNamedChoices  <- function(m, df) {
  if (!(m %in% names(df))) {
    NULL
  } else {
    if (all(is.na(df[, m]))) {
      list('NA' = 'NA')
    } else {
      if (is.factor(df[, m])) {
        choices <- levels(df[, m]) 
        choices <- choices[choices %in% df[, m]]
      } else {
        choices <- unique(as.character(df[, m]))
      }
      names(choices) <- sapply(choices, GetLabel)
      choices
    }
  }
}

# Given a lookup table, the name of the 'outdated' field and the name of the new field in the table, and a vector
# with outated values, returns a vector where old values are replaced with the updated ones.
# Use this to make fields like CU_ID, Pop_ID etc consistent across files
SubstituteValues <- function(old, new, lookup, oldVals) {
  lookup <- lookup[!is.na(lookup[, old]) & lookup[, old] != "", ]
  lookup <- unique(lookup[, c(old, new)])
  if (any(duplicated(lookup[,old]))) {
    cat("Warning: found duplicate values in column ", old, " of table ", lookup, " while translating from ", old, "to ", new, ":")
    print(lookup[lookup[, old] %in% duplicated(lookup[,old]), ])
    cat("using first occurrence ... \n")
    lookup <- lookup[!duplicated(lookup[,old]), ]
  }
  row.names(lookup) <- as.character(lookup[, old])
  lookup[as.character(oldVals), new]
}

# convert GIS data to geographic
convertToLeafletProjection <- function(map) {st_transform(map, CRS("+proj=longlat +datum=WGS84"))}

# stip trailing zeros from BC watershed code
strip <- function(code) {gsub('(-000000)*$', '', code)}

# strip CU information from Pop UID, leaving the Pop ID
get_Pop_ID_From_Pop_UID <- function(UID) {strsplit(UID, '[.]')[[1]][2]}

# strip Pip ID from Pop UID, leaving the CU ID
get_CU_ID_From_Pop_UID <- function(UID) {strsplit(UID, '[.]')[[1]][1]}

# ------------------- put together initial data set -------------------

# Get the metrics and time series data for CUs and populations
data.CU.MetricsSeries <- read.csv(metricsFile, stringsAsFactors = F)
data.CU.Metrics.Years <- sort(unique(data.CU.MetricsSeries$Year))
data.CU.TimeSeries <- read.csv(CUTimeSeriesFile, stringsAsFactors = F)
data.Pop.TimeSeries <- read.csv(PopTimeSeriesFile, stringsAsFactors = F)

# CU boundary polygons
data.CU.Spatial <- convertToLeafletProjection(st_read(CUBoundariesFile, stringsAsFactors=F, quiet=TRUE))

# stream selector network data
# Don't use ESRI shp for this! The lists of CUs and populations associated with the various stream segments will end up truncated.
data.Streams <- convertToLeafletProjection(st_read(StreamNetworkFile, stringsAsFactors=F, quiet=TRUE))
data.StreamsExtended <- convertToLeafletProjection(st_read(ExtendedStreamNetworkFile, stringsAsFactors=F, quiet=TRUE))
row.names(data.Streams) <- data.Streams$code
row.names(data.StreamsExtended) <- data.StreamsExtended$code

# Lookup table for joining metrics and spatial information for CUs
data.CU.Lookup <- read.csv(CULookupFile, stringsAsFactors = F)

# Lookup table for joining metrics and spatial information for Populations
data.Pop.Lookup <- read.csv(PopLookupFile, stringsAsFactors = F)

# ** Fix the CU_ID field in the metrics and data files, as well as the map data to make CU IDs consistent across files
data.CU.MetricsSeries$CU_ID <- SubstituteValues('CU_MetricsData_CU_ID', 'CU_ID', data.CU.Lookup, data.CU.MetricsSeries$CU_ID)
data.CU.TimeSeries$CU_ID <- SubstituteValues('CU_TimeSeriesData_CU_ID', 'CU_ID', data.CU.Lookup, data.CU.TimeSeries$CU_ID)
data.CU.Spatial$CU_ID <- SubstituteValues('MapData_CU_ID', 'CU_ID', data.CU.Lookup, data.CU.Spatial$CU_INDEX)
data.Pop.TimeSeries$CU_ID <- SubstituteValues('Pop_TimeSeriesData_CU_ID', 'CU_ID', data.CU.Lookup, data.Pop.TimeSeries$CU_ID)
data.Pop.Lookup$CU_ID <- SubstituteValues('MapData_CU_ID', 'CU_ID', data.CU.Lookup, data.Pop.Lookup$MapData_CU_ID)

#** Create a unique population ID, 'Pop_UID', consisting of CU_ID and Pop_ID, to be used across files.
# add the unique pop ID to the various files containing population data
# Pop Lookup file
data.Pop.Lookup$Pop_UID <- paste(data.Pop.Lookup$CU_ID, data.Pop.Lookup$Pop_ID, sep='.')
row.names(data.Pop.Lookup) <- data.Pop.Lookup$Pop_UID

# Population time series data
# right now, pop ID is missing for Coho, so we need to do a somewhat complicated lookup here, by either pop ID or by pop name 
# hopefully this is temporary and will be fixed as part of the data assembly process evantually
# Identify first data year for each time series. This is only done to avoid printing out warnings about id or name mismatches more than once.
# Should be removed in production version since it takes some time to run.
# data.Pop.TimeSeries$FirstYear <- apply(data.Pop.TimeSeries, 1, function(data.row, popData){
#   popID  <- as.numeric(data.row[['Pop_ID']])
#   if (!is.na(popID)) 
#     popDataRows <- !is.na(popData$Pop_ID) & popData$Pop_ID == popID & popData$DataSet == data.row[['DataSet']]
#   else
#     popDataRows <- popData$Pop_Name == data.row[['Pop_Name']] & popData$DataSet == data.row[['DataSet']]
#   all(as.numeric(popData[popDataRows, 'Year']) >= as.numeric(data.row[['Year']]))
#   
# }, data.Pop.TimeSeries)

data.Pop.TimeSeries$Pop_UID <- apply(data.Pop.TimeSeries, 1, function(data.row) {
  matches <- c(F)
  popID  <- as.numeric(data.row[['Pop_ID']])
  if (!is.na(popID)) { # lookup with Pop ID and species - this is the preferred way to match 
    matches <- data.Pop.Lookup$TimeSeriesData_Species == data.row[["DataSet"]] & as.numeric(data.Pop.Lookup$Pop_ID) == popID
    # check for potential name mismatch
    # if (data.row[["FirstYear"]]) {
    #   if(any(matches) && data.Pop.Lookup[matches, 'TimeSeriesData_Pop_Name'][1] != data.row[['Pop_Name']]) 
    #     cat("Warning: name mismatch for site ", data.row[["DataSet"]], ' - ', data.row[["Pop_Name"]], ' ( Pop ID: ', popID,
    #       '). Expected ', data.Pop.Lookup[matches, 'TimeSeriesData_Pop_Name'][1], '!\n')
    # }
  }
  else if (data.row[["Pop_Name"]] != "") {  # if no Pop ID available, try lookup with Pop Name and Species - temporary fix until Coho pop IDs integrated into data
    matches <- data.Pop.Lookup$TimeSeriesData_Species == data.row[["DataSet"]] & data.Pop.Lookup$TimeSeriesData_Pop_Name == data.row[["Pop_Name"]]
  }
  if (any(matches)) return(data.Pop.Lookup[matches, 'Pop_UID'][1])
  else {
    # if(data.row[["FirstYear"]])  # show warning only for first occurence
    #   cat("Warning: no match found in lookup file for site ",
    #       data.row[["DataSet"]], ' - ', data.row[["Pop_Name"]], ' ( Pop ID: ', popID, '). This site will not be selectable.\n')
    return("")
  }
})
data.Pop.TimeSeries <- data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID != '', ]
data.Pop.TimeSeries$TS_Name <- data.Pop.TimeSeries$Pop_Name

# there are sometimes multiple time series for one pop UID
# get the names for those and add to pop lookup table
data.Pop.Lookup$tsNames <- rep('', nrow(data.Pop.Lookup))
for (p in unique(data.Pop.TimeSeries$Pop_UID)) {
  data.Pop.Lookup[p, 'tsNames'] <- paste(unique(data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID == p, 'TS_Name']), collapse = ':')
  for (name in strsplit(data.Pop.Lookup[p, 'tsNames'], ':')[[1]]) {
    ds <- data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID == p & data.Pop.TimeSeries$TS_Name == name, ]
    if(any(duplicated(ds[, 'Year'])))  {
        cat("Warning: site ", p, ' (', name, ' ) has duplicate enries in time series data\n')
        yr <- ds$Year[duplicated(ds$Year)][1]
        print(ds[ds$Year == yr, ])
    }
  }
  rm(ds)
}

#** Eliminate data not selectable (out of bounds or not properly identified etc)
data.CU.TimeSeries <- data.CU.TimeSeries[!is.na(data.CU.TimeSeries$CU_ID), ]
data.CU.MetricsSeries <- data.CU.MetricsSeries[!is.na(data.CU.MetricsSeries$CU_ID), ]
data.Pop.TimeSeries$Pop_Name <- data.Pop.Lookup[data.Pop.TimeSeries$Pop_UID, 'Pop_Name']
data.CU.Spatial <- data.CU.Spatial[!is.na(data.CU.Spatial$CU_ID), ]

# For the stream data: create a new field that only contains CUs in the current database
data.Streams$CUsSelectable <- unlist(lapply(data.Streams$AllCUsAbove, function(CUs) {
  CUs <- strsplit(CUs, ':')[[1]]
  CUs <- CUs[CUs %in% data.CU.Lookup$CU_ID]
  if (length(CUs) > 0) paste(sort(CUs), collapse=",")
  else ""
}))
# ditto for populations
data.Streams$PopsSelectable <- unlist(lapply(data.Streams$AllSitesAbove, function(sites) {
  sites <- strsplit(sites, ':')[[1]]
  sites <- sites[sites %in% data.Pop.Lookup$Pop_UID]
  if (length(sites) > 0) paste(sort(sites), collapse=",")
  else ""
}))

# Now prune the stream network to remove any streams that don't potentially select CUs in the current CU database
data.Streams <- data.Streams[data.Streams$CUsSelectable != "", ]

# add stream order to stream network - use new code analogous to FWA watershed code,
# but composed of unique segment keys
data.Streams$StreamOrder <- unlist(lapply(data.Streams$code, function(code) {
  length(strsplit(code, '-')[[1]])
}))

data.Streams <- data.Streams[order(data.Streams$StreamOrder, decreasing=F), ]
data.StreamsExtended <- data.StreamsExtended[row.names(data.Streams), ]

#** Rearrange the metrics data so all metrics are in columns and create an associated 'Status' metric for each main metric (labeled <metric>.Status)
data.CU.MetricsSeries.MetricNames <- unique(data.CU.MetricsSeries$Metric)
names(data.CU.MetricsSeries)[names(data.CU.MetricsSeries) == 'Label'] <- 'DataType'
data.CU.MetricsSeries.StatusMetricNames <- paste(data.CU.MetricsSeries.MetricNames, 'Status', sep='.')
data.CU.Metrics <- unique(data.CU.MetricsSeries[, c("CU_ID", "DataType", "Year")])
row.names(data.CU.Metrics) <- paste(data.CU.Metrics$CU_ID, data.CU.Metrics$DataType, data.CU.Metrics$Year, sep=".")
for (l in unique(data.CU.Metrics$DataType)) {
  for (m in data.CU.MetricsSeries.MetricNames) {
    data.subs <- unique(data.CU.MetricsSeries[data.CU.MetricsSeries$Metric == m & data.CU.MetricsSeries$DataType == l, c('CU_ID', 'Year', 'Value', 'Status')])
    row.names(data.subs) <- paste(data.subs$CU_ID, l, data.subs$Year, sep='.')
    data.CU.Metrics[row.names(data.subs), m] <- data.subs$Value
    data.CU.Metrics[row.names(data.subs), paste(m, "Status", sep=".")] <- factor(data.subs$Status, levels=c('Red', 'Amber', 'Green'), ordered=T)
  }
}
rm(l, m)

# Identify data years present in CU metrics data
data.CU.Metrics.Years <- as.character(sort(unique(as.numeric(data.CU.Metrics$Year))))

#** Attach attributes from CU lookup to metrics file 
data.CU.Metrics <- merge(data.CU.Metrics, unique(data.CU.Lookup[ , c("CU_ID", "Species", "FAZ", "Area", "RunTiming", "LifeHistory", "AvGen")]), by=c("CU_ID"), all.x=T, all.y=F)
for (attrib in names(data.CU.Metrics)) {
  if (attrib %in% names(AttribLevels)) { 
    # turn this attribute into a factor with the specified ordering
    data.CU.Metrics[is.na(data.CU.Metrics[, attrib]), attrib] <- 'NA'
    data.CU.Metrics[, attrib] <- factor(as.character(data.CU.Metrics[, attrib]), levels = AttribLevels[[attrib]])
  }
}
row.names(data.CU.Metrics) <- paste(data.CU.Metrics$CU_ID, data.CU.Metrics$DataType, data.CU.Metrics$Year, sep=".")

# Attach attributes to CU polygon data
#data.CU.Spatial <- sp::merge(data.CU.Spatial, unique(data.CU.Lookup[ , c('CU_Name' , 'CU_ID', MapAttribs)]), by=c("CU_ID"), all.x=T, all.y=F)

# Eliminate unnecessary columns from map data
attribsToKeep <- c('CU_NAME', 'CU_ID', 'geom')
data.CU.Spatial[, names(data.CU.Spatial)[!(names(data.CU.Spatial) %in% attribsToKeep)]] <- NULL
rm(attribsToKeep)

# identify CUs and populations in dataset
data.CUs <- unique(data.CU.Lookup$CU_ID)
data.Pops <- unique(data.Pop.Lookup$Pop_UID)
data.Watersheds <- unique(data.Streams$code)

# add information about availability of associated metrics and time-series data to lookup tables
getMinYr <- function(df) {if (nrow(df) > 0) min(df$Year) else NA}
getMaxYr <- function(df) {if (nrow(df) > 0) max(df$Year) else NA}
data.Pop.Lookup$HasTimeSeriesData <- unlist(lapply(data.Pop.Lookup$Pop_UID, function(uid) {if(uid %in% data.Pop.TimeSeries$Pop_UID) 'Yes' else 'No'}))

data.CU.Lookup$HasMetricsData <- unlist(lapply(data.CU.Lookup$CU_ID, function(cu_id) {if(cu_id %in% data.CU.MetricsSeries$CU_ID) 'Yes' else 'No'}))  
data.CU.Lookup$HasTimeSeriesData <- unlist(lapply(data.CU.Lookup$CU_ID, function(cu_id) {if(cu_id %in% data.CU.TimeSeries$CU_ID) 'Yes' else 'No'})) 
data.Pop.Lookup$DataStartYear <- unlist(lapply(data.Pop.Lookup$Pop_UID, function(p) {getMinYr(data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID == p, ])}))
data.Pop.Lookup$DataEndYear <- unlist(lapply(data.Pop.Lookup$Pop_UID, function(p) {getMaxYr(data.Pop.TimeSeries[data.Pop.TimeSeries$Pop_UID == p, ])}))
data.CU.Lookup$DataStartYear <- unlist(lapply(data.CU.Lookup$CU_ID, function(CU) {getMinYr(data.CU.TimeSeries[data.CU.TimeSeries$CU_ID == CU, ])}))
data.CU.Lookup$DataEndYear <- unlist(lapply(data.CU.Lookup$CU_ID, function(CU) {getMaxYr(data.CU.TimeSeries[data.CU.TimeSeries$CU_ID == CU, ])}))

