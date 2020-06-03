# ------------ Contact form customization ----------------
SoS_email <- 'StateOfTheSalmon@gmail.com'
# after changing this email or the associated password, run the following R command on the command line, 
# then upload the resulting gmail_credentials file to shinyapps.io:
# create_smtp_creds_file(file = "gmail_credentials", user = SoS_email, provider = "gmail")
# if email fails to send, make sure google isn't blocking smtp requests from R/Shiny
# go to https://myaccount.google.com/lesssecureapps to allow login

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
  RelUBM = "Ratio of the current generational average (geometric) in relation to the CU-specific upper abundance benchmark",
  AbsUBM = "Ratio of the current generational average (geometric) in relation to the COSEWIC criteria D1 upper benchmark (10,000 spawners)",
  RelLBM = "Ratio of the current generational average (geometric) in relation to the CU-specific lower abundance benchmark",
  AbsLBM = "Ratio of the current generational average (geometric) in relation to the COSEWIC criteria D1 lower benchmark (1,000 spawners)",
  LongTrend = "Ratio of the current generational average (geometric) in relation to the long-term average (geometric)",
  PercChange = "Percent change over the most recent three generations (geometric)",
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
               RelUBM = "RelUBM",
               AbsUBM = "AbsUBM",
               RelLBM = "RelLBM",
               AbsLBM = "AbsLBM",
               RelUBM.Status = 'Rel.Status',
               RelLBM.Status = 'Rel.Status',
               AbsUBM.Status = 'Abs.Status',
               AbsLBM.Status = 'Abs.Status',
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
  AvGen = c('3', '4', '5')
)

# ------------------------ UI customization -----------------

# ------------------ Shared styling ---------
BoxHeaderStatus = 'primary'
WellPanelStyle <- "background: white"
PickerOptsSingleSelect <- list(`show-tick`=TRUE)
PickerOptsMultiSelect <- list(`show-tick`=TRUE, `actions-box`=TRUE, `selected-text-format`='count')
#ButtonStyle <- "color: #fff; background-color: #337ab7; border-color: #2e6da4; height:70px; font-size: 100%"
ButtonStyle <- "color: #fff; background-color: #337ab7; border-color: #337ab7; height:40px; font-size: 100%"

# species and status metrics are offered as color themes by default. Add additional options here. 
# then add a corresponding entry in the list of color palettes below
ColorThemes <- c(Species = 'Species', 
                 Rel.Status = "RelLBM.Status", 
                 Abs.Status = "AbsLBM.Status", 
                 LongTrend.Status = "LongTrend.Status", 
                 PercChange.Status = "PercChange.Status",
                 'Run Timing' = 'RunTiming', 
                 'Life History' = 'LifeHistory', 
                 'Has Metrics Data' = 'HasMetricsData', 
                 'Has Time Series Data' = 'HasTimeSeriesData')

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
default.colorTheme <-'Species'

# ------------ Data Filtering UI --------------

# attribute filter customization
# the names of the attributes users may filter by, shown in this order
FilterAttributes <- c('Species', 'FAZ', 'Area', 'RunTiming', 'LifeHistory', 'AvGen', 'CU_ID')

# allow only a single choice for these attributes:
SingleChoice <- c() 

# the names of the attributes users may choose from
FilterMFAttributes <- c("Species", "FAZ", "Area", "RunTiming", "LifeHistory", "AvGen")

# attributes for which it doesn't make sense to let the user select whether they should be shown
FilterMFhiddenAttributes <- c("CU_ID")

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
                          "RelLBM.Status", "RelUBM", "RelLBM",
                          "AbsLBM.Status", "AbsUBM", "AbsLBM",
                          "LongTrend.Status", "LongTrend", 
                          "PercChange.Status", "PercChange", 
                          "ProbDeclBelowLBM.Status", "ProbDeclBelowLBM",
                          "Species",
                          "RunTiming",
                          "LifeHistory",
                          "CU_ID")

# don't ever display these columns 
# (Note: RelLBM.Status and RelUBM.Status are always the same, ditto for AbsLBM.Status and AbsUBM.Status, so show only one of each)
ParcoordsDrop <-c("RelUBM.Status", "AbsUBM.Status", "ProbDeclBelowLBM.Status", "ProbDeclBelowLBM")

# hide these columns initially (i.e., make them appear as unchecked by default)
ParcoordsHideOnInit <- c('RelUBM', 'AbsUBM', 'RelLBM', 'AbsLBM', 'LongTrend', 'PercChange', 'ProbDeclBelowLBM', 'CU_ID')

# sort CUs on y-axis by these attributes
ParcoordsCUOrder <- c("Species", "FAZ", "RelLBM")

# rotation of axis labels for metric axes (in degrees from horizontal)
ParcoordsLabelRotation <- -15 

# use these rounding factors to control what is shown on the slider inputs that control the axes
# e.g., 0 rounds to nearest integer, 2 rounds to nearest 2 digits after . etc
ParcoordsRound <- list( RelUBM = 2,
                        AbsUBM = 2,
                        RelLBM = 2,
                        AbsLBM = 2,
                        LongTrend = 2,
                        PercChange = 0,
                        ProbDeclBelowLBM = 2,
                        # when 'status' actually shows change in status between years
                        RelUBM.Status = 0,  
                        AbsUBM.Status = 0,
                        RelLBM.Status = 0,  
                        AbsLBM.Status = 0,
                        LongTrend.Status = 0,
                        ProbDeclBelowLBM.Status = 0)
                        

# ---------------- Historgram Summaries UI ------------------
# histogram summaries will be generated for these metrics/attributes in the order specified
HistoSummaryAttribs <- c("Area", "FAZ", 
                         "RelLBM.Status", "AbsLBM.Status", 
                         "LongTrend.Status", 
                         "PercChange.Status")

# the number of CUs afte which display switches to bars by default
HistoMaxDots <- 40

# this list specifies the information necessary to construct a histogram from a numeric metric 
HistoCustomInfo <- list(
  Annual = list(
    Sample.metric = list(
      breaks = c( 0,10,20,30,40,50,60,70,80,90,100),
      names = c("Below 10%","10-20%","20-30%","30%-40%","40-50%", "50%-60%","60-70%","70%-80%","80-90%","Above 90%")
    )
  ),
  Change = list(
    Sample.metric = list(
      breaks = c(-100, -10, -5, -1, 1, 5, 10, 100),
      names = c(">10% decr", "5%-10% decr", "0-5% decr","No Change", "0-5% incr", "5-10% incr",">10% incr")
    )
  ))

# --------------- Radar Plot UI ----------------

# the metrics offered as choices for the radar plot
RadarMetricOpts <- c("RelLBM", "AbsLBM", "LongTrend", "PercChange", "ProbDeclBelowLBM")

# -------------------- Map UI ______________________

# attributes from CU lookup table to pull into spatial data frame
MapAttribs <- c('Lat', 'Lon', 'Species', 'HasMetricsData', 'HasTimeSeriesData', "RunTiming",
                "LifeHistory")


# the metrics to include in the map labels (i.e., the metric information shown on clicking on a CU in the map)
MapLabelMetrics <-  c("RelLBM", "AbsLBM", "RelUBM", "AbsUBM", "LongTrend", "PercChange")


StreamStyle.normal <- list(
  color = 'blue',
  weight = 2,
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

# sparkline options
sparkDefaultDataType <- "SpnForTrend_Wild"
sparkAdditionalDataTypeOpts <- list('none' = 'none',
                                    'Wild spawners for abundance' = 'SpnForAbd_Wild',
                                    'Total spawners for trend' = 'SpnForTrend_Total',
                                    'Total spawners for abundance' = 'SpnForAbd_Total')

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
  # lineColor and fillColor are used to style the primary sparkline (SpnForTrend_Wild); 
  # lineColor2 and fillColor2 are for styling the (optional) secondary sparkline 
  lineColor = '#3333ff',
  fillColor = '#33ccff',
  lineColor2 = '#000000',
  fillColor2 = '#555555',
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
                  # lineColor and fillColor are used to style the primary sparkline (SpnForTrend_Wild); 
                  # lineColor2 and fillColor2 are for styling the (optional) secondary sparkline 
                  lineColor = '#000000',          
                  fillColor = '#a9a9a9',  
                  lineColor2 = '#000000',
                  fillColor2 = '#555555',
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
                 # lineColor and fillColor are used to style the primary sparkline (SpnForTrend_Wild); 
                 # lineColor2 and fillColor2 are for styling the (optional) secondary sparkline 
                 lineColor = '#707070',          
                 fillColor = '#d0d0d0',
                 lineColor2 = '#000000',
                 fillColor2 = '#555555',
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
  # lineColor and fillColor are used to style the primary sparkline (SpnForTrend_Wild); 
  # lineColor2 and fillColor2 are for styling the (optional) secondary sparkline 
  lineColor = '#000000',
  fillColor = '#a9a9a9',
  lineColor2 = '#000000',
  fillColor2 = '#555555',
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
  # lineColor and fillColor are used to style the primary sparkline (SpnForTrend_Wild); 
  # lineColor2 and fillColor2 are for styling the (optional) secondary sparkline 
  lineColor = '#1aa3ff',
  fillColor = '#99d6ff',
  lineColor2 = '#909090',
  fillColor2 = '#909090',
  lineWidth = '2px',
  chartWidth = '200px',
  chartHeight = '50px'
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
                  # lineColor and fillColor are used to style the primary sparkline (SpnForTrend_Wild); 
                  # lineColor2 and fillColor2 are for styling the (optional) secondary sparkline 
                  lineColor = '#e0e0e0',
                  fillColor = '#f0f0f0',
                  lineColor2 = '#909090',
                  fillColor2 = '#909090',
                  lineWidth = '2px',
                  chartWidth = '200px',
                  chartHeight = '50px',
                  metricCellValue = 'padding-left: 1px;',
                  metricCellArrow = 'padding-right: 1px;'),
  partial = list(labelAttribs = c('CU_ID'),
                 missingTSText = 'no time series data',
                 styles = list(CU_ID = 'width: 40px; font-weight: bold;',
                               missingTS = 'background-color: black; color: #b0b0b0;'),
                 sparkCanvas = 'sidebar-canvas',
                 # lineColor and fillColor are used to style the primary sparkline (SpnForTrend_Wild); 
                 # lineColor2 and fillColor2 are for styling the (optional) secondary sparkline 
                 lineColor = '#d0d0d0',
                 fillColor = '#e0e0e0',  
                 lineColor2 = '#909090',
                 fillColor2 = '#909090',
                 lineWidth = '2px',
                 chartWidth = '200px',
                 chartHeight = '50px',
                 metricCellValue = 'padding-left: 1px;',
                 metricCellArrow = 'padding-right: 1px;')
)

CUTableAttribs[['sidebar']] <- list(
  labelAttribs = c('CU_ID'),
  missingTSText = 'no time series data',
  styles = list(CU_ID = 'width: 20px;',
                missingTS = 'background-color: black; color: #b0b0b0;'),
  sparkCanvas = 'sidebar-canvas',
  # lineColor and fillColor are used to style the primary sparkline (SpnForTrend_Wild); 
  # lineColor2 and fillColor2 are for styling the (optional) secondary sparkline 
  lineColor = '#f5f5f5',
  fillColor = '#f0f0f0',
  lineColor2 = '#909090',
  fillColor2 = '#909090',
  lineWidth = '2px',
  chartWidth = '200px',
  chartHeight = '50px',
  metricCellValue = 'padding-left: 1px;',
  metricCellArrow = 'padding-right: 1px;'
)

# -------------------- DataTable UI ______________________

# include these attributes from lookup file when displaying table
#DataTable.CULookupAttribsToInclude <- c('CU_Name', 'DataStartYear', 'DataEndYear')
DataTable.CULookupAttribsToInclude <- c('CU_Name')

# show and download these columns from the metrics data frame when showing the metrics table, in this order
DataTable.MetricCols <-c("CU_Name",  "Species",
                         "RelLBM.Status", "RelLBM", "RelUBM", "AbsLBM.Status", "AbsLBM", "AbsUBM",
                         "LongTrend.Status", "LongTrend",
                         "PercChange.Status", "PercChange",
                         "ProbDeclBelowLBM.Status", "ProbDeclBelowLBM",
                         "FAZ", "Area", "RunTiming", "LifeHistory", "AvGen")

# download these columns when downloading CU time series data
DataTable.TScolsCU <- c('CU_ID', 'CU_Name', 'Species', 'Year', 
                        'SpnForTrend_Wild', 'SpnForAbd_Wild', 'SpnForTrend_Total', 'SpnForAbd_Total')

DataTable.ColsPop <- c("Pop_ID", "Pop_Name", "WSP_ts", "Species", "CU_ID", 
                       "FAZ", "MAZ", "JAZ", 
                       "Lat", "Lon", "FWA_WATERSHED_CODE",                                      
                       "HasTimeSeriesData", "tsNames", "DataStartYear", "DataEndYear")  

# download these columns when downloading Pop time series data
DataTable.TScolsPop <- c('Pop_UID', 'DataSet', 'Year', 'Pop_ID', 'Pop_Name', 'CU_ID', 'CU_Name', 
                         'SpnForTrend_Wild', 'SpnForAbd_Wild', 'SpnForTrend_Total', 'SpnForAbd_Total')

# use these to round values for display
DataTable.Round <- list( SpnForTrend_Wild = 0,
                         SpnForTrend_Total = 0,
                         SpnForAbd_Wild = 0,
                         SpnForAbd_Total = 0,
                        RelUBM = 2,
                        AbsUBM = 2,
                        RelLBM = 2,
                        AbsLBM = 2,
                        LongTrend = 2,
                        PercChange = 0,
                        ProbDeclBelowLBM = 2,
                        # when 'status' actually shows change in status between years
                        RelUBM.Status = 0,  
                        AbsUBM.Status = 0,
                        RelLBM.Status = 0,  
                        AbsLBM.Status = 0,
                        LongTrend.Status = 0,
                        ProbDeclBelowLBM.Status = 0)


