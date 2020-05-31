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

# various customization options
source('Settings.R', local=TRUE)

# helper functions
source('helpers.R')

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

# ** Fix the CU_ID field in the metrics and data files to make CU IDs consistent across files
data.CU.MetricsSeries$CU_ID <- SubstituteValues('CU_MetricsData_CU_ID', 'CU_ID', data.CU.Lookup, data.CU.MetricsSeries$CU_ID)
data.CU.TimeSeries$CU_ID <- SubstituteValues('CU_TimeSeriesData_CU_ID', 'CU_ID', data.CU.Lookup, data.CU.TimeSeries$CU_ID)
data.Pop.TimeSeries$CU_ID <- SubstituteValues('Pop_TimeSeriesData_CU_ID', 'CU_ID', data.CU.Lookup, data.Pop.TimeSeries$CU_ID)
#data.CU.Spatial$CU_ID <- SubstituteValues('MapData_CU_ID', 'CU_ID', data.CU.Lookup, data.CU.Spatial$CU_INDEX)
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
  popID  <- as.character(data.row[['Pop_ID']])
  if (!is.na(popID)) { # lookup with Pop ID and species - this is the preferred way to match 
    matches <- data.Pop.Lookup$TimeSeriesData_Species == data.row[["DataSet"]] & as.character(data.Pop.Lookup$Pop_ID) == popID
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
data.CU.MetricsSeries.StatusMetricNames <- paste0(data.CU.MetricsSeries.MetricNames, '.Status')
data.CU.Metrics <- unique(data.CU.MetricsSeries[, c("CU_ID", "Year")])
row.names(data.CU.Metrics) <- paste(data.CU.Metrics$CU_ID, data.CU.Metrics$Year, sep=".")
for (m in data.CU.MetricsSeries.MetricNames) {
  data.CU.Metrics[, m] <- data.CU.MetricsSeries[data.CU.MetricsSeries$Metric == m, 'Value']
  data.CU.Metrics[, paste(m, "Status", sep=".")] <- factor(data.CU.MetricsSeries[data.CU.MetricsSeries$Metric == m, 'Status'], 
                                                           levels=c('Red', 'Amber', 'Green'), ordered=T)
}
rm(m)

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
row.names(data.CU.Metrics) <- paste(data.CU.Metrics$CU_ID, data.CU.Metrics$Year, sep=".")

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

# add CU names to Labels structure
for (cu in data.CUs) Labels[[cu]] <- getCUname(cu)

# set the color options 
default.colorOpts <- c('Species', paste0(MapLabelMetrics, '.Status'), AdditionalColorThemes)



