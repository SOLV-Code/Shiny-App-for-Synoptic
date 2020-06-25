
# ----- explanatory information for the different metrics ---------

MetricInfo <- list(
  Base.Unit.CU.ShortName = "Conservation Unit",
  Base.Unit.Species = "Species Code (Sk = Sockeye, Ck = Chinook, ...)",
  FAZ = "Freshwater Adaptive Zone",
  BaseUnit.Watershed = "Watershed",
  Recent.Total = "Recent Generation Total: average total spawner abundance across the last generation (log space)",
  Lower.Ratio = "Lower Abundance BM Ratio: ratio of the most recent generation of total spawners to the lower WSP benchmark for abundance. 
                  Values <1 indicates that the recent abundance falls below this biological benchmark.",
  Upper.Ratio = "Upper Abundance BM Ratio: ratio of the most recent generation of total spawners to the upper WSP benchmark for abundance.
                  Values <1 indicates that the recent abundance falls below this biological benchmark.)",
  LongTerm.Ratio = "Ratio of Historic Average: ratio of the most recent generation of spawners to the long-term average (geometric)
                    Values <1 indicate that the recent abundance is below average.",
  ShortTerm.Trend = "Short Term Trend: % change over the most recent three generations of spawners (using a geometric generational running average)",
  WSP.status = "Wild Salmon Policy status: the integrated Wild Salmon Policy Status derived by integrating metrics through an expert-driven workshop 
                  (UD = Undetermined, R = Red, RA = Red/Amber, A = Amber, AG = Amber/Green, G = Green)",
  Recent.ER = "Recent Exploitation Rate: the average exploitation rate over the most recent generation",
  Management.Timing = "Stock management unit: aggregation of CUs used for fisheries management purposes")

# add labels here for any names or categories that should be shown with pretty labels
Labels <- list(Base.Unit.CU.ShortName = "CU",
               Base.Unit.Species = "Species",
               Sk = "Sockeye",
               Ck = "Chinook",
               FAZ="Freshwater Adaptive Zone",
               BaseUnit.Watershed = "Watershed",
               Management.Timing = "Stock Management Unit",
               WSP.status = "Wild Salmon Policy Status",
               Recent.Total = "Recent Generation Total (log)", 
               Recent.ER = "Recent Exploitation Rate (%)",
               Lower.Ratio = "Lower Abundance BM Ratio",
               Upper.Ratio = "Upper Abundance BM Ratio",
               LongTerm.Ratio = "Ratio of Historic Average",
               ShortTerm.Trend = "Short-term Trend (% change)",
               Estu="Early Stuart", 
               Early_Summer="Early Summer", 
               Summer="Summer", 
               Late="Late")
  
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

# ------------------- put together initial data set -------------------
# Hack alert!! Get the data from two different files. The first one covers
# only Fraser sockeye and has a complete list of metrics, but no lat-long information.
# The second one covers a much wider range of species/CUs and provides the lat-long information.
# Cross-referencing these by Base.Unit.CU.ShortName is tricky, since there is overlap in CU names 
# for Chinook and sockeye.
# For now, make this problem go away by removing duplicates in the lat-long data. 
# Ultimately, lat-long info should be attached in a pre-processing script, though.
data.start <- read.csv("data/FR SK metrics.csv")
data.start$WSP.status <- factor(data.start$WSP.status, levels =c("UD", "R", "RA", "A", "AG", "G"), ordered=T)
data.start$Management.Timing <- factor(data.start$Management.Timing, levels =c("Estu", "Early_Summer", "Summer", "Late"), ordered=T)
data.start$ShortTerm.Trend <- data.start$ShortTerm.Trend*100
data.start$Recent.ER <- data.start$Recent.ER*100

data.years <- as.character(sort(unique(as.numeric(data.start$Year))))
data.by.year <- lapply(data.years, function(yr) {
  ds <- data.start[data.start$Year == yr, ]
  row.names(ds) <- ds[, "Base.Unit.CU.ShortName"]
  ds
})
names(data.by.year) <- data.years

# Lookup table for joining metrics and spatial information
data.spatialLookup <- read.csv("data/SpatialLookup.csv", stringsAsFactors = F)
row.names(data.spatialLookup) <- data.spatialLookup$CU_INDEX

# CU polygon data
require(rgdal)
data.CUpolygons <- readOGR(dsn="data/CUpolygons.gpkg", layer="CUpolygons", stringsAsFactors=F, verbose=F)
#data.CUpolygons <- spTransform(data.CUpolygons, CRS("+proj=longlat +datum=WGS84"))

# stream data
data.streams <- readOGR(dsn="data/Streams.gpkg", stringsAsFactors=F, verbose=F)
data.getSelectableCUs <- function(CUs) {
  CUs <- strsplit(CUs, ':')[[1]]
  CUs <- CUs[CUs %in% row.names(data.spatialLookup)]
  out <- data.spatialLookup[CUs, "Base.Unit.CU.ShortName"]
  out <- out[out != ""]
  if (length(out) > 0) {
    paste(out, collapse=",")
  } else {
    ""
  } 
}

# prune the stream network to remove any streams that don't potentially select CUs in the current CU database
data.streams$CUsSelectable <- unlist(lapply(data.streams$CUs, data.getSelectableCUs))
data.streams <- data.streams[data.streams$CUsSelectable != "", ]
data.streams <- data.streams[order(data.streams$Shape_Length, decreasing=T), ]

# --------------------- Helper functions for data restructuring ---------

# pass through a CU metrics table and add a 'labels' column
# CUlabels should be a named vector that specified the label for each CU
# if CUlabels is given, uses the labels from CUlabels
# otherwise assumes that the ds row names are to be used as the labels 
WithLabels <- function(ds, CUnames = NULL) {
  if (is.null(CUnames)) {
    ds$labels <- row.names(ds)
  } else {
    ds$labels <- CUlabels[row.names(ds)]
  }
  return(ds)
}

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
FilterAttributes <- c("Base.Unit.Species", "FAZ", "BaseUnit.Watershed", "Management.Timing")
# allow only a single choice for these attributes:
FilterSingleChoiceAttributes <- c("Base.Unit.Species")

# metric selector customization
# the names of the metrics users may choose from
FilterMFMetrics <- c("WSP.status", "WSP.numeric", "Recent.Total", "Recent.ER",
                     "Lower.Ratio","Upper.Ratio","LongTerm.Ratio",
                     "ShortTerm.Trend")

# the names of the attributes users may choose from
FilterMFAttributes <- c("FAZ", "BaseUnit.Watershed", "Management.Timing")

# attributes for which it doesn't make sense to let the user select whether they should be shown
FilterMFhiddenAttributes <- c("Base.Unit.CU.ShortName", "Base.Unit.Species")

# ---------------- Data Selector UI ------------------

SelectAttributes <- c('Base.Unit.CU.ShortName',
                   'BaseUnit.Watershed',
                   'FAZ',
                   'Management.Timing',
                   'WSP.status')

# ---------------- Parcoords UI ------------------
# show the following axes in parcoords, in the order specified here
ParcoordsMetricOrder <- c("Recent.Total", "Recent.ER",
                          "Lower.Ratio","Upper.Ratio","LongTerm.Ratio",
                          "ShortTerm.Trend",
                          "WSP.status", "WSP.numeric", "Management.Timing", "FAZ") 

# rotation of axis labels for metric axes (in degrees from horizontal)
ParcoordsLabelRotation <- -15 

# ---------------- Historgram Summaries UI ------------------
# histogram summaries will be generated for these metrics/attributes in the order specified
HistoSummaryAttribs <- c("Management.Timing", "FAZ", "WSP.status", "Recent.ER")

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
RadarMetricOpts <- c("ShortTerm.Trend", "Recent.Total", "Lower.Ratio", "Upper.Ratio", "LongTerm.Ratio")


# -------------------- Map UI ______________________

# labels for levels of individual data columns
MapLevelLabels <- list(
  Management.Timing = list(Estu="Fraser Sockeye Early Stuart", 
                           Early_Summer="Fraser Sockeye Early Summer", 
                           Summer="Fraser Sockeye Summer", 
                           Late="Fraser Sockeye Late"),
  Base.Unit.Species = list(SK = "Sockeye", CK = "Chinook")
)

# the metrics to include in the map labels (i.e., the popups shown on hover)
MapLabelMetrics <-  c("ShortTerm.Trend", "Recent.Total", "Lower.Ratio", "Upper.Ratio", "LongTerm.Ratio")
