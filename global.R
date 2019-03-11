
# ----- explanatory information for the different metrics ---------

metricInfo <- list(
  Base.Unit.CU.ShortName = "Conservation Unit",
  Base.Unit.Species = "Species Code (Sk = Sockeye, Ck = Chinook, ...)",
  FAZ = "Freshwater Adaptive Zone",
  BaseUnit.Watershed = "Watershed",
  Recent.Total = "Recent.Total: average effective total spawner abundance across the last generation (log space)",
  Lower.Ratio = "Lower.Ratio: ratio of the most recent generation of effective total spawners (ETS) to the lower WSP benchmark for abundance. 
                  Values <1 indicates that the recent abundance falls below this biological benchmark.",
  Upper.Ratio = "Upper.Ratio: ratio of the most recent generation of effective total spawners (ETS) to the upper WSP benchmark for abundance.
                  Values <1 indicates that the recent abundance falls below this biological benchmark.)",
  LongTerm.Ratio = "LongTerm.Ratio: ratio of the most recent generation of effective female spawners to the historical average (geometric)
                    Values <1 indicate that the recent abundnace is below average.",
  ShortTerm.Trend = "ShortTerm.Trend: slope of the most recent three generations of effective female spawners (using a geometric generational running average)",
  WSP.status = "WSP.status: the integrated Wild Salmon Policy Status derived by integrating metrics through an expert-driven workshop 
                  (UD = Undetermined, R = Red, RA = Red/Amber, A = Amber, AG = Amber/Green, G = Green)",
  Recent.ER = "Recent.ER: the average exploitation rate over the most recent generation",
  Management.Timing = "Management timing: return timing of the spawning migration used for fisheries management purposes")

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

data.years <- as.character(sort(unique(as.numeric(data.start$Year))))
data.by.year <- lapply(data.years, function(yr) {
  ds <- data.start[data.start$Year == yr, ]
  row.names(ds) <- ds[, "Base.Unit.CU.ShortName"]
  ds
})
names(data.by.year) <- data.years

# Lookup table for joining metrics and spatial information
data.spatialLookup <- read.csv("data/SpatialLookup.csv", stringsAsFactors = F)

# CU polygon data
require(rgdal)
data.CUpolygons <- readOGR(dsn="data/Lake_Type_Sockeye_Salmon_CU_Shape/Lake_Type_Sockeye_Salmon_CU_Boundary",
                      layer="Lake_Type_Sockeye_Salmon_CU_Boundary_En", stringsAsFactors=F, verbose=F)
data.CUpolygons <- spTransform(data.CUpolygons, CRS("+proj=longlat +datum=WGS84"))

# --------------------- Helper functions for data restructuring ---------

# pass through a CU metrics table and add a 'labels' column
# CUlabels should be a named vector that specified the label for each CU
# if CUlabels is given, uses the labels from CUlabels
# otherwise assumes that the ds row names are to be used as the labels 
withLabels <- function(ds, CUnames = NULL) {
  if (is.null(CUnames)) {
    ds$labels <- row.names(ds)
  } else {
    ds$labels <- CUlabels[row.names(ds)]
  }
  return(ds)
}

# ------------ Data filtering --------------

# the names of the metrics users may choose from
CUMetrics <- list("WSP Status"="WSP.status",
                    "Recent Total"="Recent.Total", 
                    "Recent ER"="Recent.ER",
                    "Lower Ratio"="Lower.Ratio",
                    "Upper Ratio"="Upper.Ratio",
                    "Long-term Ratio"="LongTerm.Ratio",
                    "Short-term Trend"="ShortTerm.Trend")

# the names of the attributes users may choose from
CUAttributes <- list("FAZ"="FAZ",
                     "Watershed"="BaseUnit.Watershed",
                     "Management Timing"="Management.Timing")

# attributes for which it doesn't make sense to let the user select whether they should be shown
hiddenAttributes <- list("CU"="Base.Unit.CU.ShortName",
                         "Species"="Base.Unit.Species")


# ------------------------ UI customization -----------------

# show the following axes in parcoords, in the order specified here
metricOrderParcoords <- c(as.character(CUMetrics), "Management.Timing", "FAZ") 

# histogram summaries will be generated for these metrics/attributes in the order specified
histoSummaryAttribs <- c("Management.Timing", "FAZ", "WSP.status", "Recent.ER")

# this list specifies the information necessary to construct a histogram from a numeric metric 
customHistogramInfo <- list(
  Annual = list( 
    Recent.ER = list(
      breaks = c( 0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
      names = c("Below 10%","10-20%","20-30%","30%-40%","40-50%", "50%-60%","60-70%","70%-80%","80-90%","Above 90%")
      )
    ),
  Change = list(
    Recent.ER = list(
      breaks = c(-1, -0.1, -0.05, -0.01,0.01, 0.05, 0.1, 1),
      names = c(">10% decrease", "5%-10% decrease", "0-5% decrease","No Change", "0-5% increase", "5-10% increase",">10 increase")
    )
  ))

# the metrics offered as choices for the radar plot
radarMetricOpts <- c("Short Term Trend" = "ShortTerm.Trend",  
                     "Recent Total" = "Recent.Total", 
                     "Lower Ratio" = "Lower.Ratio", 
                     "Upper Ratio" = "Upper.Ratio",
                     "Long-term Ratio"="LongTerm.Ratio")


# get the label for pretty printing, given the name of a metric
getLabel <- function(m) {
  colLabels <- c(CUMetrics, CUAttributes, hiddenAttributes)
  if (m %in% colLabels) {
    names(colLabels)[which(colLabels == m)]
  } else
  {
    m
  }
}

# labels for levels of individual data columns
levelLabels <- list(
  Management.Timing = list(Estu="Fraser Sockeye Early Stuart", 
                           Early_Summer="Fraser Sockeye Early Summer", 
                           Summer="Fraser Sockeye Summer", 
                           Late="Fraser Sockeye Late"),
  Base.Unit.Species = list(SK = "Sockeye", CK = "Chinook")
)

# the metrics to include in the map labels
mapLabelMetrics <-  c("Short Term Trend" = "ShortTerm.Trend",  
                   "Recent Total" = "Recent.Total", 
                   "Lower Ratio" = "Lower.Ratio", 
                   "Upper Ratio" = "Upper.Ratio",
                   "Long-term Ratio"="LongTerm.Ratio")
