
# explanatory information for the different metrics
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


data.start <- read.csv("data/FR SK metrics.csv")
data.start$WSP.status <- factor(data.start$WSP.status, levels =c("UD", "R", "RA", "A", "AG", "G"), ordered=T)
data.start$Management.Timing <- factor(data.start$Management.Timing, levels =c("Estu", "Early_Summer", "Summer", "Late"), ordered=T)

# the names of the numeric metrics
numericMetrics <- names(data.start)[unlist(lapply(data.start, is.numeric))] 

# the names of the CUs
CUs <- unique(as.character(data.start[, "Base.Unit.CU.ShortName"]))

data.latlong <- read.csv("data/FRSK_CU_Info_masterUpdate.csv")
data.latlong <- unique(data.latlong[ ,c("Base.Unit.CU.ShortName", "Base.Unit.CU.Lat", "Base.Unit.CU.Long")])
names(data.latlong) <- c("CU", "lat", "long")
data.latlong$CU <- as.character(data.latlong$CU)
# Hack alert!! Some overlap here in Base.Unit.CU.ShortName for Chinook and Sockeye
# For now, make this go away by removing duplicates. Ultimately, lat-long info should be attached
# in a pre-processing script.
data.latlong <- data.latlong[!duplicated(data.latlong$CU), ]
row.names(data.latlong) <- data.latlong$CU

# pass through a CU metrics table and return with lat-long columns attached
# if CUnames is given, looks for CU names is CUnames column
# otherwise assumes that the ds row names are the CU names 
withLatLong <- function(ds, CUnames = NULL) {
  if (is.null(CUnames)) {
    return(cbind(ds, data.latlong[row.names(ds), c("lat", "long")]))
  } else {
    return(cbind(ds, data.latlong[ds[, CUnames], ]))
  }
}

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

