
# explanatory information for the different metrics
metricInfo <- list(
  Base.Unit.CU.ShortName = "Conservation Unit",
  Base.Unit.Species = "Species Code (Sk = Sockeye, Ck = Chinook, ...)",
  FAZ = "Freshwater Adaptive Zone",
  BaseUnit.Watershed = "Watershed",
  Recent.Total = "This is an explanation of the Recent.Total metric",
  Lower.Ratio = "This is an explanation of the Lower.Ratio metric",
  Upper.Ratio = "This is an explanation of the Upper.Ratio metric",
  LongTerm.Ratio = "This is an explanation of the LongTerm.Ratio metric",
  LongTerm.Trend = "This is an explanation of the LongTerm.Trend metric",
  WSP.status = "This is an explanation of the WSP.status metric",
  Recent.ER = "This is an explanation of the Recent.ER metric",
  Management.Timing = "Management timing")

# the names of the numeric metrics
numericMetrics <- names(data.start)[unlist(lapply(data.start, is.numeric))] 

# the names of the CUs
CUs <- unique(as.character(data.start[, "Base.Unit.CU.ShortName"]))


# data.start <- readxl::read_excel("data/FR SK metrics.xls")
# data.start$Lower.Ratio <- suppressWarnings(as.double(data.start$Lower.Ratio))
# data.start$Upper.Ratio <- suppressWarnings(as.double(data.start$Upper.Ratio))
# data.start$Recent.ER <- suppressWarnings(as.double(data.start$Recent.ER))
data.start <- read.csv("data/FR SK metrics.csv")
data.start$WSP.status <- factor(data.start$WSP.status, levels =c("UD", "R", "RA", "A", "AG", "G"), ordered=T)
data.start$Management.Timing <- factor(data.start$Management.Timing, levels =c("Estu", "Early_Summer", "Summer", "Late"), ordered=T)

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

