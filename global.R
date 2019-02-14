
# explanatory information for the different metrics
metricInfo <<- list(
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



#data.start <- readxl::read_excel("data/FR SK metrics.xls")
data.start <- read.csv("data/FR SK metrics.csv")

# the names of the numeric metrics
numericMetrics <- names(data.start)[unlist(lapply(data.start, is.numeric))] 

# the names of the CUs
CUs <- unique(as.character(data.start[, "Base.Unit.CU.ShortName"]))
