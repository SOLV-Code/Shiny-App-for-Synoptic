#-- Helper functions for use in SoS app ------------------

# assemble the id of a shiny input widget or a variable name from a prefix and a postfix, e.g. widget.1 
sId <- function(pre, post) {paste(pre, post, sep="_")}

# sum columns using select_if function of dplyr to remove empty columns
not.empty <- function(x){sum(!is.na(x)) > 0}


# get the mode (most common element) of v
getmode <- function(v) {
  names(which(table(v) == max(table(v))))[1] # if there is more than one mode, pick the first one at random
}

# the names of the numeric metrics in a data frame
numericMetrics <- function(ds) {names(ds)[unlist(lapply(ds, is.numeric))]}

# Arrange data frame according to specified column order, 
# filtering out any columns that are specified in 'hide'
# If no order is given, just removes the columns specified as hidden
arrangeColumns <- function(ds, colOrder=NULL, hide=NULL) {
  if (is.null(colOrder)) {
    colOrder <- names(ds)
  } else {
    colOrder <- colOrder[colOrder %in% names(ds)] # get rid of any columns that aren't present in the original data
  }
  ds[, colOrder[!(colOrder %in% hide)], drop=F]
}

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

# strip Pop ID from Pop UID, leaving the CU ID
get_CU_ID_From_Pop_UID <- function(UID) {strsplit(UID, '[.]')[[1]][1]}

# get full name of a CU, given the CU's ID
getCUname <- function(CU_ID) {
  paste0(CU_ID, ': ', data.CU.Lookup[data.CU.Lookup$CU_ID == CU_ID, 'CU_Name'][1])
  #  paste0(data.CU.Lookup[data.CU.Lookup$CU_ID == CU_ID, 'CU_Name'][1], ' (', CU_ID, ')')}
}

# get full name of a population, given the population Pop_UID
getPopName <- function(Pop_UID) {paste0(data.Pop.Lookup[Pop_UID, 'CU_ID'], ': ', data.Pop.Lookup[Pop_UID, 'Pop_Name'], ' (', data.Pop.Lookup[Pop_UID, 'Pop_ID'], ')')}

# get name of a population, given the population Pop_UID
getPopNameShort <- function(Pop_UID) {data.Pop.Lookup[Pop_UID, 'Pop_Name']}

# get list of populations associated with the given CUs
getPopsForCUs <- function(CUs) {data.Pop.Lookup[data.Pop.Lookup$CU_ID %in% CUs, 'Pop_UID']}

