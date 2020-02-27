#-- Helper functions for use in SoS app ------------------

# assemble the id of a shiny input widget or a variable name from a prefix and a postfix, e.g. widget.1 
sId <- function(pre, post) {paste(pre, post, sep="_")}

# sum columns using select_if function of dplyr to remove empty columns
not.empty <- function(x){sum(!is.na(x)) > 0}

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

# get the mode (most common element) of v
getmode <- function(v) {
  names(which(table(v) == max(table(v))))[1] # if there is more than one mode, pick the first one at random
}