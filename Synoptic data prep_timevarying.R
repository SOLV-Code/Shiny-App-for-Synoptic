
# Data prep file for WSP metrics plots
# Sept 27 2018
library(dplyr)

# Radar Plots

# Get Data
homedir <- getwd()

setwd("../"); setwd("Fraser/Sockeye")  

#Streams.Data <- as.tibble(read.csv("FRSK_Streams_Info.csv"))
cu.info.tmp <- read.csv("FRSK_CU_Info_working_6.csv")


metrics <- read.csv("FRSK_CU_Info_SKonly_timevarying.csv")

setwd(homedir)

#Esc.Data <- read.csv("Fraser Sockeye Escapement Data.csv")
metrics.short <- tidyr::spread(metrics, key="Metric", value="Value")

metrics.full <- merge(metrics.short, cu.info.tmp)

data.df <- metrics.full %>%   
  select(Base.Unit.CU.ShortName, Year, Base.Unit.Species, FAZ=BaseUnit.FAZ, BaseUnit.Watershed, Management.Timing =BaseUnit..ManagementTiming, Base.Unit.CU.ID, 
          Recent.Total, Lower.Ratio, Upper.Ratio, LongTerm.Ratio, ShortTerm.Trend, WSP.status, Recent.ER) %>%
 # filter(startsWith(as.character(Base.Unit.CU.ID), 'SK')) %>%
  select(-Base.Unit.CU.ID)%>%
  filter(!(is.na(ShortTerm.Trend) & is.na(LongTerm.Ratio) & is.na(Recent.Total) & is.na(Upper.Ratio) & is.na(Lower.Ratio)))  


trans <- function(x){
  x <- as.numeric(as.character(x))
  round(x, 2)
}

data.trans <- data.df %>% mutate_at(c("Recent.Total", "Lower.Ratio", "Upper.Ratio", "LongTerm.Ratio", "ShortTerm.Trend","Recent.ER"),funs(trans))
                
  
 

# Re-scale as log
data.trans$Recent.Total <- round(log(as.numeric(data.trans$Recent.Total)),2)

# Set max for STT and LTT values ************************************
data.start <- data.trans %>%
 # mutate(ShortTerm.Trend=replace(ShortTerm.Trend, ShortTerm.Trend>=3, 3)) %>%
 # mutate(LongTerm.Ratio=replace(LongTerm.Ratio, LongTerm.Ratio>=5, 5)) %>%
  arrange(Base.Unit.Species, Management.Timing) %>%
  select(-Management.Timing, Management.Timing) %>%
  filter(BaseUnit.Watershed !="") 

data.start$WSP.status <- factor(data.start$WSP.status, levels =c("UD", "R", "RA", "A", "AG", "G"), ordered=T)
data.start$Management.Timing <- factor(data.start$Management.Timing, levels =c("Estu", "Early_Summer", "Summer", "Late"), ordered=T)
