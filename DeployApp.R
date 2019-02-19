
deploy.packages <- c("rsconnect",
                     "openssl",
                     "clipr",
                     "curl",
                     "git2r")

new.packages <- deploy.packages[!(deploy.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(deploy.packages, require, character.only = TRUE)

rsconnect::deployApp(appTitle="SOS SST V2.3_git")

rsconnect::showLogs()

#library(rsconnect)
#rsconnect::deployApp('C:/Users/macdonaldbro/OneDrive/State of Salmon Shared/Data/Shiny Code/Deployed')