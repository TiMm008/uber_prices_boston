#---------------------------------------
# Main function

## clear workspace
rm(list = ls())

#---------------------------------------
# import libraries
packages = c('stringr','readr', 'dplyr', 'plyr', 'sqldf', 'hms', 'lubridate', 
             'data.table', 'units', 'purrr', 'ggpubr', 'psych', 'ggplot2')
package.check <- lapply(
  packages,
  FUN = function(x){
    if(!require(x, character.only = TRUE)){
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE, quietly = T)
    }
  }
)

#---------------------------------------
# call defined functions
source("f_UNIXtoDate.R")
source("f_cleanMerge.R")
source("f_analyse.R")

#--------------------------------------
# input all raw data as data.table
weather  <- fread("weather.csv")
uber     <- fread("cab_rides.csv")
rushhour <- fread("rush_hours.csv")
#---------------------------------------
# function_pre: import raw files
MergeFile <- f_cleanMerge(uber, weather, rushhour)

#--------------------------------------
# call function_analyse:
out_analyse <- f_analyse(MergeFile)

#--------------------------------------
# call plot function
f_plot(out_analyse, MergeFile)



