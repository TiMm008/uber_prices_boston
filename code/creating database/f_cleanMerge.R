f_cleanMerge <- function(uber, weather, rushhour){

  weather$rain[is.na(weather$rain)] <- 0
  uber           <-na.omit(uber)
  
  uber$n_time    <- f_UNIXtoDate(uber$time_stamp,    'America/New_York')
  weather$n_time <- f_UNIXtoDate(weather$time_stamp, 'America/New_York')
  
  uber           <-uber%>%
    select(-id, -product_id,-time_stamp)%>%
    filter(cab_type=="Uber")%>%mutate(ymdh=substr(n_time,1,15), merge=paste(destination, ymdh))

#------------------------------------------------------------------------------
  # prepare weather file for merge
  weather        <-weather%>%
    select(-time_stamp)%>%
    mutate(ymdh=substr(n_time,1,15), merge=paste(location, ymdh))
 
#-------------------------
weather[,c("n_avg_temp","n_avg_clouds","n_avg_pressure","n_avg_rain", "n_avg_humidity", "n_avg_wind")
        := lapply(.SD, mean), by = c("merge"),
        .SDcols=c('temp','clouds','pressure','rain','humidity','wind')]
weather <- weather[!duplicated(weather[,"merge"])]
weather <- weather%>%select(-temp, -clouds, - pressure, -rain, -humidity, -wind)

colold <- c('temp','clouds','pressure','rain','humidity','wind')
colnew <- c("n_avg_temp","n_avg_clouds","n_avg_pressure","n_avg_rain", "n_avg_humidity", "n_avg_wind")
i <- 1
for (i in 1: length(colnew)) {
  names(weather)[ names(weather) == colnew[i]] <- colold[i]
}

# weather <- weather%>%select(-n_avg_temp, -n_avg_clouds, - n_avg_pressure, -n_avg_rain, -n_avg_humidity, -n_avg_wind)
#-------------------------
  unique(uber$surge_multiplier)
  
  uber_weather   <-merge(uber, weather, by="merge", all=T, no.dups=TRUE)
  
  uber_weather   <-uber_weather%>%
    select(-merge, -ymdh.x, -ymdh.y, -location, -n_time.y)%>%
    mutate(n_days = weekdays(as.Date(n_time.x)))
  
  names(uber_weather)[names(uber_weather) == "n_time.x"] <-'n_pick_up_time'
#------------------------------------------------------------------------------
  # Prepare uber_weather file for second merge
  n_tform            <- format(uber_weather$n_pick_up_time, format = "%H:%M:%S")
  n_hour             <- substr(n_tform, 1, 2)
  n_min              <- substr(n_tform, 4, 5)
  n_min              <- as.numeric(n_min)
  n_hour             <- as.numeric(n_min > 30) + as.numeric(n_hour)
  n_hour[n_hour== 24]<- 0
  uber_weather$key   <- paste(n_hour,uber_weather$n_days,sep = "")
  uber_weather$n_hour<- n_hour
  
# prepare key name for rushhour after unlist function
  rownames(rushhour) <- rushhour$hours
  rushhourClean      <- select(rushhour, -c(1))
  x                  <- colnames(rushhourClean)
  y                  <- rownames(rushhourClean)
  nameRush           <- apply(expand.grid(y,x), 1, function(x) paste0(x, collapse=""))

  temp               <- unlist(rushhourClean, use.names = TRUE)
  names(temp)        <- nameRush
  rushhourTemp       <- data.frame(key = names(temp), traffic=temp)
  
# Merge rushour file by
  MergeFile          <- merge(uber_weather, rushhourTemp , by="key", all=T)
  
  MergeFile          <- MergeFile%>%select(-key)

# check unique and remove NA for distance
  # remove the column na
  MergeFile <- MergeFile[!is.na(MergeFile$distance),]
  MergeFile <- unique(MergeFile)

#------------------------------------------------------------------------------
  # filter observation by more criteria
  # price_min <- 2
  # price_max <- 100
#------------------------------------------------------------------------------
  # set factors
  
# check merge file with sqldf  
#------------------------------------------------------------------------------
  
  return (MergeFile)
}
