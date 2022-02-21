f_UNIXtoDate <- function(UNIX, tZone){
  # this function aims to convert UNIX to date 
  # 
  curDate <- as.character(Sys.Date()) 
  curUNIX <- as.numeric(as.POSIXct(curDate))
  if (min(UNIX) > curUNIX * 1.5){
    UNIX <- UNIX / 1000
  }
  out <- as.POSIXct(UNIX, origin="1970-01-01", tz=tZone)
  return (out)
}
