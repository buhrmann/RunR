# ------------------------------------------------------------------------------
# Gpx to gps csv file
# ------------------------------------------------------------------------------
gpxToCsv = function(fnmIn, fnmOut){
  command = paste("gpsbabel -t -i gpx -f ", fnmIn, " -o unicsv -F", fnmOut)
  system(command)
}

gpsFromCsv = function(fnmIn){
  fnmOut = gsub("gpx", "csv", fnmIn)
  gpxToCsv(fnmIn, fnmOut)  
  gps = read.csv(fnmOut)
  processGps(gps)
}

# ------------------------------------------------------------------------------
# Build and return DF from gpx file
# ------------------------------------------------------------------------------
processGps = function(gps){
  
  # Remove missing data
  missing = gps==""
  gps[missing] = NA
  gps = na.omit(gps)
  n = nrow(gps)
  
  # Add time (seconds)
  gps$posixTime = strptime(gps$Time,"%H:%M:%S")
  gps$elapsedSec = as.numeric(gps$posixTime - gps$posixTime[1])
  
  # Add distance (km)
  gps$dist2d = rep(0,n)
  for(i in 2:nrow(gps)){
    d = haverDist(gps$Longitude[i-1], gps$Latitude[i-1], gps$Longitude[i], gps$Latitude[i])
    gps$dist2d[i] = d
  }
  
  gps$dist2d[1] = 0
  gps$cumDist = cumsum(gps$dist2d)
  totalDist = sum(gps$dist2d)
  
  # Todo: take into account elevation difference in distance calculations
  # Todo: calculate gradient to correlate speed with gradient
  
  # Add speed
  gps$speed = rep(0,n)
  for(i in 2:(n-1)){
    dd = gps$cumDist[i+1] - gps$cumDist[i-1]
    dt = (gps$elapsedSec[i+1] - gps$elapsedSec[i-1]) / 3600 # in hours
    gps$speed[i] = dd/dt # km/h
  }
  gps$speed[1] = gps$speed[2]
  gps$speed[n] = gps$speed[n-1]
  gps$speed = smooth(gps$speed)
  
  # Still to jerky...
  los = loess(speed ~ elapsedSec, gps, span=0.15)
  gps$speedSmooth = los$fitted
  gps$pace = 1 / (gps$speedSmooth / 60) # min/km
  
  totalDistance = round(max(gps$cumDist),1) # km
  duration = hoursToTimeString(gps$elapsedSec[n]/3600) #H:M:S
  speedMax = round(max(gps$speedSmooth),1)  # km/h
  speedAvg = round(mean(gps$speedSmooth),1) # km/h
  paceMax = round(min(gps$pace),1)  # min/km
  paceAvg = round(mean(gps$pace),1) # min/km
  totalElevation = round(max(gps$Altitude) - min(gps$Altitude),1) # m
  
  # Date and time of run
  dateTime = as.POSIXct(strptime(paste(gps$Date[1], gps$Time[1]), "%Y/%m/%d %H:%M:%S"))
  
  stats = list(dateTime=dateTime, totalDistance=totalDistance, duration=duration, speedMax=speedMax, 
            speedAvg=speedAvg, paceMax=paceMax, paceAvg=paceAvg, totalElevation=totalElevation)
  
  attr(gps, "stats") = as.data.frame(stats)
  #attr(gps, "dateTime") = dateTime
  
  return(gps)
}

# load all
# ------------------------------------------------------------------------------
loadGpx = function(){
  
  dfs = list()
  files = list.files(pattern = "\\.gpx")
  numFiles = length(files)
  
  # Collect overall stats in new df
  dfs[[1]] = gpsFromCsv(files[1])
  stats = attr(dfs[[1]], "stats")
  
  for(i in 2:numFiles)
  {
    dfs[[i]] = gpxToDf(files[i])
    statsDf = rbind(statsDf, attr(dfs[[i]], "stats"))
    print(paste("Converted file", files[i]))
  }
  
}

  