# ------------------------------------------------------------------------------
# Distance between two coordinates
# ------------------------------------------------------------------------------
earthRad = 6371
haverDist = function(aLong,aLat,bLong,bLat){
  dLat = pi * (bLat-aLat) / 180.0; 
  dLon = pi * (bLong-aLong) / 180.0;
  a = (sin(dLat/2))^2 + cos(pi*aLat/180) * cos(pi*bLat/180) * (sin(dLon/2)^2)
  return(earthRad * 2 * atan2(sqrt(a), sqrt(1-a)))
}

# ------------------------------------------------------------------------------
# Convert decimal hours to time format (can then be converted to Posix e.g.)
# ------------------------------------------------------------------------------
hoursToTimeString = function(hours){
  # start by converting to seconds
  seconds = hours * 3600
  # we're given hours, so let's get those the easy way
  hours = floor(hours)
  # since we've "calculated" hours, let's remove them from the seconds variable
  seconds = seconds - hours * 3600
  # calculate minutes left
  minutes = floor(seconds / 60)
  # remove those from seconds as well
  seconds = floor(seconds - minutes * 60)
  # return the time formatted HH:MM:SS
  time = paste(hours, minutes, seconds, sep=":")
  return(time)
}

# ------------------------------------------------------------------------------
# For printing statistics of a single run
# ------------------------------------------------------------------------------
singleRunSummary = function(statsDf, id, print=F){
  stats = statsDf[statsDf$activityId==id,]
  res = list()
  res[1] = paste(stats$totalDistance, "km", "in", stats$duration)
  res[2] = paste("Speed: max = ", stats$speedMax, " km/h, ", " avg = ", stats$speedAvg, " km/h", sep="")
  res[3] = paste("Pace: max = ", stats$paceMax, " min/km, ", " avg = ", stats$paceAvg, " min/km", sep="")
  res[4] = paste("Elevation range:", stats$totalElevation, "m")
  
  if(print) cat(sapply(res, function(x) x), sep="\n")
  return(res)
}

# ------------------------------------------------------------------------------
# Collates statistics across multiple runs into data frame for easy conversion
# to markdown table
# ------------------------------------------------------------------------------
multiRunSummary = function(stats){
  farthest = which(stats$totalDistance == max(stats$totalDistance))
  farthestDate = stats[farthest,]$startTime
  farthestDist = stats[farthest,]$totalDistance
  
  longest = which(stats$durationH == max(stats$durationH))
  longestDate = stats[longest,]$startTime
  longestDuration = stats[longest,]$durationH
  
  res = list()
  res[["Number of runs"]] = list(Value=nrow(stats), Units="", Date="")
  res[["Total distance"]] = list(Value=sum(stats$totalDistance), Units="km", Date="")
  res[["Average distance"]] = list(Value=mean(stats$totalDistance), Units="km", Date="")
  res[["Average speed"]] = list(Value=mean(stats$speedAvg), Units="km/h", Date="")
  res[["Average pace"]] = list(Value=mean(stats$paceAvg), Units="min/km", Date="")
  res[["Farthest distance"]] = list(Value=farthestDist, Units="km", Date=strftime(farthestDate))
  res[["Longest run"]] = list(Value=longestDuration, Units="", Date=strftime(farthestDate))
  return(res)
}

# ------------------------------------------------------------------------------
# Filter summary statistics
# ------------------------------------------------------------------------------
filterByMonth = function(stats, year, month){
  startDate = as.POSIXct(paste(year, "-", month, "-01", sep=""), "%Y-%m-%d")
  endDate = startDate + 31*24*3600
  statsFiltered = stats[stats$startTime > startDate & stats$startTime < endDate,]
  return(statsFiltered)
}

# ------------------------------------------------------------------------------
filterByRecent = function(stats, days){
  endDate = Sys.time()
  startDate = endDate - days*24*3600
  statsFiltered = stats[stats$startTime > startDate & stats$startTime < endDate,]
  return(statsFiltered)
}

# ------------------------------------------------------------------------------
# Add distance and speed etc..  (from long, lat, elev)
# ------------------------------------------------------------------------------
processGps = function(gps){
  
  message("Processing...")
  
  # Remove missing data
  missing = gps==""
  gps[missing] = NA
  gps = na.omit(gps)
  n = nrow(gps)
  
  # Add time (seconds): Nike returns 10s intervals only
  gps$elapsedSec = seq(from=0, by=1, length.out=n)
  
  # Add distance (km)
  gps$dist2d = rep(0,n)
  gps$gradient = rep(0,n)
  gps$dist3d = rep(0,n)
  for(i in 2:nrow(gps)){
    gps$dist2d[i] = haverDist(gps$longitude[i-1], gps$latitude[i-1], gps$longitude[i], gps$latitude[i]) # km
    gps$gradient[i] = (gps$elevation[i] - gps$elevation[i-1]) / 1000 # m to km
    gps$dist3d[i] = sqrt(gps$dist2d[i]^2 + gps$gradient[i]^2)
  }
  
  gps$dist2d[1] = 0
  gps$dist3d[1] = 0
  gps$gradient[1] = 0
  gps$cumDist = cumsum(gps$dist2d)
  gps$cumDist3d = cumsum(gps$dist3d)
  
  # Todo: take into account elevation difference in distance calculations
  # Todo: calculate gradient to correlate speed with gradient
  
  # Add speed
  gps$speed = rep(0,n)
  for(i in 2:n){
    dd = gps$cumDist[i] - gps$cumDist[i-1]
    dt = (gps$elapsedSec[i] - gps$elapsedSec[i-1]) / 3600 # in hours
    gps$speed[i] = dd / dt # km/h
  }
  gps$speed[1] = gps$speed[2]
  gps$speed = smooth(gps$speed)
  
  # Still to jerky...
  los = loess(speed ~ elapsedSec, gps, span=0.15)
  gps$speedSmooth = los$fitted
  gps$pace = 1 / (gps$speedSmooth / 60) # min/km
  
  return(gps)
}

# ------------------------------------------------------------------------------
# Calculate basic statistics. Assumes gps is a df for only a single run
# ------------------------------------------------------------------------------
gpsStats = function(gps){
  n = nrow(gps)
  totalDistance = round(max(gps$cumDist),1) # km
  duration = hoursToTimeString(gps$elapsedSec[n]/3600) #H:M:S
  durationH = gps$elapsedSec[n]/3600
  speedMax = round(max(gps$speedSmooth),1)  # km/h
  speedAvg = round(mean(gps$speedSmooth),1) # km/h
  paceMax = round(min(gps$pace),1)  # min/km
  paceAvg = round(mean(gps$pace),1) # min/km
  totalElevation = round(max(gps$elevation) - min(gps$elevation),1) # m
  
  stats = data.frame(startTime=gps$startTime[1], activityId=gps$activityId[[1]], totalDistance=totalDistance, duration=duration, durationH=durationH, speedMax=speedMax, 
               speedAvg=speedAvg, paceMax=paceMax, paceAvg=paceAvg, totalElevation=totalElevation)
  
  return(stats)
}

# ------------------------------------------------------------------------------
# Takes a df of gps data and returns a new df of basic per-run statistics
# ------------------------------------------------------------------------------
allStats = function(gps) {
  ids = unique(gps$activityId)
  stats = gpsStats(gps[gps$activityId == ids[[1]], ])
  for (id in ids[2:length(ids)]){
    stats = rbind(stats, gpsStats(gps[gps$activityId == id, ]))
  }
  return(stats)
}