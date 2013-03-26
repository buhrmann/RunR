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
# Print statistics
# ------------------------------------------------------------------------------
printRunSummary = function(gps){
  stats = attr(gps, "stats")
  print(paste("Total distance:", stats[["totalDistance"]], "km"))
  print(paste("Duration: ", stats[["duration"]]))
  print(paste("Speed Max:", stats[["speedMax"]], "Avg:", stats[["speedAvg"]], "km/h"))
  print(paste("Pace Max: ", stats[["paceMax"]], "Avg:", stats[["paceAvg"]],  "min/km"))
  print(paste("Elevation covered: ", stats[["totalElevation"]], "m"))
}

# ------------------------------------------------------------------------------
# Calculate statistics on gps data (from long, lat, elev)
# ------------------------------------------------------------------------------
gpsStats = function(gps){
  
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
  
  totalDistance = round(max(gps$cumDist),1) # km
  duration = hoursToTimeString(gps$elapsedSec[n]/3600) #H:M:S
  durationH = gps$elapsedSec[n]/3600
  speedMax = round(max(gps$speedSmooth),1)  # km/h
  speedAvg = round(mean(gps$speedSmooth),1) # km/h
  paceMax = round(min(gps$pace),1)  # min/km
  paceAvg = round(mean(gps$pace),1) # min/km
  totalElevation = round(max(gps$elevation) - min(gps$elevation),1) # m
  
  stats = list(totalDistance=totalDistance, duration=duration, durationH=durationH, speedMax=speedMax, 
               speedAvg=speedAvg, paceMax=paceMax, paceAvg=paceAvg, totalElevation=totalElevation)
  
  attr(gps, "stats") = as.data.frame(stats)
  return(gps)
}