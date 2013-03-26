library(ggplot2)
# ------------------------------------------------------------------------------
# gpx statistics
# http://cnr.lwlss.net/GarminR/
# http://lwlss.net/GarminReports/GPXFunctions.R
# ------------------------------------------------------------------------------
rm(list=ls(all=TRUE)) 

setwd("~/Documents/Runs/RunR")
source("gpsPlot.R")
source("gpsStats.R")
source("nike.R")

setwd("~/Documents/Runs/RunR/Data")

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

# Retrieve list of runs from Nike+
maxRuns = 20
runs = nikeGetRunsInfo(maxRuns)
nruns = nrow(runs)

# Retrieve gps data for each run listed
localCsvFiles = list.files(pattern = "\\.csv")
runsGps = list()
for(i in seq(nruns)){
  print(paste("Processing run ", i, " of ", nruns, "...", sep=""))
  # Get basic gps data (long, lat, elev)
  runsGps[[i]] = getRunGps(runs$startTime[i])
  # Process gps to add further columns (speed etc) and calculate statistics
  runsGps[[i]] = gpsStats(runsGps[[i]])
}

# Put overall statistics into df
stats = attr(runsGps[[1]], "stats")
for(i in 2:nruns) stats = rbind(stats, attr(runsGps[[i]], "stats"))
stats$startTime = runs$startTime

save(runsGps, stats, file="AllRuns.R")

load("AllRuns.R")

# Summary
# ------------------------------------------------------------------------------
stats

farthest = which(stats$totalDistance == max(stats$totalDistance))
farthestDate = stats[farthest,]$startTime
farthestDist = stats[farthest,]$totalDistance

longest = which(stats$durationH == max(stats$durationH))
longestDate = stats[longest,]$startTime
longestDuration = stats[longest,]$duration

print(paste("Accumulated:", sum(stats$totalDistance), "km"))
print(paste("Farthest run:", farthestDist, "km. On",  farthestDate))
print(paste("Longest run:", longestDuration, ". On",  farthestDate))

# Plots: single run
# ------------------------------------------------------------------------------
run = 1
gps = runsGps[[run]]

plotSpeedElevation(gps, time=F, pace=F, legend=F, date=stats$startTime[run])

boxplot(gps$speedSmooth)

plotMap(gps, zoomFac=14, src="stamen", type="toner", col="red")

plotTrack3d(gps)

# Statistics
# ------------------------------------------------------------------------------
filterby = "30d"

if(filterby == "month"){
  year = 2013
  month = 3
  startDate = as.POSIXct(paste(year, "-", month, "-01", sep=""), "%Y-%m-%d")
  endDate = startDate + 31*24*3600
  statsFiltered = stats[stats$startTime > startDate & stats$startTime < endDate,]
} else if (filterby == "30d"){
  endDate = Sys.time()
  startDate = endDate - 31*24*3600
  statsFiltered = stats[stats$startTime > startDate & stats$startTime < endDate,]
}

par(mar=c(5,4,4,1)+0.1)
plotDistanceBars(statsFiltered)
plotDistanceBarsCal(statsFiltered)
plotPaceDistance(statsFiltered)

# Todo boxplot of speed across all runs
# boxplot(stats$speedAvg)

#barplot(statsDf$duration, names.arg=statsDf$dateTime, las=1, xlab="Run", ylab="Duration [h]")
