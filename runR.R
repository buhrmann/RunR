library(ggplot2)
# ------------------------------------------------------------------------------
# gpx statistics
# http://cnr.lwlss.net/GarminR/
# http://lwlss.net/GarminReports/GPXFunctions.R
# ------------------------------------------------------------------------------
rm(list=ls(all=TRUE)) 

setwd("~/Code/RunR")
source("gpsPlot.R")
source("gpsStats.R")
source("nike.R")

setwd("~/Code/RunR/Data")

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

mrs = multiRunSummary(stats)
print(mrs)

# Plots: single run
# ------------------------------------------------------------------------------
runId = 1
gps = runsGps[[runId]]

numPlots = 3
par(mfrow=c(numPlots, 1))
for(i in 1:3)
  plotSpeedElevation(runsGps[[i]], time=F, pace=F, legend=F, date=stats$startTime[i])
par(mfrow=c(1, 1))

boxplot(gps$speedSmooth)

plotMap(gps, zoomFac=14, src="stamen", type="toner", col="red")

plotTrack3d(gps)

# Statistics
# ------------------------------------------------------------------------------
filterby = "recent"

if(filterby == "month"){
  year = 2013
  month = 3
  statsFiltered = filterByMonth(stats, year, month)
} else if (filterby == "recent"){
  days = 31
  statsFiltered = filterByRecent(stats, days)
}

multiRunSummary(statsFiltered)

dev.off()
par(mar=c(5,4,4,1)+0.1)
plotDistanceBars(statsFiltered)
plotDistanceBarsCal(statsFiltered)
plotPaceDistance(statsFiltered)

# Todo plot of speed across all runs

# Todo boxplot of speed across all runs
# boxplot(stats$speedAvg)

#barplot(statsDf$duration, names.arg=statsDf$dateTime, las=1, xlab="Run", ylab="Duration [h]")
