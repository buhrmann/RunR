# ------------------------------------------------------------------------------
# Example analysis
# ------------------------------------------------------------------------------
#rm(list=ls(all=TRUE)) 

setwd("~/Code/RunR")
source("runR.R")
source("gpsPlot.R")
setwd("~/Code/RunR/Data")

# Load runs
# ------------------------------------------------------------------------------
updateLocalStore(maxDownloads=50)

# Summary of all runs
# ------------------------------------------------------------------------------
head(stats)
print(ldply(multiRunSummary(stats), data.frame))

# Plots: single run
# ------------------------------------------------------------------------------
runId = stats$activityId[1]
run = gps[gps$activityId == runId,]
srs = singleRunSummary(stats, runId, print=T)

# n most recent runs
numPlots = 3
par(mfrow=c(numPlots, 1))
for(i in 1:numPlots){
  id = stats$activityId[i]
  plotSpeedElevation(gps[gps$activityId == id,], time=F, pace=F, legend=F, date=stats$startTime[i])
}

par(mfrow=c(1, 1))
boxplot(speedSmooth ~ activityId, data=gps, names=strftime(stats$startTime, format="%d-%m"), las=2, outline=F)

plotMap(run, zoomFac=14, src="stamen", type="toner", col="red")

plotTrack3d(run)

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

ldply(multiRunSummary(statsFiltered), data.frame)

dev.off()
par(mar=c(5,4,4,1)+0.1)
par(mfrow=c(1,3))
plotDistanceBars(statsFiltered)
plotDistanceBarsCal(statsFiltered)
plotPaceDistance(statsFiltered)
par(mfrow=c(1,1))

# plot of speed across all runs
plot(as.factor(stats$activityId), stats$speedAvg, xaxt="n", bty="n")
axis(1, at=c(1:length(stats$activityId)), labels=strftime(stats$startTime, format="%d-%m"), las=2)

# Duration across runs
barplot(stats$durationH, names.arg=strftime(stats$startTime, format="%d-%m"), las=2, xlab="Run", ylab="Duration [h]")

# Accumulated kilometers
cumDist = rev(cumsum(stats$totalDistance))
plot(stats$startTime, cumDist, type="s", bty="n")