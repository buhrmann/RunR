# ------------------------------------------------------------------------------
# Visualizations of gps data
# ------------------------------------------------------------------------------
library(ggmap)
library(scatterplot3d)

# ------------------------------------------------------------------------------
# Round to nearest (bigger) even number
# ------------------------------------------------------------------------------
roundEvenUp = function(x){
  r = ceiling(x)
  r = ifelse(r%%2==0, r, r+1)
  return(r)
}

roundEvenDown = function(x){
  r = floor(x)
  r = ifelse(r%%2==0, r, r-1)
  return(r)
}

# ------------------------------------------------------------------------------
# Plot speed (or pace) and elevation against distance (or time)
# ------------------------------------------------------------------------------
plotSpeedElevation = function(gps, time=F, pace=F, legend=F, date=Sys.time()){
  
  par(mar=c(5,4,4,5)+0.1, font.main=1, cex.main = 1.25, cex.axis=1)
  cexLab = 0.8
  
  x = if(time) gps$elapsedSec/3600 else gps$cumDist
  y = if(pace) gps$pace else gps$speedSmooth
  
  xlab = if(time) "Time [h]" else "Distance [km]"
  ylab = if(pace) "Pace [min/km]" else "Speed [km/h]"
  
  plot(x, y, axes=F, type="l", xlab="", ylab="", lwd=2)
  axis(2)
  mtext(ylab, side=2, line=2.5, cex=cexLab)
  
  par(new=T)
  plot(x, gps$elevation, ylim=c(0.95*min(gps$elevation), 1.3*max(gps$elevation)), type="l", axes=F, col="red", xlab="", ylab="")
  polygon(c(x, rev(x)), c(rep(0,length(x)), rev(gps$elevation)), col=rgb(1,0,0,0.2), border=NA)
  axis(4, col="red", pretty(c(0.95*min(gps$elevation), 1.0*max(gps$elevation))))
  mtext("Elevation [m]", side=4, line=3, adj=0, cex=cexLab)
  
  axis(1)
  mtext(xlab, side=1, line=2.5, cex=cexLab)
  
  if(legend){
    legend("topleft", col=c("black","red"), lty=1, legend=c(ylab,"elevation"))  
  }
  
  title(strftime(date, format="%A %d of %B, %Y at %H:%M"))
}

# ------------------------------------------------------------------------------
# Plot track on a map
# ------------------------------------------------------------------------------
plotMap = function(gps, zoomFac=14, src="google", type="terrain", col="red"){
  
  mapImageData = get_map(location = c(lon = median(gps$longitude), lat = median(gps$latitude)),
                         zoom = zoomFac,
                         source = src,
                         maptype = c(type))
  
  ggmap(mapImageData, extent = "device") + # takes out axes, etc.
    geom_point(aes(x = longitude, y = latitude), data = gps, colour = col, size = 1, pch = 20) 
}


# ------------------------------------------------------------------------------
# Plots track as 3d trajectory (long, lat, alt)
# ------------------------------------------------------------------------------
plotTrack3d = function(gps){  
  scatterplot3d(x=gps$longitude, y=gps$latitude, z=gps$altitude, type="l")
  par(new=T)
  scatterplot3d(x=gps$longitude, y=gps$latitude, z=gps$altitude, type="h", pch=".", color=rgb(0,0,0,0.1))
}

# ------------------------------------------------------------------------------
# Distance run at dates
# ------------------------------------------------------------------------------
plotDistanceBars = function(stats,...){
  max = roundEvenUp(max(stats$totalDistance))
  barplot(rev(stats$totalDistance), names.arg=strftime(rev(stats$startTime), format="%d-%m"), 
          xlab="", ylab="Distance [km]", col="black", las=2, ylim=c(0, max),...)
}

plotDistanceBarsCal = function(stats){
  max = roundEvenUp(max(stats$totalDistance))
  #days = seq(from=stats$startTime[length(stats$startTime)], to=stats$startTime[1], by="day")
  days = seq(from=min(stats$startTime), to=max(stats$startTime), by="day")
  atLab = days[seq(1, length(days), length=length(days)/4)]
  atSub = days[seq(1, length(days), length=length(days)/2)]
  plot(stats$startTime, stats$totalDistance, type="h", bty="n", pch=19, 
       xaxt="n", xlab="", ylab="Distance [km]", lwd=8, ylim=c(0, max))
  axis.POSIXct(1, at=atSub, labels="")
  axis.POSIXct(1, at=atLab, format="%d-%m", las=2)
} 

# ------------------------------------------------------------------------------
# Distance run at dates
# ------------------------------------------------------------------------------
plotPaceDistance = function(stats){
  max = ceiling(max(stats$paceAvg))
  min = floor(min(stats$paceAvg))
  plot(stats$totalDistance, stats$paceAvg, col="black", pch=19, bty="n", 
       xlab="Distance [km]", ylab="Average Pace [min/km]", ylim=c(min, max))
  abline(lm(paceAvg ~ totalDistance, data=stats))
}