# ------------------------------------------------------------------------------
# GPS statistics
# Inspired by http://cnr.lwlss.net/GarminR/
# ------------------------------------------------------------------------------
source("gpsStats.R")
source("nike.R")

defaultLocalFnm = "Runs.rdat"

# ------------------------------------------------------------------------------
# Loads data from disk if available or from Nike otherwise. Returns df of gps
# data from multiple runs.
# ------------------------------------------------------------------------------
loadGps = function(fnm, maxRuns=10)
{
  accessToken = getNikeAccessToken()
  
  # Retrieve list of run metadata from Nike+
  runs = listNikeRuns(maxRuns, accessToken)
  
  # See whether we have previously cached runs. File stores gps data in variable "gps"
  if(file.exists(fnm)){
    # Only dowload non-cached runs
    load(fnm)
    cachedRuns = unique(gps$activityId)
    downloadIds = setdiff(runs$activityId, cachedRuns)
    message(paste("Need to download", length(downloadIds), "runs."))
  } else{
    # Dowload all runs
    message(paste("Need to download", nrow(runs), "runs:"))
    gps = processGps(getNikeSingleRun(runs$activityId[[1]], accessToken))
    gps$startTime = runs$startTime[1]
    downloadIds = runs$activityId[2:nrow(runs)]
  }
  
  # Download and process all necessary runs
  for(id in downloadIds){
    run = processGps(getNikeSingleRun(id, accessToken))
    run$startTime = runs[runs$activityId==id, "startTime"]
    gps = rbind(gps, run)
  }
  
  return(gps)
}

# ------------------------------------------------------------------------------
# Update locally store runs and load into global environment
# ------------------------------------------------------------------------------
updateLocalStore = function(fnm=defaultLocalFnm, maxDownloads=2){
  gps <<- loadGps(fnm, maxDownloads)
  stats <<- allStats(gps)
  save(gps, stats, file=fnm)
}

# ------------------------------------------------------------------------------
# Converts df to D3 compatible json
# From http://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis/
# ------------------------------------------------------------------------------
toD3JSON = function(df){
  names = colnames(df)
  
  nameValue = function(nm){
    quote = '';
    if(class(df[, nm]) != 'numeric'){
      quote = '"';
    }
    
    paste('"', nm, '" : ', quote, df[,nm], quote, sep='')
  }
  
  objs = apply(sapply(names, nameValue), 1, function(x){paste(x, collapse=', ')})
  objs = paste('{', objs, '}')
  
  res = paste('[', paste(objs, collapse=', '), ']')
  
  return(res)
}