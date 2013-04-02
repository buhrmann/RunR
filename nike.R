library(RCurl)
library(rjson)
library(plyr)

accTokenFnm = "accessToken.txt"
baseAddr = "https://api.nike.com/"
header = c(Accept="application/json", "Content-Type"="application/json", appid="fuelband")

# ------------------------------------------------------------------------------
# Get user's access token
# ------------------------------------------------------------------------------
getNikeAccessToken = function(fnm=accTokenFnm){
  accessTokenFile = file(fnm, "rt", blocking=T)
  accessToken = readLines(accessTokenFile, 1, ok=T, warn=F)
  close(accessTokenFile)
  return(accessToken)
}

# ------------------------------------------------------------------------------
# Download single run gps data
# Nike returns some overall data and list of waypoints. We only need latter.
# GPS doesn't contain times. But all waypoints are exact and equal intervals apart.
# ------------------------------------------------------------------------------
getNikeSingleRun = function(activityId, accessToken){
  message(paste("Downloading run", activityId, "..."))
  address = paste(baseAddr, "me/sport/activities/", sep="")
  url = paste(address, activityId, "/gps?access_token=", accessToken, sep="")
  json = getURL(url, httpheader=header)
  data = fromJSON(json)
  
  df = ldply(data$waypoints, data.frame)
  df$activityId = activityId
  return(df)
}

# ------------------------------------------------------------------------------
# Get list of all activities
# Nike returns "data" and "paging". "data" contains url param "count" runs.
# Each run contains 9 values one of which is sublist
# ------------------------------------------------------------------------------
listNikeRuns = function(count, accessToken){
  address = paste(baseAddr, "me/sport/activities/", sep="")
  url = paste(address, "?access_token=", accessToken, "&count=", count, sep="")
  json = getURL(url, httpheader=header)
  data = fromJSON(json)
  
  # Extract only interesting data for each run (returns list of list)
  vars = c("activityId", "startTime")
  extracted = lapply(data$data, function(d) d[vars]) 
  
  # Now bind into df
  df = as.data.frame(t(sapply(extracted, function(x) rbind(x))))
  names(df) = names(extracted[[1]])
  
  df$activityId = as.numeric(df$activityId)
  df$startTime = gsub("T"," ", df$startTime)
  df$startTime = gsub("Z","", df$startTime)
  df$startTime = as.POSIXct(strptime(df$startTime, "%Y-%m-%d %H:%M:%S"))
  return(df)
}
