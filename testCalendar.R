#Loading Libraries ----
library(tidyverse)
library(lubridate)

source("loadData.R")

#rm(list=ls())


gpsTest <- loadData("gps", "gps/post")
gpsFiltered <- filterCalendar(gpsTest)
gpsFiltered <- dateTimeDifference(gpsFiltered)



#Testing Code ---- 


#Calendar Loading Functions ----
loadCalendar <- function()
{
  directoryPath <- paste("raw/", "events", "/", sep="")
  fileList <- list.files(directoryPath)
  filePath <- paste(directoryPath, fileList[1], sep="") 
  
  events <- read.csv(filePath, stringsAsFactors = FALSE)
  events$Collar_On <- as.POSIXct(events$Collar_On)
  events$Collar_Off <- as.POSIXct(events$Collar_Off)
  
  return(events)
}



#Calendar Assignment Functions ----
filterCalendar <- function(dataIn)
{
  eventCalendar <- loadCalendar()
  eventList <- list()
  
  
  for(i in 1:nrow(eventCalendar))
  {
    onTime <- eventCalendar$Collar_On[i]
    offTime <- eventCalendar$Collar_Off[i]
    fileID <- as.factor(eventCalendar$Collar_ID[i])
    BullID <- as.factor(eventCalendar$Bull_ID[i])
    
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyFrame <- copyFrame[onTime < copyFrame$DateTime & offTime > copyFrame$DateTime, ]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      copyFrame <- transform(copyFrame, BullID = BullID)
      
      logNames <- names(eventList) == varname
      listSum <- sum(logNames)
      
      if(listSum > 0)
      {
        eventList[[varname]] <- rbind(eventList[[varname]], copyFrame)
      }
      else
      {
        eventList[[varname]] <- copyFrame
      }
    }
    
  }
  
  return(eventList)
}


#DateTime Difference Function ----
dateTimeDifference <- function(dataIn)
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    timeFirst <-  copyFrame[1:(nrow(copyFrame) - 1), ]
    timeSecond <- copyFrame[2:nrow(copyFrame), ]
    
    timeFirst <- as.numeric(timeFirst$DateTime)
    timeSecond <- as.numeric(timeSecond$DateTime)
    
    time <- timeSecond - timeFirst
    copyFrame <- copyFrame[2:nrow(copyFrame), ]
    
    copyFrame <- cbind(copyFrame, timeDifference = time)
    
    logNames <- names(dataList) == varname
    listSum <- sum(logNames)
    
    if(listSum > 0)
    {
      dataList[[varname]] <- rbind(dataList[[varname]], copyFrame)
    }
    else
    {
      dataList[[varname]] <- copyFrame
    }
  }
  return(dataList)
}
