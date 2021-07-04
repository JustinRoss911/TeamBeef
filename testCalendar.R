#Loading Libraries ----
library(tidyverse)
library(lubridate)
library(geosphere)


source("loadData.R")

#rm(list=ls())


gpsTest <- loadData("gps", "gps/post")
gpsFiltered <- filterCalendar(gpsTest)
gpsFiltered <- speedAndTimeDifference(gpsFiltered)

gpsTestResults <- gpsFixCheck(gpsFiltered)


#Testing Code ---- 

# testData <- fatFree[[2]]
# 
# testCalendar <- loadCalendar()
# 
# start <- testCalendar$Collar_On[1] 
# end <- testCalendar$Collar_Off[1]
# #sets sequence, change interval second to desired. 
# expectedFixes <- data.frame(expectedRecording = seq(start, end, by = 300))
# #gives the dates 
# dateSequence <- data.frame(dates = seq(as.Date(start), as.Date(end), by="days"))
# 
# copyFrame <- data.frame(Dates = character(0), Expected_Fixes = numeric(0), On_Time_Fix = numeric(0), Early_Fix = numeric(0), Late_Fix = numeric(0), Missing_Fix = numeric(0), CollarID = numeric(0), BullID = numeric(0))
# 
# 
# new <- testData[as.Date(testData$DateTime) == dateSequence$dates[29],]
# 
# meanTime <- mean(testData$TimeDifference)
# meanTime
# test <- as.factor(c(1, 2))
# #Function for extracting no fix events and returning list (will just filter out following this step)
# dataIn <- gpsFiltered
# 
# eventCalendar <- loadCalendar()
# dataList <- list()
# 
# for(j in 1:nrow(eventCalendar))
# {
# 
#   outputFrame <- data.frame(Dates = character(0), Expected_Fixes = numeric(0), On_Time_Fix = numeric(0), No_Fix = numeric(0), Early_Fix = numeric(0), Late_Fix = numeric(0), Missing_Fix = numeric(0), CollarID = numeric(0), BullID = numeric(0))
# 
#   start <- eventCalendar$Collar_On[j] 
#   end <- eventCalendar$Collar_Off[j]
# 
#   expectedFixes <- data.frame(dates = seq(start, end, by = 300))
#   dateSequence <- data.frame(dates = seq(as.Date(start), as.Date(end), by="days"))
#   
#   fileID <- as.factor(eventCalendar$Collar_ID[j])
#   BullID <- as.factor(eventCalendar$Bull_ID[j])
#   
#   prefix <- "ID"
#   varname <- paste(prefix, fileID, sep="_")
#   
#   copyFrame <- dataIn[[varname]]
#   
#   
#   for(i in 1:nrow(dateSequence))
#   {
#     dateFrame <- copyFrame[as.Date(copyFrame$DateTime) == dateSequence$dates[i], ]
#     expFix <- length(expectedFixes[as.Date(expectedFixes$dates) == dateSequence$dates[i], ] )
#     
#    
#     
#     date <- dateSequence$dates[i]
#     
#     if(nrow(dateFrame) == 0)
#     {
#       fix <- 0 
#       noFix <- 0 
#       early <- 0 
#       late <- 0 
#       missing <- expFix
#     }
#     else
#     {
#       withZero <- nrow(dateFrame)
#       withoutZero <- nrow(dateFrame[dateFrame$Latitude != 0 | dateFrame$Longitude != 0 | dateFrame$Altitude != 0,])
#       noFix <- withZero - withoutZero
#       
#       dateFrame <- dateFrame[dateFrame$Latitude != 0 | dateFrame$Longitude != 0 | dateFrame$Altitude != 0,]
#       
#       fix <- nrow(dateFrame[dateFrame$TimeDifference >= 240 & dateFrame$TimeDifference <= 360, ])
#       early <- nrow(dateFrame[dateFrame$TimeDifference < 240, ])
#       late <- nrow(dateFrame[dateFrame$TimeDifference > 360, ])
#       
#       missing <- expFix - (fix + early + late)
#     }
#     
#     holdFrame <- data.frame(date, expFix, fix, noFix, early, late, missing, fileID, BullID)
#     outputFrame <- rbind(outputFrame, holdFrame)
#   }
#   
#   logNames <- names(dataList) == varname
#   listSum <- sum(logNames)
#   
#   if(listSum > 0)
#   {
#     dataList[[varname]] <- rbind(dataList[[varname]], outputFrame)
#   }
#   else
#   {
#     dataList[[varname]] <- outputFrame
#   }   
#   
# }  

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
speedAndTimeDifference <- function(dataIn)
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
    
    p1 <-  copyFrame[1:(nrow(copyFrame) - 1), ]
    p2 <- copyFrame[2:nrow(copyFrame), ]
    
    
    #Time Difference 
    timeFirst <- as.numeric(timeFirst$DateTime)
    timeSecond <- as.numeric(timeSecond$DateTime)
    
    time <- timeSecond - timeFirst
    copyFrame <- copyFrame[2:nrow(copyFrame), ]
    
    copyFrame <- cbind(copyFrame, TimeDifference = time)
    
    #Speed
    p1GPS <- p1 %>% select(Latitude, Longitude)
    p2GPS <- p2 %>% select(Latitude, Longitude)
    
    p1GPS <- p1GPS[c('Longitude', "Latitude")]
    p2GPS <- p2GPS[c('Longitude', "Latitude")]
    
    distance <- distGeo(p1GPS, p2GPS)
    speed <- abs(distance / time)
    
    copyFrame <- cbind(copyFrame, Speed = speed)
    
    #Loading In 
    
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

#GPS Zero Filter Functions ---- 
extractZeroGPS <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- copyFrame[copyFrame$Latitude == 0 | copyFrame$Longitude == 0 | copyFrame$Altitude == 0,]
    
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

filterZeroGPS <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- copyFrame[copyFrame$Latitude != 0 | copyFrame$Longitude != 0 | copyFrame$Altitude != 0,]
    
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

#Errenous Dates Selection Function ---- 

#this kind of works but there is a lot that it may be missing. 
#should look into a little more
extractErrenousDates <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- copyFrame[copyFrame$TimeDifference < 0 | copyFrame$TimeDifference >= 600,]
    
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

#Early, Late and Missing Fixes ----
gpsFixCheck <- function(dataIn)
{
  eventCalendar <- loadCalendar()
  dataList <- list()
  
  for(j in 1:nrow(eventCalendar))
  {
    
    outputFrame <- data.frame(Dates = character(0), Expected_Fixes = numeric(0), On_Time_Fix = numeric(0), No_Fix = numeric(0), Early_Fix = numeric(0), Late_Fix = numeric(0), Missing_Fix = numeric(0), Out_Bounds = numeric(0), CollarID = numeric(0), BullID = numeric(0))
    
    start <- eventCalendar$Collar_On[j] 
    end <- eventCalendar$Collar_Off[j]
    
    expectedFixes <- data.frame(dates = seq(start, end, by = 300))
    dateSequence <- data.frame(dates = seq(as.Date(start), as.Date(end), by="days"))
    
    fileID <- as.factor(eventCalendar$Collar_ID[j])
    BullID <- as.factor(eventCalendar$Bull_ID[j])
    
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    
    
    for(i in 1:nrow(dateSequence))
    {
      dateFrame <- copyFrame[as.Date(copyFrame$DateTime) == dateSequence$dates[i], ]
      expFix <- length(expectedFixes[as.Date(expectedFixes$dates) == dateSequence$dates[i], ] )
      
      date <- dateSequence$dates[i]
      
      if(nrow(dateFrame) == 0)
      {
        fix <- 0 
        noFix <- 0 
        early <- 0 
        late <- 0 
        missing <- expFix
        outBounds <- 0 
      }
      else
      {
        withZero <- nrow(dateFrame)
        withoutZero <- nrow(dateFrame[dateFrame$Latitude != 0 | dateFrame$Longitude != 0 | dateFrame$Altitude != 0,])
        noFix <- withZero - withoutZero
        
        dateFrame <- dateFrame[dateFrame$Latitude != 0 | dateFrame$Longitude != 0 | dateFrame$Altitude != 0,]
        
        outBounds <- nrow(dateFrame[dateFrame$Speed > 10, ]) 
        dateFrame <- dateFrame[dateFrame$Speed <= 10, ] 
        
        fix <- nrow(dateFrame[dateFrame$TimeDifference >= 240 & dateFrame$TimeDifference <= 360, ])
        early <- nrow(dateFrame[dateFrame$TimeDifference < 240, ])
        late <- nrow(dateFrame[dateFrame$TimeDifference > 360, ])
        
        missing <- expFix - (fix + early + late)
        
        
      }
      
      holdFrame <- data.frame(date, expFix, fix, noFix, early, late, missing, outBounds, fileID, BullID)
      outputFrame <- rbind(outputFrame, holdFrame)
    }
    
    logNames <- names(dataList) == varname
    listSum <- sum(logNames)
    
    if(listSum > 0)
    {
      dataList[[varname]] <- rbind(dataList[[varname]], outputFrame)
    }
    else
    {
      dataList[[varname]] <- outputFrame
    }   
  }  
  return(dataList)
}
