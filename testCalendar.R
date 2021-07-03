#Loading Libraries ----
library(tidyverse)
library(lubridate)

source("loadData.R")

#rm(list=ls())


jkTest <- loadData("accl", "accl/post")
newU <- filterCalendar(jkTest)
test <- newU[[1]]

testi <- test[1:(nrow(test) - 1), ]
testn <- test[2:nrow(test), ]

testi <- as.numeric(testi$DateTime)
testn <- as.numeric(testn$DateTime)

testNew <- data.frame(testn - testi)

testNeg <- data.frame(testNew[testNew > 86400, ])
testYO <- c(1:18864)

testYe <- cbind(test, testYO)

coolQuick <- testNew[testNew$testn...testi != 2,]

#Notes ----
#The inclusion of the %p allows for the proper parsing of the datetime object. Lastly, just need to select for 
#only the scehdulded GPS events, 

# testCal <- loadCalendar()
# #Testing ----
# # testHoldFrme <- testGPS[["ID_80378"]]
# # testHoldFrme <- transform(testHoldFrme, new = as.numeric(testHoldFrme$DateTime))
# # testGPS[["ID_80378"]] <- testHoldFrme
# 
# exploring <- bigTest[["ID_80389"]]
# exploringGood <- testGPSNew[["ID_80389"]]
# 
# #Function Testing for Calendar of Events ----
# 
# copyFrameNew <- testGPSNew[["ID_80386"]]
# collarEvents <- testCal[testCal$Collar_ID == 80386, ]
# dateBounds <- copyFrameNew[collarEvents$Collar_On[1] < copyFrameNew$DateTime & collarEvents$Collar_Off[1] > copyFrameNew$DateTime ,]
# 
# testBounds <- transform(dateBounds, BullID = 1)


#the above works for selection of items within the calendar of events, just need to recursively do it in a loop for all objects within the list 
#only tricky thing is  maybe trying to assign if multiple items within the list of events. Could select then loop through the list and Rbind

#the errenous dates piece is still tricky ngl 

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


filterCalendar <- function(dataIn)
{
  eventCalendar <- loadCalendar()
  eventList <- list()
  
  
  for(i in 1:nrow(eventCalendar))
  {
    onTime <- eventCalendar$Collar_On[i]
    offTime <- eventCalendar$Collar_Off[i]
    fileID <- eventCalendar$Collar_ID[i]
    BullID <- eventCalendar$Bull_ID[i]
    
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyFrame <- copyFrame[onTime < copyFrame$DateTime & offTime > copyFrame$DateTime, ]
    
    if(is.null(copyFrame))
    {print(nrow(copyFrame))}
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

#Calendar Assignment Functions ----
