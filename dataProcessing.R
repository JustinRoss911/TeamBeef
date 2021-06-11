#Library Loading ----

library(tidyverse)
#library(anytime)
#library(plyr)
#library(dplyr)
#library(readr)
library(lubridate)

#to remove all variables when needed
#rm(list=ls())


#Data Loading ----
# PGdf1 <- read.csv(file = 'Data/GPS/PinPoint 80378 2020-11-09 10-57-44.csv', header = TRUE)
# PGdf2 <- read.csv(file = 'Data/GPS/PinPoint 80379 2020-11-09 13-05-18.csv', header = TRUE)
# PGdf3 <- read.csv(file = 'Data/GPS/PinPoint 80380 2020-11-09 13-18-22.csv', header = TRUE)
# PGdf4 <- read.csv(file = 'Data/GPS/PinPoint 80381 2020-11-09 13-48-25.csv', header = TRUE)
# PGdf5 <- read.csv(file = 'Data/GPS/PinPoint 80382 2020-11-09 12-11-25.csv', header = TRUE)
# PGdf6 <- read.csv(file = 'Data/GPS/PinPoint 80383 2020-11-09 11-31-51.csv', header = TRUE)
# PGdf7 <- read.csv(file = 'Data/GPS/PinPoint 80384 2020-11-05 15-35-36.csv', header = TRUE)
# PGdf8 <- read.csv(file = 'Data/GPS/PinPoint 80385 2020-11-05 13-28-27.csv', header = TRUE)
# PGdf9 <- read.csv(file = 'Data/GPS/PinPoint 80386 2020-11-09 15-35-23.csv', header = TRUE)
# PGdf10 <- read.csv(file = 'Data/GPS/PinPoint 80387 2020-11-09 11-45-02.csv', header = TRUE)
# PGdf11 <- read.csv(file = 'Data/GPS/PinPoint 80388 2020-11-09 14-28-51.csv', header = TRUE)
# PGdf12 <- read.csv(file = 'Data/GPS/PinPoint 80389 2020-11-09 10-05-18.csv', header = TRUE)
# PGdf13 <- read.csv(file = 'Data/GPS/PinPoint 80390 2020-11-05 14-37-10.csv', header = TRUE)
# PGdf14 <- read.csv(file = 'Data/GPS/PinPoint 80391 2020-11-09 12-39-21.csv', header = TRUE)
# 
#Calling Functions ----
# PGdf1 <- mstConversion(df1)
# PGdf2 <- mstConversion(df2)
# PGdf3 <- mstConversion(df3)
# PGdf4 <- mstConversion(df4)
# PGdf5 <- mstConversion(df5)
# PGdf6 <- mstConversion(df6)
# PGdf7 <- mstConversion(df7)
# PGdf8 <- mstConversion(df8)
# PGdf9 <- mstConversion(df9)
# PGdf10 <- mstConversion(df10)
# PGdf11 <- mstConversion(df11)
# PGdf12 <- mstConversion(df12)
# PGdf13 <- mstConversion(df13)
# PGdf14 <- mstConversion(df14)
# 
# PGdf1 <- analyzeFixes(df1)
# PGdf2 <- analyzeFixes(df2)
# PGdf3 <- analyzeFixes(df3)
# PGdf4 <- analyzeFixes(df4)
# PGdf5 <- analyzeFixes(df5)
# PGdf6 <- analyzeFixes(df6)
# PGdf7 <- analyzeFixes(df7)
# PGdf8 <- analyzeFixes(df8)
# PGdf9 <- analyzeFixes(df9)
# PGdf10 <- analyzeFixes(df10)
# PGdf11 <- analyzeFixes(df11)
# PGdf12 <- analyzeFixes(df12)
# PGdf13 <- analyzeFixes(df13)
# PGdf14 <- analyzeFixes(df14)
# 
# 
# 
#Data Output ----
# write.csv(PGdf1, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post1GPS.csv", row.names = FALSE)
# write.csv(PGdf2, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post2GPS.csv", row.names = FALSE)
# write.csv(PGdf3, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post3GPS.csv", row.names = FALSE)
# write.csv(PGdf4, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post4GPS.csv", row.names = FALSE)
# write.csv(PGdf5, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post5GPS.csv", row.names = FALSE)
# write.csv(PGdf6, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post6GPS.csv", row.names = FALSE)
# write.csv(PGdf7, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post7GPS.csv", row.names = FALSE)
# write.csv(PGdf8, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post8GPS.csv", row.names = FALSE)
# write.csv(PGdf9, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post9GPS.csv", row.names = FALSE)
# write.csv(PGdf10, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post10GPS.csv", row.names = FALSE)
# write.csv(PGdf11, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post11GPS.csv", row.names = FALSE)
# write.csv(PGdf12, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post12GPS.csv", row.names = FALSE)
# write.csv(PGdf13, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post13GPS.csv", row.names = FALSE)
# write.csv(PGdf14, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\Post14GPS.csv", row.names = FALSE)


#Working Tests ----

testData <- read.csv(file = 'Data/GPS/testGPS.csv', header = TRUE)


#Functions ----
mstConversion <- function(data)
{
  tempDate <- strptime(data$GMT, "%Y-%m-%d %H:%M", tz = "GMT")
  tempData  <- with_tz(tempDate, "America/Edmonton")
  data$MST <- tempData 
  
  return(data)  
}

indexGPSZero <- function(dataInput)
{
  #selects the data within the headers specified in the argument
  tempData <- dataInput %>% select(Latitude, Longitude, Altitude)
  #checks the headers specified if a 0 is within 
  tempData <- tempData[tempData$Latitude == 0 | tempData$Longitude == 0 | tempData$Altitude == 0, ]
  #previous loads as char, type cast to numeric for later use
  dataOutput <- as.numeric(rownames(tempData))
  
  return(dataOutput)
  
}

indexCopyFrame5 <- function(dataInput, index)
{
  #set the same structure as the input dataframe
  copyFrame <- dataInput[0, ]
  
  #runs the length of the index vector
  for(i in 1:length(index))
  {
    #checks if it is not out of bounds
    if (index[i] >= 3)
      #appends the specifed index of the input data, to bottom of copyFrame
      copyFrame <- rbind(copyFrame, dataInput[index[i] - 2,])
    
    if (index[i] >= 2)
      copyFrame <- rbind(copyFrame, dataInput[index[i] - 1,])
    
    copyFrame <- rbind(copyFrame, dataInput[index[i],])
    
    if (index[i] <= (nrow(dataInput) - 1))
      copyFrame <- rbind(copyFrame, dataInput[index[i] + 1,])
    
    if (index[i] <= (nrow(dataInput) - 2))
      copyFrame <- rbind(copyFrame, dataInput[index[i] + 2,])

    #for sake of formatting, adds a row of NA
    copyFrame[nrow(copyFrame) + 1,] <- NA
  }
  
  return(copyFrame)
}

indexCopyFrame6 <- function(dataInput, index)
{
  #set the same structure as the input dataframe
  copyFrame <- dataInput[0, ]
  
  #runs the length of the index vector
  for(i in 1:length(index))
  {
    #checks if it is not out of bounds
    if (index[i] >= 3)
      #appends the specifed index of the input data, to bottom of copyFrame
      copyFrame <- rbind(copyFrame, dataInput[index[i] - 2,])
    
    if (index[i] >= 2)
      copyFrame <- rbind(copyFrame, dataInput[index[i] - 1,])
    
    copyFrame <- rbind(copyFrame, dataInput[index[i],])
    
    if (index[i] <= (nrow(dataInput) - 1))
      copyFrame <- rbind(copyFrame, dataInput[index[i] + 1,])
    
    if (index[i] <= (nrow(dataInput) - 2))
      copyFrame <- rbind(copyFrame, dataInput[index[i] + 2,])
    
    if (index[i] <= (nrow(dataInput) - 3))
      copyFrame <- rbind(copyFrame, dataInput[index[i] + 3,])
    
    #for sake of formatting, adds a row of NA
    copyFrame[nrow(copyFrame) + 1,] <- NA
  }
  
  return(copyFrame)
}


analyzeFixes <- function(data)
{
  dates <- seq(as.Date("2020-06-18"), as.Date("2020-09-16"), by="days")
  
  copyFrame <- data.frame(Dates=character(0), Expected_Fixes=numeric(0), On_Time_Fix=numeric(0), 
                          No_Fix=numeric(0), Early_Fix=numeric(0), Late_Fix=numeric(0), 
                          Per_No_Fix=character(0), Per_Missing_Fix=character(0))
  
  
  
  for(i in 1:length(dates))
  {
    dateCounter <- 0
    noFix <- 0 
    lateFix <- 0 
    earlyFix <- 0 
    Fix <- 0 
    expFix <- 0 
    differenceTime <- 0 
    firstCheck <- TRUE 
    dateStart <- 0 
    dateEnd <- 0 
    perNo <- 0
    perMiss <- 0 
    expectedFix <- 0 
    
    for(j in 2:(nrow(data)-1))
    {
      if(as.Date(data$MST[j]) == dates[i]) { #same day
        
        differenceTime <- as.numeric((data$MST[j] - data$MST[j - 1]), units ="mins")
        
        if(data$Latitude[j] == 0 | data$Longitude[j] == 0 | data$Altitude[j] == 0) {
          noFix <- noFix + 1
        }
        
        else if(3 < differenceTime && differenceTime < 7) { #fix
          Fix <- Fix + 1
        }
        
        else if(3 >= differenceTime) { #early
          earlyFix <- earlyFix + 1
        }
        
        else if(differenceTime <= 7) { #late
          lateFix <- lateFix + 1
        }
        
        if(firstCheck) {  #first
          dateStart <- data$MST[j]
          firstCheck <- FALSE
        }
        
        else if(TRUE) { #last
          dateEnd <- data$MST[j]
        }
        
        dateCounter <- dateCounter + 1
      }
      
    }
    
    rowDate <- as.character(dates[i])
    
    
    if(dateCounter == 0) {
      holdFrame <- data.frame(rowDate, expectedFix, Fix, noFix, earlyFix, lateFix, perNo, perMiss)
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    
    else {
      periodDate <- as.interval(dateStart, dateEnd)
      
      #expectedFix <- ceiling( periodDate / minutes(5) )
      expectedFix <- 288
      
      perNo <- (noFix / (lateFix + earlyFix + Fix)) * 100 
      perNo <- paste(perNo, "%")
      
      perMiss <- ((expectedFix - (lateFix + earlyFix + Fix)) / expectedFix) * 100 
      perMiss <- paste(perMiss, "%")
      
      holdFrame <- data.frame(rowDate, expectedFix, Fix, noFix, earlyFix, lateFix, perNo, perMiss)
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    
    
  }
  
  colnames(copyFrame) <- c("Date", "Expected_Fixes", "On_Time_Fix", "No_fix", "Early_Fix", "Late_Fix", "Percent_No_Fix_to_Total", "Per_Missing__to_Expected_Fix")
  return(copyFrame)
}

