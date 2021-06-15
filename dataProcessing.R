#Library Loading ----

library(tidyverse)
#library(anytime)
#library(plyr)
#library(dplyr)
#library(readr)
library(lubridate)
library(geosphere)

#to remove all variables when needed
#rm(list=ls())


#Data Loading ----
PGdf1 <- read.csv(file = 'raw/gps/post/PinPoint 80378 2020-11-09 10-57-44.csv', header = TRUE)
PGdf2 <- read.csv(file = 'raw/gps/post/PinPoint 80379 2020-11-09 13-05-18.csv', header = TRUE)
# PGdf3 <- read.csv(file = 'raw/gps/post/PinPoint 80380 2020-11-09 13-18-22.csv', header = TRUE)
# PGdf4 <- read.csv(file = 'raw/gps/post/PinPoint 80381 2020-11-09 13-48-25.csv', header = TRUE)
# PGdf5 <- read.csv(file = 'raw/gps/post/PinPoint 80382 2020-11-09 12-11-25.csv', header = TRUE)
# PGdf6 <- read.csv(file = 'raw/gps/post/PinPoint 80383 2020-11-09 11-31-51.csv', header = TRUE)
# PGdf7 <- read.csv(file = 'raw/gps/post/PinPoint 80384 2020-11-05 15-35-36.csv', header = TRUE)
# PGdf8 <- read.csv(file = 'raw/gps/post/PinPoint 80385 2020-11-05 13-28-27.csv', header = TRUE)
# PGdf9 <- read.csv(file = 'raw/gps/post/PinPoint 80386 2020-11-09 15-35-23.csv', header = TRUE)
# PGdf10 <- read.csv(file = 'raw/gps/post/PinPoint 80387 2020-11-09 11-45-02.csv', header = TRUE)
# PGdf11 <- read.csv(file = 'raw/gps/post/PinPoint 80388 2020-11-09 14-28-51.csv', header = TRUE)
# PGdf12 <- read.csv(file = 'raw/gps/post/PinPoint 80389 2020-11-09 10-05-18.csv', header = TRUE)
# PGdf13 <- read.csv(file = 'raw/gps/post/PinPoint 80390 2020-11-05 14-37-10.csv', header = TRUE)
# PGdf14 <- read.csv(file = 'raw/gps/post/PinPoint 80391 2020-11-09 12-39-21.csv', header = TRUE)
# 
#Calling MST Conversion Functions ----
PGdf1 <- mstConversion(PGdf1)
# PGdf2 <- mstConversion(PGdf2)
# PGdf3 <- mstConversion(PGdf3)
# PGdf4 <- mstConversion(PGdf4)
# PGdf5 <- mstConversion(PGdf5)
# PGdf6 <- mstConversion(PGdf6)
# PGdf7 <- mstConversion(PGdf7)
# PGdf8 <- mstConversion(PGdf8)
# PGdf9 <- mstConversion(PGdf9)
# PGdf10 <- mstConversion(PGdf10)
# PGdf11 <- mstConversion(PGdf11)
# PGdf12 <- mstConversion(PGdf12)
# PGdf13 <- mstConversion(PGdf13)
# PGdf14 <- mstConversion(PGdf14)

#Calling Analyze Fix Functions ----
 RPGdf1 <- analyzeFixes(PGdf1)
# RPGdf2 <- analyzeFixes(PGdf2)
# RPGdf3 <- analyzeFixes(PGdf3)
# RPGdf4 <- analyzeFixes(PGdf4)
# RPGdf5 <- analyzeFixes(PGdf5)
# RPGdf6 <- analyzeFixes(PGdf6)
# RPGdf7 <- analyzeFixes(PGdf7)
# RPGdf8 <- analyzeFixes(PGdf8)
# RPGdf9 <- analyzeFixes(PGdf9)
# RPGdf10 <- analyzeFixes(PGdf10)
# RPGdf11 <- analyzeFixes(PGdf11)
# RPGdf12 <- analyzeFixes(PGdf12)
# RPGdf13 <- analyzeFixes(PGdf13)
# RPGdf14 <- analyzeFixes(PGdf14)
# 
# 
# 
#Data Output ----
# write.csv(RPGdf1, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost1GPS.csv", row.names = FALSE)
# write.csv(RPGdf2, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost2GPS.csv", row.names = FALSE)
# write.csv(RPGdf3, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost3GPS.csv", row.names = FALSE)
# write.csv(RPGdf4, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost4GPS.csv", row.names = FALSE)
# write.csv(RPGdf5, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost5GPS.csv", row.names = FALSE)
# write.csv(RPGdf6, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost6GPS.csv", row.names = FALSE)
# write.csv(RPGdf7, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost7GPS.csv", row.names = FALSE)
# write.csv(RPGdf8, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost8GPS.csv", row.names = FALSE)
# write.csv(RPGdf9, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost9GPS.csv", row.names = FALSE)
# write.csv(RPGdf10, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost10GPS.csv", row.names = FALSE)
# write.csv(RPGdf11, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost11GPS.csv", row.names = FALSE)
# write.csv(RPGdf12, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost12GPS.csv", row.names = FALSE)
# write.csv(RPGdf13, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost13GPS.csv", row.names = FALSE)
# write.csv(RPGdf14, "F:\\Development\\Projects\\Research\\TeamBeef\\workingProject\\output\\APost14GPS.csv", row.names = FALSE)


#Working Tests Distance----

# testData <- read.csv(file = 'Data/GPS/testGPS.csv', header = TRUE)
# yot <- mstConversion(yot)
# 
# testGPS1 <- testData[90,]
# testGPS2 <- testData[91,]
# 
# avgSpeed <- speedFromGPS(PGdf1[(142),], PGdf1[10000,])
# 
# 
# new <- speedFromGPS(testGPS1, testGPS2)
# 
# #Working Test Proximity Selc
# #need to extract before converting to MST 
# yot <- filter(tempData, Fix %in% c("GPS Schedule"))
# 
# #Testing Functions ----
tempData <- subset(PGdf1, date(GMT) == date(GMT) == as.Date("2020-08-09"))
# 
for(i in 2:nrow(tempData)) {
  speed <- speedFromGPS(tempData[i-1,], tempData[i,])

  print(speed)
}
#   
# tempData <- mstConversion(tempData)
 test <- analyzeFixes(tempData)
# tempData <- tempData
# 
# 
# #testing why inf speed, if can't figure out will just set to 0 
# speedFromGPS(tempData[169,], tempData[170,])
# 
# yot <- TRUE

#Functions ----

mstConversion <- function(data)
{
  #remove if wanted to analyze Schedulde and Proximity together, will remove when fixed up. 
  
  tempDate <- strptime(data$GMT, "%Y-%m-%d %H:%M", tz = "GMT")
  tempData  <- with_tz(tempDate, "America/Edmonton")
  data$GMT <- tempData 
  
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
  #2020-07-13 for pre/post switch over 2020-06-18
  dates <- seq(as.Date("2020-06-18"), as.Date("2020-09-16"), by="days")
  
  copyFrame <- data.frame(Dates=character(0), Expected_Fixes=numeric(0), On_Time_Fix=numeric(0), 
                          No_Fix=numeric(0), Early_Fix=numeric(0), Late_Fix=numeric(0), Avg_Speed_Day=numeric(0),
                          Avg_Speed_Night=numeric(0), Out_of_Bounds=numeric(0), Per_No_Fix=character(0), Per_Missing_Fix=character(0))
  
  
  
  for(i in 1:length(dates))
  {
    dateCounter <- 0
    
    noFix <- 0 
    lateFix <- 0 
    earlyFix <- 0 
    Fix <- 0 
    
    differenceTime <- 0 
    firstCheck <- TRUE 
    dateStart <- 0 
    dateEnd <- 0 
    
    perNo <- 0
    perMiss <- 0 
    expectedFix <- 0 
    
    avgSpeedDay <- 0
    avgSpeedNight <- 0
    speedcounterD <- 0
    speedcounterN <- 0
    speedEvent <- TRUE
    outOfBounds <- 0
    
    day <- FALSE
    night <- FALSE
    
    for(j in 2:(nrow(data)-1))
    {
      if(as.Date(data$GMT[j]) == dates[i]) { #same day
        
        differenceTime <- as.numeric((data$GMT[j] - data$GMT[j - 1]), units ="mins")
        speedCheck <- speedFromGPS(data[(j-1),], data[j,])
        
        if(speedCheck > 10)
          speedEvent <- FALSE
        
        daytime <- format(data$GMT[j], format = "%H")

        if(daytime == "06") {
          day <- TRUE
          night <- FALSE
        }
        else if(daytime == "20") {
          night <- TRUE
          day <- FALSE
        }
        
        if(data$Latitude[j] == 0 | data$Longitude[j] == 0 | data$Altitude[j] == 0) {
          noFix <- noFix + 1
        }
        
        else if(3 < differenceTime && differenceTime < 7 & speedEvent) { #fix
          Fix <- Fix + 1
          
          if(day) {
          avgSpeedDay <- avgSpeedDay + speedFromGPS(data[(j-1),], data[j,])
          speedcounterD <- speedcounterD + 1
          }
          else if(night) {
          avgSpeedNight <- avgSpeedNight + speedFromGPS(data[(j-1),], data[j,])
          speedcounterN <- speedcounterN + 1         
          }
          
        }
        
        else if(3 >= differenceTime & speedEvent) { #early
          earlyFix <- earlyFix + 1
          
          if(day) {
            avgSpeedDay <- avgSpeedDay + speedFromGPS(data[(j-1),], data[j,])
            speedcounterD <- speedcounterD + 1
          }
          else if(night) {
            avgSpeedNight <- avgSpeedNight + speedFromGPS(data[(j-1),], data[j,])
            speedcounterN <- speedcounterN + 1         
          }
          
        }
        
        else if(differenceTime <= 7 & speedEvent) { #late
          lateFix <- lateFix + 1
          
          if(day) {
            avgSpeedDay <- avgSpeedDay + speedFromGPS(data[(j-1),], data[j,])
            speedcounterD <- speedcounterD + 1
          }
          else if(night) {
            avgSpeedNight <- avgSpeedNight + speedFromGPS(data[(j-1),], data[j,])
            speedcounterN <- speedcounterN + 1         
          }
          
        }
        
        else if(!speedEvent) {
          outOfBounds <- outOfBounds + 1
          speedEvent <- TRUE
        }
        
        
        if(firstCheck) {  #first
          dateStart <- data$GMT[j]
          firstCheck <- FALSE
        }
        
        else if(TRUE) { #last
          dateEnd <- data$GMT[j]
        }
        
        dateCounter <- dateCounter + 1
      }
      
    }
    
    rowDate <- as.character(dates[i])
    
    
    if(dateCounter == 0) {
      holdFrame <- data.frame(rowDate, expectedFix, Fix, noFix, earlyFix, lateFix, avgSpeedDay, avgSpeedNight, outOfBounds, perNo, perMiss)
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    
    else {
      periodDate <- as.interval(dateStart, dateEnd)
      
      if (i == 1) {
        expectedFix <- ceiling( periodDate / minutes(5) )
      }
      else if (i == length(dates)) {
        expectedFix <- ceiling( periodDate / minutes(5) )
      }
      else {
        expectedFix <- 288
        
      }
      
      perNo <- (noFix / (lateFix + earlyFix + Fix)) * 100 
      perNo <- paste(perNo, "%")
      
      perMiss <- ((expectedFix - (lateFix + earlyFix + Fix)) / expectedFix) * 100 
      perMiss <- paste(perMiss, "%")

      avgSpeedDay <- avgSpeedDay / speedcounterD
      avgSpeedNight <- avgSpeedNight / speedcounterN

      holdFrame <- data.frame(rowDate, expectedFix, Fix, noFix, earlyFix, lateFix, avgSpeedDay, avgSpeedNight, outOfBounds, perNo, perMiss)
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    
    
  }
  
  colnames(copyFrame) <- c("Date", "Expected_Fixes", "On_Time_Fix", "No_fix", "Early_Fix", "Late_Fix", "Avg_Speed_Day", "Avg_Speed_Night", "Out_of_Bounds", "Percent_No_Fix_to_Total", "Per_Missing__to_Expected_Fix")
  return(copyFrame)
}

speedFromGPS <- function(p1, p2)
{
  p1GPS <- p1 %>% select(Latitude, Longitude)
  p2GPS <- p2 %>% select(Latitude, Longitude)
  
  p1GPS <- p1GPS[c('Longitude', "Latitude")]
  p2GPS <- p2GPS[c('Longitude', "Latitude")]
  
  distance <- distGeo(p1GPS, p2GPS)
  
  time <- as.numeric((p2$GMT - p1$GMT), units ="secs")

  speed <- abs(distance / time)

  if(speed == Inf)
    speed <- 0 
  
  return(speed)
}

