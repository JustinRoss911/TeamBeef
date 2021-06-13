#Library Loading ----

library(tidyverse)
#library(anytime)
#library(plyr)
#library(dplyr)
#library(readr)
library(lubridate)
#library(geosphere)

#to remove all variables when needed
#rm(list=ls())

#Data Loading ---- 

testData1 <- read.csv(file = "raw/testData/PostProx1.csv", header = TRUE)
testData1 <- transform(testData1, ID = 30380)
 
testData2 <- read.csv(file = "raw/testData/PostProx2.csv", header = TRUE)
testData2 <- transform(testData1, ID = 30381)

combTest <- testData1[1,]
combTest$Stop <- testData1$Stop[2] 
combTest$Session <- combTest$Session +testData1$Session[2]
#Working Tests ----

test <- rbind(testData1, testData2)

newTest <- subset(test, Session != 0) 

combTest <- newTest[1,]
copyFrame <- newTest[0,]

for(i in 1:(nrow(newTest) - 1))
{
  # if(newTest$Proximity[i] == newTest$Proximity[i + 1] & newTest$Stop[i] == newTest$Start[i + 1])
  # {
  #   combTest <- newTest[i, ]
  #   combTest$Stop <- newTest$Stop[i + 1]
  #   combTest$Session <- combTest$Session + newTest$Session[i + 1]
  #   i <- i + 1
  # }
  # else
  {
    copyFrame <- newTest[i,]
  }
  combTest <- cbind(combTest, copyFrame)
}


#Functions ----
mstConversion <- function(data)
{
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

indexCopyFrame <- function(dataInput, index)
{
  #set the same structure as the input dataframe
  copyFrame <- dataInput[0, ]
  
  #runs the length of the index vector
  for(i in 1:length(index))
  {
    copyFrame <- rbind(copyFrame, dataInput[index[i],])
  }
  
  return(copyFrame)
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
  #2020-07-13 for pre/post switch over
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
      
      holdFrame <- data.frame(rowDate, expectedFix, Fix, noFix, earlyFix, lateFix, perNo, perMiss)
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    
    
  }
  
  colnames(copyFrame) <- c("Date", "Expected_Fixes", "On_Time_Fix", "No_fix", "Early_Fix", "Late_Fix", "Percent_No_Fix_to_Total", "Per_Missing__to_Expected_Fix")
  return(copyFrame)
}

speedFromGPS <- function(p1, p2)
{
  p1GPS <- p1 %>% select(Latitude, Longitude)
  p2GPS <- p2 %>% select(Latitude, Longitude)
  
  p1GPS <- p1GPS[c('Longitude', "Latitude")]
  p2GPS <- p2GPS[c('Longitude', "Latitude")]
  
  distance <- distGeo(p1GPS, p2GPS)
  
  time <- as.numeric((p1$GMT - p2$GMT), units ="secs")
  
  speed <- distance / differenceTime
  
  return(speed)
}

acclPrep <- function(index)
{
  testList <- list.files("raw/accl/post")
  counter <- 0 
  
  for(i in 1:length(testList))
  {
    if(substr(testList[i], 9, 10) == index) {
      
      filePath <- paste('raw/accl/post/', testList[i], sep="") 
      
      holdFrame <- read.csv(file = filePath, header = TRUE)
      
      if(counter == 0) {
        copyFrame <- holdFrame
        
      }
      else {
        copyFrame <- rbind(copyFrame, holdFrame)
      }
      
      counter <- counter + 1
      
    }
  }
  
  data <- mstConversion(copyFrame)
  
  data$X <- data$X * 0.31392 
  data$Y <- data$Y * 0.31392 
  data$Z <- data$Z * 0.31392 
  
  return(data)
}

proxPrep <- function(index, inputID)
{
  testList <- list.files("raw/prox/post")
  counter <- 0 
  
  for(i in 1:length(testList))
  {
    if(substr(testList[i], 9, 10) == index) {
      
      filePath <- paste('raw/accl/post/', testList[i], sep="") 
      
      holdFrame <- read.csv(file = filePath, header = TRUE)
      
      if(counter == 0) {
        copyFrame <- holdFrame
        
      }
      else {
        copyFrame <- rbind(copyFrame, holdFrame)
      }
      
      counter <- counter + 1
      
    }
  }
  
  data <- mstConversion(copyFrame)
  
  data <- transform(data, ID = inputID)
  
  return(data)
}
