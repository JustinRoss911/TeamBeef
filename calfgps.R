#> -- Packages --
library(tidyverse)
library(lubridate)
library(geosphere)

#> -- Clear Workspace --
rm(list=ls())

#> -- Source Files --
source("loadData.R")

#> -- Load Raw GPS Data --
GPSraw <- loadData("gps", "raw/calfgps")
GPSraw <- timeDifference(GPSraw)

#> -- Filter GPS Data Collected Outside the Start to End of Trial Period --
GPScalendered <- filterCalendar(GPSraw)

# determining the # of extraneous data points by finding the difference in total data points
# raw_data - calendar_filtered_data = number_extraneous_data
CollarID <- character(0)
for (i in 1:length(GPSraw)) {
  prefix <- "ID"
  fileID <- as.factor(eventCalendar$Collar_ID[i])
  CollarID[i] <- paste(prefix, fileID, sep="_")
}
Extraneous <- numeric(0)
for (i in 1:length(GPSraw)) {
  Extraneous[i] <- nrow(GPSraw[[CollarID[i]]]) - nrow(GPScalendered[[CollarID[i]]])
}
findings <- data.frame(CollarID, Extraneous)
write.csv(findings, "output/Extraneous.csv", row.names = FALSE)

#> -- GPS Fix Check --
GPSfix <- gpsFixCheck(GPScalendered)


GPSfiltered[["ID_955"]][as.Date(GPSfiltered[["ID_955"]]$DateTime) == as.Date("27may2021", "%d%b%Y")]
copyFrame <- GPSfiltered[["ID_955"]]
copyFrame <- copyFrame[as.Date(copyFrame$DateTime) == as.Date("27may2021", "%d%b%Y")]

#> -- Filter Erreneous Dates --
GPSerroneous <- extractErronousDates(GPSfiltered)
GPSfiltered <- filterErronousDates(GPSfiltered)

#> -- Filter Zero Coordinate Data --
GPSzeros <- extractZeroGPS(GPSfiltered)
GPSfiltered <- filterZeroGPS(GPSfiltered)

#> -- Filter Out of Bounds --

#> -- All Filters Applied Together --
GPSraw <- loadData("gps", "raw/calfgps")
GPSraw <- timeDifference(GPSraw)
GPScalendered <- filterCalendar(GPSraw)

#> -- Functions --

# Calendar Loading Function
# This function loads a .csv file into a dataframe containing 4 columns which provide information about collar operation time periods
# Before using this function ensure there is a .csv file formatted correctly in the respective directory path
# Column1 = Collar_On, Column2 = Collar_Off, Column3 = Collar_ID, Column4 = Calf_ID
# NOTE: Collar_On and Collar_Off should still be in MDT not GMT
loadCalendar <- function()
{
  directoryPath <- paste("data/", "calendar", "/", sep="")
  fileList <- list.files(directoryPath)
  filePath <- paste(directoryPath, fileList[1], sep="") 
    
  events <- read.csv(filePath, stringsAsFactors = FALSE)
  events$Collar_On <- as.POSIXct(events$Collar_On)
  events$Collar_Off <- as.POSIXct(events$Collar_Off)
  
  return(events)
}

# Calendar Assignment Function
# This function takes raw data in and formats in two ways. Firstly all points outside the onTime-offTime interval are removed.
# Secondly it adds the CalfID column.
filterCalendar <- function(dataIn)
{
  eventCalendar <- loadCalendar() # dataframe containing all collars and their opertion times
  eventList <- list() # defining a null list
  
  
  for(i in 1:nrow(eventCalendar))
  {
    # assinging vectors to respective columns
    onTime <- eventCalendar$Collar_On[i]
    offTime <- eventCalendar$Collar_Off[i]
    fileID <- as.factor(eventCalendar$Collar_ID[i])
    calfID <- as.factor(eventCalendar$Calf_ID[i])
    
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- dataIn[[varname]] # set copy dataframe to dataframe in list
    copyFrame <- copyFrame[copyFrame$DateTime > onTime & copyFrame$DateTime < offTime, ] # select all points within onTime and offTime interval
    copyFrame <- GPSraw[["ID_955"]]
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0) # creates dataframe titled "ID_Collar#" in List. Additionally, adds CalfID column to dataframe
    {
      copyFrame <- transform(copyFrame, CalfID = calfID)
      
      logNames <- names(eventList) == varname
      listSum <- sum(logNames)
      
      if(listSum > 0)
      {
        eventList[[varname]] <- rbind(List[[varname]], copyFrame)
      }
      else
      {
        eventList[[varname]] <- copyFrame
      }
    }
    
  }
  
  return(eventList)
}

# GPS Zero Extract
# This function takes in a list of GPS dataframes and extracts all cases of Zero coordinates from each dataframe
extractZeroGPS <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- copyFrame[copyFrame$Latitude == 0 | copyFrame$Longitude == 0 | copyFrame$Altitude == 0,] # finding all cases were either 3 coordinates is equal to 0
    
    logNames <- names(dataList) == varname
    listSum <- sum(logNames)
    
    # inserting copyFrame dataframe into dataList
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


# GPS Zero Filter
# This function removes all zero coordinate values from list of GPS dataframes
filterZeroGPS <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- copyFrame[copyFrame$Latitude != 0 | copyFrame$Longitude != 0 | copyFrame$Altitude != 0,] # puts all instances that dont have a zero coordinate in copyFrame
    
    logNames <- names(dataList) == varname
    listSum <- sum(logNames)
    
    # inserting copyFrame dataframe into dataList
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

#this kind of works but there is a lot that it may be missing. 
#should look into a little more

# GPS Erroneous Dates Extract
# This function extracts all 
extractErronousDates <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    if(nrow(copyFrame) > 0)
      copyFrame <- copyFrame[copyFrame$TimeDifference < 0 | copyFrame$TimeDifference >= 3600,] # selects all rows which have a 
    
    
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

filterErronousDates <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    if(nrow(copyFrame) > 0)
      copyFrame <- copyFrame[copyFrame$TimeDifference >= 0 & copyFrame$TimeDifference < 3600,]
    
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

# Fix Check Function
# This function determines the number of missing scheduled fixes
gpsFixCheck <- function(dataIn)
{
  eventCalendar <- loadCalendar()
  dataList <- list()
  
  for(j in 1:nrow(eventCalendar))
  {
    # outputFrame is the dataframe we want to return in the end
    outputFrame <- data.frame(Dates = character(0), Expected_Fixes = numeric(0), On_Time_Fix = numeric(0), Missing_Fix = numeric(0), CollarID = numeric(0), CalfID = numeric(0))
    
    start <- eventCalendar$Collar_On[j] # Start date-time for specific collar
    end <- eventCalendar$Collar_Off[j] # End date-time for specific collar
    
    expectedFixes <- data.frame(dates = seq(start, end, by = 300)) # creates dataframe with the number of expected fixes, 1 fix every 5mins (300sec)
    dateSequence <- data.frame(dates = seq(as.Date(start), as.Date(end), by="days"))
    
    CollarID <- as.factor(eventCalendar$Collar_ID[j])
    CalfID <- as.factor(eventCalendar$Calf_ID[j])
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyFrame <- GPSfiltered[["ID_955"]]
    for(i in 1:nrow(dateSequence))
    {
      dateFrame <- copyFrame[as.Date(copyFrame$DateTime) == dateSequence$dates[i], ] # returns all GPS fixes on the specified day
      expFix <- length(expectedFixes[as.Date(expectedFixes$dates) == dateSequence$dates[i], ] ) # returns number of expected fixes on specific day
      
      date <- dateSequence$dates[i]
      
      if(is.null(copyFrame))
      {
        fix <- 0 
        early <- 0 
        late <- 0 
        totalfix <- 0
        missing <- expFix
      }
      else if(nrow(copyFrame) == 0)
      {
        fix <- 0 
        early <- 0 
        late <- 0 
        totalfix <- 0
        missing <- expFix
      }
      else if(nrow(copyFrame) > 0)
      {
        fix <- nrow(dateFrame[dateFrame$TimeDifference >= 300 & dateFrame$TimeDifference <= 370, ])
        early <- nrow(dateFrame[dateFrame$TimeDifference < 300, ])
        late <- nrow(dateFrame[dateFrame$TimeDifference > 370, ])
        totalfix <- fix + early + late
        missing <- expFix - totalfix
      }
      
      holdFrame <- data.frame(date, expFix, totalfix, missing, CollarID, CalfID)
      outputFrame <- rbind(outputFrame, holdFrame)
    }
    
    outputFrame <- outputFrame[as.Date(outputFrame$date) >= "2021-05-27" & as.Date(outputFrame$date) <= "2021-06-17", ]
    outputFrame <- arrange(outputFrame, date)
    
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

# Time Difference Function
# This function creates an additional column with a time difference value. This value is representative of the time difference between the previous row and the current row.
# Example, Row3 Time difference is Row3_Time - Row2_Time
timeDifference <- function(dataIn)
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[i])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    timeFirst <-  copyFrame$DateTime[1:(nrow(copyFrame) - 1)]
    timeSecond <- copyFrame$DateTime[2:nrow(copyFrame)]
    
    
    # Time Difference in Seconds
    timeFirst <- as.numeric(timeFirst) # point_n
    timeSecond <- as.numeric(timeSecond) # point_n+1
    
    time <- timeSecond - timeFirst # determines difference between n and n+1
    time <- c(0,time) # since the first row value cant be compared it is defines as null case which wont be filtered out
    copyFrame <- copyFrame[1:nrow(copyFrame), ]
    
    copyFrame <- cbind(copyFrame, TimeDifference = time)
    #copyFrame$DateTime <- date(copyFrame$DateTime)
    
    #Loading Into List
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

# Speed Function
# This function creates an additional column highlighting the travel speed from point_n to point_n+1
speed <- function(dataIn)
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      fileID <- levels(copyFrame$CollarID[1])
      prefix <- "ID"
      varname <- paste(prefix, fileID, sep="_")
      
      p1 <-  copyFrame[1:(nrow(copyFrame) - 1), ] # copies data from all coumns except for last row
      p2 <- copyFrame[2:nrow(copyFrame), ] # copies data from all columns except for row 1
      
      # Determining Time Difference in Seconds
      p1t <- as.numeric(p1$DateTime)
      p2t <- as.numeric(p2$DateTime)
      
      time <- p1t - p2t
      
      # Deteriming Speed
      p1s <- p1[c("Longitude", "Latitude")]
      p2s <- p2[c("Longitude", "Latitude")]
      
      distance <- distGeo(p1s, p2s) # calculates distance between two points in meters
      speed <- abs(distance / time)
      
      copyFrame <- cbind(copyFrame, Speed = speed)
      copyFrame$DateTime <- date(copyFrame$DateTime)
      
      # Loading Into List
      
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
    
    else if(nrow(copyFrame) == 0)
    {
      varname <- names(dataIn[i])
      
      copyFrame <- cbind(copyFrame, Speed = numeric(0))
      
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
  }
  
  return(dataList)
}

#GPS Out of Bounds ----
filterOutBounds <- function(dataIn)
{
  dataList <- list()
  movementCalendar <- loadMovement()
  
  shapes <- loadShapeFiles("raw/bounds")
  sharedCRS <- proj4string(shapes[[1]])
  
  gpsCoords <- loadGPSCoordsList(dataIn, sharedCRS)
  
  for(j in 1:nrow(movementCalendar))
  {
    copyCalendar <- movementCalendar[j, ]
    
    CollarID <- copyCalendar$Collar
    BullID <- copyCalendar$Bull
    
    pen <- copyCalendar$arcGISShapeFileName
    penList <- strsplit(pen, ", ")
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyCoords <- gpsCoords[[varname]]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      copyFrame <- copyFrame[copyFrame$BullID == BullID, ] 
      copyFrame <- copyFrame[copyCalendar$StartDate <= as.Date(copyFrame$DateTime) & copyCalendar$EndDate >= as.Date(copyFrame$DateTime), ]
      
      for(i in 1:length(penList[[1]]))
      {
        penName <- penList[[1]][i]
        copyShape <- shapes[[penName]]
        
        if(i == 1)
        {
          outputFrame <- over(copyCoords, copyShape)
        }
        else if(i > 1)
        {
          if("CN_SE" != penName)
          {
            holdFrame <- over(copyCoords, copyShape)
            outputFrame <- rbind(outputFrame, holdFrame)
          }
        }
      }
      
      outBounds <- outputFrame[!is.na(outputFrame$Shape_Leng) | !is.na(outputFrame$Shape_Area), ]
      
      index <- rownames(outBounds)
      
      outputFrame <- copyFrame[rownames(copyFrame) %in% index,]
      
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
  }
  return(dataList)
}

extractOutBounds <- function(dataIn)
{
  dataList <- list()
  movementCalendar <- loadMovement()
  
  shapes <- loadShapeFiles("raw/bounds")
  sharedCRS <- proj4string(shapes[[1]])
  
  gpsCoords <- loadGPSCoordsList(dataIn, sharedCRS)
  
  for(j in 1:nrow(movementCalendar))
  {
    copyCalendar <- movementCalendar[j, ]
    
    CollarID <- copyCalendar$Collar
    BullID <- copyCalendar$Bull
    
    pen <- copyCalendar$arcGISShapeFileName
    penList <- strsplit(pen, ", ")
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyCoords <- gpsCoords[[varname]]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      copyFrame <- copyFrame[copyFrame$BullID == BullID, ] 
      copyFrame <- copyFrame[copyCalendar$StartDate <= as.Date(copyFrame$DateTime) & copyCalendar$EndDate >= as.Date(copyFrame$DateTime), ]
      
      for(i in 1:length(penList[[1]]))
      {
        penName <- penList[[1]][i]
        copyShape <- shapes[[penName]]
        
        if(i == 1)
        {
          outputFrame <- over(copyCoords, copyShape)
        }
        else if(i > 1)
        {
          if("CN_SE" != penName)
          {
            holdFrame <- over(copyCoords, copyShape)
            outputFrame <- rbind(outputFrame, holdFrame)
          }
        }
      }
      
      outBounds <- outputFrame[is.na(outputFrame$Shape_Leng) | is.na(outputFrame$Shape_Area),]
      
      index <- rownames(outBounds)
      
      outputFrame <- copyFrame[rownames(copyFrame) %in% index,]
      
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
  }
  return(dataList)
}

OutBounds <- function(dataIn)
{
  dataList <- list()
  movementCalendar <- loadMovement()
  
  shapes <- loadShapeFiles("raw/bounds")
  sharedCRS <- proj4string(shapes[[1]])
  
  gpsCoords <- loadGPSCoords(dataIn, sharedCRS)
  
  for(j in 1:nrow(movementCalendar))
  {
    copyCalendar <- movementCalendar[j, ]
    
    CollarID <- copyCalendar$Collar
    BullID <- copyCalendar$Bull
    
    pen <- copyCalendar$arcGISShapeFileName
    penList <- strsplit(pen, ", ")
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyCoords <- gpsCoords[[varname]]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      copyFrame <- copyFrame[copyFrame$BullID == BullID, ] 
      copyFrame <- copyFrame[copyCalendar$StartDate <= as.Date(copyFrame$DateTime) & copyCalendar$EndDate >= as.Date(copyFrame$DateTime), ]
      
      for(i in 1:length(penList[[1]]))
      {
        penName <- penList[[1]][i]
        copyShape <- shapes[[penName]]
        
        if(i == 1)
        {
          outputFrame <- over(copyCoords, copyShape)
        }
        else if(i > 1)
        {
          if("CN_SE" != penName)
          {
            holdFrame <- over(copyCoords, copyShape)
            outputFrame <- rbind(outputFrame, holdFrame)
          }
        }
      }
      
      outBounds <- outputFrame[is.na(outputFrame$Shape_Leng) | is.na(outputFrame$Shape_Area),]
      
      index <- rownames(outBounds)
      
      outputFrame <- copyFrame[rownames(copyFrame) %in% index,]
      
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
  }
  return(dataList)
}

#Loading Shape Functions ---- 
loadShapeFiles <- function(path)
{
  directoryPath <- path
  fileList <- list.files(directoryPath, pattern = "\\.shp$")
  fileNames <- strsplit(fileList, "[.]")
  
  shapeList <- list()
  
  for(i in 1:length(fileNames))
  {
    name <- fileNames[[i]][1]
    shapeList[[name]] <- readOGR(dsn = directoryPath, layer = name)
  }
  
  return(shapeList)
}


#GPS Coordinate Change ----
loadGPSCoordsList <- function(dataIn, sharedCRS)
{
  dataNames <- names(dataIn)
  gpsCoordiantes <- list()
  
  for(i in 1:length(dataIn))
  {
    name <- dataNames[i]
    copyFrame <- dataIn[[name]]
    
    copyFrame <- copyFrame[c('Longitude', "Latitude")]
    coordinates(copyFrame) <- cbind(copyFrame$Longitude, copyFrame$Latitude)
    
    proj4string(copyFrame) <- sharedCRS
    
    gpsCoordiantes[[name]] <- copyFrame
  }
  
  return(gpsCoordiantes)
}

loadGPSCoordsFrame <- function(dataIn, sharedCRS)
{
  copyFrame <- dataIn
  
  copyFrame <- copyFrame[c('Longitude', "Latitude")]
  coordinates(copyFrame) <- cbind(copyFrame$Longitude, copyFrame$Latitude)
  
  proj4string(copyFrame) <- sharedCRS
  
  return(copyFrame)
}