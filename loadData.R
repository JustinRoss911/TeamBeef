loadData <- function(type, path)
{
  
  directoryPath <- paste("raw/", path, "/", sep="")
  fileList <- list.files(directoryPath)
  
  dataList <- list()
  
  for(i in 1:length(fileList))
  {
    filePath <- paste(directoryPath, fileList[i], sep="") 
    copyFrame <- read.csv(filePath, header = F, stringsAsFactors = FALSE)
    fileID <- as.integer(substr(copyFrame$V1[2], 13, 17))
    
    copyFrame <- copyFrame[5:nrow(copyFrame), ]
    
    if(type == "gps")
    {
      colnames(copyFrame) <- c("DateTime", "Latitude", "Longitude", "Altitude", "Duration", "Temperature", "DOP", "Satellites", "Fix")
      
     # copyFrame$DateTime <- as.POSIXct(copyFrame$DateTime)
      copyFrame$DateTime <- strptime(copyFrame$DateTime, "%Y-%m-%d %I:%M:%S %p", tz = "GMT")
    #  copyFrame$DateTime <- with_tz(copyFrame$DateTime, "America/Edmonton")
      # copyFrame$DateTime <- tempData 
      
      class(copyFrame$DateTime)
    }
    else if(type == "prox")
    {
      colnames(copyFrame) <- c("Start", "Stop", "Proximity", "Duration", "RSSI")
      
      #copyFrame$Start <- as.POSIXct(copyFrame$Start)
      #copyFrame$Stop <- as.POSIXct(copyFrame$Stop)
      
      
      copyFrame$Start <- strptime(copyFrame$Start, "%Y-%m-%d %I:%M:%S %p", tz = "GMT")
     # tempStart  <- with_tz(tempStart, "America/Edmonton")
     # copyFrame$Start <- tempStart

      copyFrame$Stop <- strptime(copyFrame$Stop, "%Y-%m-%d %I:%M:%S %p", tz = "GMT")
     # tempStop  <- with_tz(tempStop, "America/Edmonton")
    #  copyFrame$Stop <- tempStop
    }
    else if(type == "accl")
    {
      colnames(copyFrame) <- c("DateTime", "X", "Y", "Z", "Temperature")
      
      copyFrame$DateTime <- strptime(copyFrame$DateTime, "%Y-%m-%d %I:%M:%S %p", tz = "GMT")
      
    #  copyFrame$DateTime <- as.POSIXct(copyFrame$DateTime)
    #  copyFrame$DateTime <- strptime(copyFrame$DateTime, "%Y-%m-%d %H:%M:%S", tz = "GMT")
      # tempData  <- with_tz(tempDate, "America/Edmonton")
      # copyFrame$DateTime <- tempData 
    }
    
    copyFrame <- transform(copyFrame, CollarID = fileID)
    
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
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
