# NOTE: this function can only be used with Lotek formatted .csv files

# This function loads all .csv files from a folder into a list data structure
# The type argument is a string name for the type of data you want to load ("gps", "accl", "prox")
# The path argument is a string, here you specify the path where your data is kept within the data folder of the working project
# example: "raw/calfgps", this describes the path within the data folder where the files you want to load would be in calfgps
# NOTE: you can only load one type so make sure all files within the folder are the same type of data.
loadData <- function(type, path)
{
  
  directoryPath <- paste("data/", path, "/", sep="") # takes 3 string arguments and combines them into 1 string with no space for a seperation (sep="")
  fileList <- list.files(directoryPath) # creates a character vector of files in the specified directory
  
  dataList <- list() # creates null list
  
  for(i in 1:length(fileList))
  {
    filePath <- paste(directoryPath, fileList[i], sep="") # specifies the path to the specific .csv file in directory
    print(filePath) # used to debug, checks if all files in specified directory get read
    copyFrame <- read.csv(filePath, header = F, stringsAsFactors = FALSE) # creates data frame of .csv file
    fileID <- as.integer(substr(copyFrame$V1[2], 13, 15)) # takes the number from the "product ID" cell in the data frame. 13 and 15 describes the 3 characters at the end of the string, these are the product ID numbers
    # NOTE: If using this code, change 13 and 15 respectively to account for product numbers of Lotek devices used in your study, so if the IDs are 4 characters than the numbers you would use are 13 and 16.
    # NOTE: The 15 can be any size integer like 1000 for example and it will still work, however, for formality reasons make the second integer argument 13 + length_of_#
    
    copyFrame <- copyFrame[5:nrow(copyFrame), ] # removes the first 3 rows above the column headers and removes column headers.
    
    if(type == "gps") # for gps .csv files
    {
      colnames(copyFrame) <- c("DateTime", "Latitude", "Longitude", "Altitude", "Duration", "Temperature", "DOP", "Satellites", "Fix") # renames column headers
      
      #copyFrame <- copyFrame[copyFrame$Fix == "GPS Schedule", ] # removes all Fixes not caused by GPS Schedule, this is useless for the cow-calf pair trial
      
      # below is assigning each column to the respective data type, for numbers we want them as a numeric type
      copyFrame$Latitude <- as.numeric(copyFrame$Latitude)
      copyFrame$Longitude <- as.numeric(copyFrame$Longitude)
      copyFrame$Altitude <- as.numeric(copyFrame$Altitude)
      copyFrame$Duration <- as.numeric(copyFrame$Duration)
      copyFrame$Temperature <- as.numeric(copyFrame$Temperature)
      copyFrame$DOP <- as.numeric(copyFrame$DOP)
      copyFrame$Satellites <- as.factor(copyFrame$Satellites) # this is defined as a factor to save memory, also has other benefits
      copyFrame$DateTime <- strptime(copyFrame$DateTime, "%Y-%m-%d %I:%M:%S %p", tz = "GMT") # converts date-time string to date-time data
      # NOTE: the date-time data is in GMT and not MST
      
      # class(copyFrame$DateTime) # tells you the "type" of class an object is. This is to confirm that DateTime is of class date-time "POSIXt"
    }
    else if(type == "prox") # for proximity .csv files
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
    else if(type == "accl") # for accelerometer .csv files
    {
      if(length(copyFrame) == 5)
      {
        colnames(copyFrame) <- c("DateTime", "X", "Y", "Z", "Temperature")
      }
      else if(length(copyFrame) == 4)
      {
        colnames(copyFrame) <- c("DateTime", "X", "Y", "Temperature")
      }
      
      copyFrame$X <- as.numeric(copyFrame$X) * 0.31392 
      copyFrame$Y <- as.numeric(copyFrame$Y) * 0.31392 
      
      if(!is.null(copyFrame$Z))
      {
        copyFrame$Z <- as.numeric(copyFrame$Z) * 0.31392 
      }
           
      copyFrame$Temperature <- as.numeric(copyFrame$Temperature)
      
      # ForceSum <- sqrt(copyFrame$X^2 + copyFrame$Y^2 + copyFrame$Z^2)
      # 
      # copyFrame <- cbind(copyFrame, ForceSum)
        
      copyFrame$DateTime <- strptime(copyFrame$DateTime, "%Y-%m-%d %I:%M:%S %p", tz = "GMT")
      
    #  copyFrame$DateTime <- as.POSIXct(copyFrame$DateTime)
    #  copyFrame$DateTime <- strptime(copyFrame$DateTime, "%Y-%m-%d %H:%M:%S", tz = "GMT")
      # tempData  <- with_tz(tempDate, "America/Edmonton")
      # copyFrame$DateTime <- tempData 
    }
    
    copyFrame <- transform(copyFrame, CollarID = as.factor(fileID)) # creates a collumn called "CollarID" and fills each cell with respective collar #
    
    # creates sub list name (collar ID specific to that set of data)
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    logNames <- names(dataList) == varname
    listSum <- sum(logNames)

    if(listSum > 0)
    {
      dataList[[varname]] <- rbind(dataList[[varname]], copyFrame) # concatenates following sub-list items so the previous lists are not lost.
    }
    else
    {
      dataList[[varname]] <- copyFrame # creates first sub-list item
    }
    
  }
  return(dataList) # returns list of each .csv files data
}
