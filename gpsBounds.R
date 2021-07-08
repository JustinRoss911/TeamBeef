#Library Loading ----

library(tidyverse)
#library(anytime)
#library(plyr)
#library(dplyr)
#library(readr)
library(lubridate)
#library(geosphere)
library(rgdal)
library(raster)
library(sp)
#to remove all variables when needed
#rm(list=ls())
library(rgeos)

source("loadData.R")

#Testing Loading Code ---- 
testFile <- loadData("gps", "gps/pre")
testFile <- testFile[[1]]

testGPS <- testFile[c('Longitude', "Latitude")]


directoryPath <- paste("raw/", "bounds/polygons/pen1", "/", sep="")
fileList <- list.files(directoryPath)
filePath <- paste(directoryPath, fileList[6], sep="")

pen1 <- shapefile(filePath)

directoryPath <- paste("raw/", "bounds/polygons/pen2", "/", sep="")
fileList <- list.files(directoryPath)
filePath <- paste(directoryPath, fileList[6], sep="")

pen2 <- shapefile(filePath)

directoryPath <- paste("raw/", "bounds/polygons/pen3", "/", sep="")
fileList <- list.files(directoryPath)
filePath <- paste(directoryPath, fileList[6], sep="")

pen3 <- shapefile(filePath)


#Testing Working Code ---- 
CRS.new<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")  #EPSG:102003
testCRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(testGPS) <- testCRS 
proj4string(pen2) <- CRS.new 


crs_wgs84 <- CRS("EPSG:4326") # WGS84 has EPSG code 4326

t <- over(testGPS, pen1)


plot(t, col="#f2f2f2", bg="skyblue", lwd=0.25)
