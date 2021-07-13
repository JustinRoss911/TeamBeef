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
coordinates(testGPS) <- cbind(testGPS$Longitude, testGPS$Latitude)

directoryPath <- paste("raw/", "bounds/test", "/", sep="")
fileList <- list.files(directoryPath)
filePath <- paste(directoryPath, fileList[6], sep="")

shape <- readOGR(dsn = directoryPath, layer = "CE_1")


#Testing Working Code ---- 
# CRS.new<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")  #EPSG:102003
# testCRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# proj4string(testGPS) <- testCRS 
# proj4string(pen2) <- CRS.new 
# 
# 
# crs_wgs84 <- CRS("EPSG:4326") # WGS84 has EPSG code 4326
# 
# t <- over(pen3, testGPS)
# 
# t <- t[!is.na(t$Shape_Leng) | !is.na(t$Shape_Area),]
# 
# pen1WG <- spTransform(pen1, CRS("+proj=longlat +datum=WGS84"))
# proj4string(testGPS) <- proj4string(pen1WG)
# 
# plot(pen1)

directoryPath <-"raw/bounds/test"
shape <- readOGR(dsn = directoryPath, layer = "CE_1")

testGPS <- testFile[[1]]
testGPS <- testGPS[c('Longitude', "Latitude")]
testGPS <- testGPS[1:2000, ]

coordinates(testGPS) <- cbind(testGPS$Longitude, testGPS$Latitude)
proj4string(testGPS) <- proj4string(shape)


t <- over(testGPS, shape)
test <- t[!is.na(t$Shape_Leng),]


plot(testGPS)
plot(shape)
