#Libraries
library(readr)
library(dplyr)
library(plyr)
library(caTools)
library(caret)
# visualizations
library(plotly)
library(rgl)

# Data Loading

DataTraining <- read_csv("trainingData.csv")
DataValidation <- read.csv("validationData.csv")



###------------------ TRAINING SET -------------------------


# DATA CLEANING ####


# Removing repeated rows in DataTraining             
DataTraining <- distinct(DataTraining)              #<- 19937 to 19300

# Data Type Conversions into factor/datetime
factors<-c("FLOOR", "BUILDINGID", "RELATIVEPOSITION", "USERID", "PHONEID")
DataTraining[,factors] <-lapply (DataTraining[,factors], as.factor)
rm(factors)


# converting TIMESTAMP from unix time to more suitable one (even if this variable is yet not needed)
DataTraining <- mutate(DataTraining, Time = TIMESTAMP)
DataTraining$Time <- as.POSIXct(DataTraining$Time, origin = "1970-01-01")


# Change value of WAPS = 100 to WAPS = -120
WAPS<-grep("WAP", names(DataTraining), value=T)
DataTraining[,WAPS] <- sapply(DataTraining[,WAPS], function(x) ifelse(x==100,-120,x))
# (easier version)
# DataTraining[DataTraining==100]<- -120


# Teatment of - Near Zero Variance WAPS
WAPS_NZV_DT <- nearZeroVar (DataTraining[,1:520], saveMetrics=TRUE)

DataTraining <- DataTraining [-which(WAPS_NZV_Train$zeroVar==TRUE)]   # 529 -> 475 variables

#Deleting all rows with zero Variance
DataTraining2 <- DataTraining1[-as.numeric(which(apply(DataTraining1[,1:465], 1, var) == 0)),] 
# 19227 observations







#### Data Explorations / vizualizations ####

# dividing the 3 buildings
# Building 1, only 2 users in Building 1!
Building1 <- filter(DataTraining, BUILDINGID == 0)
Building1_11 <- filter(Building1, USERID == 11)
Building1_1 <- filter(Building1, USERID == 1)
# Building 2
Building2 <- filter(DataTraining, BUILDINGID == 1)
# Building 3
Building3 <- filter(DataTraining, BUILDINGID == 2)


# 3D Scatterplot, rgl library
plot3d(DataTraining$LONGITUDE, DataTraining$LATITUDE, DataTraining$FLOOR, col = "blue", size= 3)
# 3D line plot, plotly library
plot_ly(Building1_11, x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, type = 'scatter3d', mode = 'lines',
        line = list(color = '#1f77b4', width = 1))




str(DataTraining[,521:530])
