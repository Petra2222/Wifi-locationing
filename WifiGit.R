#Libraries
library(readr)
library(dplyr)
library(plyr)
library(caTools)
library(caret)
library(reshape)
# visualizations
library(plotly)
library(rgl)

# Data Loading

DataTraining <- read_csv("trainingData.csv")
DataValidation <- read.csv("validationData.csv")



#------------------ TRAINING SET -------------------------


# DATA CLEANING ####


# Removing repeated rows in DataTraining             
DataTraining <- distinct(DataTraining)              # 19937 to 19300

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


# Treatment of - Near Zero Variance WAPS
WAPS_NZV_DT <- nearZeroVar (DataTraining[,1:520], saveMetrics=TRUE)

DataTraining <- DataTraining [-which(WAPS_NZV_DT$zeroVar==TRUE)]   # 529 -> 475 variables

#Deleting all rows with zero Variance
DataTraining1 <- DataTraining[-as.numeric(which(apply(DataTraining[,1:465], 1, var) == 0)),] 
# 19227 observations



#------------------ Feature engineering -------------------------

# New variable - HighestWAP (will be used in Building prediction)

DataTraining1 <- DataTraining1 %>% mutate (HighestWAP=NA)

WAPS <- grep("WAP", names(DataTraining1), value=T)

DataTraining1 <- DataTraining1 %>% mutate (HighestWAP=colnames(DataTraining1[WAPS]) [apply(DataTraining1[WAPS],1,which.max)])

DataTraining1$HighestWAP <- as.factor(DataTraining1$HighestWAP)



# not part of feature engineering, but still important (will be used in Building prediction)
# EACH building has a special WAPS, which are not present in other 2 buildings

# Buildings 1,2,3
Building1 <- filter(DataTraining1, BUILDINGID == 0) # 5245 obs.
Building2 <- filter(DataTraining1, BUILDINGID == 1) # 4904 obs. 
Building3 <- filter(DataTraining1, BUILDINGID == 2) # 9078 obs.

# used WAPS in Building 1
B1WAPS <- Building1 [,c(1:465)]
DELB1 <- apply (B1WAPS, 2, function(x) length(unique(x))==1) 
    # 2 indicates columns
    # length(unique(Building1$WAP010) returns the total number of "unique" values
B1WAPS <- B1WAPS [,-c(which(DELB1==TRUE))] # change from 476 to 200 variables

# used WAPS in Building 2
B2WAPS <- Building2 [,c(1:465)]
DELB2 <- apply (B2WAPS, 2, function(x) length(unique(x))==1)
B2WAPS <- B2WAPS [,-c(which(DELB2==TRUE))] # change from 476 to 207 variables

# used WAPS in Building 3
B3WAPS <- Building3 [,c(1:465)]
DELB3 <- apply (B3WAPS, 2, function(x) length(unique(x))==1)
B3WAPS <- B3WAPS [,-c(which(DELB3==TRUE))] # change from 476 to 203 variables











#------------------ Data Explorations / vizualizations -------------------------
# part of Exploratory analysis

# Building 1, only 2 users in Building 1!
Building1 <- filter(DataTraining, BUILDINGID == 0)
# unique(Building1$USERID)
Building1_11 <- filter(Building1, USERID == 11)
Building1_1 <- filter(Building1, USERID == 1)


# Users (2x) in Building 1
 ggplot(data = Building1) + geom_point(aes(x=LONGITUDE, y= LATITUDE, color=USERID)) + 
    facet_grid(. ~ FLOOR)
 
 
 # Number of Detected WAPS
 
 WAPSDT1 <- DataTraining1[,1:465]
 totalWAPS <- c(seq(from = 0, to = 0, length.out = nrow(WAPSDT1)))
 
 for(i in 1:nrow(WAPSDT1)){
   da <-  WAPSDT1[i,] != -120
   da <- c(da)
   totalWAPS[i] <- length(WAPSDT1[i,da])
 }
 
hist(totalWAPS, breaks = 50, main= "Breaks = 50", xlab = "Number of detected WAPS")
 

 # WAPS signal strength distribution 
 
 # melting
 training_melt <- melt(WAPSDT1)
 # excluding NA (-120)
 training_melt <- subset(training_melt, value != -120)
 ggplot(training_melt, aes(value)) + geom_histogram(binwidth = 1) 


 # Most frequent USER
 plot(DataTraining1$USERID, xlab="USER NUMBER", ylab="frequency", 
      main="Most frequent User", col="blue")
  

# 3D Scatterplot, rgl library
plot3d(DataTraining$LONGITUDE, DataTraining$LATITUDE, DataTraining$FLOOR, col = "blue", size= 3)
# 3D line plot, plotly library
plot_ly(Building1_11, x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, type = 'scatter3d', mode = 'lines',
        line = list(color = '#1f77b4', width = 1))





