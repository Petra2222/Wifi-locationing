#Libraries
library(readr)
library(dplyr)
library(plyr)
library(caTools)
library(caret)
# visualizations
library(plotly)
library(rgl)


DataTraining <- read_csv("trainingData.csv")

# Data Type Conversions (for now / FLOOR and BUILDIND ID are the most pressing ones)
DataTraining$FLOOR <- as.factor(DataTraining$FLOOR)
DataTraining$BUILDINGID <- as.factor(DataTraining$BUILDINGID)

# converting TIMESTAP from unix time to more suitable one (even if this variable is yet not needed)
DataTraining <- mutate(DataTraining, Time = TIMESTAMP)
DataTraining$Time <- as.POSIXct(DataTraining$Time, origin = "1970-01-01")

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
