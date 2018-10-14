
##################################################################
# Use this block comment at the top of each R code file you submit
# IST 687 Homework 7 – Submitted by Ryan French on October 18, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

#import necessary libraries
library(tibble)
library(dplyr)
library(ggplot2)
library(ggmap)



#STEP A
#read in census dataset
#URL for population data
urlToRead <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
#read in data from URL
urlToRead <- url(urlToRead)
dfStates <- read.csv(urlToRead, stringsAsFactors = FALSE)

#Remove unnecesary rows
dfStates <- dfStates[-c(1, 53),]
#Remove unnecesary columns
dfStates <- dfStates[,-c(1:4)]

#utilize colnames to change the names of columns in dfStates
cnames <- colnames(dfStates)
cnames[1] <- "stateName"
cnames[2] <- "population"
cnames[3] <- "popOver18"
cnames[4] <- "percentOver18"

colnames(dfStates) <- cnames
colnames(dfStates)

#copy USArrests dataset to local variable
arrests <- USArrests
#convert row names of states to new column for merging
arrests <- arrests %>% rownames_to_column("stateName")

#merge the two data frames
common_col_names <- intersect(names(arrests), names(dfStates))
allStateData <- merge.data.frame(arrests, dfStates, by=common_col_names, all.x=TRUE)



#create data frame with the state data from gg map
stateStats <- data.frame(state.name, state.center, state.area)

#rename column names to make more sense
cnames <- colnames(stateStats)
cnames[1] <- "stateName"
cnames[2] <- "x_center"
cnames[3] <- "y_center"
cnames[4] <- "area"

colnames(stateStats) <- cnames
colnames(stateStats)

#merge data sets off of interesting column
common_col_names <- intersect(names(allStateData), names(stateStats))
allStateData <- merge.data.frame(allStateData, stateStats, by=common_col_names, all.x=TRUE)



#assign variable to US map
us <- map_data("state")

#convert all state names to lowercase
allStateData$stateName <- tolower(allStateData$stateName)

#create a map of the US where color map is equal to area of the state
map.areaColor <- ggplot(allStateData, aes(map_id = stateName))
map.areaColor <- map.areaColor + geom_map(map = us, aes(fill = area))
map.areaColor <- map.areaColor + expand_limits(x = us$long, y = us$lat)
map.areaColor <- map.areaColor + coord_map() + ggtitle('state area')
map.areaColor



#STEP C
#create a map of the US where color map is equal to murder rate within the state
map.murderColor <- ggplot(allStateData, aes(map_id = stateName))
map.murderColor <- map.murderColor + geom_map(map = us, aes(fill = Murder))
map.murderColor <- map.murderColor + expand_limits(x = us$long, y = us$lat)
map.murderColor <- map.murderColor + coord_map() + ggtitle('state murders')
map.murderColor


#create a map of the US where color map is equal to murder rate within the state and the circles are equal to the population size
map.murderAndPopColor <- map.murderColor + geom_point(aes(x = allStateData$x_center, y = allStateData$y_center, size = allStateData$population, color = 'red'))
map.murderAndPopColor <- map.murderAndPopColor + ggtitle('state murders & population')
map.murderAndPopColor
           


#STEP D
#create the same map as step 3B but restricted to the North East
#attempted to get NYC geocode with API but call failed so had to get it off of google manually
#latlon <- geocode('new york city, ny')
nycLat <- 40.7128
nycLon <- -74.0060

map.murderAndPopColorNE <- map.murderAndPopColor + xlim(nycLon-10, nycLon+10) + ylim(nycLat-10, nycLat+10) + ggtitle('state murders & population (North East)')
map.murderAndPopColorNE

           
           
