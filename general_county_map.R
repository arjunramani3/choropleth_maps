#general function for county maps

#####Section 1.#####
#install the below packages if any/all don't load successfully
#and then reload
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(colorspace)
library(dplyr)
library(maptools)
library(RColorBrewer)
library(rgdal)

#####README######
#To use this file:
#1. import the above packages
#2. read in a csv with your data and the shape file from the census with the counties
#3. call the county_map function with the 3 arguments as specified

#####Begin Section 2.#####
#import csv with fips codes and ind to plot
#replace the file path below with your path
myData <- read.csv("C:\\Users\\WHIT\\Downloads\\voting_cars.csv")

#Read in shapefile of counties 
#location is https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
#replace the dsn and layer below the your path
myFile <- readOGR(dsn = "C:\\Users\\WHIT\\Downloads\\fipscodes\\cb_2017_us_county_500k.shp", layer = "cb_2017_us_county_500k");

#The county_map map function takes as input:
# myData, a csv with 2 or more column, 1 column with fips codes
#   and at least 1 other column with ind to shade
# file, the shape file of the counties from 
# ind, the index of the column with the ind to shade
#and outputs a graduated colors map with the appropriate shading
#NOTE: The colors, title, and other labels can be changed
#      Read through the comments to find the portion of the code
#      to change theese attributes

county_map <- function(data, file, ind) {
  colnames(data)[ind] = "heat"
  file$fips <- paste(file$STATEFP, file$COUNTYFP, sep="")
  colnames(data)[1] = "fips"
  file$fips <- as.integer(as.character(file$fips))

  #to commemorate troy durie, we name the memrged df troy2.0  
  troy2.0 <- merge(file, data, by="fips")
  troy2.0$STATEFP <- as.integer(as.character(troy2.0$STATEFP))
  
  #eliminate random states that aren't in the contiguous US
  troy2.0 <- troy2.0[troy2.0$STATEFP < 57, ]
  elim <- c(2,3,7,14,15,43,52)
  troy2.0 <- troy2.0[!(troy2.0$STATEFP %in% elim), ]
  
  #transform spatial polygons data frame into ggplot compatible dataframe
  #this sounds confusing but is basically just flattening the lat/long coordinates of all the states
  troy2.0$id <- rownames(as.data.frame(troy2.0))
  troy2.0.pts <- fortify(troy2.0, region = 'id')
  troy3.0 <- merge(troy2.0.pts, troy2.0, by = 'id', type = 'left')
  
  #use qt if you want a fixed number of colors
  troy3.0$qt <- cut(troy3.0$heat, 7)
  #change the name field if you want a different color 
  col_set = brewer.pal(n=8, name='Reds')
  
  ggplot(troy3.0, aes(x=long,y=lat,group=group))+
    geom_polygon(aes(fill=heat))+
    geom_path()+ 
    scale_fill_gradientn(colours=col_set, na.value="grey90")+
    coord_map()+
    theme(rect = element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank(),
          axis.ticks = element_blank(), axis.text.x = element_blank(), 
          axis.text.y = element_blank())+
    labs(title = "Title", 
         caption="Caption",
         fill = "Legend")
}

#####Begin Section 3.#####
#replace the number in the third argument with your index
county_map(myData, myFile, 4)
