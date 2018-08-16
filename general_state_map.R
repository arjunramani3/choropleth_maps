#####README######
#To use this file:
#1. import the below packages
#2. read in a csv with your data
#3. obtain the fifty_states dataframe by running fifty_state_maker.R
#4. call the state_map function with the 3 arguments as specified

#####Section 1.#####
#install the below packages if any/all don't load successfully
#and then reload

library(ggplot2)
library(fiftystater)
#library(extrafont)
library(dplyr)
library(RColorBrewer)

#####Begin Section 2.#####
#import csv with fips codes and ind to plot
#replace the file path below with your path
#First column should contain all state names fully spelled out
myData <- read.csv("~/Documents/poverty_rates2.csv", header=FALSE)
#####Begin Section 3.#####
#obtain the fifty_states dataframe by calling the make_fifty_state()
#method from the fifty_state_maker.R file.
#NOTE: You must run the entire fifty_state_maker.R file before running this line
fifty_states <- make_fifty_state() # this line is optional due to lazy data loading
colnames(fifty_states)[6] = 'region';


abbs <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv" , header = TRUE)

#The state_map map function takes as input:
# data, a csv with 2 or more column, 1 column with state names
#   and at least 1 other column with values for shading
# states, the shape file of the states
# abbs, a csv with the state names and the corresponding abbreviations
# ind, the index of the column with the ind to shade
#and outputs a graduated colors map with the appropriate shading
#NOTE: The colors, title, and other labels can be changed
#      Read through the comments to find the portion of the code
#      to change theese attributes

state_map <- function(data, states, abbs, ind) {
  colnames(data)[ind] = 'heat'
  colnames(data)[1] = 'region'; 
  data$region <- tolower(data$region)
  
  #convert abbrevations to mergeable format
  colnames(abbs) = c('region', 'abb')
  abbs$region <- tolower(abbs$region)
  
  df <- merge(states, data, by = "region")
  df <- df[order(df$order), ]
  df <- merge(df, abbs, by = "region")
  
  #Make labels dataframe and update locations
  lab <- df %>% group_by(abb) %>% summarise_at(c("heat"), mean)
  #NOTE: the next line creates the labels that will be displayed
  #Change the digits field to change the number of decimal points shown
  lab$comb <- paste(lab$abb, "\n", round(lab$heat, digits=1), sep = "")
  #Update label locations
  lab_locs <- read.csv("https://raw.githubusercontent.com/arjunramani3/choropleth_maps/master/label_locs.csv")
  lab <- merge(lab, lab_locs, by = 'abb')
  #change the name field if you want a different color 
  col_set = brewer.pal(n=8, name='Reds')
  ggplot(df, aes(x=long,y=lat,group=group))+
    geom_polygon(aes(fill=heat))+
    geom_path()+ 
    scale_fill_gradientn(colours=col_set, na.value="grey90")+
    coord_map()+
    theme(rect = element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank(),
          axis.ticks = element_blank(), axis.text.x = element_blank(), 
          axis.text.y = element_blank())+
    geom_text(data=lab, aes(x=long, y=lat, group=NA, label=comb), 
              size=2.7, vjust=0.5, hjust=0.5)+
    labs(title = "Title", 
         caption="Caption",
         fill = "Legend")
}

#The change_lab_locs helper function simply updates the state captions
#so that the appropriate state have single-line captions
change_lab_locs <- function(labels) {
  changes <- c(7,8,9,20,21, 28,32,40,43)
  for (x in changes) {
  labels$comb[x] <- paste(labels$abb[x], round(labels$heat[x], digits=0),  sep = " ")
  }
  return(labels)
}

#####Begin Section 4.#####
#replace the number in the third argument with your index
state_map(myData, fifty_states, abbs, 2)
