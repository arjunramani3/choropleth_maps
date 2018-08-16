#FIFTY STATE MAKER

#####README#####
#This file creates the spatial dataframe to get all 50 states
#contiguous + HI + AK
#If you only want the 48 contiguous, forget this file
#To use this file, literally just highlight and run everything

# install then use the following packages: maptools, rgeos, rgdal, dplyr
library(maptools)
library(rgeos)
library(rgdal)
library(dplyr)
library(mapproj)

#This function applies the appropriate transformations to the Alaska and Hawaii boundaries 
#in order to display them on the same map as the contiguous United States
transform_state <- function(object, rot, scale, shift) {
  object %>% elide(rotate = rot) %>%
    elide(scale = max(apply(bbox(object), 1, diff)) / scale) %>%
    elide(shift = shift)
}

#This function outputs a dataframe with the appropriate coordinates for plotting the 50 states
make_fifty_state <- function() {
  #state shape file from
  # http://www.arcgis.com/home/item.html?id=f7f805eb65eb4ab787a0a3e1116ca7e5
  loc <- file.path(tempdir(), "stats_dat")
  unzip(system.file("extdata", "states_21basic.zip", package = "fiftystater"),
        exdir = loc)
  fifty_states_sp <- readOGR(dsn = loc, layer = "states", verbose = FALSE) %>%
    spTransform(CRS("+init=epsg:2163"))
  
  alaska <- fifty_states_sp[fifty_states_sp$STATE_NAME == "Alaska", ] %>%
    transform_state(-35, 2.5, c(-2400000, -2100000))
  proj4string(alaska) <- proj4string(fifty_states_sp)
  
  hawaii <- fifty_states_sp[fifty_states_sp$STATE_NAME == "Hawaii", ] %>%
    transform_state(-35, .75, c(-1170000,-2363000))
  proj4string(hawaii) <- proj4string(fifty_states_sp)
  
  fifty_states <-
    fifty_states_sp[!fifty_states_sp$STATE_NAME %in% c("Alaska","Hawaii"), ] %>%
    rbind(alaska) %>%
    rbind(hawaii) %>%
    spTransform(CRS("+init=epsg:4326")) %>%
    fortify(region = "STATE_NAME") %>%
    mutate(id = tolower(id))
  return(fifty_states)
}

#END FIFTY STATE MAKER