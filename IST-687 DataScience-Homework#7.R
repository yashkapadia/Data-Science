#IST 687 - Introduction to Data Science
#Yash Kapadia
#Homework#6- Visualize Median Income on map
#Date of assignment due Wednesday,October 17, 2018
#Date in which the assignment is submitted is October 17, 2018.


#Step A: Load and Merge datasets
#1)	Read in the census and the USArrests datasets and merge them. 
MyCleanDataframe <- function(){
  states <- read.csv(file ="states.csv")
  
  states<-states[-1,] # deleting 1st row
  
  
  states <- states[-52,]# deleting last row
  
  
  
  states <-states[-1:-4]#deleting 1st 4 columns
  
  
  
  colnames(states) <- c("stateName","population","popOver18","percentOver18")#changing column names
  str(states)
  return(states)
}
states <- MyCleanDataframe()
View(states)

arrests <- USArrests

arrests$stateName <- row.names(arrests) #adding the name of the column 
mergedf <- merge(arrests,states,by="stateName")
View(mergedf)

#installing the required packages
install.packages("maps")
library(maps)

install.packages("ggplot2")
library(ggplot2)

install.packages("ggmap")
library(ggmap)

#Create a new Data frame that has the area of each state (state.area), 
#and the center of each state (state.center), and then merge (by stateName) it with your final data frame in step #1. 
stateName <- state.name
area <- state.area
center <- state.center
new.frame <- data.frame(stateName,area,center) # creation of new data frame

mergedf <- merge(mergedf,new.frame,by="stateName") # merging it with final data frame 
View(mergedf)

mergedf$stateName <- tolower(mergedf$stateName) #because ggplot,ggmap supports only lower case
View(mergedf)

us <- map_data("state")
View(us)

#Step B: Generate a color coded map
#3)	Create a color coded map, based on the area of the state 
map.color <- ggplot(mergedf,aes(map_id=stateName)) # defining the aesthetics 
map.color <- map.color + geom_map(map=us,aes(fill=mergedf$area)) # using the geom_map method and filling it with state area
map.color <- map.color + expand_limits(x=us$long,y=us$lat) # setting the cordinates for the map
map.color <- map.color + coord_map() + ggtitle("Basic Map") # plotting the map and giving a title
map.color

#Step C: Create a color shaded map of the U.S. based on the Murder rate for each state 
#4)	Repeat step B, but color code the map based on the murder rate of each state.
map.color1 <- ggplot(mergedf,aes(map_id=stateName)) # defining the aesthetics 
map.color1 <- map.color1 + geom_map(map=us,aes(fill=mergedf$Murder)) # using the geom_map method and filling it with Murder rate
map.color1 <- map.color1 + expand_limits(x=us$long,y=us$lat)  # setting the cordinates for the map
map.color1 <- map.color1 + coord_map() + ggtitle("Basic Map-Murder rate") # plotting the map and giving a title
map.color1

#5)	 Show the population as a circle per state (the larger the population, the larger the circle), 
#using the location defined by the center of each state
map.color2 <- ggplot(mergedf,aes(map_id=stateName)) # defining the aesthetics 
map.color2 <- map.color2 + geom_map(map=us,aes(fill=mergedf$Murder)) # using the geom_map method and filling it with Murder rate
map.color2 <- map.color2 + expand_limits(x=us$long,y=us$lat) # setting the cordinates for the map
map.color2 <- map.color2 + geom_point(aes(x=x,y=y,size=mergedf$population)) #plotting the points as circle per state using the center
map.color2 <- map.color2 + coord_map() + ggtitle("Basic Map-population") # plotting the map and giving a title
map.color2

#Step D: Zoom the map
#6)	Repeat step C, but only show the states in the north east
latlon <- geocode(source ="dsk","nyc, new york, ny") #getting the cordinates for New York City
latlon

mapzoom <- ggplot(mergedf, aes(map_id =stateName)) # defining the aesthetics 
mapzoom <- mapzoom + geom_map(map = us, aes(fill=mergedf$Murder))# using the geom_map method and filling it with Murder rate
mapzoom <- mapzoom + geom_point(aes(x=mergedf$x,y=mergedf$y,size=mergedf$population))
mapzoom <- mapzoom + xlim(latlon$lon-10,latlon$lon + 10) + ylim(latlon$lat-10, latlon$lat+10) #zooming the map using Xlim and Ylim function
mapzoom <- mapzoom + coord_map() + ggtitle("Map Zoom - NYC") # plotting the map and giving a title
mapzoom






