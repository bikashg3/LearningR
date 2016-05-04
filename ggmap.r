# Lab 02 Urban Policy: Load a base map

# install package of ggmap
install.packages("ggmap")

# load package
library(ggmap)
library(ggplot2)

# function of get map
map <- get_map(location="syracuse ny")

# get map by using location name and longitude/latitude coordinates
mapgg <- get_map(location = c(lon = -76.172645, lat = 43.017725))

# get visual map
ggmap(mapgg)

# source osm:open street map or use google map as "google"
map <- get_map( location="syracuse university ny", source="osm" )

# zoom parameter
# 21 is too detail, can not see overall sight
map <- get_map( location="syracuse university",
                source="google",
                zoom=21 )

map <- get_map( location="syracuse university",
                source="google",
                zoom=18 )

map <- get_map( location="syracuse university ny",
                source="google",
                zoom=15 )

map <- get_map( location="syracuse university",
                source="osm",
                zoom=18 )
ggmap(map)


# maptype = c(“terrain”, “satellite”, “roadmap”, “hybrid”, “toner”, “watercolor”)
# Google provides four types of maps (terrain, satellite, roadmap and hybrid), 
# whereas Stamen Maps provides three (terrain, watercolor, toner). 
# OSM has only one template.

ggmap( get_map( location="baltimore", source="google", maptype="terrain", zoom=13 ) )

ggmap( get_map( location="baltimore", source="google", maptype="satellite", zoom=13 ) )

ggmap( get_map( location="baltimore", source="google", maptype="hybrid", zoom=13 ) )

# Creating black and white maps, color parameter

ggmap( get_map( location="baltimore", source="google", maptype="terrain", zoom=13, color="bw" ) )

ggmap( get_map( location="baltimore", source="google", maptype="satellite", zoom=13, color="bw" ) )

ggmap( get_map( location="baltimore", source="google", maptype="hybrid", zoom=13, color="bw" ) )

# quotes
# specific option(quote) vs. an object(not quote)
map <- get_map( location="boston" )
ggmap( map )

# lab 02 case study city: boston
ggmap( get_map( location=" boston", 
                source="google", 
                maptype="terrain", zoom=13) )

# preview about lab 04
library(ggplot2)
syracuse <- get_map(  
  location="syracuse ny", 
  zoom = 16, 
  # color="bw"        
) 
  
syr.map <- ggmap( 
  syracuse, 
  extent = "device"                  
  ) 
  
# transfer location name to longitude and latitude coordinates 
nobhill <- geocode("nobhill appartment syracuse ny")
  
# add point to the basemap  
syr.map + geom_point( 
data=nobhill, 
  aes(x=lon, y=lat), 
  size=20, # control the point size
  col="orange", # point color 
  alpha=0.4 # transparent level
  )

