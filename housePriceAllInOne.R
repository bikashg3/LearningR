#### Step 00 - Install Required Packages

# setwd( "C:/Users/jdlecy/Documents/GitHub/hedonic-prices" )
# https://docs.google.com/forms/d/13RWJ9LR3mFbQBNsX9bnLt5XRotfmZKxkwetYzh-h-oA/viewform
setwd("E:/R Training/hedonic-prices-master/hedonic-prices-master")

install.packages( "RCurl" )

install.packages( "ggmap" )

install.packages( "jsonlite" )

install.packages( "memisc" )




#### Step 01 - Load Housing Data

# setwd( "C:/Users/jdlecy/Documents/GitHub/hedonic-prices" )




# load package to read URL data
library( RCurl )

# address of google spreadsheet
# shared with Professor Lecy census survy on google doc with csv
my.url <- "https://docs.google.com/spreadsheets/d/1W0vM5sCRhZjkQh6A0WGV8j1rhQAjecCrQW7PguHft-E/pub?gid=1989082857&single=true&output=csv"


housing.raw <- getURL( my.url)#, ssl.verifypeer=FALSE )

# read as text, stringAsFactors=FALSE
dat <- read.csv( textConnection(housing.raw), stringsAsFactors=FALSE )

head( dat )


# RENAME VARIABLES

names( dat ) <- c("timestamp","price","X1","X2","sqft","your.name","lot.size","beds",
                  "bath","garage","year","elementary","middle","high","walk","tax","highway",
                  "restaurant","starbucks","park","mall","address","zip","tract" )


# remove commas from numbers

dat$price <- as.numeric( gsub( ",","", dat$price ) )
dat$tax <- as.numeric( gsub( ",","", dat$tax ) )
dat$lot.size <- as.numeric( gsub( ",","", dat$lot.size ) )
dat$sqft <- as.numeric( gsub( ",","", dat$sqft ) )
# replace if the value is missing
dat$lot.size[ is.na( dat$lot.size ) ] <- mean( dat$lot.size, na.rm=T )



# clean up

rm( housing.raw )
rm( my.url )



#### Step 01.01 - Graph Relationships

# setwd( "C:/Users/jdlecy/Documents/GitHub/hedonic-prices" )




source( "Step 01 - Load Housing Data.R" )


# create plot function with desired aesthetics

plotFun <- function( x1, x2=price, lab1, lab2="House Price" )
{
  
  plot( x1, x2,
        pch=19, 
        col=gray(0.6, alpha = 0.2), 
        cex=3.5,  
        bty = "n",
        xlab=lab1, 
        ylab=lab2, cex.lab=1.5 )
  
  lines( lowess(x2~x1), col="red", lwd=3 )
  
}






# CREATE GRAPHS AND SAVE AS PDF


dir.create( "Results" ) # set up a results directory

# start to create a pdf file, end with dev.off()  # end of pdf call

pdf( "./Results/Predictors of Price.pdf" )


# HOUSE SIZE (SQFT)

plotFun( x1=dat$sqft, x2=dat$price, lab1="Size (Square Feet)", lab2="House Price" )


# LOT SIZE

plotFun( x1=dat$lot.size, x2=dat$price, lab1="Lot Size (Square Feet)", lab2="House Price" )



# AGE vs PRICE

plotFun( x1=(2014-dat$year), x2=dat$price, lab1="Age (Years)", lab2="House Price" )



# AGE vs SIZE

plotFun( x1=(2014-dat$year), x2=dat$sqft, lab1="Age (Years)", lab2="Size (Square Feet)" )



# WALK SCORE

plotFun( x1=dat$walk, x2=dat$price, lab1="Walk Score", lab2="House Price" )



# SCHOOL

school <- dat$elementary + dat$middle + dat$high

plotFun( x1=school, x2=dat$price, lab1="School Quality", lab2="House Price" )



# DIST TO RESTAURANT

plotFun( x1=dat$restaurant, x2=dat$price, lab1="Dist to Good Restaurant", lab2="House Price" )



# DIST TO STARBUCKS

plotFun( x1=dat$starbucks, x2=dat$price, lab1="Distance to Starbucks", lab2="House Price" )



# DIST TO PARK

plotFun( x1=dat$park, x2=dat$price, lab1="Dist to Park", lab2="House Price" )



# DIST TO Mall

plotFun( x1=dat$restaurant, x2=dat$price, lab1="Dist to Mall", lab2="House Price" )





plot( as.factor(dat$garage), dat$price, ylab="House Price", xlab="Garage" )
tapply( dat$price, as.factor(dat$garage), mean )


plot( as.factor(dat$bath), dat$price, ylab="House Price", xlab="Number of Bathrooms", cex.lab=1.5 )
tapply( dat$price, as.factor(dat$bath), mean )


plot( as.factor(dat$beds), dat$price, ylab="House Price", xlab="Number of Bedrooms", cex.lab=1.5 )
tapply( dat$price, as.factor(dat$beds), mean )


plot( as.factor(dat$highway), dat$price, ylab="House Price", xlab="Near a Highway?", cex.lab=1.5 )
tapply( dat$price, as.factor(dat$highway), mean )



dev.off()  # end of pdf call

#### Step 02 - Geocode House Addresses

# setwd( "C:/Users/jdlecy/Documents/GitHub/hedonic-prices" )

source( "Step 01 - Load Housing Data.R" )


houses <- dat[ , c("address","zip") ]


houses$address <- gsub( ",", "", houses$address )
houses$address <- gsub( "\\.", "", houses$address )

addresses <- paste( houses$address, "Syracuse, NY", houses$zip, sep=", " )

head( addresses )


library( ggmap )


# translate street address to latitude longitude coordinates
#
# lat.long <- geocode( addresses )
#
# takes about 5 min to run



# pre-geocoded version of dataset for demo

lat.long <- read.csv( "Data/lat.long.csv" )

head( lat.long )





syracuse <- get_map(  
  location='syracuse, ny', 
  zoom = 12, 
  color="bw"
) 



syr.map <- ggmap( 
  syracuse, 
  extent = "device"                    
) 




syr.map + geom_point( 
  data=lat.long, 
  aes(x=lon, y=lat), 
  size=2, 
  col="red", 
  alpha=1     
) 




dat <- cbind( dat, lat.long )

rm( houses )
rm( addresses )

#### Step 03 - Match House Address to Census Tract

# setwd( "C:/Users/jdlecy/Documents/GitHub/hedonic-prices" )

source( "Step 02 - Geocode House Addresses.R" )





### MATCH GEOCODED ADRESSES TO A CENSUS TRACT

# to add census data we need to associate a house with a census tract

# use census API:

# # https://transition.fcc.gov/form477/censustracts.html



require( RCurl )



tract.id <- NULL

for( i in 1:nrow(lat.long) )
{
  
  print( i )
  
  aURL <- paste( "http://data.fcc.gov/api/block/2010/find?latitude=",lat.long$lat[i],"&longitude=",lat.long$lon[i], sep="" )
  
  x <- getURL( aURL )
  
  start.here <- regexpr( "Block FIPS", x )
  
  this.one <- substr( x, (start.here+12), (start.here+26) )
  
  # FIPS: 360670040001007  36=state, 067=county, 004000=census.tract 1007=block.group
  
  tract.id[i] <- substr( this.one, 6, 11 )
  
}

# http://rfunction.com/archives/1719 #about regexpr



# combine house data with lat lon coordinates and census tract IDs

dat <- cbind( dat, tract.id )

rm( tract.id )


#### Step 04 - Download Census Data

# setwd( "C:/Users/jdlecy/Documents/GitHub/hedonic-prices" )

source( "Step 03 - Match House Address to Census Tract.R" )




### DOWNLOAD CENSUS DATA THROUGH API

# http://www.census.gov/developers/


library(RCurl)
library( jsonlite )

APIkey <- "b431c35dad89e2863681311677d12581e8f24c24"



# use this function to convert json data format to a data frame

json.to.data <- function( x )
{
  a.matrix <- fromJSON(x)  # converts json table to a matrix
  
  c.names <- a.matrix[ 1 , ]  # column names are the first row
  
  a.matrix <- a.matrix[ -1 , ]
  
  my.dat <- data.frame( a.matrix )
  
  names( my.dat ) <- c.names
  
  return( my.dat )
}



# you need to find variable codes in data dictionary:

# poverty: DP03_0119PE
# total pop: DP05_0028E
# pop black: DP05_0033E

fieldnm <- "DP03_0119PE" # poverty
state <- "36"
county <- "067"


resURL <-  paste("http://api.census.gov/data/2013/acs5/profile/?get=",fieldnm,
                 "&for=tract:*&in=state:",state,"+county:",county,"&key=",
                 APIkey,sep="")


### Fetch the data



poverty <- getURL( resURL, ssl.verifypeer = FALSE )

poverty <- json.to.data( poverty )

# tract.id2 <- paste( poverty$state, poverty$county, poverty$tract, sep="" )




fieldnm <- "DP05_0033E" # black

resURL <-  paste("http://api.census.gov/data/2013/acs5/profile/?get=",fieldnm,
                 "&for=tract:*&in=state:",state,"+county:",county,"&key=",
                 APIkey,sep="")

black <- getURL( resURL, ssl.verifypeer = FALSE )

black <- json.to.data( black )

black <- as.numeric( as.character( black[,1] ) )


fieldnm <- "DP05_0028E" # tot.pop

resURL <-  paste("http://api.census.gov/data/2013/acs5/profile/?get=",fieldnm,
                 "&for=tract:*&in=state:",state,"+county:",county,"&key=",
                 APIkey,sep="")

tot.pop <- getURL( resURL, ssl.verifypeer = FALSE )

tot.pop <- json.to.data(tot.pop)

tot.pop <- as.numeric( as.character( tot.pop[,1] ) )

prop.black <- black / tot.pop


cen.dat <- cbind( poverty, prop.black )

names( cen.dat ) <- c( "poverty", "state", "county", "tract", "prop.black" )



rm( APIkey )
rm( black )
rm( county )
rm( fieldnm )
rm( json.to.data )
rm( poverty )
rm( prop.black )
rm( resURL )
rm( state )
rm( tot.pop ) 

#### Step 05 - Count Nearby Crimes

# setwd( "C:/Users/jdlecy/Documents/GitHub/hedonic-prices" )

source( "Step 04 - Download Census Data.R" )



### HOW MANY NEARBY CRIMES

# 2014 data downloaded from:  http://www.syracuse.com/crime/police-reports/
# 
# It has been geocoded using block locations:

crime.dat <- read.csv( "Data/crime.lat.lon.csv" )

library( ggmap )

syracuse <- get_map( location='syracuse, ny', zoom = 11, color="bw" ) 

syr.map <- ggmap( syracuse, extent = "device" ) 

syr.map + geom_point( 
  data=crime.dat, 
  aes(x=lon, y=lat), 
  size=3, 
  col="steel blue", 
  alpha=0.5     
) 



# reference for distance formula: sqrt( (43.056353-43.062111)^2 + (-76.140454 - -76.128620)^2 )


crime.count <- NULL

for( i in 1:nrow(lat.long) )
{
  lat.i <- lat.long$lat[i]
  lon.i <- lat.long$lon[i]
  
  dist.c <- sqrt( (lat.i - crime.dat$lat)^2 + (lon.i - crime.dat$lon)^2 )
  
  crime.count[i] <- sum( dist.c < 0.01 )
  
}


dat <- cbind( dat, crime.count )




####   MERGE DATA


dat <- merge( dat, cen.dat, by.x="tract.id", by.y="tract" )




names( dat )

rm( lat.long )

#### Step 05.01 - Graph Demographic Predictors


# setwd( "C:/Users/jdlecy/Documents/GitHub/hedonic-prices" )

source( "Step 05 - Count Nearby Crimes.R" )




### PLOT DEMOGRAPHIC VARIABLES VS HOME PRICES



# create plot function with desired aesthetics

plotFun <- function( x1, x2=price, lab1, lab2="House Price" )
{
  
  plot( x1, x2,
        pch=19, 
        col=gray(0.6, alpha = 0.2), 
        cex=3.5,  
        bty = "n",
        xlab=lab1, 
        ylab=lab2, cex.lab=1.5 )
  
  lines( lowess(x2~x1), col="red", lwd=3 )
  
}





pdf( "Results/Demographic Factors.pdf" )


# CRIME

plotFun( x1=dat$crime.count, x2=dat$price, lab1="Num of Nearby Crimes", lab2="House Price" )



# POVERTY

pov.vec <- as.numeric( as.factor( dat$poverty ) )

plotFun( x1=pov.vec, x2=dat$price, lab1="Poverty Rate", lab2="House Price" )



# BLACK

plotFun( x1=dat$prop.black, x2=dat$price, lab1="Proportion of Population Black", lab2="House Price" )


dev.off()





#### Step 05.02 - Regressions


# setwd( "C:/Users/jdlecy/Documents/GitHub/hedonic-prices" )

source( "Step 05 - Count Nearby Crimes.R" )





dat$school <- dat$elementary + dat$middle + dat$high



m.01 <- lm( price ~ sqft + lot.size +  bath + as.factor(garage) + year + school + as.factor(highway), data=dat )  

options( scipen=6 )

summary( m.01 )



m.02 <- lm( price ~ sqft + lot.size +  bath + as.factor(garage) + year + 
              school + as.factor(highway) + crime.count + prop.black, data=dat )  

options( scipen=6 )

summary( m.02 )




library( memisc )

mtab <- mtable( 
  "Model 1"=m.01, 
  "Model 2"=m.02, 
  summary.stats=c("R-squared","N", "p"), 
  digits=2 
)

mtab

####

####

####

