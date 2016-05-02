# Syracuse parcel data is located in folder of "01-05-2015"
# Syracuse cencus data is in folder of "shapefiles"


# Dowloading TIGER Shapefiles
# 
# GIS uses underlying 'shapefiles' in order to draw map borders and represent data using choropleth maps.
# 
# The interface for finding and downloading TIGER shapefiles is pretty reasonable, so this can be done directly on the site.
# 
# https://www.census.gov/geo/maps-data/data/tiger-line.html
# 
# These are big files and difficult to share, however, so it is nice to create an easily reproducible way to create the same map. 
# You can do this by searching the FTP section of the download interface for the map you desire, copying the link, 
# and using that to download the shapefile right into R.




### create a new directory for your data

# dir.create( "shapefiles" )

setwd( "Z:/Yueming/YSun/R_Training/Mar 4 GIS/shapefiles" )
setwd( "Z:/Yueming/YSun/R_Training/Mar 4 GIS/01-05-2015" )

# it's the same for syracuse parcel data, just set working directory in folder of 01-05-2015
# and change the readShapePoly function with folder name(fn) as "01-05-2015"


### download TIGER shapefile from the census

# for information on available shapefiles:
#
# https://www.census.gov/geo/maps-data/data/tiger-line.html


# already downloaded in folder
# download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2010/TRACT/2010/tl_2010_36067_tract10.zip", "onondaga census tracts.zip" )
# 
# unzip( "onondaga census tracts.zip" )
# 
# file.remove( "onondaga census tracts.zip" )
# 
# dir()


# load and plot your shapefile
library( sp )
library( maptools )


syr <- readShapePoly( fn="tl_2010_36067_tract10", proj4string=CRS("+proj=longlat +datum=WGS84") )
syr <- readShapePoly( fn="01-05-2015", proj4string=CRS("+proj=longlat +datum=WGS84") )

plot( syr,  border="gray10" )

# Extract Parcel Data for Analysis

head( as.data.frame( syr ) )

dat <- as.data.frame( syr )

names( dat )

nrow( dat )

table( dat$LandUse )

# Examine Specific Neighborhood

these.downtown <- syr$Nhood == "Downtown"

downtown <- syr[ these.downtown , ]

these.downtown[ is.na(these.downtown) ] <- F

downtown <- syr[ these.downtown , ]

plot( downtown )

pdf( "Land Use test.pdf" )


# Vacant Land
vacant <- rep( "gray80", nrow(dat) )

vacant[ dat$LandUse == "Vacant Land" ] <- "red" 

plot( syr,  border=NA, col=vacant, main="Vacant" )



# Single Family
single.family <- rep( "gray80", nrow(dat) )

# gray.red <- rep( "gray80", nrow(dat) )

single.family[ dat$LandUse == "Single Family" ] <- "red" 

# gray.red[ dat$LandUse == "Single Family" ] <- "red" 

plot( syr,  border=NA, col=single.family, main="Single Family Homes" )


# Apartment
apartment <- rep( "gray80", nrow(dat) )

apartment[ dat$LandUse == "Apartment" ] <- "red" 

plot( syr,  border=NA, col=apartment, main="Apartments" )


# Parking
parking <- rep( "gray80", nrow(dat) )

parking[ dat$LandUse == "Parking" ] <- "red" 

plot( syr,  border=NA, col=parking, main="Parking Lots" )


# Commercial and Industrial
com.ind <- rep( "gray80", nrow(dat) )

com.ind[ dat$LandUse == "Industrial" | dat$LandUse == "Commercial"  ] <- "red" 

plot( syr,  border=NA, col=com.ind, main="Commercial and Industrial")


# Schools, Parks & Cemetaries
park.cem <- rep( "gray80", nrow(dat) )

park.cem[ dat$LandUse == "Schools" | dat$LandUse == "Parks" | dat$LandUse == "Cemetery"  ] <- "red" 

plot( syr,  border=NA, col=park.cem, main="Schools, Parks & Cemetaries" )


# Rental Units & Apartments
landlords <- rep( "gray80", nrow(dat) )

landlords[ dat$LandUse == "Two Family" | dat$LandUse == "Three Family" | dat$LandUse == "Multiple Residence"  | dat$LandUse == "Apartment" ] <- "red" 

plot( syr,  border=NA, col=landlords, main="Rental Units & Apartments" )


# Community Services
comm.services <- rep( "gray80", nrow(dat) )

comm.services[ dat$LandUse == "Community Services"  ] <- "red" 

plot( syr,  border=NA, col=comm.services, main="Community Services" )


# Utilities
utilities <- rep( "gray80", nrow(dat) )

utilities[ dat$LandUse == "Utilities"  ] <- "red" 

plot( syr,  border=NA, col=utilities, main="Utilities" )


# Religious
religious <- rep( "gray80", nrow(dat) )

religious[ dat$LandUse == "Religious"  ] <- "red" 

plot( syr,  border=NA, col=religious, main="Religious" )



# Property Valued > $100k
high.price <- rep( "gray80", nrow(dat) )

high.price[ dat$AssessedVa > 100000  ] <- "red" 

plot( syr,  border=NA, col=high.price, main="Property Valued > $100k" )





### PRICE PER ACRE

price.acre <- dat$AssessedVa / dat$Acres

price.acre[ price.acre > 10000000 ] <- NA

hist( price.acre, col="gray", xlim=c(0,2000000), breaks=1000 )



high.val <- rep( "gray80", nrow(dat) )

high.val[ price.acre > 600000  ] <- "red" 

plot( syr,  border=NA, col=high.val, main="Property Valued > $600k/acre" )




# control color by label 

price.groups <- cut( price.acre, breaks=c(0,200000,400000,600000,800000,1000000,10000000), 
                     labels=c("gray90","gray70","gray50","gray30","gray10","steel blue") )

price.groups <- as.character( price.groups )

plot( syr,  border=price.groups, col=price.groups, bg="white", main="Appraised Property Value" )

tapply( dat$AssessedVa, price.groups, mean, na.rm=T )


legend( "bottomright", bg="white",
        pch=19, pt.cex=1.5, cex=0.7,
        legend=c("$1,353k","$164k","$114k","$85k","$82k","$54k"), 
        col=c("steel blue","gray10","gray30","gray50","gray70","gray90"), 
        box.col="white",
        title="Average Property Value" 
       )



# > tapply( dat$AssessedVa, price.groups, mean, na.rm=T )
#    gray10    gray30    gray50    gray70    gray90 
# 163507.99 114350.59  85115.57  82035.95  54325.86







### NON-TAXABLE PARCELS

non.taxable <- rep( "gray80", nrow(dat) )

non.taxable[ dat$LandUse == "Religious" |  dat$LandUse == "Utilities" | dat$LandUse == "Vacant Land" | 
             dat$LandUse == "Parks" |  dat$LandUse == "Recreation" | dat$LandUse == "Community Services" | 
             dat$LandUse == "Schools" |  dat$LandUse == "Cemetery"  ] <- "red" 

plot( syr,  border=NA, col=non.taxable, main="Non-Taxable Properties" )


tapply( dat$Acres, non.taxable, sum, na.rm=T )


tapply( dat$AssessedVa, non.taxable, sum, na.rm=T )


dev.off()



### NEIGHBORHOODS


sedgwick <- rep( NA, nrow(dat) )

sedgwick[ dat$Nhood == "Sedgwick"  ] <- "red" 

plot( syr,  border="gray80", col=sedgwick, main="Sedgwick" )

