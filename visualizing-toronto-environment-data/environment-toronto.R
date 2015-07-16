
Open Data Toronto

ESRI Shapefile - Boundaries of Toronto Neighborhoods
www1.toronto.ca/wps/portal/contentonly?vgnextoid=04b489fe9c18b210VgnVCM1000003dd60f89RCRD&vgnextchannel=1a66e03bb8d1e310VgnVCM10000071d60f89RCRD

Wellbeing Toronto - Environment Data
http://www1.toronto.ca/wps/portal/contentonly?vgnextoid=67f6d05685a0c410VgnVCM10000071d60f89RCRD&vgnextchannel=1a66e03bb8d1e310VgnVCM10000071d60f89RCRD

Notes on Spatial Data Operation in R - Frank Davenport
https://dl.dropboxusercontent.com/u/9577903/broomspatial.pdf

# Date accessed: July 7, 2015


library(rgdal)
library(spatstat)
library(ggplot2)
library(stringr)


wd <- getwd()
download.file()

fileUrl <- "http://opendata.toronto.ca/gcc/neighbourhoods_planning_areas_wgs84.zip"

unzip("neighbourhoods_planning_areas_wgs84.zip")



# for shapefiles, first argument of the read/write/info functions is the
# directory location, and the second is the file name without suffix

# optionally report shapefile details
ogrInfo(".", "NEIGHBORHOODS_WGS84")
# Source: ".", layer: "nw-rivers"
# Driver: ESRI Shapefile number of rows 12 
# Feature type: wkbLineString with 2 dimensions
# +proj=longlat +datum=WGS84 +no_defs  
# Number of fields: 2 
#     name type length typeName
#     1   NAME    4     80   String
#     2 SYSTEM    4     80   String

# read in shapefiles
neighborhoods.rg <- readOGR(".", "NEIGHBORHOODS_WGS84")

# note that readOGR will read the .prj file if it exists
print(proj4string(neighborhoods.rg))
# [1] " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# generate a simple map showing all three layers
plot(neighborhoods.rg, axes=TRUE, border="gray")


# the bounding box around the Toronto dataset
# win <- bbox(neighborhoods.rg)
# win

# win <- as.vector(win)
# t(win)

# dran <-runifpoint(100, win = as.vector(t(bbox(neighborhoods.rg))))
#create 100 random points

# plot(neighborhoods.rg, axes=TRUE, border="gray")
# points(dran)

# dp <- as.data.frame(dran)
#This creates a simple data frame with 2 colunms, x and y
# head(dp)


# Read in environmental data
environment <- read.csv("E:/Users/Kie/OneDrive/DataAnalytics/Personal Projects/toronto-environment-data/WB-Environment.csv", 
         header = TRUE, 
         stringsAsFactors = FALSE)

# create consistent column names for merging
names(environment)[2] <- "id"

# Ensure id's have initial 00's in same format as .shp file
environment$id <- as.factor(str_pad(environment$id, 3, pad = "0"))


d <- environment
d2 <- neighborhoods.rg@data$id #Extract data from shape file
# d3 <- merge(d, d2)
# head(d3)

# Join our csv to our SHapefile

# Change row names to ids
row.names(d) <- d$id
# unique(d$id)
# unique(neighborhoods.rg$AREA_S_CD)

# Do the same for .shp
row.names(neighborhoods.rg) <- as.character(neighborhoods.rg$AREA_S_CD)

# We need to reorder the rownames for merging
neighborhoods.rg <- neighborhoods.rg[order(neighborhoods.rg$AREA_S_CD), ]

# Binding
ds1 <- spCbind(neighborhoods.rg, d)
head(neighborhoods.rg@data)

# Change dataset into ggplot usable format

fort <- fortify(neighborhoods.rg)
head(fort)

# Create the plot

p1 <- ggplot(d)
p1 <- p1 + geom_map(aes(fill = environment$Green.Spaces, 
                    map_id = id), 
                    map = fort)
p1 <- p1 +expand_limits(x = fort$long, y = fort$lat)
p1 <- p1 +coord_equal()
p1 +
xlab("Basic Map with Default Elements")

p1 <- p1 + scale_fill_gradient (name ="Green \nSpace",
                                low ="lightgoldenrod", 
                                high = "forestgreen")  #to set break points, enter in breaks=c(...,..)
# The \n in Green Space \nChange'indicates a carriage return
p1 + xlab( "We Changed the Color Scale and Gave the Legend a Proper Name")

# -----Get Rid of the Background-----
# Blank Grid, Background,Axis,and Tic Marks
bGrid <- theme(panel.grid = element_blank())
bBack <-theme(panel.background = element_blank())


bTics <- theme(axis.text = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks = element_blank())

p1 <- p1 + bTics + bGrid + bBack
p1 + xlab("We got rid of all the unneccessary background material")


# For polygon labels, we will need to calc centroids
cent <- as.data.frame(coordinates(neighborhoods.rg))
cent$Neighborhood <- neighborhoods.rg$AREA_NAME
cent$id <- neighborhoods.rg$AREA_S_CD
head(cent)

# Merge centroids with Environment data so we can draw upon infor for labels
envlab <- merge(cent,d)

p1 <- p1 + geom_text(data = cent,aes(V1, V2, label = Neighborhood), size = 2.5, vjust = 1)
p1 <- p1 + geom_text(data = envlab, 
                aes(V1, V2, label = paste("(", round(Green.Spaces,2), ")", sep = "")),
                    size = 2.5,
                    vjust = 2)
torontoGreenSpace <- p1 + ggtitle("Green Space in Toronto (Squared Km)") + 
  xlab("Longitude") +
  ylab("Latitude")



# write out a new shapefile (including .prj component)
# writeOGR(torontoGreenSpace, ".", "torontoGreenSpace", driver="ESRI Shapefile")

png("./torontoGreenSpace.png", width=2000, 
    height=2000)
plot(torontoGreenSpace)
dev.off()

# THINGS TO DO
# REMOVE NUMBERS FROM AREA NAME
  