library(maps)
library(mapdata)
library(maptools)
gpclibPermit()
library(plyr)
library(doMC)
registerDoMC(8)
library(fastshp) # install with install.packages('fastshp',repos='http://www.rforge.net/')


zipcl <- read.table("./LookupCraigsZip2010.txt", header=TRUE)
zipcl$zip <- (gsub("Zip", "", zipcl$ZipName, fixed=TRUE))
zipcl <- zipcl[,c("CraigsList", "zip")]
names(zipcl) <- c("craigslist", "zip")

#' Zip Code data from ftp://ftp2.census.gov/geo/tiger/TIGER2012/ZCTA5/
#' Zip Codes by CL from http://uxblog.idvsolutions.com/2011/12/craigszips.html

getinfo.shape("./ZipMaps/ZCTA_2010Census_DP1.shp")
xx <- readShapePoly("./ZipMaps/ZCTA_2010Census_DP1.shp")

# pull out all polygons
curves <- slot(xx, "polygons")

options(expressions=10000)


# load in additional data
otherdat <- slot(xx,"data")
otherdat$id <- (1:nrow(otherdat))
otherdat <- otherdat[,c("ZCTA5CE10", "GEOID10", "INTPTLAT10", "INTPTLON10", "DP0010001", "DP0010020", "DP0010039", "Shape_Area", "id")]
names(otherdat) <- c("zip", "geoid", "intptlat", "intptlong", "pop", "malepop", "femalepop", "area", "id")

otherdat <- merge(otherdat, zipcl, stringsAsFactors=FALSE)
otherdat[,c(1:4, 10)] <- apply(otherdat[,c(1:4,10)], 2, as.character)
otherdat$intptlat <- as.numeric(otherdat$intptlat)
otherdat$intptlong <- as.numeric(otherdat$intptlong)

sub <- subset(otherdat, craigslist==otherdat$craigslist[1])
temp <- unionSpatialPolygons(SpatialPolygons(curves[sub$id]), sub$id)

# create cluster
# function
plyfcn <- function(df, curves=curves){
  library(maps)
  library(mapdata)
  library(maptools)
  gpclibPermit()
  library(fastshp) 
  temp <- unionSpatialPolygons(SpatialPolygons(curves[df$id]), df$id)
  pop <- sum(df$pop)
  malepop <- sum(df$malepop)
  femalepop <- sum(df$femalepop)
  area <- sum(df$area)
  id <- min(df$id)
  intptlat <- mean(df$intptlat)
  intptlong <- mean(df$intptlong)
  df2 <- data.frame(pop=pop, malepop=malepop, femalepop=femalepop, area=area, id=id, intptlat=intptlat, intptlong=intptlong)
  coordList <- NULL
  segments <- max(temp@plotOrder)
  for(i in 1:length(temp@plotOrder)){
    res <- temp@polygons[[temp@plotOrder[i]]]@Polygons[[1]]@coords
    res <- as.data.frame(res)
    names(res) <- c("x", "y")
    if(nrow(res)>1000){
      res.thin <- thin(res$x, res$y, 1e-1)
      res <- res[res.thin,]
    }
    tlen <- nrow(res)
    coordList <- rbind(coordList, cbind(res, group=as.numeric(temp@polygons[[temp@plotOrder[i]]]@ID)+(temp@plotOrder[i])/segments, seq=1:tlen))
  }
  coordList <- as.data.frame(coordList)
  names(coordList) <- c("x", "y", "group", "len")
  coordList <- coordList[chull(coordList$x, coordList$y),]
  n <- nrow(coordList)
  coordList <- cbind(coordList, df2)
  coordList
}

CraigslistShapes <- ddply(otherdat, .(craigslist), plyfcn, curves=curves, .parallel=TRUE)
save(CraigslistShapes, file="CLShapes.RData")
library(ggplot2)
qplot(data=CraigslistShapes, x=x, y=y, group=group, geom="polygon", fill=craigslist) + xlim(c(-100, -90)) + ylim(c(35,45))
qplot(data=subset(CraigslistShapes, craigslist=="http://omaha.craigslist.org/"), x=x, y=y, group=group, geom="polygon", fill=craigslist)