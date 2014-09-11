library(scrapeR)
library(plyr)
library(reshape2)
library(lubridate)
library(multicore)
library(stringr)
library(ggplot2)

# setwd("/home/susan/Dropbox/GraphicsGroup/Craigslist/")
# source("./timezones.R")
timezones <- read.csv("timezones.csv")
timezones.conv <- unique(timezones[,c("Abbr.", "trunc")])

# url <- "http://www.craigslist.org/about/sites"

data <- read.csv("./CL-mis.csv", stringsAsFactors=FALSE)
data <- apply(data, 2, str_trim)
data <- apply(data, 2, function(i) {
                i[str_length(i)==0] <- NA
                return(i)})
data <- as.data.frame(data, stringsAsFactors=FALSE)

data$timezone <- substr(data$datetime, 21, 23)
data$timezone[is.na(data$timezone)] <- "UTC"
timezoneidx <- unlist(mclapply(data$timezone, function(i) which(timezones.conv$trunc==i)[1]))
tz.tab <- timezones.conv[unique(timezoneidx),]
data$timezone <- timezones.conv$Abbr.[timezoneidx]

data$datetime2 <- unlist(mclapply(1:nrow(data), function(i) strftime(substr(data$datetime[i], 1, 19), format="%Y-%m-%d, %I:%M%p", tz=data$timezone[i], usetz=TRUE)))

# clean latitude/longitude
data$lat <- as.numeric(data$lat)
data$long <- as.numeric(data$long)
data$data.latitude <- as.numeric(data$data.latitude)
data$data.longitude <- as.numeric(data$data.longitude)
data$lat[data$lat==0] <- NA
data$long[data$long==0] <- NA
data$data.latitude[data$data.latitude==0] <- NA
data$data.longitude[data$data.longitude==0] <- NA

data$lat <- rowSums(cbind(data$lat, data$data.latitude), na.rm=TRUE)
data$lat[data$lat==0] <- NA
data$long <- rowSums(cbind(data$long, data$data.longitude), na.rm=TRUE)
data$long[data$long==0] <- NA
data <- data[,-which(names(data) %in% c("data.longitude", "data.latitude"))]


# get rid of all NA columns
data <- data[,apply(data, 2, function(i) sum(!is.na(i)))!=0]

# correlation matrix of missingness
cor.melt <- melt(cor(is.na(data)))
qplot(data=cor.melt, x=Var1, y=Var2, geom="tile", fill=value) + 
  scale_fill_gradient2(limits=c(-1.1,1.1))
cor.melt[which(cor.melt$value< -0.5),]

data$postCategory <- data$itemcg
data$age <- as.numeric(sapply(data$postTitle, function(i) 
            strsplit(strsplit(i, ' - ')[[1]][3], " (", fixed=TRUE)[[1]][1]))
data$location <- gsub(")", "", gsub("(", "", gsub(" )", "", data$itempn, fixed=TRUE), fixed=TRUE))

data <- data[,-(names(data)%in%c("itemcg", "itempn"))]

# clean up weird columns
data$ih[which(data$ih==" ")] <- NA
if("itemsep"%in%names(data)) data <- data[,-which(names(data)=="itemsep")]

# get rid of single-character img links
idx <- which(str_length(data$i)==1 & !is.na(data$i))
if(length(idx)>0) data$i[idx] <- NA
rm(list="idx")

# itempp is similar to age but not always the same. 
# itempnr has location ish stuff.... for replicating map
# itemcg is craigslist posted to


alldata <- data # make a backup copy

data <- unique(data[,-which(names(data)%in%c("ImageLink", "ImageTitle", "ih", "i"))])
data$postTitleOnly <- unlist(mclapply(data$postTitle, function(i) strsplit(i, " - ")[[1]][1]))

words <- tolower(unlist(strsplit(data$postTitleOnly, " ")))
words2 <- words[grepl("\\w", words)]
words3 <- gsub("[:punct:]", "", words2)
wordfreq <- table(words3)
wordfreq[order(wordfreq, decreasing=TRUE)][1:100]

wordmatrix <- do.call("rbind", mclapply(words3, function(i) as.numeric(grepl(i, data$postTitleOnly, fixed=TRUE))))