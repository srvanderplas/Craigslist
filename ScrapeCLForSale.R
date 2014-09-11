library(scrapeR)
library(plyr)
library(reshape2)

setwd("/home/susan/Documents/R Projects/Craigslist")
url <- "http://www.craigslist.org/about/sites"

cities <- scrape(url)[[1]]
regions <- getNodeSet(cities, '//*[@class="body"]/div[@class="colmask"]')
region.name = sapply(getNodeSet(cities, '//*[@class="body"]/h1/a'), xmlAttrs, "name")
regionList <- lapply(1:length(regions), function(i) getNodeSet(cities, path=paste('//*[@id="pagecontainer"]/section/div[@class="colmask"][', i, ']/div', sep="")))

getRegionInfo <- function(states, regions=regions, cities=cities){
  df2 <- rbind.fill(lapply(states, function(j) {
    m <- xmlChildren(j)
    p <- as.character(sapply(m[which(names(m)%in%"h4")], xmlValue))
    n <- lapply(m[which(names(m)%in%"ul")], xmlChildren)
    if(length(n)==0) return(data.frame())
    df <- lapply(n, function(o) as.data.frame(cbind(name = as.character(sapply(o[which(names(o)=="li")], xmlValue)), url = as.character(sapply(o[which(names(o)=="li")], function(k) xmlAttrs(xmlChildren(k)[[1]])))), stringsAsFactors=FALSE))
    p <- rep(p, sapply(df, function(i) nrow(i)))
    df <- rbind.fill(df, stringsAsFactors=FALSE)
    df$state <- p
    df
  }))
  
  df2
}

craigslistURLs <- lapply(regionList, getRegionInfo)
region.name <- rep(region.name, sapply(craigslistURLs, nrow))
craigslistURLs <- rbind.fill(craigslistURLs)
craigslistURLs$region <- region.name
craigslistURLs$url[which(craigslistURLs$region=="CA")] <- gsub(".craigslist.ca", "en.craigslist.ca", craigslistURLs$url[which(craigslistURLs$region=="CA")])

parsePost <- function(i, city){
  linkinfo <- try({
    kids <- xmlChildren(i)
    pagelinkattrs <- data.frame(t(xmlAttrs(i)), stringsAsFactors=FALSE)
    pagelink <- paste(city, as.character(xmlAttrs(getNodeSet(i, "a[@class='i']")[[1]]))[1], sep="")
    pagelinkclass <- t(data.frame(t(sapply(which(names(kids)=="span"), function(j) unlist(c(xmlAttrs(kids[[j]]), xmlValue(kids[[j]])))[1:2])), row.names=1))
    data.frame(pagelinkattrs, link=pagelink, pagelinkclass, stringsAsFactors=FALSE)
  })
  postinfodata <- try({
    postinfo <- scrape(pagelink, follow=TRUE)
    emailreply <- xmlValue(sapply(postinfo, getNodeSet, '//*[@id="pagecontainer"]/section/section[@class="dateReplyBar"]/div[@class="returnemail"]')[[1]])
    #     emailreply <- if(length(emails)>0) as.character(lapply(emails, xmlValue, FALSE, FALSE))
    dateposted <- as.character(lapply(sapply(postinfo, getNodeSet, 
                                             '//*[@id="pagecontainer"]/section/section[@class="dateReplyBar"]/p[@class="postinginfo"]/date'), 
                                      xmlValue, FALSE, FALSE))
    
    imgs <- sapply(postinfo, getNodeSet, '//*[@id="pagecontainer"]/section/section[@class="userbody"]/figure/div[@id="thumbs"]/a')
    if(length(unlist(imgs))>0)   imglink <- data.frame(do.call("rbind", lapply(imgs, xmlAttrs, FALSE, FALSE)), stringsAsFactors=FALSE) else imglink <- data.frame(NA, NA)  
    names(imglink) <- c("ImageLink", "ImageTitle")
    text <- as.character(lapply(sapply(postinfo, getNodeSet, '//*/section[@id="postingbody"]'), xmlValue, FALSE, FALSE))
    title <- as.character(lapply(sapply(postinfo, getNodeSet, '//*/h2[@class="postingtitle"]'), xmlValue, FALSE, FALSE))
    id <- strsplit(as.character(lapply(sapply(postinfo, getNodeSet, '//*/div[@class="postinginfos"]/p[@class="postinginfo"][1]'), xmlValue, FALSE, FALSE)), ": ")[[1]][2]
    data.frame(email=emailreply, datetime=dateposted, imglink, postText=text, postTitle=title, postID=id, stringsAsFactors=FALSE)
  })
  if(is.character(postinfodata) & is.character(linkinfo)) return(data.frame()) else
    if(is.character(postinfodata) & !is.character(linkinfo)) return(linkinfo) else
      if(!is.character(postinfodata) & is.character(linkinfo)) return(postinfodata) else
        return(cbind(postinfodata, linkinfo))
  return(cbind(postinfodata, linkinfo))
}
getCityPosts <- function(city, subcl="sss"){
  url <- paste(paste(city, "/", subcl, sep=""), c("/", "/index100.html", "/index200.html"), sep="")
  site <- try(scrape(url, follow=TRUE))
  if(is.character(site)) return(data.frame(city=city, subcl=subcl))
  posts <- unlist(lapply(site, function(i) getNodeSet(i, "//*/p")[1:100]))
  postdata <- suppressWarnings(rbind.fill(lapply(posts, parsePost, city), stringsAsFactors=FALSE))
  cbind(city=city, subcl = subcl, postdata)
}

# postcity <- getCityPosts(craigslistURLs[100,3], subcl="sss")


AllCraigslistURLs <- craigslistURLs
craigslistURLs <- craigslistURLs[which(craigslistURLs$region%in%c("US", "CA")),]
source("./StatePop.R")

library(multicore)

samplecities <- sample(1:nrow(craigslistURLs), 80, replace=FALSE, prob=craigslistURLs$weight)
temp <- getCityPosts(craigslistURLs[samplecities[1],"url"], subcl="sss")
for(i in samplecities[2:length(samplecities)]){
  a <- getCityPosts(craigslistURLs[i,"url"], subcl="sss")
  if(is.data.frame(a)) temp <- rbind.fill(temp, a)
}

data <- read.csv("./CL-sss.csv", stringsAsFactors=TRUE)
data <- rbind.fill(data, temp)
data <- unique(data)

write.csv(data, "CL-sss.csv", row.names=FALSE)