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

# get link to post
getpagelink <- function(i, city){as.character(try(
  paste(city, as.character(xmlAttrs(getNodeSet(i, "a[@class='i']")[[1]]))[1], sep="")))
}
# get class attr, value for page link
getpagelinkclass <- function(kids){
  idx <- which(names(kids)=="span")
  if(length(idx)==0) return(NULL)
  attrs <- as.character(try(sapply(idx, function(j) unlist(xmlAttrs(kids[[j]])))))
  values <- as.character(try(sapply(idx, function(j) unlist(xmlValue(kids[[j]])))))
  t(data.frame(cbind(attrs, values), row.names=1))
}

# get reply email, if present
getpostemail <- function(postinfo){as.character(try(
  strsplit(gsub("mailto:", "", xmlAttrs(getNodeSet(postinfo[[1]], '//*[@id="pagecontainer"]/section/section[1]/a')[[1]])), "?", fixed=TRUE)[[1]][1]
  ))
}

# get post date
getpostdate <- function(postinfo){as.character(try(
  as.character(lapply(sapply(postinfo, getNodeSet, 
               '//*[@id="pagecontainer"]/section/section[@class="dateReplyBar"]/p[@class="postinginfo"]/date'), 
                      xmlValue, FALSE, FALSE))
  ))
}

# get images
getpostimages <- function(postinfo){
  imgs <- NULL
  try(
    imgs <- sapply(postinfo, getNodeSet, '//*[@id="pagecontainer"]/section/section[@class="userbody"]/figure/div[@id="thumbs"]/a')
  )
  try({
  if(length(unlist(imgs))>0){
    imglink <- data.frame(do.call("rbind", 
                    lapply(imgs, xmlAttrs, FALSE, FALSE)), 
                          stringsAsFactors=FALSE)
  } else imglink <- data.frame(NA, NA)  
  names(imglink) <- c("ImageLink", "ImageTitle")
  return(imglink)
  })
  
return(data.frame(ImageLink=NA, ImageTitle=NA))
}

# post text
getposttext <- function(postinfo){as.character(try(
  as.character(lapply(sapply(postinfo, getNodeSet, '//*/section[@id="postingbody"]'), xmlValue, FALSE, FALSE)))
  )
}

# post title
getposttitle <- function(postinfo){try(
  as.character(lapply(sapply(postinfo, getNodeSet, '//*/h2[@class="postingtitle"]'), xmlValue, FALSE, FALSE))
  )
}

# post id
getpostid <- function(postinfo){try(
  strsplit(as.character(lapply(sapply(postinfo, getNodeSet, '//*/div[@class="postinginfos"]/p[@class="postinginfo"][1]'), xmlValue, FALSE, FALSE)), ": ")[[1]][2]
)
}

parsePost <- function(i, city){
  linkinfo <- try({
    kids <- xmlChildren(i)
    pagelinkattrs <- data.frame(t(xmlAttrs(i)), stringsAsFactors=FALSE)
    pagelink <- getpagelink(i, city)
    pagelinkclass <- getpagelinkclass(kids)
    data.frame(pagelinkattrs, link=pagelink, pagelinkclass, 
               stringsAsFactors=FALSE)
  })
  postinfodata <- try({
    postinfo <- scrape(pagelink, follow=TRUE)
    data.frame(email=getpostemail(postinfo), 
               datetime=getpostdate(postinfo), 
               getpostimages(postinfo), 
               postText=getposttext(postinfo), 
               postTitle=getposttitle(postinfo), 
               postID=getpostid(postinfo), 
               stringsAsFactors=FALSE)
  })
  
  if(is.character(postinfodata) & is.character(linkinfo)) {
    return(data.frame())
  } else if(is.character(postinfodata) & !is.character(linkinfo)) {
      return(linkinfo)    
  } else if(!is.character(postinfodata) & is.character(linkinfo)) {
      return(postinfodata) 
  } else return(cbind(postinfodata, linkinfo))
  
  # justincase
  return(cbind(postinfodata, linkinfo))
}
getCityPosts <- function(city, subcl="ppp"){
#   url <- paste(city, "/", subcl, sep="") # debugging - makes shorter runs
  url <- paste(paste(city, "/", subcl, sep=""), c("/", "/index100.html", "/index200.html"), sep="")
  site <- try(scrape(url=url, follow=TRUE))
  if(is.character(site)) return(data.frame(city=city, subcl=subcl))
  posts <- unlist(lapply(site, function(i) getNodeSet(i, "//*/p")[1:100]))
  postdata <- suppressWarnings(rbind.fill(lapply(posts, parsePost, city), stringsAsFactors=FALSE))
  cbind(city=city, subcl = subcl, postdata)
}

# postcity <- getCityPosts(craigslistURLs[100,"url"])
# postcity <- getCityPosts(craigslistURLs[sample( 1:300, 1),"url"])


AllCraigslistURLs <- craigslistURLs
craigslistURLs <- craigslistURLs[which(craigslistURLs$region%in%c("US", "CA")),]
source("./StatePop.R")

library(multicore)


SamplePosts <- function(N=80, subcl="ppp"){
  samplecities <- sample(1:nrow(craigslistURLs), N, replace=FALSE, prob=craigslistURLs$weight)
  temp <- getCityPosts(craigslistURLs[samplecities[1],"url"], subcl="ppp")
  for(i in samplecities[2:length(samplecities)]){
    a <- getCityPosts(craigslistURLs[i,"url"], subcl)
    if(is.data.frame(a)) temp <- rbind.fill(temp, a)
  }
  return(temp)
}

temp <- SamplePosts()

# get personal ads
data <- read.csv("./CL-mis.csv", stringsAsFactors=TRUE)
data <- rbind.fill(data, temp)
data <- unique(data)
write.csv(data, "CL-mis.csv", row.names=FALSE)

# get for sale stuff
temp <- SamplePosts(subcl="sss")
data <- read.csv("./CL-sss.csv", stringsAsFactors=TRUE)
data <- rbind.fill(data, temp)
data <- unique(data)
write.csv(data, "CL-sss.csv", row.names=FALSE)
