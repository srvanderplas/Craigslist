#!/usr/bin/Rscript

library(scrapeR)
library(doMC)
registerDoMC(15)
library(plyr)
library(stringr)
library(RMySQL)
# library(multicore)

# Set working directory
setwd("/home/susan/Documents/R Projects/Craigslist/")

# Read in table of Craigslist Cities in the US and Canada
craigslistURLs <- read.csv("CraigslistURLs.csv", stringsAsFactors=FALSE)

# Read in functions for getting the last 300 posts from a city and for reading each post
source("getCityCraigslistPosts.R")

col.order <- c("cityurl", "subcl", "post.id", "post.link", "post.data.id", "post.date.raw", "post.title", "post.price", "post.subcl", "post.subcltype", "post.loc", "post.text", "post.date", "post.last.update", "post.pics")
null.df <- data.frame(cityurl=NA, subcl=NA, post.id=NA, post.link=NA, post.data.id=NA, post.date.raw=NA, post.title=NA, post.price=NA, post.subcl=NA, post.subcltype=NA, post.loc=NA, post.text=NA, post.date=NA, post.last.update=NA, post.pics=NA)


SamplePosts <- function(N=10, subcl="ppp"){
  samplecities <- sample(1:nrow(craigslistURLs), N, replace=FALSE, prob=craigslistURLs$weight)
  temp <- ldply(craigslistURLs$link[samplecities], 
                function(i) {              
                  a <- try(getCityPosts(i, subcl))
                  if(mode(a)=="character") return(data.frame())
                  # only save rows with at least a post id
                  a <- a[which(!is.na(a$post.id)),]
                  if(nrow(a)==0 || ncol(a)==0) return(data.frame())

#                   a <- rbind.fill(null.df, a)
                  rownames(a) <- NULL
                  if(is.data.frame(a)){
                    return(a)
                  } else return(data.frame())
                }, .parallel=TRUE)
  if(nrow(temp)==0 || ncol(temp)==0){
    print("warning: empty data frame")
    return(data.frame())
  }
  temp <- as.data.frame(apply(temp, 2, as.character), stringsAsFactors=FALSE)
  # force data frame to have the same column order and number of columns
  # to make storage in the MySQL database orderly
  cn <- names(temp)
  temp <- temp[,order(as.numeric(sapply(cn, function(i) which(i==col.order))))]
  names(temp) <- gsub(".", "_", names(temp), fixed=TRUE)

  con <- dbConnect(MySQL(), user="susan", dbname="susan", host="localhost")
  temp <- as.data.frame(apply(temp, 2, dbEscapeStrings, con=con),stringsAsFactors=FALSE)
  success <- as.numeric(dbWriteTable(con, "craigslist", temp, append=TRUE, row.names=0))
  dbDisconnect(con)
  print(success)
  return(temp)
}

temp <- SamplePosts(N=30)

con <- dbConnect(MySQL(), user="susan", dbname="susan", host="localhost")
cl <- dbReadTable(con, "craigslist", row.names=0)
cl <- cl[!is.na(cl$post_id),-1]
cl <- unique(cl)
rownames(cl) <- 1:nrow(cl)
dim(cl)
cl <- as.data.frame(apply(cl, 2, dbEscapeStrings, con=con),stringsAsFactors=FALSE)
dbWriteTable(conn=con, name="craigslist", value=cl, append=FALSE, overwrite=TRUE)
dbDisconnect(con)
