library(RMySQL)

setwd("/home/susan/Documents/R Projects/Craigslist/")
source("./SamplePosts.R")
library(doMC)
registerDoMC(8)
con <- dbConnect(MySQL(), user="susan", dbname="susan", host="localhost")
cl <- dbReadTable(con, "craigslist", row.names=0)
cl <- cl[!is.na(cl$post_id),-1]
dbDisconnect(con)

posts <- cl
names(posts) <- gsub("_", ".", names(posts), fixed=TRUE)


library(lubridate)
library(stringr)
# strip timezone information
posts$timezone <- str_sub(posts$post.date, -5, -1)
# posts$post.date2 <- str_sub(posts$post.date, 1, -6)
posts$post.date2 <- posts$post.date
posts$post.date2 <- str_replace(posts$post.date2, "T", " ")
posts$post.date2 <- ymd_hms(posts$post.date2)
posts$post.last.update2 <- str_replace(posts$post.last.update, "T", " ")
posts$post.last.update2 <- ymd_hms(posts$post.last.update)
posts$post.time <- hour(posts$post.date2) + minute(posts$post.date2)/60

library(ggplot2)
# Relative proportions of posts
qplot(posts$post.subcltype) + theme(axis.text.x=element_text(angle=90))

qplot(posts$post.time)

qplot(wday(posts$post.date2, label=TRUE, abbr=TRUE))


# Map of lower 48 Craigslist proximity regions
load("CLPolygons.rda")
library(ggplot2)
library(maps)
#load us map data
states <- map_data("state")

# CraigslistPolygons$fill <- as.numeric(as.factor(CraigslistPolygons$craigslist))%%15
# ggplot() + 
#   geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="white", colour="black")  + 
#   coord_map() +
#   geom_polygon(data=CraigslistPolygons, 
#                aes(x=x, y=y, group=group, fill=factor(fill)), alpha=.25) +
#   scale_fill_discrete(guide="none") + 
#   ylim(c(25, 50)) + xlim(c(-125, -65))


library(tm)
# need to clean up text before putting it into tm_map...
posts$post.text2 <- str_replace_all(posts$post.text, "[\'\"]", "")
posts$post.text2 <- str_replace_all(posts$post.text2, "[[:punct:]]", " ")
posts$post.text2 <- str_replace_all(posts$post.text2, "\\n", " ")

myCorpus <- Corpus(VectorSource(posts$post.text2))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myStopwords <- stopwords('english')
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
dict <- Dictionary(unlist(lapply(posts$post.text2, strsplit, " ")))
#myCorpus <- tm_map(myCorpus, stemDocument)
#myCorpus2 <- tm_map(myCorpus, stemCompletion, dictionary=dict, type="shortest")
myDtm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(2, Inf)))
findFreqTerms(myDtm, lowfreq=500)
findAssocs(myDtm, "attractive", .10)
findAssocs(myDtm, "single", .10)
findAssocs(myDtm, "long", .15)
findAssocs(myDtm, "married", .1)
findAssocs(myDtm, "nsa", .075)
findAssocs(myDtm, "lbs", .1)
findAssocs(myDtm, "thick", .1)
myDtm2 <- myDtm
removeSparse
