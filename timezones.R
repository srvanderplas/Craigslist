library(XML)
objs <- ls()
url <- "http://en.wikipedia.org/wiki/List_of_time_zone_abbreviations"
page <- scrape(url)[[1]]
# read all tables
table <- readHTMLTable(page)
# get correct table
tablenum <- as.numeric(which(lapply(lapply(table, dim), length)>0))[1]
timezones <- as.data.frame(apply(table[[tablenum]], 2, as.character), stringsAsFactors=FALSE)

library(stringr)
timezones$trunc <- str_sub(timezones$Abbr., 1, 3)
timezones$offset <- gsub(":45", ".75", gsub("UTC", "", gsub(":30", ".5", timezones$'UTC offset' )))
timezones$offset[which(str_length(timezones$offset)==0)] <- "+0"
sgn <- (str_sub(timezones$offset, 1, 1)=="−")*-1 + (str_sub(timezones$offset, 1, 1)=="+")*1
timezones$offset <- sgn*as.numeric(str_sub(timezones$offset, 2, -1))
objs2 <- ls()
objs <- c(objs, "timezones")
rm(list=objs2[!objs2%in%objs])

write.csv(timezones, "timezones.csv")