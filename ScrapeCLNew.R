#!/usr/bin/Rscript

library(lubridate)

# Set working directory
# setwd("/home/susan/Documents/R Projects/Craigslist/")

source("./SamplePosts.R")
library(doMC)
# registerDoMC(15)

Sys.setenv(http_proxy="127.0.0.1:8118")
Sys.getenv("http_proxy")

temp <- SamplePosts(N=1)

write.csv(temp, paste0("./data/", now(), ".csv"))


# con <- dbConnect(MySQL(), user="susan", dbname="susan", host="localhost")
# cl <- dbReadTable(con, "craigslist", row.names=0)
# cl <- cl[!is.na(cl$post_id),-1]
# cl <- unique(cl)
# rownames(cl) <- 1:nrow(cl)
# dim(cl)
# cl <- as.data.frame(apply(cl, 2, dbEscapeStrings, con=con),stringsAsFactors=FALSE)
# dbWriteTable(conn=con, name="craigslist", value=cl, append=FALSE, overwrite=TRUE)
# dbDisconnect(con)

# rm(cl)
gc()
