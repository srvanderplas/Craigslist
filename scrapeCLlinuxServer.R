#!/usr/bin/Rscript

library(lubridate)

source("./SamplePosts.R")


for(i in 1:20){
  temp <- SamplePosts(N=1)
  
  write.csv(temp, paste0("./data/", now(), ".csv"))
}

system("git pull")
system("git add ./data/*")
system("git commit -a -m 'New data files'")
system("git push")
