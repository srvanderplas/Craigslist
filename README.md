Craigslist
==========

Functions to scrape posts from craigslist. 

Usage: 

```r
# Modify SamplePosts.R to have your MySQL database setup
source("./SamplePosts.R")
library(doMC)
registerDoMC(15)

temp <- SamplePosts(N = 30)
```

