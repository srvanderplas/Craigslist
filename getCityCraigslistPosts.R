library(scrapeR)
library(stringr)
library(plyr)
library(compiler)

# Craigslist has various boards (ppp for personal ads, sss for "for sale", etc.) that  contain all posts. 
# These acronyms are consistent across city pages

# Function to parse the actual CL post instead of the link in the post directory
parsePosts <- cmpfun(function(posturl){
  post <- scrape(posturl, follow=TRUE, maxSleep = 10, chunkSize=25, userAgent="Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36")
  post.data <- ldply(post, function(i){
    # If scraping that URL doesn't return an HTML page, return an empty data frame
    if(mode(i)!="externalptr") {
      return(data.frame(
        post.text = NA,
        post.date = NA,
        post.last.update = NA,
        post.pics = NA
      ))
    }
      
    # get text of post
    txt <- getNodeSet(i, path="//*[@id='postingbody']")
    # get dates of post and last update
    time <- as.character(sapply(getNodeSet(i, "//time"), function(j) unlist(xmlAttrs(j))))
    # did post include a picture?
    pics <- getNodeSet(i, path="//figure/div[@id='thumbs']/a")
    # get ID for merge
    id <- str_replace(xmlValue(getNodeSet(i, "//div[@class='postinginfos']/p")[[1]]), "post id: ", "")
    data.frame(post.text = ifelse(length(txt)>0, str_trim(as.character(xmlValue(txt[[1]]))), ""),
               post.date = time[1],
               post.last.update = rev(time)[1],
               post.pics = length(pics),
               post.id = id, 
               stringsAsFactors=FALSE
    )
  })
  return(post.data)
})


# Function to get the list of posts from the index of whatever sub-CL is provided
getCityPosts <- cmpfun(function(cityurl, subcl="ppp"){
  # Let's get the first 300 posts from each city
  url <- paste(paste(cityurl, "/", subcl, sep=""), c("/", "/index100.html", "/index200.html"), sep="")
  
  # try scrape the posts from each URL
  site <- try(scrape(url=url, follow=TRUE))
  
  # If scraping did not work, return a data frame with the url and subcl to indicate a problem
  if(is.character(site)) return(data.frame(cityurl=cityurl, subcl=subcl))
  
  # Separate the posts: posts are in //*/div[@class='content']/p
  posts <- unlist(llply(site, function(i) getNodeSet(i, "//*/div[@class='content']/p")[1:100]))
  
  # Get data from each entry on the post list (from the link page, not from the post itself)
  linkdata <- ldply(posts, function(i){
    # names of nodes
    nodenames <- xmlApply(i, xmlName)
    
    # actual child nodes
    kids <- xmlChildren(i)
    
    # Of the span nodes, the first is "star", the second actually has the post title info
    title <- xmlChildren(kids[[which(nodenames=="span")]])
    title <- title[which(lapply( title, xmlName)!="text")]
    postinfo <- xmlChildren(title[[3]])
    postinfo <- postinfo[which(lapply(postinfo, xmlName)!="text")]
    title <- xmlChildren(title[[2]])
    title <- title[which(lapply( title, xmlName)!="text")]
    
    date <- xmlValue(title$span)
    post.title <- xmlValue(title$a)
    post.id <- xmlAttrs(title$a)[['data-id']]
    
    # Link information
    linkinfo <- xmlAttrs(kids[[which(nodenames=="a")]])
    
    # The third span node has the post information
    values <- lapply(postinfo, xmlAttrs)
    values <- paste(names(values), lapply(values, paste, collapse="."), sep="-")
    price <- ifelse("span-price"%in%values, xmlValue(postinfo[values=="span-price"]$span), NA)
    pnr <- ifelse("span-pnr"%in%values, str_trim(str_extract(xmlValue(postinfo[values=="span-pnr"]$span), "^.*\\(.*)")), NA)
    cat.abbrev <- ifelse(sum(grepl("a-gc", values))>0, rev(xmlAttrs(postinfo[which(grepl("a-gc", values))]$a))[1], NA)
    post.link <- xmlAttrs(title$a)[["href"]]
    
    postinfo <- unlist(postinfo[which(names(postinfo)!="span.text")])
    postinfo <- postinfo[names(postinfo)!="text"]
    
    post.title.2 <- str_trim(word(post.title, sep="-"))
    post.type <- str_trim(word(post.title, sep="-", start=-1))
    if(post.title.2==post.type){
      post.type <- NA
    }
    
    post.dataid <- try(as.character(xmlAttrs(i)["data-pid"]))
    # Combine into a data frame
    data.frame(
      post.id = post.id,
      post.link = post.link,
      post.data.id = post.dataid,
      post.date.raw = str_replace(date, "  ", " "),
      post.title = post.title.2,
      post.type = post.type,
      post.price = price,
      post.subcl = as.character(cat.abbrev),
      post.subcltype = xmlValue(rev(postinfo)[[1]]),
      post.loc = str_trim(str_replace(str_replace(pnr, "\\(", ""), "\\)", "")), 
      stringsAsFactors=FALSE
    )
  })
  
  linkdata$post.link <- paste(word(linkdata$.id, sep=paste("/", subcl, "/", sep="")),
                              linkdata$post.link, sep="")
  linkdata <- linkdata[,-which(names(linkdata)==".id")]
  
  idx <- c(1:nrow(linkdata), rep(NA, times=(300-nrow(linkdata))))
  tmp <- matrix(idx, nrow=10, byrow=TRUE)
  
  postdata <- do.call("rbind.fill", 
                      lapply(1:nrow(tmp), 
                             function(i) 
                               parsePosts(linkdata$post.link[tmp[i,][which(!is.na(tmp[i,]))]])
                             )
                      )
  
  
  if(!is.character(postdata)  & "post.id"%in%names(postdata)) {
    # Return a data frame
    df <- merge(linkdata, postdata, stringsAsFactors=FALSE)
    df$cityurl <- cityurl
    df$subcl <- subcl
  } else df <- cbind(cityurl = cityurl, subcl = subcl, linkdata, post.text=NA, post.date=NA, post.last.update=NA, post.pics=NA)
  
  
  return(df[,-which(names(df)==".id")])
})

