library(scrapeR)
library(stringr)
library(plyr)
library(compiler)

# Craigslist has various boards (ppp for personal ads, sss for "for sale", etc.) that  contain all posts. 
# These acronyms are consistent across city pages

# Function to parse the actual CL post instead of the link in the post directory
parsePosts <- cmpfun(function(posturl){
  post <- scrape(posturl, follow=TRUE)
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
    title <- xmlChildren(kids[[which(nodenames=="span")[2]]])
    title <- title[which(lapply( title, xmlName)!="text")]
    
    # Link information
    linkinfo <- xmlAttrs(kids[[which(nodenames=="a")]])
    
    # The third span node has the post information
    postinfo <- xmlChildren(kids[[which(nodenames=="span")[3]]])
    postinfo <- postinfo[which(lapply( postinfo, xmlName)!="text")]
    
    # What type of post information? 
    postinfo.class <- lapply(postinfo, xmlAttrs)
    
    # Price (or age in personal ads) may or may not be included
    price <- ifelse("price"%in%postinfo.class, xmlValue(postinfo[[which(postinfo.class=="price")]]), NA)
    
    # Location is in parentheses
    loc <- str_trim(str_extract(xmlValue(postinfo[[which(postinfo.class=="pnr")]]), "^.*\\(.*)"))
    
    # Combine into a data frame
    data.frame(
      post.id = as.character(xmlAttrs(i)["data-pid"]),
      post.link = as.character(linkinfo["href"]),
      post.data.id = as.character(linkinfo["data-id"]),
      post.date.raw = str_replace(xmlValue(title[[1]]), "  ", " "),
      post.title = str_trim(word(xmlValue(title[[2]]), sep="-")),
      post.price = price,
      post.subcl = as.character(xmlAttrs(rev(postinfo)[[1]])["data-cat"]),
      post.subcltype = xmlValue(rev(postinfo)[[1]]),
      post.loc = str_trim(str_replace(str_replace(loc, "\\(", ""), "\\)", "")), 
      stringsAsFactors=FALSE
    )
  })
  
  linkdata$post.link <- paste(word(linkdata$.id, sep=paste("/", subcl, "/", sep="")),
                              linkdata$post.link, sep="")
  linkdata <- linkdata[,-which(names(linkdata)==".id")]
  
  idx <- c(1:nrow(linkdata), rep(NA, times=(300-nrow(linkdata))))
  tmp <- matrix(idx, nrow=10, byrow=TRUE)
  
  postdata <- do.call("rbind.fill", lapply(1:nrow(tmp), function(i) parsePosts(linkdata$post.link[tmp[i,][which(!is.na(tmp[i,]))]])))
  if(!is.character(postdata)  & "post.id"%in%names(postdata)) {
    # Return a data frame
    df <- merge(linkdata, postdata, stringsAsFactors=FALSE)
    df$cityurl <- cityurl
    df$subcl <- subcl
  } else df <- cbind(cityurl = cityurl, subcl = subcl, linkdata, post.text=NA, post.date=NA, post.last.update=NA, post.pics=NA)
  
  
  return(df[,-which(names(df)==".id")])
})

