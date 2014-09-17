library(scrapeR)

# Site URL
url <- "https://www.craigslist.org/about/sites#US"

# In browser, right click -> "inspect element"
# We notice that <div class="colmask"> contains the entire US list of Craigslists

# We need to read the HTML into R
craigslists <- scrape(url)[[1]]
# Let's select the various regions, each within their own <div class="colmask"></div> tags
regions <- getNodeSet(craigslists, '//*[@class="body"]/div[@class="colmask"]')
# ok, so there are 7 regions: US, Canada, Europe, Asia/Pacific/Middle East, Oceania, Latin America/Carribean, Africa.

# get region names
regionNames <- sapply(getNodeSet(craigslists, '//*[@class="body"]/h1/a'), xmlAttrs, "name")
# get HTML sections corresponding to each region
regionList <- lapply(1:length(regions), function(i) getNodeSet(craigslists, path=paste('//*[@id="pagecontainer"]/section/div[@class="colmask"][', i, ']/div', sep="")))

# A function to aggregate region, state, city, and CL link for each city into a data frame
getSites <- function(regionName, regionInfo){
  # get information from each column
  boxes <- unlist(lapply(regionInfo, xmlChildren))
  
  # Need to differentiate <h4> state names from <ul> lists of links
  types <- names(boxes)
  
  # get a list of all state names
  states <- sapply(boxes[which(types=="h4")], xmlValue)
  
  # get a list of all state CL lists
  cls <- boxes[which(types=="ul")]
  
  # Need to get lists of links corresponding to each state name
  links <- ldply(1:length(cls), function(idx){
    i <- cls[[idx]]
    
    # get everything in the list
    stateCLs <- xmlChildren(i)
    
    # remove any node that isn't from a <li> tag
    stateCLs <- stateCLs[which(names(stateCLs)=="li")]
    
    # check to make sure that our list has any values left
    if(length(stateCLs)==0) return(data.frame())
    
    # convert information from each <li> tag into a data frame
    cities <- ldply(stateCLs, function(j) {
      data.frame(region=regionName,
                 state=states[idx],
                 city=as.character(xmlValue(j)), 
                 link=as.character(xmlAttrs(xmlChildren(j)$a)))
    })
      
    
    return(cities[,-which(names(cities)==".id")])
  })

  
  return(links)
}



# Try it on the US for now
craigslistURLs <- getSites(regionName=regionNames[1], regionInfo=regionList[[1]])

# Our function also works for Canada
craigslistURLs <- rbind(getSites(regionName=regionNames[1], regionInfo=regionList[[1]]), 
                        getSites(regionName=regionNames[2], regionInfo=regionList[[2]]))

# Replace French Canadian CL links with their english counterparts:
craigslistURLs$link <- gsub(".craigslist.ca", ".en.craigslist.ca", craigslistURLs$link)

# Add in weights to sample by population of each state
source("StatePop.R")
write.csv(craigslistURLs, "CraigslistURLs.csv", row.names=FALSE)
