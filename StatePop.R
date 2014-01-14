library(XML)
url <- "http://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population"
page <- scrape(url)[[1]]
# read all tables
table <- readHTMLTable(page)
# get correct table
tablenum <- as.numeric(which(lapply(lapply(table, dim), length)>0))[1]
table <- table[[tablenum]]

table <- apply(table, 2, function(i) gsub("(^ +)|( +$)", "", gsub("^[0]{1,6}", "", gsub("%", "", gsub("[", "", gsub("[[[:digit:]]]", "", gsub("[[:digit:]]{6}[0]{13}", "", gsub("—", "", gsub("!", "", gsub(",", "", i, fixed=TRUE), fixed=TRUE), fixed=TRUE))), fixed=TRUE)))))

us.table<- data.frame(table[,3], apply(table[,c(1:2, 4:12)], 2, as.numeric), stringsAsFactors=FALSE)

us.table[,1] <- gsub(substr(us.table[1,1], 1, 1), "", us.table[,1])
us.table <- us.table[1:52,c(1, 4, 12)]
names(us.table) <- c("locale", "pop", "percent.pop")
us.table$percent.pop <- us.table$percent.pop/sum(us.table$percent.pop)

url <- "http://en.wikipedia.org/wiki/List_of_Canadian_provinces_and_territories_by_population"
page <- scrape(url)[[1]]

# read all tables
table <- readHTMLTable(page)
# get correct table
tablenum <- as.numeric(which(lapply(lapply(table, dim), length)>0))[1]
table <- table[[tablenum]]

table <- apply(table, 2, function(i) gsub("(^ +)|( +$)", "", gsub("^[0]{1,6}", "", gsub("%", "", gsub("[", "", gsub("[[[:digit:]]]", "", gsub("[[:digit:]]{10,12}([0]{6,7}|[9]{6,8})", "", gsub("—", "", gsub("!", "", gsub(",", "", i, fixed=TRUE), fixed=TRUE), fixed=TRUE))), fixed=TRUE)))))

cn.table<- suppressWarnings(data.frame(table[,2], apply(table[,c(1, 3:10)], 2, as.numeric), stringsAsFactors=FALSE)[-14,])
cn.table <- cn.table[, c(1, 4, 10)]
names(cn.table) <- c("locale", "percent.pop", "pop")
cn.table$percent.pop <- cn.table$percent.pop/sum(cn.table$percent.pop)

table <- rbind.fill(us.table, cn.table)
table$percent.pop <- table$pop/sum(table$pop)

craigslistURLs$weight <- unlist(llply(craigslistURLs$state, function(x) {temp <- which(table$locale%in%x);  if(length(temp)>0)  rep(table$percent.pop[temp]/length(x), length(x)) else rep(.001, length(x))}))

craigslistURLs$weight <- craigslistURLs$weight/sum(craigslistURLs$weight)
