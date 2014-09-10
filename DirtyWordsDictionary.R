# Scrape Dirty Words list

url <- "http://cltampa.com/dailyloaf/archives/2012/09/16/dirty-sex-dictionary?showFullText=true"

library(scrapeR)

site <- scrape(url)[[1]]

defs <- tolower(sapply(getNodeSet(site, "//*[@id='BlogsBlogPost']/div[1]/div[2]/p"), xmlValue))
defs <- subset(defs, nchar(defs)>3)
defs <- defs[!grepl("jump to:", defs)]

sex.synonyms <- defs[grepl("sex: ask your mother.", defs)]




guy.stuff <- gsub("^\\s|(\\.\\.\\.)", "", paste(unlist(str_split(gsub("^[[:alpha:]]:", "", defs[grepl("^[[:alpha:]]:", defs)]), pattern = ", ")), ": male masturbation", sep=""))
defs <- defs[!grepl("^[[:alpha:]]:", defs)]

defs[!grepl("^[[:alpha:]]:", defs)]

problem.entries <- subset(defs, str_count(defs, ":")>1)

# Fix entries with synonyms at the end by repeating the definition and adding a new line for each synonym. (Not perfect, but it'll do)
synonyms <- problem.entries[grepl("(synonym(s)?(y)?:)|(variation(s)?:)|(related to:)|(similar( to)?:)|(also:)", problem.entries)]
problem.entries <- problem.entries[!grepl("(synonym(s)?(y)?:)|(variation(s)?:)|(related to:)|(similar( to)?:)|(also:)", problem.entries)]

term <- str_split(word(synonyms, sep="(synonym(s)?(y)?:)|(variation(s)?:)|(related to:)|(similar( to)?:)|(also:)", start=-1), ",")
term.def <- str_replace(word(synonyms, sep=":", 2), "(synonym(s)?(y)?)|(variation(s)?)|(related to)|(similar( to)?)|(also)", "")
synonyms.fixed <- gsub("\\.:", ":", unlist(lapply(1:length(term), function(i) paste0(str_replace(unlist(term[i]), "^(\\s)+|\\.$", ""), ":", term.def[i]))))
synonyms.fixed <- synonyms.fixed[!grepl("^:", synonyms.fixed)]
synonyms.fixed <- gsub(":$", "", synonyms.fixed)

# Remove "origins" at the end of definition
origins <- problem.entries[grepl("origins:", problem.entries)]
origins <- word(origins, sep="origin")
problem.entries <- problem.entries[!grepl("origins:", problem.entries)]

# Remove other addenda
problem.entries <- word(problem.entries, sep="usage:")
problem.entries <- word(problem.entries, sep="antonym(s)?:")
problem.entries <- word(problem.entries, sep="pop-culture reference(s)?:")
problem.entries <- word(problem.entries, sep="quote:")
problem.entries <- word(problem.entries, sep="see:")
problem.entries <- word(problem.entries, sep=": see")
problem.entries <- word(problem.entries, sep="history: ")

# Split definitions that aren't separated
notseparated <- problem.entries[grepl("\\.\\w*( \\w*)?:", problem.entries)]
problem.entries <- problem.entries[!grepl("\\.\\w*( \\w*)?:", problem.entries)]

notseparated <- c(word(notseparated, sep="\\.\\w*( \\w*)?:"), gsub("^\\.", "", str_extract(notseparated, "\\.\\w*( \\w*)?:.*$")))


