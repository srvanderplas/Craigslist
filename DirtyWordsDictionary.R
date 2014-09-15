# Scrape Dirty Words list

url <- "http://cltampa.com/dailyloaf/archives/2012/09/16/dirty-sex-dictionary?showFullText=true"

library(scrapeR)
library(stringr)

site <- scrape(url)[[1]]

defs <- tolower(sapply(getNodeSet(site, "//*[@id='BlogsBlogPost']/div[1]/div[2]/p"), xmlValue))
defs <- subset(defs, nchar(defs)>3)
defs <- defs[!grepl("jump to:", defs)]

sex.synonyms <- defs[grepl("sex: ask your mother.", defs)]
defs[grepl("sex: ask your mother.", defs)] <- "sex: ask your mother. apparently she is an expert on the subject."
sex.synonyms <- gsub("sex: ask your mother. apparently she is an expert on the subject. sex.synonyms: ", "", sex.synonyms)
sex.synonyms <- str_split(sex.synonyms, pattern="(([[:alpha:]]|#): )|(, )")[[1]]
sex.synonyms <- sex.synonyms[!grepl(":", sex.synonyms) & nchar(sex.synonyms)>1]
sex.synonyms <- paste0(sex.synonyms, ": sex")

guy.stuff <- gsub("^\\s|(\\.\\.\\.)", "", paste(unlist(str_split(gsub("^[[:alpha:]]:", "", defs[grepl("^[[:alpha:]]:", defs)]), pattern = ", ")), ": male masturbation", sep=""))
defs <- defs[!grepl("^[[:alpha:]]:", defs)]

girl.stuff <- paste0(str_trim(str_split(word(defs[grepl("—female ", defs)], sep=":", 2), ",")[[1]]), ": ", "female masturbation")
defs <- defs[!grepl("—female ", defs)]


problem.entries <- subset(defs, str_count(defs, ":")>1)

# Fix entries with synonyms at the end by repeating the definition and adding a new line for each synonym. (Not perfect, but it'll do)
synonyms <- problem.entries[grepl("(synonym(s)?(y)?:)|(variation(s)?:)|(related to:)|(similar( to)?:)|(also:)", problem.entries)]
problem.entries <- problem.entries[!grepl("(synonym(s)?(y)?:)|(variation(s)?:)|(related to:)|(similar( to)?:)|(also:)", problem.entries)]

term <- str_split(word(synonyms, sep="(synonym(s)?(y)?:)|(variation(s)?:)|(related to:)|(similar( to)?:)|(also:)", start=-1), ",")
term.def <- str_replace(word(synonyms, sep=":", 2), "(synonym(s)?(y)?)|(variation(s)?)|(related to)|(similar( to)?)|(also)", "")
synonyms.fixed <- gsub("\\.:", ":", unlist(lapply(1:length(term), function(i) paste0(str_replace(unlist(term[i]), "^(\\s)+|\\.$", ""), ":", term.def[i]))))
synonyms.fixed <- synonyms.fixed[!grepl("^:", synonyms.fixed)]
synonyms.fixed <- gsub(":$", "", synonyms.fixed)
rm(term, term.def)

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

defs2 <- c(defs, synonyms, synonyms.fixed, sex.synonyms, girl.stuff, guy.stuff, origins, problem.entries)
rm(list=ls()[ls()!="defs2"])

df <- data.frame(word = word(defs2, sep=":", 1))
df$definition <- str_replace(defs2, paste0(df$word, ":"), "")
