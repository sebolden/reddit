# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(igraph)
library(viridis)
library(ggrepel)
library(ggthemes)
library(hrbrthemes)
library(plotly)
library(htmlwidgets)


# DATA --------------------------------------------------------------------
data <- makeBabyDF(getcsv('trp_nw_full.csv'))
d <- getcsv('clean_comments.csv') # FOR TEXT/COSINE ONLY
txt<- getcsv('text_files_combined.csv')
linx <- getLinks(data)
fname <- unique(linx$subreddit)

# NETWORK MEASURES --------------------------------------------------------
" FINAL CSV: 'network_data_subreddit_year.csv'"

mod <- getModByYear()
deg <- getDegreeByYear()
mod$year <- as.numeric(mod$year)
deg$year <- as.numeric(deg$year)
nw <- mod %>% left_join(deg, by=c('year', 'subreddit'))
nw <- nw %>% select(year, everything()) %>% select(subreddit, everything())
linx$mod=0
linx$mod[linx$is_mod=="moderator"] <- 1
linx$admin=0
linx$admin[linx$is_mod=="admin"] <- 1
wt <- linx %>% group_by(subreddit,year) %>% summarise(total.comments=n())
substats <- linx %>% group_by(subreddit, year) %>% summarise(avg.score=mean(score), total.controv=sum(controversiality), total.submitter=sum(is_submitter), total.locked=sum(locked), total.mod=sum(mod), total.admin=sum(admin))
nw <- nw %>% inner_join(substats, by=c('subreddit', 'year'))
nw <- nw %>% inner_join(wt, by=c('subreddit', 'year'))
nw$sub_group[nw$subreddit %in% blue] <- 'blue'
nw$sub_group[nw$subreddit %in% red] <- 'red'
write.csv(nw, 'network_data_subreddit_year.csv', row.names=F)

# NETWORK MOVEMENT  -------------------------------------------------------
" FINAL CSVS: 'movement_by_author.csv' and 'membership_movement.csv'"
membership <- getMembership(data)
auth.mvmt <- getAuthorMovement(membership)
auth.mvmt <- auth.mvmt[auth.mvmt$date != "NA - NA",]


# NETWORK CROSSOVER -------------------------------------------------------
" FINAL CSV: 'network_crossover.csv'"
xoverDF <- getCrossoverDF(data)
xoverMA <- getCrossoverMatrix(xoverDF) 


# COSINE ANALYSIS ---------------------------------------------------------
d <- d %>% select(body, id, subreddit, year, score, author)
x <- subset_quantiles(d,.98,.02,.75)
x <- combine_and_clean(x, txt)

# create a document term matrix
dtm <- CreateDtm(doc_vec = x$text, # character vector of documents
                 doc_names = x$id, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")),
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system

tf_mat <- TermDocFreq(dtm)
tf_bigrams <- tf_mat[ stringr::str_detect(tf_mat$term, "_") , ]

dtm <- dtm[ , ! stringr::str_detect(colnames(dtm),"women|men|dont|people|im|youre|time|man|woman|make|good|sex|life|things|doesnt|shit|guy|work|feel|thing|shes|girl|girls|fuck|back|male|lot|isnt|didnt|point|ive|years|find|give|fucking|social|love|relationship|person|bad|theyre|day|world|wife|part|fact|â|long|made|friends|problem|post|trp|hes|hard|arent|put|talk|wrong|reason")]

dtm <- dtm[ , ! stringr::str_detect(colnames(dtm),"red_pill|men_women|women_men|women_dont|dont_care|â_â|nice_guy|men_dont|years_ago|people_dont|blue_pill|doesnt_matter|women_women|lift_lift|dont_understand|social_media|shit_test|dont_give|gender_roles|men_men|ðÿ_ðÿ|shut_shut|lol_lol|blah_blah") ]

tf_mat <- TermDocFreq(dtm)
tf_bigrams <- tf_mat[ stringr::str_detect(tf_mat$term, "_") , ]

tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
tfidf <- t(tfidf)
csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
csim <- csim %*% t(csim)
csim <- as.dist(1 - csim)

cdist <- as.matrix(csim)
cdist <- as.tibble(cdist)

comment_ids <- colnames(cdist)
text_ids <- txt$doc_id

t_ids <- comment_ids[comment_ids %in% text_ids]

cdist$id <- colnames(cdist)
cdist <- cdist[cdist$id %in% ids,]
cdist <- as.data.frame(t(cdist))
cdist <- cdist[-9720,]
cdist$id <- comment_ids
cidst <- cdist %>% inner_join(x, by="id")
cidst <- cd[cd$quantile_group!="canon",]
sc <- data %>% select(score,id)
cidst <- cidst %>% inner_join(sc, by="id")
commentid <- cdist$id
cd <- sapply(cdist[,1:211], as.character )
cd <- as.tibble(cd)
cd <- sapply(cd[,1:211], as.numeric)
cd <- as.tibble(cd)
cd$row.avg <- rowMeans(cd[,1:211])
colnames(cd) <- t_ids
cd$id <- cdist$id
cd$score <- cdist$score
cd$subreddit <- cdist$subreddit
cd$quantile_group <- cdist$quantile_group
cd$author <- cdist$author
cd$year <- cdist$year
#write.csv(cd, "cosine_test.csv", row.names=F)

