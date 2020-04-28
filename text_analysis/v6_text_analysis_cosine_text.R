# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(tm)
library(factoextra)
library(ggthemes)
library(viridis)
library(textmineR)
library(ggrepel)

# SOURCE(s) ---------------------------------------------------------------
# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html

# DATA --------------------------------------------------------------------
data <- getcsv('clean_comments.csv')
txt<- getcsv('text_files_combined.csv')

colnames(data)
d <- data %>% select(body, id, subreddit, year, score, author)

# PREP TEXT ---------------------------------------------------------------

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
head(tf_mat[ order(tf_mat$term_freq, decreasing = TRUE) , ], 20)
tf_bigrams <- tf_mat[ stringr::str_detect(tf_mat$term, "_") , ]
head(tf_bigrams[ order(tf_bigrams$term_freq, decreasing = TRUE) , ], 20)

# remove common ngrams from the DTM
dtm <- dtm[ , ! stringr::str_detect(colnames(dtm),"women|men|dont|people|im|youre|time|man|woman|make|good|sex|life|things|doesnt|shit|guy|work|feel|thing|shes|girl|girls|fuck|back|male|lot|isnt|didnt|point|ive|years|find|give|fucking|social|love|relationship|person|bad|theyre|day|world|wife|part|fact|â|long|made|friends|problem|post|trp|hes|hard|arent|put|talk|wrong|reason")]
# remove common bigrams from the DTM
dtm <- dtm[ , ! stringr::str_detect(colnames(dtm),"red_pill|men_women|women_men|women_dont|dont_care|â_â|nice_guy|men_dont|years_ago|people_dont|blue_pill|doesnt_matter|women_women|lift_lift|dont_understand|social_media|shit_test|dont_give|gender_roles|men_men|ðÿ_ðÿ|shut_shut|lol_lol|blah_blah") ]

## UPDATED
tf_mat <- TermDocFreq(dtm)
head(tf_mat[ order(tf_mat$term_freq, decreasing = TRUE) , ], 20)
tf_bigrams <- tf_mat[ stringr::str_detect(tf_mat$term, "_") , ]
head(tf_bigrams[ order(tf_bigrams$term_freq, decreasing = TRUE) , ], 20)
rm(tf_bigrams)


tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
tfidf <- t(tfidf)
csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
csim <- csim %*% t(csim)
rm(tfidf,tf_mat)
rm(commentid,i,name,tmp1)
csim <- as.dist(1 - csim)
# SUBSET STUFF ------------------------------------------------------------
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
#rm(comment_ids, comment_ids_new, t_ids, text_ids,sc)
#rm(csim,ids,idsc,tf_mat,tfidf, dtm,cdist,cc,sc)
#write.csv(cd, "cosine_test.csv", row.names=F)

cd <- getGroups(cd)
cd$logscore <- cd$score
cd$logscore[cd$logscore<1] <- .5
myc <- getColors()

cd$class <- "none"
cd$class[cd$quantile_group=="high" & cd$co=="red"] <- 'red_high'
cd$class[cd$quantile_group=="high" & cd$co=="blue"] <- 'blue_high'
cd$class[cd$quantile_group=="low" & cd$co=="red"] <- 'red_low'
cd$class[cd$quantile_group=="low" & cd$co=="blue"] <- 'blue_low'
cd$class <- as.factor(cd$class)
unique(cd$class)

# STATS TESTS -------------------------------------------------------------
options(scipen=999)
m <- getCorr(cd)
m <- m %>% select(class, everything())
View(m)

# COSINE ANALYSIS ---------------------------------------------------------
myc <- c(  "red_high" = "#ff6361"
         , "red_low" = "tomato4"
         , "blue_high" = "#00abb6"
         , "blue_low" = "#003f5c")


m %>% ggplot(aes(fill=class)) +
  geom_histogram(aes(score, alpha=.5)) +
  scale_fill_manual(values=myc) +
  facet_grid(~class) +
  geom_vline(xintercept=0, color="black",lwd=1) +
  theme_fivethirtyeight() +
  ggtitle("correlation btwn comment scores & canon similarity")

cd %>% ggplot(aes(color=class)) +
  geom_jitter(aes(score, row.avg), alpha=.8)+
  #geom_vline(xintercept=0,lwd=2) +
  scale_y_log10() +
  facet_grid(~class) +
  scale_color_manual(values=myc) 
  

cd %>% filter(year!=2014) %>% 
  group_by(author,year,class) %>% 
  summarise(ravg=mean(row.avg), savg=mean(logscore)) %>% 
  ggplot(aes(color=class)) +
    geom_jitter(aes(year,ravg), alpha=.8)+
    #geom_vline(xintercept=0,lwd=2) +
    scale_y_log10() +
    #scale_x_log10() +
    #facet_grid(~class) +
    scale_color_manual(values=myc) 
    

  
cd %>% filter(year!=2014) %>% 
  group_by(subreddit,year,class) %>% 
  summarise(ravg=mean(row.avg), savg=mean(logscore)) %>% 
  ggplot(aes(color=class)) +
    geom_jitter(aes(year,ravg), alpha=.8,size=3)+
    geom_label_repel(aes(year,ravg,label=subreddit), size=3) +
    #geom_vline(xintercept=0,lwd=2) +
    scale_y_log10() +
    #scale_x_log10() +
    #facet_grid(~class) +
    scale_color_manual(values=myc) 
  
  cd %>% filter(year!=2014) %>% 
  group_by(subreddit,class) %>% 
  summarise(ravg=mean(row.avg), savg=mean(logscore)) %>% 
  ggplot(aes(color=class)) +
    geom_jitter(aes(savg,ravg), alpha=.8,size=3)+
    geom_label_repel(aes(savg,ravg,label=subreddit), size=3) +
    #geom_vline(xintercept=0,lwd=2) +
    scale_y_log10() +
    scale_x_log10() +
    #facet_grid(~class) +
    scale_color_manual(values=myc) 
  
  cd %>% filter(year!=2014) %>% 
  group_by(subreddit,class) %>% 
  summarise(ravg=mean(row.avg), savg=mean(logscore)) %>% 
  ggplot(aes(color=class)) +
    scale_y_log10() +
    scale_x_log10() +
    geom_jitter(aes(savg,ravg), alpha=.8,size=3)+
    geom_hline(aes(yintercept=median(ravg))) +
    scale_color_manual(values=myc) +
    geom_label_repel(aes(savg,ravg,label=subreddit), size=3) +
    ggtitle("Average score/canon similarity by subreddit (logx10, logy10)", subtitle="horizontal line is median row average")
    
    #geom_vline(xintercept=0,lwd=2) +
    #facet_grid(~class) +

median(cd$score)
  
  cd %>% filter(year!=2014) %>% 
  group_by(subreddit,class,year) %>% 
  summarise(ravg=mean(row.avg), savg=mean(logscore)) %>% 
  ggplot(aes(color=class)) +
    geom_point(aes(year,ravg), alpha=.8,size=3)+
    geom_line(aes(year,ravg,group=subreddit,color=class)) +
    scale_color_manual(values=myc) +
    facet_wrap(~subreddit) +
    ggtitle("average canon similarity by subreddit/year")
    #geom_label_repel(aes(savg,ravg,label=class), size=3) 
    #geom_vline(xintercept=0,lwd=2) +
    #scale_y_log10() +
    #scale_x_log10() +
    #facet_grid(~class) +
    
  myc
#write.csv(cd, "text_similarity.csv", row.names=F)










# HIERARCHICAL CLUSTERING -------------------------------------------------
hc <- hclust(csim, "ward.D")

p_words <- colSums(dtm) / sum(dtm)
clustering <- cutree(hc, 50)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]})



# create a summary table of the top 5 words defining each cluster
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")}),
                              stringsAsFactors = FALSE)

cluster_summary


# plot a word cloud of one cluster as an example
wordcloud::wordcloud(words = names(cluster_words[[ 5 ]]), 
                     freq = cluster_words[[ 5 ]], 
                     max.words = 50, 
                     random.order = FALSE, 
                     main = "Top words in cluster 100")

x$cluster <- clustering
cluster_summary
x <- x %>% inner_join(cluster_summary, by="cluster")
xc <- x %>% inner_join(sc, by="id")
xd <- x %>% left_join(xc, by="id")
xd <- xd %>% select(score, everything())
x <- x %>% select(-size)

#rm(cluster_summary, cluster_words, dtm, cdist, clustering, p_words)


colnames(x) <- c("score",'id','subreddit','quantile_group','year','text','author','cluster','top_words','co')
x$class <- "canon"
x$class[x$quantile_group=="high" & x$co=="red"] <- 'red_high'
x$class[x$quantile_group=="high" & x$co=="blue"] <- 'blue_high'
x$class[x$quantile_group=="low" & x$co=="red"] <- 'red_low'
x$class[x$quantile_group=="low" & x$co=="blue"] <- 'blue_low'
x$class <- as.factor(x$class)
#nrow(x[x$class=="canon",])
#write.csv(x, "subset_with_clusters.csv", row.names=F)














































