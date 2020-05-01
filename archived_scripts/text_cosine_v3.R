# SOURCE(S) ---------------------------------------------------------------
# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(tm)
library(factoextra)
library(ggthemes)
library(viridis)
library(textmineR)
library(corrplot)

# DATA --------------------------------------------------------------------
data <- getcsv('trp_nw_full.csv')
txt <- getcsv('text_files_combined.csv')

# PREP TEXT ---------------------------------------------------------------
x <- subset_quantiles(data,.95,.05,.15)
x <- combine_and_clean(x, txt)

# create a document term matrix 
dtm <- CreateDtm(doc_vec = x$text, # character vector of documents
                 doc_names = x$id, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system


# GET COSINE --------------------------------------------------------------
tf_mat <- TermDocFreq(dtm)
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
csim <- csim %*% t(csim)

cdist <- as.matrix(1 - csim)
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

# COSINE ANALYSIS ---------------------------------------------------------
cd <- getGroups(cd)
cd$logscore <- cd$score
cd$logscore[cd$logscore<1] <- .5
myc <- getColors()

ggplot(cd, aes(row.avg,logscore, color=co)) +
  geom_jitter(alpha=.5) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values=myc) +
  theme_fivethirtyeight()



ggplot(cd, aes(row.avg,logscore, color=co)) +
  geom_jitter(alpha=.5) +
  #scale_y_log10() +
  #scale_x_log10() +
  scale_color_manual(values=myc) +
  theme_fivethirtyeight() +
  #facet_wrap(~year)
  facet_wrap(~co)


ggplot(cd, aes(row.avg,logscore, color=co)) +
  geom_jitter(alpha=.5) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values=myc) +
  theme_fivethirtyeight() +
  #facet_wrap(~year)
  facet_wrap(~co)

colnames(cd[,1:11])
ggplot(cd, aes(ag1.txt,logscore, color=co)) +
  geom_jitter(alpha=.4) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values=myc) +
#  facet_wrap(~co) +
  theme_fivethirtyeight() 

ggplot(cd, aes(trp_57.txt,logscore, color=co)) +
  geom_jitter(alpha=.4) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values=myc) +
  facet_wrap(~co) +
  theme_fivethirtyeight() 




options(scipen=999)
m <- cd %>% select(score,everything())
m <- cds[,1:212]
m <- as.data.frame(m)
m <- as.data.frame(cor(m))
m <- m[-1,]

red_high <- cd[cd$co=="red",]
red_high <- red_high[red_high$quantile_group=="high",]
red_high <- red_high %>% select(score, everything())
red_high <- red_high[,1:212]
red_high <- as.data.frame(cor(red_high))
red_high <- red_high[-1,]

red_low <- cd[cd$co=="red",]
red_low <- red_low[red_low$quantile_group=="low",]
red_low <- red_low %>% select(score, everything())
red_low <- red_low[,1:212]
red_low <- as.data.frame(cor(red_low))
red_low <- red_low[-1,]


blue_high <- cd[cd$co=="blue",]
blue_high <- blue_high[blue_high$quantile_group=="high",]
blue_high <- blue_high %>% select(score, everything())
blue_high <- blue_high[,1:212]
blue_high <- as.data.frame(cor(blue_high))
blue_high <- blue_high[-1,]

blue_low <- cd[cd$co=="blue",]
blue_low <- blue_low[blue_low$quantile_group=="low",]
blue_low <- blue_low %>% select(score, everything())
blue_low <- blue_low[,1:212]
blue_low <- as.data.frame(cor(blue_low))
blue_low <- blue_low[-1,]

scores <- data.frame(red_high$score, red_low$score, blue_high$score, blue_low$score)
colnames(scores) <- c("red_high", "red_low", "blue_high", "blue_low")

scor %>% ggplot(aes()) +
  geom_histogram(aes(blue_corr), fill="#003f5c", alpha=.5)+
  geom_vline(xintercept=0,lwd=2)

 # geom_histogram(aes(red_corr), fill="#ff6361", alpha=.5) +
boxplot(scores$red_high, scores$red_low, scores$blue_high, scores$blue_low)
abline(h=0, col="tomato3",lwd=2)

sc <- as.matrix(scores)
heatmap(sc,keep.dendro=F)
??heatmap()
library("PerformanceAnalytics")
chart.Correlation(scores, histogram=TRUE, pch=19)
chart.Correlation(cds[1:10,])

colnames(cds)
omfg <- cd %>% select(score, row.avg,year,quantile_group,co)
high <- omfg  %>%  filter(quantile_group=="high")
low <- omfg %>% filter(quantile_group=="low")

rh <- high %>% filter(co=="red")
bh <- high %>% filter(co=="blue")
rl <- low %>% filter(co=="red")
bl <- low %>% filter(co=="blue")
omfg$quantile_group <- as.factor(omfg$quantile_group)
omfg$co <- as.factor(omfg$co)


chisq.test(omfg$quantile_group,omfg$row.avg)
chisq.test(omfg$co,omfg$score)
chisq.test(cd$score,cd$quantile_group)
chisq.test(cd$score,cd$row.avg)
chisq.test(cd$score,cd$trpw_24.txt)

test <- chisq.test(cd$score,cd$trpw_20.txt)
class(test)
chisq.test(cd$score, cd[cd$[i]])
df <- tibble(subq=0, subco=0, txt="ok")
for (i in 2:212) {
  name <- names(cd)[i]
  tmp <- cd %>% select(co, quantile_group, name)
  colnames(tmp) <- c("co", "quantile_group", "txt")
  tmp1 <- chisq.test(tmp$co, tmp$txt)
  tmp1 <- tmp1$p.value
  tmp2 <- chisq.test(tmp$quantile_group, tmp$txt)
  tmp2 <- tmp2$p.value
  df[nrow(df)+1,] <- c(tmp1,tmp2,name)}




# HIERARCHICAL CLUSTERING -------------------------------------------------
#R’s various clustering functions work with distances, not similarities. We convert cosine similarity to cosine distance by subtracting it from 1. This works because cosine similarity is bound between 0 and 1. While we are at it, we’ll convert the matrix to a dist object.

cdist <- as.dist(1 - csim)
rm(csim,tfidf,tf_mat,dtm)

#The last step is clustering. There are many clustering algorithms out there. My preference is agglomerative hierarchical clustering using Ward’s method as the merge rule. Compared to other methods, such as k-means, hierarchical clustering is computationally inexpensive.

#In the example below, I choose to cut the tree at 10 clusters. This is a somewhat arbitrary choice. I often prefer to use the silhouette coefficient. You can read about this method here. Performing this is an exercise I’ll leave to the reader.

hc <- hclust(cdist, "ward.D")

p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]})

# create a summary table of the top 5 words defining each cluster
clustering <- cutree(hc, 50)
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")}),
                              stringsAsFactors = FALSE)

cluster_summary

saveRDS(hc, file="hierarchical_clustering.RDS")
saveRDS(cluster_summary, "hierarchical_clustering_50c.RDS")

# plot a word cloud of one cluster as an example
wordcloud::wordcloud(words = names(cluster_words[[ 5 ]]), 
                     freq = cluster_words[[ 5 ]], 
                     max.words = 50, 
                     random.order = FALSE, 
                     colors = c("red", "yellow", "blue"),
                     main = "Top words in cluster 100")

x$cluster <- clustering
#write.csv(x, "subset_with_clusters.csv", row.names=F)
#rm(cluster_summary, cluster_words, dtm, cdist, clustering, p_words)
# colnames(x)
 xwt$subcol[xwt$subreddit=="bropill"] <- "blue"
 xwt$subcol[xwt$subreddit=="exredpill"] <- "blue"
 xwt$subcol[xwt$subreddit=="FeMRADebates"] <- "blue"
 xwt$subcol[xwt$subreddit=="MensLib"] <- "blue"
 xwt$subcol[xwt$subreddit=="PunchingMorpheus"] <- "blue"
 xwt$subcol[xwt$subreddit=="socialjustice101"] <- "blue"
 xwt$subcol[xwt$subreddit=="WitchesVsPatriarchy"] <- "blue"
 xwt$subcol[xwt$subreddit=="asktrp"] <- "red"
 xwt$subcol[xwt$subreddit=="RedPillParenting"] <- "red"
 xwt$subcol[xwt$subreddit=="RedPillWomen"] <- "red"
 xwt$subcol[xwt$subreddit=="RedPillWives"] <- "red"
 xwt$subcol[xwt$subreddit=="ThankTRP"] <- "red"
 xwt$subcol[xwt$subreddit=="TheRedPill"] <- "red"
 xwt$subcol[xwt$subreddit=="canon"] <- "canon"



# PLOT --------------------------------------------------------------------
xwt <- getcsv('cluster_with_names.csv')
x <- getcsv('cluster_with_SCORES.csv')

getColors()
myc <- c("blue" = "#003f5c", "red"="#ff6361", "canon"="tomato4")

ggplot(xwt, aes(cluster, fill=subcol)) +
  geom_histogram(stat='identity') +
  scale_fill_manual(values=myc)

ggplot(x,aes(year, score, color=subcol)) +
    geom_jitter(alpha=.4) +
    geom_text(aes(label=cluster),hjust=0, vjust=0) +
    scale_color_manual(values=myc) 

x <- x %>% left_join(cnames,by="cluster")
x$name <- as.factor(x$name)

c10 <- x[x$cluster<11,]
c20 <- x[x$cluster<21 & x$cluster>10,]
c30 <- x[x$cluster<31 & x$cluster>20,]
c40 <- x[x$cluster<41 & x$cluster>30,]
c50 <- x[x$cluster>41,]
#colnames(x)

# did this for all 5 DFs to get the combined plot
ggplot(c10,aes(year, score, fill=subcol)) +
  geom_bar(position='dodge', stat='identity') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("subreddit group & cluster: score ranges over time", subtitle="clusters 1-10")
rm(c10,c20,c30,c40,c50)


xwt$quantile_group[xwt$quantile_group=="not_applicable"] <- "canon"
c10 <- xwt[xwt$cluster<11,]
c20 <- xwt[xwt$cluster<21 & xwt$cluster>10,]
c30 <- xwt[xwt$cluster<31 & xwt$cluster>20,]
c40 <- xwt[xwt$cluster<41 & xwt$cluster>30,]
c50 <- xwt[xwt$cluster>41,]

ggplot(c10,aes(quantile_group, fill=subcol)) +
  geom_bar(position='dodge', stat='count') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("cluster frequency by document and quantile group", subtitle="clusters 1-10")

ggplot(c20,aes(quantile_group, fill=subcol)) +
  geom_bar(position='dodge', stat='count') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("cluster frequency by document and quantile group", subtitle="clusters 11-20")


ggplot(c30,aes(quantile_group, fill=subcol)) +
  geom_bar(position='dodge', stat='count') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("cluster frequency by document and quantile group", subtitle="clusters 21-30")


ggplot(c40,aes(quantile_group, fill=subcol)) +
  geom_bar(position='dodge', stat='count') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("cluster frequency by document and quantile group", subtitle="clusters 31-40")

ggplot(c50,aes(quantile_group, fill=subcol)) +
  geom_bar(position='dodge', stat='count') +
  facet_wrap(~name,nrow=2) +
  theme_fivethirtyeight() +
  scale_fill_manual(values=myc) +
  ggtitle("cluster frequency by document and quantile group", subtitle="clusters 41-50")






















